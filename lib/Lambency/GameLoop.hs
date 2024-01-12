module Lambency.GameLoop (
  GameLoopState, mkLoopState,
  GameLoopConfig, mkLoopConfig,
  runGameLoop
  ) where

--------------------------------------------------------------------------------
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS.Strict
import qualified Control.Wire as W

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Data.Time

import FRP.Netwire.Input.GLFW

import Lambency.ResourceLoader
import Lambency.Sound
import Lambency.Sprite
import Lambency.Texture
import Lambency.Types
import Lambency.GameSession

import System.CPUTime

import Linear
--------------------------------------------------------------------------------

maximumFramerate :: NominalDiffTime
maximumFramerate = fromRational . toRational $ (1.0 / 10.0 :: Double)

-- When we handle actions, only really print logs and play any sounds
-- that may need to start or stop.
handleAction :: OutputAction -> IO ()
handleAction (SoundAction sound cmd) = handleCommand sound cmd
handleAction (LogAction s) = putStrLn s
handleAction (WireframeAction True) = GL.polygonMode GL.$= (GL.Line, GL.Line)
handleAction (WireframeAction False) = GL.polygonMode GL.$= (GL.Fill, GL.Fill)

step :: a -> Game a -> TimeStep ->
        GameMonad (Maybe a, Camera, [Light], Game a)
step go game t = do
  (Right cam, nCamWire) <-
    W.stepWire (getContinuousWire $ mainCamera game) t (Right ())
  (lights, lwires) <-
    collect <$>
    mapM (\w -> W.stepWire w t $ Right ()) (getContinuousWire <$> dynamicLights game)
  (Right result, gameWire) <-
    W.stepWire (getContinuousWire $ gameLogic game) t (Right go)
  case result of
    Nothing -> return (result, cam, lights, newGame nCamWire lwires W.mkEmpty)
    Just x  -> return (x `seq` result, cam, lights, newGame nCamWire lwires gameWire)
  where
    collect :: [(Either e b, GameWire a b)] -> ([b], [GameWire a b])
    collect [] = ([], [])
    collect ((Left _, _) : _) = error "Internal -- Light wire inhibited?"
    collect ((Right obj, wire) : rest) = (obj : objs, wire : wires)
      where
        (objs, wires) = collect rest

    newGame cam lights logic =
      Game { mainCamera = CW cam
           , dynamicLights = CW <$> lights
           , gameLogic = CW logic
           }

data GameLoopConfig = GameLoopConfig {
  gameRenderer :: Renderer,
  simpleQuadSprite :: Sprite,
  glfwInputControl :: Maybe GLFWInputControl,
  windowDimensions :: V2 Int
}

mkLoopConfig :: Renderer -> Maybe GLFW.Window -> IO (GameLoopConfig, IO ())
mkLoopConfig r win' = do
  -- !FIXME! Use fully opaque 'mask' texture that we can change the color and
  -- size for dynamically. This isn't the best way to do this, but it'll work.
  (sprite, unloadSprite) <-
    runResourceLoader r $ createSolidTexture (pure 255)
                      >>= loadStaticSpriteWithMask

  -- Collect the window dimensions. TODO: This should be done every frame so
  -- that we can properly update our UI on state changes. For now, we just tell
  -- GLFW to prevent the user from resizing the window, but that need not be a
  -- restriction.
  (winDims, ictl) <- case win' of
    Just win -> do
      winDims <- uncurry V2 <$> liftIO (GLFW.getWindowSize win)
      ictl <- Just <$> mkInputControl win
      return (winDims, ictl)
    Nothing -> return (V2 0 0, Nothing)

  return (GameLoopConfig r sprite ictl winDims, unloadSprite)

data GameLoopState a = GameLoopState {
  currentGameValue :: a,
  currentGameLogic :: Game a,
  currentGameSession :: GameSession,
  currentPhysicsAccum :: NominalDiffTime,
  lastFramePicoseconds :: Integer
}

mkLoopState :: a -> Game a -> GameLoopState a
mkLoopState initialVal initGame = GameLoopState
    { currentGameValue = initialVal
    , currentGameLogic = initGame
    , currentGameSession = mkGameSession
    , currentPhysicsAccum = toEnum 0
    , lastFramePicoseconds = 0
    }

type GameLoopM a = ReaderT GameLoopConfig (StateT (GameLoopState a) IO)

runLoop :: UTCTime -> GameLoopM a ()
runLoop prevFrameTime = do
  (GameLoopState _ _ _ accumulator _) <- get
  -- Step
  thisFrameTime <- liftIO getCurrentTime
  let newAccum = accumulator + (diffUTCTime thisFrameTime prevFrameTime)
  modify $ \ls -> ls { currentPhysicsAccum = min newAccum maximumFramerate }
  (go, (nextsession, accum), nextGame) <- stepGame

  case go of
    Just gobj -> do
      ls <- get
      put $ GameLoopState gobj nextGame nextsession accum (lastFramePicoseconds ls)
      runLoop thisFrameTime
    Nothing -> return ()

runGameLoop :: GameLoopState a -> GameLoopConfig -> IO ()
runGameLoop st config = do
  curTime <- getCurrentTime
  evalStateT (runReaderT (runLoop curTime) config) st

type TimeStepper = (GameSession, NominalDiffTime)

stepGame :: GameLoopM a (Maybe a, TimeStepper, Game a)
stepGame = do
  (GameLoopState go game session accum _) <- get
  if (accum < physicsDeltaUTC)
    then return (Just go, (session, accum), game)
    else runGame

runGame :: GameLoopM a (Maybe a, TimeStepper, Game a)
runGame = do
  gameLoopConfig <- ask
  gls <- get
  (hasInput, ipt) <- liftIO $ case glfwInputControl gameLoopConfig of
    Just glfwIpt -> getInput glfwIpt >>= (\x -> return (True, x))
    Nothing -> return (False, emptyGLFWState)

  -- Retreive the next time step from our game session
  (ts, nextSess) <- liftIO $ W.stepSession (currentGameSession gls)

  let
    -- The game step is the complete GameMonad computation that
    -- produces four values: Either inhibition or a new game value
    -- A new camera, a list of dynamic lights, and the next simulation
    -- wire
    gameStep = step (currentGameValue gls) (currentGameLogic gls) ts

    -- We need to render if we're going to fall below the physics threshold
    -- on the next frame. This simulates a while loop. If we don't fall
    -- through this threshold, we will perform another physics step before
    -- we finally decide to render.
    accum = currentPhysicsAccum gls

    winDims = windowDimensions gameLoopConfig
    renderTime = lastFramePicoseconds gls
    sprite = simpleQuadSprite gameLoopConfig
    frameConfig = GameConfig (gameRenderer gameLoopConfig) renderTime winDims sprite

  -- This is the meat of the step routine. This calls runRWS on the
  -- main game wire, and uses the results to figure out what needs
  -- to be done.
  ((result, cam, lights, nextGame), newIpt, (actions, renderActs)) <-
      liftIO $ runRWST (nextFrame gameStep) frameConfig ipt

  -- The ReaderT RenderConfig IO program that will do the actual rendering
  let accumRemainder = accum - physicsDeltaUTC
      needsRender = case result of
        Nothing -> False
        Just _ -> accumRemainder < physicsDeltaUTC

  frameTime <-
    if needsRender
    then liftIO $ do
      t <- getCPUTime
      render (gameRenderer gameLoopConfig) lights cam renderActs
      t' <- getCPUTime
      return (t' - t)
    else return renderTime

  _ <- liftIO $ do
    -- Actually do the associated actions
    mapM_ handleAction actions

    -- Poll the input
    when hasInput $ case glfwInputControl gameLoopConfig of
      Just glfwIpt -> pollGLFW newIpt glfwIpt >> return ()
      Nothing -> return ()

  -- If our main wire inhibited, return immediately.
  case result of
    Just obj -> do
      put $ GameLoopState obj nextGame nextSess accumRemainder frameTime
      stepGame
    Nothing -> return (result, (nextSess, accum), nextGame)

{-# LANGUAGE DeriveGeneric #-}
module Lambency (
  initLambency,
  module Lambency.Bounds,
  module Lambency.Camera,
  module Lambency.Font,
  module Lambency.GameObject,
  module Lambency.Light,
  module Lambency.Loaders,  
  module Lambency.Material,

  Mesh,
  genNormalsV3,
  genNormalsTV3,
  genTexCoordsV3,
  genTexCoordsOV3,
  triangle, cube, plane, quad,

  module Lambency.Renderer,
  module Lambency.Shader,
  module Lambency.Sprite,
  Texture, loadTexture, createSolidTexture,
  module Lambency.Transform,
  Camera, CameraType, CameraViewDistance,
  LightType, Light,
  ShaderValue(..), ShaderMap,
  Material,
  Renderer, RenderFlag(..), RenderObject(..),
  OutputAction(..),
  TimeStep,
  Sprite,
  Game(..), GameConfig(..), GameMonad,
  GameWire, ContWire, ResourceLoader, ResourceContext, ResourceContextWire,
  module Lambency.UI,
  module Lambency.Utils,

  RendererType(..),
  run, runOpenGL,
  toggleWireframe,
  module Lambency.Sound
) where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Wire ((.))
import qualified Control.Wire as W

import Data.Time

import GHC.Generics (Generic)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import FRP.Netwire.Input.GLFW

import Lambency.Bounds
import Lambency.Camera
import Lambency.Font
import Lambency.GameObject
import Lambency.GameSession
import Lambency.Light
import Lambency.Loaders
import Lambency.Material
import Lambency.Mesh
import Lambency.Renderer
import Lambency.ResourceLoader
import Lambency.Shader
import Lambency.Sound
import Lambency.Sprite
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.UI
import Lambency.Utils

import Linear

import System.CPUTime
import System.IO
--------------------------------------------------------------------------------

data RendererType
  = RendererType'OpenGL
  deriving(Eq, Ord, Bounded, Enum, Show, Read, Generic)

initLambency :: IO ()
initLambency = do
  putStrLn "Initializing..."
  printInfo GL.vendor "Vendor: "
  printInfo GL.renderer "Renderer: "
  printInfo GL.glVersion "OpenGL Version: "
  printInfo GL.shadingLanguageVersion "GLSL Version: "
  -- (mapM_ putStrLn) =<< (GL.get GL.glExtensions)
  putStrLn "Done initializing..."
  where
    printInfo :: (GL.GettableStateVar String) -> String -> IO ()
    printInfo sv s = GL.get sv >>= putStrLn . (s ++)

errorCallback :: GLFW.Error -> String -> IO()
errorCallback e s = putStrLn $ concat ["GLFW Error: ", show e, " ", s]

makeWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
makeWindow width height title = do
  putStr "Initializing GLFW..."
  r <- GLFW.init
  if not r then ioError (userError "Failed!") else return ()
  putStrLn "Done"

  GLFW.setErrorCallback $ Just errorCallback
  putStr $ "Creating window of size " ++ show (width, height) ++ "..."
  GLFW.windowHint $ GLFW.WindowHint'Samples (Just 4)
  GLFW.windowHint $ GLFW.WindowHint'Resizable False
  jm <- GLFW.createWindow width height title Nothing Nothing
  m <- case jm of
    Nothing -> ioError (userError "Failed!")
    Just m' -> return m'
  putStrLn "Done."

  GLFW.makeContextCurrent (Just m)

  -- Implement the viewport size to be the framebuffer size
  -- in order to properly deal with retina displays...
  -- !FIXME! The user should have some say over this
  (szx, szy) <- GLFW.getFramebufferSize m
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

  -- Initial defaults
  GL.blend GL.$= GL.Enabled
  GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.cullFace GL.$= Just GL.Back
  initLambency
  initSound
  GL.dither GL.$= GL.Disabled

  GL.rowAlignment GL.Unpack GL.$= 1

  GL.get GL.errors >>= mapM_ print

  -- !FIXME! Why is this Maybe?
  return (Just m)

destroyWindow :: Maybe GLFW.Window -> IO ()
destroyWindow m = do
  case m of
    (Just win) -> GLFW.destroyWindow win
    Nothing -> return ()
  GLFW.terminate
  freeSound

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withWindow width height title f = do
  mwin <- makeWindow width height title
  x <- case mwin of
    Just w -> f w
    Nothing -> do
      putStrLn $ concat ["Lambency.hs (withWindow): Could not create window (",
                         show width, ", ", show height, ", ", title, ")"]
      return undefined
  destroyWindow mwin
  return x

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

data GameLoopState a = GameLoopState {
  currentGameValue :: a,
  currentGameLogic :: Game a,
  currentGameSession :: GameSession,
  currentPhysicsAccum :: NominalDiffTime,
  lastFramePicoseconds :: Integer
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

-- TODO: This function uses a  hacky method of quitting the game currently
-- which doesn't release all of the resources associated with the game logic.
-- As such, care should be taken to make sure that you're not actually trying to
-- relaunch GLFW commands within the same window after this function returns.
-- It's OK that we leak resources here since we're usually using this with a
-- call to withWindow...
runWithGLFW :: GLFW.Window -> Renderer -> a -> Game a -> IO ()
runWithGLFW win r initialGameObject (Game cam lights (CW logic)) = do
  oldBuffering <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering

  GLFW.swapInterval 1
  ictl <- mkInputControl win
  curTime <- getCurrentTime

  -- Stick in an initial poll events call...
  GLFW.pollEvents

  -- !FIXME! Use fully opaque 'mask' texture that we can change the color and
  -- size for dynamically. This isn't the best way to do this, but it'll work.
  (sprite, unloadSprite) <-
    runResourceLoader r $ createSolidTexture (pure 255)
                      >>= loadStaticSpriteWithMask

  -- Collect the window dimensions. TODO: This should be done every frame so
  -- that we can properly update our UI on state changes. For now, we just tell
  -- GLFW to prevent the user from resizing the window, but that need not be a
  -- restriction.
  winDims <- uncurry V2 <$> liftIO (GLFW.getWindowSize win)

  let statePrg = runReaderT (runLoop curTime)
               $ GameLoopConfig r sprite (Just ictl) winDims

      -- Amend the game logic such that it actually quits if we hit the little
      -- x in the corner of the window.
      quitLogic = let mkQuitter w = W.mkGen $ \dt x -> do
                        needsQuit <- GameMonad
                                   $ liftIO $ GLFW.windowShouldClose win
                        (res, w') <- W.stepWire w dt (Right x)
                        if needsQuit
                          then return (Right Nothing, mkQuitter w')
                          else return (res, mkQuitter w')
                  in CW $ mkQuitter logic

  evalStateT statePrg $
    GameLoopState
    { currentGameValue = initialGameObject
    , currentGameLogic = Game cam lights quitLogic
    , currentGameSession = mkGameSession
    , currentPhysicsAccum = toEnum 0
    , lastFramePicoseconds = 0
    }

  unloadSprite
  hSetBuffering stdout oldBuffering

run :: RendererType -> Int -> Int -> String -> a -> Game a -> IO ()
run RendererType'OpenGL w h title startObject game =
  withWindow w h title $ \win -> runWithGLFW win (openGLRenderer win) startObject game

runOpenGL :: Int -> Int -> String -> a -> Game a -> IO ()
runOpenGL = run RendererType'OpenGL

toggleWireframe :: Bool -> GameMonad ()
toggleWireframe b = GameMonad $ tell $ ([WireframeAction b], mempty)

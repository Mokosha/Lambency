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

  module Lambency.Render,
  module Lambency.Shader,
  module Lambency.Sprite,
  loadTexture, createSolidTexture, destroyTexture,
  module Lambency.Transform,
  Camera, CameraType, CameraViewDistance,
  LightType, Light,
  ShaderValue(..), ShaderMap,
  Material,
  RenderFlag(..), RenderObject(..),
  OutputAction(..),
  TimeStep,
  Sprite,
  Game(..), GameConfig(..), GameWire, GameMonad,
  module Lambency.UI,
  module Lambency.Utils,

  makeWindow, destroyWindow, withWindow,
  run, loadAndRun, runSimple,
  quitWire,
  toggleWireframe,
  module Lambency.Sound
) where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Wire ((.))
import qualified Control.Wire as W

import Data.Time

import GHC.Float

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW

import Lambency.Bounds
import Lambency.Camera
import Lambency.Font
import Lambency.GameObject
import Lambency.Light
import Lambency.Loaders
import Lambency.Material
import Lambency.Mesh
import Lambency.Render
import Lambency.Shader
import Lambency.Sound
import Lambency.Sprite
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.UI
import Lambency.Utils

import System.CPUTime
import System.Exit
import System.IO
--------------------------------------------------------------------------------

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
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
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

-- The physics framerate in frames per second
physicsDeltaTime :: Double
physicsDeltaTime = 1.0 / 60.0

physicsDeltaUTC :: NominalDiffTime
physicsDeltaUTC = fromRational . toRational $ physicsDeltaTime

maximumFramerate :: NominalDiffTime
maximumFramerate = fromRational . toRational $ (1.0 / 10.0 :: Double)

handleAction :: OutputAction -> IO ()
handleAction (SoundAction sound cmd) = handleCommand sound cmd
handleAction (LogAction s) = putStrLn s
handleAction (WireframeAction True) = GL.polygonMode GL.$= (GL.Line, GL.Line)
handleAction (WireframeAction False) = GL.polygonMode GL.$= (GL.Fill, GL.Fill)

-- When we handle actions, only really print logs and play any sounds
-- that may need to start or stop.
handleActions :: [OutputAction] -> IO ()
handleActions = mapM_ handleAction

step :: a -> Game a -> TimeStep ->
        GameMonad (Either String a, Camera, [Light], Game a)
step go game t = do
  (Right cam, nCamWire) <- W.stepWire (mainCamera game) t (Right ())
  (result, gameWire) <- W.stepWire (gameLogic game) t (Right go)
  lightObjs <- mapM (\w -> W.stepWire w t $ Right ()) (dynamicLights game)
  let (lights, lwires) = collect lightObjs
  return (result, cam, lights, newGame nCamWire lwires gameWire)
    where
      collect :: [(Either e b, GameWire a b)] -> ([b], [GameWire a b])
      collect [] = ([], [])
      collect ((Left _, _) : rest) = collect rest
      collect ((Right obj, wire) : rest) = (obj : objs, wire : wires)
        where
          (objs, wires) = collect rest

      newGame cam lights logic = game {
        mainCamera = cam,
        dynamicLights = lights,
        gameLogic = logic}

data GameLoopConfig = GameLoopConfig {
  simpleQuadSprite :: Sprite,
  glfwInputControl :: GLFWInputControl,
  glfwWin :: GLFW.Window
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
    Right gobj -> do
      ls <- get
      put $ GameLoopState gobj nextGame nextsession accum (lastFramePicoseconds ls)
      needsQuit <- glfwWin <$> ask >>= (liftIO . GLFW.windowShouldClose)
      case needsQuit of
        True -> return ()
        False -> runLoop thisFrameTime
    Left _ -> return ()

type TimeStepper = (GameSession, NominalDiffTime)

stepGame :: GameLoopM a (Either String a, TimeStepper, Game a)
stepGame = do
  (GameLoopState go game session accum _) <- get
  if (accum < physicsDeltaUTC)
    then return (Right go, (session, accum), game)
    else runGame

runGame :: GameLoopM a (Either String a, TimeStepper, Game a)
runGame = do
  gameLoopConfig <- ask
  gls <- get
  ipt <- liftIO $ getInput (glfwInputControl gameLoopConfig)

  -- Retreive the next time step from our game session
  (ts, nextSess) <- liftIO $ W.stepSession (currentGameSession gls)

  -- Collect the window dimensions at the current time.
  winDims <- liftIO $ GLFW.getWindowSize (glfwWin gameLoopConfig)

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
    needsRender = ((accum - physicsDeltaUTC) < physicsDeltaUTC)

    renderTime = lastFramePicoseconds gls
    sprite = simpleQuadSprite gameLoopConfig
    frameConfig = GameConfig renderTime winDims sprite

  -- This is the meat of the step routine. This calls runRWS on the
  -- main game wire, and uses the results to figure out what needs
  -- to be done.
  ((result, cam, lights, nextGame), newIpt, (actions, renderActs)) <-
      liftIO $ runRWST (nextFrame gameStep) frameConfig ipt

  -- The ReaderT RenderConfig IO program that will do the actual rendering
  let renderPrg = performRenderActions lights cam renderActs

  frameTime <-
    if needsRender
    then liftIO $ do
      t <- getCPUTime

      -- !FIXME! This should be moved to the camera...
      GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
      clearBuffers
      () <- evalStateT renderPrg initialRenderState
      GL.flush
      GLFW.swapBuffers (glfwWin gameLoopConfig)

      t' <- getCPUTime

      GL.get GL.errors >>= foldM (\() e -> print e >> exitFailure) ()

      return (t' - t)
    else return renderTime

  _ <- liftIO $ do
    -- Actually do the associated actions
    handleActions actions

    -- Poll the input
    pollGLFW newIpt (glfwInputControl gameLoopConfig)

  -- If our main wire inhibited, return immediately.
  case result of
    Right obj -> do
      put $ GameLoopState obj nextGame nextSess (accum - physicsDeltaUTC) frameTime
      stepGame
    Left _ -> return (result, (nextSess, accum), nextGame)

run :: a -> Game a -> GLFW.Window -> IO ()
run initialGameObject initialGame win = do
  oldBuffering <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering

  GLFW.swapInterval 1
  ictl <- mkInputControl win
  let session = W.countSession (double2Float physicsDeltaTime) W.<*> W.pure ()
  curTime <- getCurrentTime

  -- Stick in an initial poll events call...
  GLFW.pollEvents

  -- !FIXME! Use fully opaque 'mask' texture that we can change the color and
  -- size for dynamically. This isn't the best way to do this, but it'll work.
  sprite <- createSolidTexture (255, 255, 255, 255) >>= loadStaticSpriteWithMask

  let statePrg = runReaderT (runLoop curTime) $ GameLoopConfig sprite ictl win
  evalStateT statePrg $
    GameLoopState initialGameObject initialGame session (toEnum 0) 0

  hSetBuffering stdout oldBuffering

loadAndRun :: a -> IO (Game a) -> GLFW.Window -> IO ()
loadAndRun initialGameObject loadGamePrg win =
  loadGamePrg >>= (\g -> run initialGameObject g win)

runSimple :: Camera -> (Float -> a -> GameMonad a) -> a -> GLFW.Window -> IO ()
runSimple cam f x win = do
  let gameWire = W.mkGen $ \ts obj -> do
        result <- f (W.dtime ts) obj
        return (Right result, gameWire)

  run x (Game (W.pure cam) [] gameWire) win

-- Wire that behaves like the identity wire until the given key
-- is pressed, then inhibits forever.
quitWire :: GLFW.Key -> GameWire a a
quitWire key =
  W.rSwitch W.mkId .
  (W.mkId W.&&& (W.now . W.pure W.mkEmpty . keyPressed key <|> W.never))

toggleWireframe :: Bool -> GameMonad ()
toggleWireframe b = GameMonad $ tell $ ([WireframeAction b], mempty)

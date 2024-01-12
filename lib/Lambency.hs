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
  GameWire, ContWire(..), ResourceLoader, ResourceContext, ResourceContextWire,
  module Lambency.UI,
  module Lambency.Utils,

  RendererType(..),
  run, runOpenGL,
  toggleWireframe,
  module Lambency.Sound
) where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

import Control.Monad (unless)
import Control.Monad.RWS.Strict
import Control.Wire ((.))
import qualified Control.Wire as W

import GHC.Generics (Generic)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Bounds
import Lambency.Camera
import Lambency.Font
import Lambency.GameLoop
import Lambency.GameObject
import Lambency.GameSession
import Lambency.Light
import Lambency.Loaders
import Lambency.Material
import Lambency.Mesh
import Lambency.Renderer
import Lambency.Shader
import Lambency.Sound
import Lambency.Sprite
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.UI
import Lambency.Utils

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
    printInfo :: GL.GettableStateVar String -> String -> IO ()
    printInfo sv s = GL.get sv >>= putStrLn . (s ++)

errorCallback :: GLFW.Error -> String -> IO()
errorCallback e s = putStrLn $ concat ["GLFW Error: ", show e, " ", s]

makeWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
makeWindow width height title = do
  putStr "Initializing GLFW..."
  r <- GLFW.init
  unless r $ ioError (userError "Failed!")
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

  -- Stick in an initial poll events call...
  GLFW.pollEvents

  (config, unloadSprite) <- mkLoopConfig r (Just win)

  let -- Amend the game logic such that it actually quits if we hit the little
      -- x in the corner of the window.
      quitLogic = let mkQuitter w = W.mkGen $ \dt x -> do
                        needsQuit <- GameMonad
                                   $ liftIO $ GLFW.windowShouldClose win
                        (res, w') <- W.stepWire w dt (Right x)
                        if needsQuit
                          then return (Right Nothing, mkQuitter w')
                          else return (res, mkQuitter w')
                  in CW $ mkQuitter logic

      loopState = mkLoopState initialGameObject $ Game cam lights quitLogic

  runGameLoop loopState config

  unloadSprite
  hSetBuffering stdout oldBuffering

run :: RendererType -> Int -> Int -> String -> a -> Game a -> IO ()
run RendererType'OpenGL w h title startObject game =
  withWindow w h title $ \win -> runWithGLFW win (openGLRenderer win) startObject game

runOpenGL :: Int -> Int -> String -> a -> Game a -> IO ()
runOpenGL = run RendererType'OpenGL

toggleWireframe :: Bool -> GameMonad ()
toggleWireframe b = GameMonad $ tell ([WireframeAction b], mempty)

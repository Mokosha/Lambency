module Graphics.UI.Lambency (
  makeWindow,
  destroyWindow,
  run
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency

import Control.Applicative
import Control.Monad (unless, when)
import System.IO

--------------------------------------------------------------------------------

makeWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
makeWindow width height title = do
  r <- GLFW.init
  unless r $ return ()
  m <- GLFW.createWindow width height title Nothing Nothing
  case m of
    Nothing -> return ()
    (Just win) -> GLFW.makeContextCurrent m
  return m

destroyWindow :: Maybe GLFW.Window -> IO ()
destroyWindow m = do
  case m of
    (Just win) -> do
      GLFW.destroyWindow win
    Nothing -> return ()
  GLFW.terminate  

charCallback :: GLFW.Window -> Char -> IO ()
charCallback win c = do
  putStrLn [c]
  hFlush stdout

run :: GLFW.Window -> [ RenderObject ] -> IO ()
run win objs = do
  GLFW.pollEvents
  keyState <- GLFW.getKey win GLFW.Key'Q
  case keyState of
    GLFW.KeyState'Pressed -> GLFW.setWindowShouldClose win True
    _ -> return ()
  GL.clearColor GL.$= GL.Color4 0.0 0.0 0.5 1
  GL.clear [GL.ColorBuffer]
  sequence_ $ render <$> objs
  GL.flush
  GLFW.swapBuffers win
  q <- GLFW.windowShouldClose win
  unless q $ run win objs

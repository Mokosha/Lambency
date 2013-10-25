module Graphics.UI.Lambency (
  makeWindow
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad (unless, when)

--------------------------------------------------------------------------------

makeWindow :: Int -> Int -> String -> IO ()
makeWindow width height title = do
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      Nothing -> return ()
      (Just win) -> do
        GLFW.makeContextCurrent m
        keepWin win
        GLFW.destroyWindow win
    GLFW.terminate
  where
    keepWin w = do
      GLFW.setCharCallback w $ Just $ charCallback
      run w

charCallback :: GLFW.Window -> Char -> IO ()
charCallback win c = putChar c               

run :: GLFW.Window -> IO ()
run win = do
  GLFW.pollEvents
  keyState <- GLFW.getKey win GLFW.Key'Q
  case keyState of
    GLFW.KeyState'Pressed -> GLFW.setWindowShouldClose win True
    _ -> return ()
  GL.clearColor GL.$= GL.Color4 0.0 0.0 0.5 1
  GL.clear [GL.ColorBuffer]
  GL.flush
  GLFW.swapBuffers win
  q <- GLFW.windowShouldClose win
  unless q $ run win

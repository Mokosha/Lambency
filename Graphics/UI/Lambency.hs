module Graphics.UI.Lambency (
  makeWindow,
  destroyWindow,
  run
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.Rendering.Lambency as LR

import Control.Monad (unless)

--------------------------------------------------------------------------------

errorCallback :: GLFW.Error -> String -> IO()
errorCallback e s = putStrLn $ "GLFW Error: " ++ (show e ++ s)

makeWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
makeWindow width height title = do
  putStr "Initializing GLFW..."
  r <- GLFW.init
  unless r $ return ()
  putStrLn "Done"
  GLFW.setErrorCallback $ Just errorCallback
  putStr $ "Creating window of size (" ++ (show width) ++ ", " ++ (show height) ++ ")..."
  m <- GLFW.createWindow width height title Nothing Nothing
  case m of Nothing -> ioError $ userError "Failed to create window!"
            Just _ -> putStrLn "Done."
  case m of
    Nothing -> return ()
    (Just _) -> GLFW.makeContextCurrent m
  LR.initLambency
  return m

destroyWindow :: Maybe GLFW.Window -> IO ()
destroyWindow m = do
  case m of
    (Just win) -> do
      GLFW.destroyWindow win
    Nothing -> return ()
  GLFW.terminate  

run :: GLFW.Window -> LR.GameCamera -> [ LR.GameObject a ] -> IO ()
run win (LR.GameCamera cam updCam) objs = do

  GLFW.pollEvents
  keyState <- GLFW.getKey win GLFW.Key'Q
  case keyState of
    GLFW.KeyState'Pressed -> GLFW.setWindowShouldClose win True
    _ -> return ()
  GL.clearColor GL.$= GL.Color4 0.0 0.0 0.5 1
  GL.clear [GL.ColorBuffer]
  LR.renderCamera cam objs
  GL.flush
  GLFW.swapBuffers win
  q <- GLFW.windowShouldClose win
  unless q $ run win updateCamera $ LR.interactObjs $ LR.updateObjs dt objs
  where
    dt :: Double
    dt = 0.05

    updateCamera :: LR.GameCamera
    updateCamera = LR.GameCamera (updCam dt cam) updCam

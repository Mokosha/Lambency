module Graphics.UI.Lambency (
  Input(..),
  makeWindow,
  destroyWindow,
  run
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import qualified Graphics.Rendering.Lambency as LR

import Graphics.UI.Lambency.Input

import Control.Monad (unless)

import qualified Data.Set as Set

--------------------------------------------------------------------------------

errorCallback :: GLFW.Error -> String -> IO()
errorCallback e s = putStrLn $ "GLFW Error: " ++ (show e ++ s)

makeWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
makeWindow width height title = do
  putStr "Initializing GLFW..."
  r <- GLFW.init
  if not r then ioError (userError "Failed!") else do
    putStrLn "Done"
    GLFW.setErrorCallback $ Just errorCallback
    putStr $ "Creating window of size (" ++ (show width) ++ ", " ++ (show height) ++ ")..."
    m <- GLFW.createWindow width height title Nothing Nothing
    if m == (Nothing) then ioError (userError "Failed!") else do
      putStrLn "Done."
      GLFW.makeContextCurrent m

      -- Initial defaults
      GL.depthFunc GL.$= Just GL.Lequal
      GL.cullFace GL.$= Just GL.Back
      LR.initLambency
      return m

destroyWindow :: Maybe GLFW.Window -> IO ()
destroyWindow m = do
  case m of
    (Just win) -> do
      GLFW.destroyWindow win
    Nothing -> return ()
  GLFW.terminate  

run' :: InputControl -> GLFW.Window -> LR.GameCamera -> [ LR.GameObject a ] -> IO ()
run' ctl win (LR.GameCamera cam updCam) objs = do

  GLFW.pollEvents

  input <- getInput ctl
  if Set.member GLFW.Key'Q (keysPressed input)
    then GLFW.setWindowShouldClose win True
    else return ()

  -- !FIXME! This should be moved to the camera...
  GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  LR.renderCamera cam objs
  GL.flush

  -- Swap buffers and poll events...
  GLFW.swapBuffers win

  -- Update camera
  let (ipt, newcam) = updCam cam dt input

  -- Update game objects
  -- !FIXME!

  setInput ctl ipt

  q <- GLFW.windowShouldClose win
  unless q $ run' ctl win newcam $ LR.updateObjs dt objs
  where
    dt :: Double
    dt = 0.05

run :: GLFW.Window -> LR.GameCamera -> [ LR.GameObject a ] -> IO ()
run win cam objs = do
  ctl <- mkInputControl win
  run' ctl win cam objs

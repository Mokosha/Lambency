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
import qualified Control.Wire as W

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

run :: GLFW.Window -> LR.GameWire -> IO ()
run win w = do
  ctl <- mkInputControl win
  let session = W.countSession 0.05
  run' ctl session w
  where
    run' :: InputControl -> W.Session IO (LR.Timestep) -> LR.GameWire -> IO ()
    run' ctl session wire = do
      GLFW.pollEvents

      input <- getInput ctl
      if Set.member GLFW.Key'Q (keysPressed input)
        then GLFW.setWindowShouldClose win True
        else return ()

      -- Step
      (timestep, nextsession) <- W.stepSession session
      (result, nextwire) <- W.stepWire wire timestep (Right input)

      case result of
        Left _ -> return ()
        Right (ipt, lights, ros) -> do
          -- !FIXME! This should be moved to the camera...
          GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
          GL.clear [GL.ColorBuffer, GL.DepthBuffer]
          mapM_ (flip LR.renderLight ros) lights
          GL.flush

          -- Swap buffers and poll events...
          GLFW.swapBuffers win

          setInput ctl ipt

          q <- GLFW.windowShouldClose win
          unless q $ run' ctl nextsession nextwire

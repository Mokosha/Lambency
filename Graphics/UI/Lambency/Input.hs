module Graphics.UI.Lambency.Input (
  MiscInput(..),
  Input(..),
  InputControl,
  mkInputControl,
  getInput
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent

--------------------------------------------------------------------------------

data MiscInput = Scroll Double Double
               | Resize

data Input = Input {
  keysPressed :: [GLFW.Key],
  keysReleased :: [GLFW.Key],
  mbPressed :: [Int],
  cursor :: Maybe (Float, Float),
  misc :: [MiscInput]
}

kEmptyInput :: Input
kEmptyInput = Input {
  keysPressed = [], keysReleased = [], mbPressed = [], cursor = Nothing, misc = []
}

type InputControl = MVar Input

-- Returns a snapshot of the input and resets it
getInput :: InputControl -> IO(Input)
getInput input = do
  ipt <- takeMVar input
  putMVar input ipt
  return ipt

scrollCallback :: InputControl -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback ctl _ xoff yoff = do
  putStrLn "Scrollin!"
  input <- takeMVar ctl
  putMVar ctl (updateScroll input)
  where
    updateScroll :: Input -> Input
    updateScroll input =
      (\ipt -> ipt { misc = (Scroll xoff yoff) : (filter isScroll $ misc input) }) input

    isScroll :: MiscInput -> Bool
    isScroll (Scroll _ _) = True
    isScroll _ = False

mkInputControl :: GLFW.Window -> IO (InputControl)
mkInputControl win = do
  ctl <- newMVar kEmptyInput
  GLFW.setScrollCallback win (Just $ scrollCallback ctl)
  return ctl
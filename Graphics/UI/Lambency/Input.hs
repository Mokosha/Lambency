module Graphics.UI.Lambency.Input (
  MiscInput(..),
  Input(..),
  InputControl,
  mkInputControl,
  getInput
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent.STM

import qualified Data.Set as Set

--------------------------------------------------------------------------------

data MiscInput = Scroll Double Double
               | Resize

data Input = Input {
  keysPressed :: Set.Set GLFW.Key,
  mbPressed :: [Int],
  cursor :: Maybe (Float, Float),
  misc :: [MiscInput]
}

kEmptyInput :: Input
kEmptyInput = Input { keysPressed = Set.empty, mbPressed = [], cursor = Nothing, misc = [] }

type InputControl = TVar Input

-- Returns a snapshot of the input
getInput :: InputControl -> IO(Input)
getInput = readTVarIO

scrollCallback :: InputControl -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback ctl _ xoff yoff = atomically $ modifyTVar' ctl updateScroll
  where
    updateScroll :: Input -> Input
    updateScroll =
      (\input -> input { misc = (Scroll xoff yoff) : (filter notScroll $ misc input) })

    notScroll :: MiscInput -> Bool
    notScroll (Scroll _ _) = False
    notScroll _ = True

keyCallback :: InputControl -> GLFW.Window ->
               GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback ctl _ key _ keystate _ = atomically $ modifyTVar' ctl modifyKeys
  where
    updateKeys :: (Set.Set GLFW.Key -> Set.Set GLFW.Key) -> Input -> Input
    updateKeys fn = (\input -> input { keysPressed = fn (keysPressed input) })

    modifyKeys :: Input -> Input
    modifyKeys = case keystate of
      GLFW.KeyState'Pressed -> updateKeys $ Set.insert key
      GLFW.KeyState'Released -> updateKeys $ Set.delete key
      _ -> id

mkInputControl :: GLFW.Window -> IO (InputControl)
mkInputControl win = do
  ctl <- newTVarIO kEmptyInput
  GLFW.setScrollCallback win (Just $ scrollCallback ctl)
  GLFW.setKeyCallback win (Just $ keyCallback ctl)
  return ctl

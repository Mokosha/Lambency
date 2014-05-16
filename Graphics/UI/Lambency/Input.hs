module Graphics.UI.Lambency.Input (
  MiscInput(..),
  Input(..),
  InputControl,
  mkInputControl,
  getInput, setInput,
  resetCursorPos,

  isKeyPressed, withPressedKey, debounceKey
) where

--------------------------------------------------------------------------------
import Graphics.Rendering.Lambency.Utils
import qualified Graphics.UI.GLFW as GLFW

import Control.Concurrent.STM

import qualified Data.Set as Set

import GHC.Float
--------------------------------------------------------------------------------

data MiscInput = Scroll Double Double
               | Resize
                 deriving (Show)

data Input = Input {
  keysPressed :: Set.Set GLFW.Key,
  mbPressed :: [Int],
  cursor :: Maybe (Float, Float),
  misc :: [MiscInput]
} deriving(Show)

kEmptyInput :: Input
kEmptyInput = Input { keysPressed = Set.empty, mbPressed = [], cursor = Nothing, misc = [] }

isKeyPressed :: GLFW.Key -> Input -> Bool
isKeyPressed key = (Set.member key) . keysPressed

withPressedKey :: Input -> GLFW.Key -> (a -> a) -> a -> a
withPressedKey input key fn v = if isKeyPressed key input then fn v else v

debounceKey :: GLFW.Key -> Input -> Input
debounceKey key = (\input -> input { keysPressed = Set.delete key (keysPressed input) })

data InputControl = IptCtl (TVar Input) GLFW.Window

-- Returns a snapshot of the input
setCursorToWindowCenter :: GLFW.Window -> IO ()
setCursorToWindowCenter win = do
  (w, h) <- GLFW.getWindowSize win
  GLFW.setCursorPos win (fromIntegral w / 2.0) (fromIntegral h / 2.0)

getInput :: InputControl -> IO(Input)
getInput (IptCtl var _) = readTVarIO var

setInput :: InputControl -> Input -> IO ()
setInput (IptCtl var win) ipt = do
  case (cursor ipt) of
    Just _ -> return ()
    Nothing -> setCursorToWindowCenter win
  atomically $ writeTVar var ipt

resetCursorPos :: Input -> Input
resetCursorPos = (\input -> input { cursor = Nothing })

--------------------------

scrollCallback :: InputControl -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback (IptCtl ctl _) _ xoff yoff = atomically $ modifyTVar' ctl updateScroll
  where
    updateScroll :: Input -> Input
    updateScroll =
      (\input -> input { misc = (Scroll xoff yoff) : (filter notScroll $ misc input) })

    notScroll :: MiscInput -> Bool
    notScroll (Scroll _ _) = False
    notScroll _ = True

keyCallback :: InputControl -> GLFW.Window ->
               GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback (IptCtl ctl _) _ key _ keystate _ = atomically $ modifyTVar' ctl modifyKeys
  where
    updateKeys :: (Set.Set GLFW.Key -> Set.Set GLFW.Key) -> Input -> Input
    updateKeys fn = (\input -> input { keysPressed = fn (keysPressed input) })

    modifyKeys :: Input -> Input
    modifyKeys = case keystate of
      GLFW.KeyState'Pressed -> updateKeys $ Set.insert key
      GLFW.KeyState'Released -> updateKeys $ Set.delete key
      _ -> id

-- !HACK! Right now we're simply setting the cursor position as disabled
-- regardless of application ... we should really expose this to the user
-- somehow...

cursorPosCallback :: InputControl -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback (IptCtl ctl _) win x y = do
  (w, h) <- GLFW.getWindowSize win
  let xf = newRangeC (double2Float x) (0, fromIntegral w) (-1, 1)
      yf = newRangeC (double2Float y) (0, fromIntegral h) (-1, 1)
  atomically $ modifyTVar' ctl (\ipt -> ipt { cursor = Just (xf, yf)})

mkInputControl :: GLFW.Window -> IO (InputControl)
mkInputControl win = do

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  ctlvar <- newTVarIO kEmptyInput
  let ctl = IptCtl ctlvar win
  GLFW.setScrollCallback win (Just $ scrollCallback ctl)
  GLFW.setKeyCallback win (Just $ keyCallback ctl)
  GLFW.setCursorPosCallback win (Just $ cursorPosCallback ctl)
  return ctl

module Lambency.UI (
  UIWire, WidgetEvent(..), WidgetState(..), Widget(..), screen,
  animatedSpriteRenderer, spriteRenderer, colorRenderer,
  textRenderer, dynamicTextRenderer,
  combineRenderers,
  hbox, vbox, glue
) where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Wire hiding ((.))

import Data.Word

import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW

import Lambency.Font
import Lambency.GameObject
import Lambency.Sprite
import Lambency.Types

import Linear hiding (trace, identity)

import Prelude hiding (id)

import qualified Yoga as Y
--------------------------------------------------------------------------------

type UIWire a b = GameWire (Y.LayoutInfo, a) b

data WidgetEvent a b
  = WidgetEvent'OnMouseOver {
    eventLogic :: UIWire a b
    }
  | WidgetEvent'OnMouseDown {
    _eventMouseButton :: GLFW.MouseButton,
    eventLogic :: UIWire a b
    }
  | WidgetEvent'OnKeyDown {
    _eventKey :: GLFW.Key,
    eventLogic :: UIWire a b
    }

data WidgetState a b = WidgetState {
  idleLogic :: UIWire a b,
  eventHandlers :: [WidgetEvent a b]
}

blankState :: Monoid b => WidgetState a b
blankState = WidgetState (ignoreFst $ mkConst (Right mempty)) []

newtype Widget a b = Widget { getWidgetLayout :: Y.Layout (WidgetState a b) }

widgetRenderFn :: Monoid b =>
                  TimeStep -> a -> Y.LayoutInfo -> WidgetState a b ->
                  GameMonad (b, WidgetState a b)
widgetRenderFn dt input lytInfo widgetState =
  let eventWire :: WidgetEvent a b -> UIWire a b
      eventWire (WidgetEvent'OnMouseDown mb uiw) =
        second (mousePressed mb) >>> uiw
      eventWire (WidgetEvent'OnKeyDown key uiw) =
        second (keyPressed key) >>> uiw
      eventWire e@(WidgetEvent'OnMouseOver uiw) = mkGen $ \dt' (lyt, ipt) -> do
        (Right (mx, my), _) <- stepWire mouseCursor dt' $ Right undefined
        (V2 wx wy) <- windowSize <$> ask
        let bx0 = Y.nodeLeft lytInfo / fromIntegral wx
            bx1 = (bx0 + Y.nodeWidth lytInfo) / fromIntegral wx
            by0 = Y.nodeTop lytInfo / fromIntegral wy
            by1 = (by0 + Y.nodeHeight lytInfo) / fromIntegral wy
            x = (mx + 1.0) * 0.5
            y = (my + 1.0) * 0.5

        if x >= bx0 && x <= bx1 && y >= by0 && y <= by1 then
          do
            (result, uiw') <- stepWire uiw dt' $ Right (lyt, ipt)
            return (result, eventWire (WidgetEvent'OnMouseOver uiw'))
          else return (Left "Mouse out of bounds", eventWire e)

      handleEvent :: WidgetEvent a b -> (Y.LayoutInfo, a) ->
                     GameMonad (Maybe (b, WidgetEvent a b))
      handleEvent event arg = do
        (result, uiw') <- stepWire (eventWire event) dt $ Right arg
        case result of
          Left _ -> return Nothing
          Right x -> return $ Just (x, event { eventLogic = uiw' })

      handleEvents :: Monoid b => [WidgetEvent a b] -> (Y.LayoutInfo, a) ->
                      GameMonad (Maybe b, [WidgetEvent a b])
      handleEvents events arg =
        let eventFn (res, evts) event = do
              result <- handleEvent event arg
              case result of
                Nothing -> return (res, event : evts)
                Just (x, e) -> case res of
                  Nothing -> return (Just x, e : evts)
                  Just r -> return (Just $ r `mappend` x, e : evts)
        in foldM eventFn (Nothing, []) events

      wireArg = (lytInfo, input)
  in do
    (eventResults, events) <- handleEvents (eventHandlers widgetState) wireArg
    case eventResults of
      Nothing -> do
        (result, uiw') <- stepWire (idleLogic widgetState) dt $ Right wireArg
        let newState = widgetState { idleLogic = uiw', eventHandlers = events }
        case result of
          Right x -> return (x, newState)
          Left _ -> error "UI wire inhibited?"
      Just result -> return (result, widgetState { eventHandlers = events })

ignoreFst :: GameWire b c -> GameWire (a, b) c
ignoreFst logic = mkGen $ \dt (_, ipt) -> do
  (result, logic') <- stepWire logic dt $ Right ipt
  return (result, ignoreFst logic')

widgetWire :: Monoid b => Widget a b -> GameWire a b
widgetWire (Widget lyt) = mkGen $ \dt input -> do
  (result, newLyt) <- Y.foldRender lyt (widgetRenderFn dt input)
  return (Right result, widgetWire $ Widget newLyt)

screenPrg :: Monoid b => [Widget a b] -> GameMonad (Widget a b)
screenPrg children = do
  (V2 wx wy) <- windowSize <$> ask
  return . Widget $
    ($ blankState) $
    Y.withDimensions (fromIntegral wx) (fromIntegral wy) $
    Y.vbox (Y.startToEnd $ getWidgetLayout <$> children)

screen :: Monoid b => [Widget a b] -> GameWire a b
screen children = wireFrom (windowSize <$> ask) $ runScreen $
                  wireFrom (screenPrg children) widgetWire
  where
    runScreen ui_wire oldWinDims =
      let getUIWire False = wireFrom (screenPrg children) widgetWire
          getUIWire True = ui_wire
      in mkGen $ \dt input -> do
        winDims <- windowSize <$> ask
        let ui = getUIWire (winDims == oldWinDims)
        (result, next_wire') <- stepWire ui dt $ Right input
        return (result, runScreen next_wire' winDims)

renderSpriteAt :: Sprite -> Y.LayoutInfo -> GameMonad ()
renderSpriteAt sprite lytInfo = do
  let (x, y, w, h) = (
        Y.nodeLeft lytInfo,
        Y.nodeTop lytInfo,
        Y.nodeWidth lytInfo,
        Y.nodeHeight lytInfo)
  (V2 _ wy) <- windowSize <$> ask
  renderUISpriteWithSize sprite (V2 x (fromIntegral wy - y - h)) (V2 w h)

renderStringAt :: Font -> String -> Y.LayoutInfo -> GameMonad()
renderStringAt font str lytInfo = do
  let (x, y) = (Y.nodeLeft lytInfo, Y.nodeTop lytInfo)
  (V2 _ wy) <- windowSize <$> ask
  renderUIString font str $ V2 x (fromIntegral wy - y - stringHeight font str)

animatedRenderer :: Monoid b => GameWire a Sprite -> GameWire a b -> UIWire a b
animatedRenderer spriteWire logic = mkGen $ \dt (lytInfo, val) -> do
  (spriteResult, spriteWire') <- stepWire spriteWire dt $ Right val
  (logicResult, logic') <- stepWire logic dt $ Right val
  case spriteResult of
    Right nextSprite -> do
      renderSpriteAt nextSprite lytInfo
      (nextSpriteResult, _) <- stepWire spriteWire' dt $ Right val
      case nextSpriteResult of
        Right _ -> return (logicResult, animatedRenderer spriteWire' logic')
        Left _ -> return (logicResult, spriteRenderer nextSprite logic')
    Left _ -> error "Should never get here"

animatedSpriteRenderer :: Monoid b =>
                          Sprite -> SpriteAnimationType -> GameWire a b ->
                          UIWire a b
animatedSpriteRenderer sprite animType logic =
  animatedRenderer (animatedWire sprite animType) logic

spriteRenderer :: Monoid b => Sprite -> GameWire a b -> UIWire a b
spriteRenderer s logic = mkGen $ \dt (lytInfo, val) -> do
  renderSpriteAt s lytInfo
  (result, logic') <- stepWire logic dt $ Right val
  return (result, spriteRenderer s logic')

colorRenderer :: Monoid b => V4 Word8 -> GameWire a b -> UIWire a b
colorRenderer color logic = mkGen $ \dt (lytInfo, val) -> do
  let byteColor = fromIntegral <$> color
  s <- changeSpriteColor byteColor <$> simpleSprite <$> ask
  renderSpriteAt s lytInfo
  (result, logic') <- stepWire logic dt $ Right val
  return (result, spriteRenderer s logic')

textRenderer :: Monoid b => Font -> String -> GameWire a b -> UIWire a b
textRenderer font str logic = mkGen $ \dt (lytInfo, val) -> do
  renderStringAt font str lytInfo
  (result, logic') <- stepWire logic dt $ Right val
  return (result, textRenderer font str logic')

dynamicTextRenderer :: Monoid b => Font -> GameWire a (b, String) -> UIWire a b
dynamicTextRenderer font logic = mkGen $ \dt (lytInfo, val) -> do
  (wireResult, logic') <- stepWire logic dt $ Right val
  result <- case wireResult of
    (Right (bVal, str)) -> do
      renderStringAt font str lytInfo
      return $ Right bVal
    (Left e) -> return (Left e)
  return (result, dynamicTextRenderer font logic')

combineRenderers :: Monoid b => [UIWire a b] -> UIWire a b
combineRenderers = mconcat

hbox :: Monoid b => [Widget a b] -> Widget a b
hbox widgets = Widget
               $ ($ blankState)
               $ Y.stretched
               $ Y.hbox (Y.spaceBetween (getWidgetLayout <$> widgets))

vbox :: Monoid b => [Widget a b] -> Widget a b
vbox widgets = Widget
               $ ($ blankState)
               $ Y.stretched
               $ Y.vbox (Y.spaceBetween (getWidgetLayout <$> widgets))

glue :: Monoid b => Widget a b
glue = Widget
       $ ($ blankState)
       $ Y.growable 2.0 (Y.Min 0.0) (Y.Min 0.0)
       $ Y.exact 1.0 1.0

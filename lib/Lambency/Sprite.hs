module Lambency.Sprite (
  SpriteFrame(..),
  Sprite(..),
  loadStaticSprite,
  loadAnimatedSprite,
  loadFixedSizeAnimatedSprite,

  renderSprite,
  renderUISprite,

  SpriteAnimationType(..),
  animatedWire,
) where

--------------------------------------------------------------------------------
import Control.Comonad
import Control.Wire hiding ((.))

import qualified Data.Map as Map

import Lambency.Material
import Lambency.Mesh
import Lambency.Render
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Utils

import Linear
--------------------------------------------------------------------------------

data SpriteFrame = SpriteFrame {
  offset :: V2 Float,
  size :: V2 Int,
  frameRO :: RenderObject
}

newtype Sprite = Sprite { getFrames :: CyclicList SpriteFrame }

curFrameOffset :: Sprite -> V2 Float
curFrameOffset = offset . extract . getFrames

updateScale :: V2 Float -> V2 Float -> Material -> Material
updateScale (V2 sx sy) (V2 tx ty) =
  Map.insert "texCoordMatrix" (Matrix3Val $
                               V3 (V3 sx 0 0) (V3 0 sy 0) (V3 tx ty 1))

scaleToSize :: V2 Int -> Transform
scaleToSize sz =
  let (V2 sx sy) = fmap ((*0.5) . fromIntegral) sz
  in nonuniformScale (V3 sx sy 1) identity

initStaticSprite :: Texture -> IO (Sprite)
initStaticSprite tex = do
  ro <- createRenderObject quad (createTexturedMaterial tex)
  return . Sprite . cycleSingleton $ SpriteFrame {
    offset = zero,
    size = textureSize tex,
    frameRO = xformObject (scaleToSize $ textureSize tex) ro
  }

initAnimatedSprite :: [V2 Int] -> [V2 Int] -> Texture -> IO (Sprite)
initAnimatedSprite frameSzs offsets tex = do
  ro <- createRenderObject quad (createTexturedMaterial tex)
  return . Sprite . cyclicFromList $ map (genFrame ro) (zip frameSzs offsets)
  where
    genFrame :: RenderObject -> (V2 Int, V2 Int) -> SpriteFrame
    genFrame ro (sz, off) =
      let texOff = changeRange off
      in SpriteFrame {
        offset = texOff,
        size = sz,
        frameRO = xformObject (scaleToSize sz) $
                  ro { material = updateScale (changeRange sz) texOff (material ro)}
        }

    changeRange :: V2 Int -> V2 Float
    changeRange (V2 ox oy) =
      let (V2 tx ty) = textureSize tex
      in V2
        (newRange (fromIntegral ox) (0, fromIntegral tx) (0, 1))
        (newRange (fromIntegral oy) (0, fromIntegral ty) (0, 1))

loadSpriteWith :: FilePath -> (Texture -> IO (Sprite)) -> IO (Maybe Sprite)
loadSpriteWith f initFn = do
  tex <- loadTexture f
  case tex of
    Nothing -> return Nothing
    (Just t@(Texture _ _)) -> initFn t >>= (return . Just)
    _ -> return Nothing

loadStaticSprite :: FilePath -> IO (Maybe Sprite)
loadStaticSprite f = loadSpriteWith f initStaticSprite

loadAnimatedSprite :: FilePath -> [V2 Int] -> [V2 Int] -> IO (Maybe Sprite)
loadAnimatedSprite f frameSzs offsets = loadSpriteWith f $ initAnimatedSprite frameSzs offsets

loadFixedSizeAnimatedSprite :: FilePath -> V2 Int -> [V2 Int] -> IO (Maybe Sprite)
loadFixedSizeAnimatedSprite f frameSz offsets = loadAnimatedSprite f (repeat frameSz) offsets

renderUISprite :: Sprite -> V2 Float -> GameMonad ()
renderUISprite s pos = addRenderUIAction pos (frameRO currentFrame)
  where
    currentFrame = extract $ getFrames s

renderSprite :: Sprite -> Float -> V2 Float -> GameMonad ()
renderSprite s depth (V2 x y) = (addRenderAction xf) $ frameRO currentFrame
  where
    currentFrame = extract . getFrames $ s
    xf = translate (V3 x y depth) identity

data SpriteAnimationType
  = SpriteAnimationType'Forward
  | SpriteAnimationType'Backward
  | SpriteAnimationType'Loop
  | SpriteAnimationType'LoopBack
  | SpriteAnimationType'PingPong
    deriving(Eq, Ord, Show, Enum)

animatedWire :: Sprite -> SpriteAnimationType -> GameWire (V2 Float) (V2 Float)
animatedWire sprite SpriteAnimationType'Forward = let
  start = curFrameOffset sprite
  in
   loop $ second (delay sprite) >>> let
     loopW :: GameWire (V2 Float, Sprite) (V2 Float, Sprite)
     loopW = mkGenN $ \(p, s) -> do
        renderSprite s 0 p
        let nextSprite = Sprite . advance . getFrames $ s
            result = Right (p, nextSprite)
        if (curFrameOffset nextSprite) == start
          then return (result, mkEmpty)
          else return (result, loopW)
     in
      loopW

animatedWire (Sprite (CyclicList p c n)) SpriteAnimationType'Backward =
   animatedWire (Sprite (CyclicList n c p)) SpriteAnimationType'Forward

animatedWire s SpriteAnimationType'Loop =
  let w = animatedWire s SpriteAnimationType'Forward
  in w --> w

animatedWire s SpriteAnimationType'LoopBack =
  let w = animatedWire s SpriteAnimationType'Backward
  in w --> w

animatedWire s SpriteAnimationType'PingPong =
  let f = animatedWire s SpriteAnimationType'Forward
      b = animatedWire s SpriteAnimationType'Backward
  in
   f --> b --> (animatedWire s SpriteAnimationType'PingPong)

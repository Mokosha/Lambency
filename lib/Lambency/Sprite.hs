{-# LANGUAGE RecordWildCards #-}
module Lambency.Sprite (
  changeSpriteFrameColor,
  changeSpriteColor,
  loadStaticSprite,
  loadStaticSpriteWithTexture,
  loadStaticSpriteWithMask,
  loadAnimatedSprite,
  loadAnimatedSpriteWithTexture,
  loadAnimatedSpriteWithMask,
  loadFixedSizeAnimatedSprite,

  renderSprite,
  renderSpriteWithAlpha,
  renderUISprite,
  renderUISpriteWithSize,

  SpriteAnimationType(..),
  animatedWire,
) where

--------------------------------------------------------------------------------
import Control.Comonad
import Control.Monad.Reader
import Control.Wire

import Data.List (nub)

import Lambency.Material
import Lambency.Mesh
import Lambency.Renderer
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Utils

import Linear hiding (trace, identity)
import qualified Linear

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

updateColor :: V4 Float -> Material -> Material
updateColor c mat@(MaskedSpriteMaterial {..}) =
  mat { spriteMaskColor = updateMaterialVar4vf c spriteMaskColor }
updateColor _ m =
  error $ "Lambency.Sprite (updateColor): Unsupported material type: " ++ show m

updateAlpha :: Float -> Material -> Material
updateAlpha a' mat@(MaskedSpriteMaterial {..}) =
  case spriteMaskColor of
    MaterialVar (_, Nothing) ->
      mat { spriteMaskColor = updateMaterialVar4vf (V4 1 1 1 a') spriteMaskColor }
    MaterialVar (_, Just (Vector4Val (V4 r g b _))) ->
      mat { spriteMaskColor = updateMaterialVar4vf (V4 r g b a') spriteMaskColor }
    MaterialVar (_, Just _) ->
      error $ "Lambency.Sprite (updateAlpha): Internal error -- " <>
              "spriteMaskColor is not a V4 value??"

updateAlpha a mat@(TexturedSpriteMaterial {..}) =
  mat { spriteAlpha = updateMaterialVarf a spriteAlpha }
updateAlpha _ m =
  error $ "Lambency.Sprite (updateColor): Unsupported material type: " ++ show m

updateMatrixScale :: V2 Float -> M33 Float -> M33 Float
updateMatrixScale (V2 sx sy) (V3 _ _ t) = V3 (V3 sx 0 0) (V3 0 sy 0) t

updateMatrixTranslation :: V2 Float -> M33 Float -> M33 Float
updateMatrixTranslation (V2 tx ty) (V3 x y _) = V3 x y (V3 tx ty 1)

getShaderVarMatrix :: MaterialVar Mat3f -> Mat3f
getShaderVarMatrix (MaterialVar (_, (Just (Matrix3Val mat)))) = mat
getShaderVarMatrix _ = Linear.identity

updateScale :: V2 Float -> Material -> Material
updateScale s mat@(MaskedSpriteMaterial {..}) =
  let newMatrix = updateMatrixScale s $ getShaderVarMatrix spriteMaskMatrix
  in mat { spriteMaskMatrix = updateMaterialVar3mf newMatrix spriteMaskMatrix }
updateScale s mat@(TexturedSpriteMaterial {..}) =
  let newMatrix = updateMatrixScale s $ getShaderVarMatrix spriteTextureMatrix
  in mat { spriteTextureMatrix = updateMaterialVar3mf newMatrix spriteTextureMatrix }
updateScale _ m =
  error $ "Lambency.Sprite (updateScale): Unsupported material type: " ++ show m

updateTranslation :: V2 Float -> Material -> Material
updateTranslation t mat@(MaskedSpriteMaterial {..}) =
  let newMatrix = updateMatrixTranslation t $ getShaderVarMatrix spriteMaskMatrix
  in mat { spriteMaskMatrix = updateMaterialVar3mf newMatrix spriteMaskMatrix }
updateTranslation t mat@(TexturedSpriteMaterial {..}) =
  let newMatrix = updateMatrixTranslation t $ getShaderVarMatrix spriteTextureMatrix
  in mat { spriteTextureMatrix = updateMaterialVar3mf newMatrix spriteTextureMatrix }
updateTranslation _ m =
  error $ "Lambency.Sprite (updateTranslation): Unsupported material type: " ++ show m

-- !FIXME! These functions shouldn't be here and we should really be using lenses
mapROMaterial :: (Material -> Material) -> RenderObject -> RenderObject
mapROMaterial fn ro = ro { material = fn (material ro) }

mapFrameRO :: (RenderObject -> RenderObject) -> SpriteFrame -> SpriteFrame
mapFrameRO fn sf = sf { frameRO = fn (frameRO sf) }

addTextFlag :: SpriteFrame -> SpriteFrame
addTextFlag = mapFrameRO $ \ro -> ro { flags = nub $ Text : (flags ro) }

changeSpriteFrameColor :: V4 Float -> SpriteFrame -> SpriteFrame
changeSpriteFrameColor c = mapFrameRO $ mapROMaterial $ updateColor c

mapSpriteFrames :: (SpriteFrame -> SpriteFrame) -> Sprite -> Sprite
mapSpriteFrames f (Sprite frames) = Sprite (fmap f frames)

changeSpriteColor :: V4 Float -> Sprite -> Sprite
changeSpriteColor c = mapSpriteFrames $ changeSpriteFrameColor c

addRenderFlag :: RenderFlag -> RenderObject -> RenderObject
addRenderFlag flag = \r -> r { flags = nub $ flag : (flags r) }

initStaticSprite :: Bool -> Texture -> ResourceLoader Sprite
initStaticSprite isMask tex = do
  let mat = if isMask
            then maskedSpriteMaterial tex
            else texturedSpriteMaterial tex
  ro <- (if isMask then addRenderFlag Transparent else id)
        <$> createRenderObject quad mat
  return $ Sprite
    { spriteFrames = cycleSingleton $ SpriteFrame
        { offset = zero
        , spriteSize = textureSize tex
        , frameRO = ro
        }
    }

initAnimatedSprite :: Bool -> [V2 Int] -> [V2 Int] -> Texture
                   -> ResourceLoader Sprite
initAnimatedSprite isMask frameSzs offsets tex = do
  let mat = if isMask
            then maskedSpriteMaterial tex
            else texturedSpriteMaterial tex
  ro <- createRenderObject quad mat
  return $ Sprite $ cyclicFromList $ map (genFrame ro) (zip frameSzs offsets)
  where
    genFrame :: RenderObject -> (V2 Int, V2 Int) -> SpriteFrame
    genFrame ro (sz, off) =
      let texOff = changeRange off
      in SpriteFrame {
        offset = texOff,
        spriteSize = sz,
        frameRO = ro { material =
                          updateScale (changeRange sz) $
                          updateTranslation texOff $
                          material ro }
        }

    changeRange :: V2 Int -> V2 Float
    changeRange (V2 ox oy) =
      let (V2 tx ty) = textureSize tex
      in V2
        (newRange (fromIntegral ox) (0, fromIntegral tx) (0, 1))
        (newRange (fromIntegral oy) (0, fromIntegral ty) (0, 1))

loadSpriteWith :: FilePath
               -> (Texture -> ResourceLoader Sprite)
               -> ResourceLoader (Maybe Sprite)
loadSpriteWith f initFn = do
  tex <- loadTexture f
  case tex of
    Nothing -> return Nothing
    (Just t@(Texture _ _)) -> initFn t >>= (return . Just)
    _ -> return Nothing

loadStaticSpriteWithTexture :: Texture -> ResourceLoader Sprite
loadStaticSpriteWithTexture = initStaticSprite False

loadStaticSpriteWithMask :: Texture -> ResourceLoader Sprite
loadStaticSpriteWithMask = initStaticSprite True

loadStaticSprite :: FilePath -> ResourceLoader (Maybe Sprite)
loadStaticSprite f = loadSpriteWith f (initStaticSprite False)

loadAnimatedSprite :: FilePath -> [V2 Int] -> [V2 Int]
                   -> ResourceLoader (Maybe Sprite)
loadAnimatedSprite f frameSzs offsets =
  loadSpriteWith f $ initAnimatedSprite False frameSzs offsets

loadAnimatedSpriteWithTexture :: Texture -> [V2 Int] -> [V2 Int]
                              -> ResourceLoader (Maybe Sprite)
loadAnimatedSpriteWithTexture t frameSzs offsets =
  initAnimatedSprite False frameSzs offsets t >>= (return . Just)

loadAnimatedSpriteWithMask :: Texture -> [V2 Int] -> [V2 Int]
                           -> ResourceLoader (Maybe Sprite)
loadAnimatedSpriteWithMask t frameSzs offsets =
  -- !HACK! Not all animated (multi-frame) mask sprites are fonts...
  initAnimatedSprite True frameSzs offsets t >>=
  (return . Just . mapSpriteFrames addTextFlag)

loadFixedSizeAnimatedSprite :: FilePath -> V2 Int -> [V2 Int]
                            -> ResourceLoader (Maybe Sprite)
loadFixedSizeAnimatedSprite f frameSz = loadAnimatedSprite f (repeat frameSz)

renderUISpriteWithSize :: Sprite -> V2 Float -> V2 Float -> GameMonad ()
renderUISpriteWithSize sprite pos (V2 sx sy)
  = addTransformedRenderAction (nonuniformScale (V3 sx sy 1) identity)
  $ addRenderUIAction pos (frameRO . extract $ spriteFrames sprite)

renderUISprite :: Sprite -> V2 Float -> GameMonad ()
renderUISprite s@(Sprite frames) pos =
  renderUISpriteWithSize s pos $ fromIntegral <$> (spriteSize $ extract frames)

renderFrameAt :: RenderObject -> V2 Int -> Float -> V2 Float -> GameMonad ()
renderFrameAt ro sc depth (V2 x y) = addRenderAction xf ro
  where
    (V2 sx sy) = fmap fromIntegral sc
    xf = translate (V3 x y depth) $
         nonuniformScale (V3 sx sy 1) identity

-- Renders an opaque sprite at the given scale, depth, and position
renderSprite :: Sprite -> V2 Int -> Float -> V2 Float -> GameMonad ()
renderSprite s = renderSpriteWithAlpha s 1.0

-- Renders a sprite for the given alpha, scale, depth, and position
renderSpriteWithAlpha :: Sprite -> Float -> V2 Int -> Float -> V2 Float ->
                         GameMonad ()
renderSpriteWithAlpha (Sprite frames) a =
  renderFrameAt (setAlpha . frameRO . extract $ frames)
  where
    setAlpha ro = ro { material = updateAlpha a (material ro),
                       flags = nub $ Transparent : (flags ro) }

data SpriteAnimationType
  = SpriteAnimationType'Forward
  | SpriteAnimationType'Backward
  | SpriteAnimationType'Loop
  | SpriteAnimationType'LoopBack
  | SpriteAnimationType'PingPong
    deriving(Eq, Ord, Show, Enum)

animatedWire :: Sprite -> SpriteAnimationType -> GameWire a Sprite
animatedWire (Sprite (CyclicList _ _ [])) SpriteAnimationType'Forward = mkEmpty
animatedWire s@(Sprite frames) SpriteAnimationType'Forward =
  mkGenN $ \ _ -> do
    let nextSprite = Sprite (advance frames)
    return (Right s, animatedWire nextSprite SpriteAnimationType'Forward)

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

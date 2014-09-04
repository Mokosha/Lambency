module Lambency.Sprite (
  Sprite,
  loadStaticSprite,
  loadAnimatedSprite,

  renderSprite,
  renderUISprite,

  mkAnimatedWire,
) where

--------------------------------------------------------------------------------
import Control.Comonad
import Control.Wire hiding ((.))

import Lambency.Material
import Lambency.Mesh
import Lambency.Render
import Lambency.Texture
import Lambency.Transform
import Lambency.Types

import Linear
--------------------------------------------------------------------------------

data CyclicList a = CyclicList [a] a [a]

instance Functor CyclicList where
  fmap f (CyclicList p c n) = CyclicList (fmap f p) (f c) (fmap f n)

advance :: CyclicList a -> CyclicList a
advance (CyclicList p c []) = let (r:rs) = reverse (c:p) in CyclicList [] r rs
advance (CyclicList p c (n:ns)) = CyclicList (c:p) n ns

retreat :: CyclicList a -> CyclicList a
retreat (CyclicList [] c n) = let
  rev = reverse (c : n)
  in CyclicList (init rev) (last rev) []
retreat (CyclicList (p:ps) c n) = CyclicList ps p (c:n)

cyclicLength :: CyclicList a -> Int
cyclicLength (CyclicList x _ z) = length x + length z + 1

cyclicFromList :: [a] -> CyclicList a
cyclicFromList [] = error "Cannot create empty cyclic list"
cyclicFromList (x:xs) = CyclicList [] x xs

cycles :: CyclicList a -> [CyclicList a]
cycles cl = let
  helper 0 _ = []
  helper n cl' = cl' : (helper (n-1) $ advance cl')
  in helper (cyclicLength cl) cl

instance Comonad CyclicList where
  extract (CyclicList _ x _) = x
  duplicate = cyclicFromList . cycles

data Sprite = Sprite {
  frames :: CyclicList (V2 Int),
  size :: V2 Int,
  spriteRenderObject :: RenderObject
}

nextFrame :: Sprite -> Sprite
nextFrame s = s { frames = advance (frames s) }

initStaticSprite :: Texture -> IO (Sprite)
initStaticSprite tex@(Texture (TexHandle _ texSize) _) = do
  ro <- createRenderObject quad (createTexturedMaterial tex)
  return $ Sprite {
    frames = cyclicFromList [zero],
    size = getTextureSize texSize,
    spriteRenderObject = ro
  }
initStaticSprite _ = error "Only loaded textures can be used to create sprites"

initAnimatedSprite :: [V2 Int] -> Texture -> IO (Sprite)
initAnimatedSprite offsets tex@(Texture (TexHandle _ texSize) _) = do
  ro <- createRenderObject quad (createTexturedMaterial tex)
  return $ Sprite {
    frames = cyclicFromList offsets,
    size = getTextureSize texSize,
    spriteRenderObject = ro
  }
initAnimatedSprite _ _ = error "Only loaded textures can be used to create sprites"

loadSpriteWith :: FilePath -> (Texture -> IO (Sprite)) -> IO (Maybe Sprite)
loadSpriteWith f initFn = do
  tex <- loadTexture f
  case tex of
    Nothing -> return Nothing
    (Just t@(Texture _ _)) -> initFn t >>= (return . Just)
    _ -> return Nothing
  

loadStaticSprite :: FilePath -> IO (Maybe Sprite)
loadStaticSprite f = loadSpriteWith f initStaticSprite

loadAnimatedSprite :: FilePath -> [V2 Int] -> IO (Maybe Sprite)
loadAnimatedSprite f offsets = loadSpriteWith f $ initAnimatedSprite offsets

renderUISprite :: V2 Float -> Sprite -> GameMonad ()
renderUISprite pos s = do
  addRenderUIAction pos (xformObject xf (spriteRenderObject s))
  where
    (V2 sx sy) = fmap ((*0.5) . fromIntegral) $ size s
    xf = nonuniformScale (V3 sx sy 1) identity

renderSprite :: V2 Float -> Sprite -> GameMonad ()
renderSprite (V2 x y) s = do
  addRenderAction xf (spriteRenderObject s)
  where
    (V2 sx sy) = fmap ((*0.5) . fromIntegral) $ size s
    xf = translate (V3 x y 0) $
         nonuniformScale (V3 sx sy 1) identity

mkAnimatedWire :: FilePath -> [V2 Int] -> Float -> IO (GameWire (V2 Float) (V2 Float))
mkAnimatedWire file offsets framerate = do
  sprite <- loadAnimatedSprite file offsets
  return mkId

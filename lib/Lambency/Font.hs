module Lambency.Font (
  Font, ModifiedFont, IsFont, unloadFont,
  loadSystemFont,
  loadTTFont,
  renderUIString,
  stringWidth, stringHeight,
  setFontColor,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Monad

import Data.Array.Storable
import Data.Bits
import Data.List (mapAccumL, foldl')
import qualified Data.Map as Map
import Data.Word

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot as FT_GS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Vector hiding (x, y)

import Lambency.Sprite
import Lambency.Texture
import Lambency.Types
import Lambency.Utils

import Linear hiding (trace)

import Paths_lambency
import System.FilePath
--------------------------------------------------------------------------------

-- Helpers

logBase2 :: Int -> Int
#if __GLASGOW_HASKELL__ <= 708
logBase2 1 = 0
logBase2 x
  | x <= 0 = error "Log is undefined!"
  | otherwise = 1 + logBase2 (x `shiftR` 1)
#else
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
#endif

--------------------------------------------------------------------------------
data Font = Font
            { getOrigGlyph :: Char -> Maybe (SpriteFrame, (V2 Int, V2 Int))
            , unloadFont :: IO ()
            }
newtype ModifiedFont = MF {
  getModifiedGlyph :: Char -> Maybe (SpriteFrame, (V2 Int, V2 Int))
}

class IsFont a where
  getGlyph :: a -> Char -> Maybe (SpriteFrame, (V2 Int, V2 Int))

instance IsFont Font where
  getGlyph = getOrigGlyph

instance IsFont ModifiedFont where
  getGlyph = getModifiedGlyph

glyphSize :: Font -> Char -> (V2 Float, V2 Float)
glyphSize font c =
  case (getGlyph font c) of
    Nothing -> (zero, zero)
    Just (_, (adv, off)) -> (fmap fromIntegral adv, fmap fromIntegral off)

renderUIString :: Font -> String -> V2 Float -> GameMonad ()
renderUIString _ "" _ = return ()
renderUIString font str pos = let
  glyphSizes :: [(V2 Float, V2 Float)]
  glyphSizes = map (glyphSize font) str

  positions :: [V2 Float]
  positions = let
    helper p (_, off) [] = [(p ^+^ off)]
    helper p (adv, off) (a:as) = (p ^+^ off) : (helper (p ^+^ adv) a as)
   in
    helper pos (head glyphSizes) (tail glyphSizes)

  renderCharAtPos :: Char -> V2 Float -> GameMonad ()
  renderCharAtPos ' ' _ = return () -- no need to render spaces...
  renderCharAtPos c p =
    case (getGlyph font c) of
      Nothing -> return ()
      Just (f, _) ->
        let V2 _ glyphSzY = fmap fromIntegral $ spriteSize f
            fakeSprite = Sprite (cycleSingleton f) (return ())
        in renderUISprite fakeSprite $ p ^-^ (V2 0 glyphSzY)
  in do
    mapM_ (uncurry renderCharAtPos) $ zip str positions

stringHeight :: Font -> String -> Float
stringHeight _ "" = 0
stringHeight f str = foldl' max 0 sizes
  where
    sizes :: [Float]
    sizes = map (getGlyphHeight . getGlyph f) str

    getGlyphHeight :: Maybe (SpriteFrame, a) -> Float
    getGlyphHeight Nothing = 0.0
    getGlyphHeight (Just (frame, _)) =
      let V2 _ y = fromIntegral <$> spriteSize frame in y

stringWidth :: Font -> String -> Float
stringWidth _ "" = 0
stringWidth f str = foldl' (+) 0 sizes
  where
    sizes :: [Float]
    sizes = map (getX . fst . glyphSize f) str

    getX (V2 x _) = x

mkFont :: Sprite -> [Char] -> [V2 Int] -> [V2 Int] -> Font
mkFont sprite string advances offsets =
  flip Font (unloadSprite sprite) $ flip Map.lookup
  $ Map.fromList
  $ zip string (zip (cyclicToList $ spriteFrames sprite) (zip advances offsets))

charString :: [Char]
charString = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
             ++ "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

loadSystemFont :: Renderer -> V3 Float -> IO Font
loadSystemFont rr (V3 r g b) = let
  systemOffsets = [V2 x 0 | x <- [0,14..]]
  systemSizes = repeat (V2 13 24)
  in do
    Just tex <- getDataFileName ("font" <.> "png") >>= loadTexture rr
    Just s <- loadAnimatedSpriteWithMask rr tex systemSizes systemOffsets
    let sprite = changeSpriteColor (V4 r g b 1) s
    return $ mkFont sprite charString (repeat zero) (repeat zero)

setFontColor :: IsFont fnt => V3 Float -> fnt -> ModifiedFont
setFontColor (V3 r g b) fnt = MF $ \c -> do
  (f, x) <- getGlyph fnt c
  return (changeSpriteFrameColor (V4 r g b 1) f, x)

--------------------------------------------------------------------------------
-- Freetype fonts

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
  r <- m
  unless (r == 0) $ fail $ "FreeType Error:" ++ show r

analyzeGlyph :: FT_Face -> (Int, Int) -> Char -> IO (Int, Int)
analyzeGlyph ft_face (widthAccum, maxHeight) c = do
  runFreeType $ ft_Load_Char ft_face (toEnum . fromEnum $ c) ft_LOAD_RENDER

  -- Get the glyph
  g <- peek $ glyph ft_face

  -- Get the bitmap for the glyph
  bm <- peek $ bitmap g

  -- Figure out the rows and height of the bitmap
  return (widthAccum + (fromEnum $ width bm), max maxHeight (fromEnum $ rows bm))

getGlyphAdvanceOffset :: FT_Face -> Char -> IO (V2 Int, V2 Int)
getGlyphAdvanceOffset ft_face c = do
  runFreeType $ ft_Load_Char ft_face (toEnum . fromEnum $ c) ft_LOAD_RENDER

  -- Get the glyph
  g <- peek $ glyph ft_face

  -- Get the advance
  FT_Vector advx advy <- peek $ FT_GS.advance g

  -- Get the offset
  offx <- peek $ bitmap_left g
  offy <- peek $ bitmap_top g

  return $ (fmap fromIntegral $ V2 advx advy, fmap fromIntegral $ V2 offx offy)

uploadGlyph :: Renderer -> FT_Face -> Texture -> Int -> Char -> IO (Int)
uploadGlyph r ft_face tex widthAccum c = do
  runFreeType $ ft_Load_Char ft_face (cvt c) ft_LOAD_RENDER
  g <- peek $ glyph ft_face
  bm <- peek $ bitmap g
  updateTexture r
    tex
    (buffer bm)
    (V2 (cvt widthAccum) 0)
    (cvt <$> (V2 (width bm) (rows bm)))
  return (widthAccum + (cvt $ width bm))
  where
    cvt :: (Enum a, Enum b) => a -> b
    cvt = toEnum . fromEnum

loadTTFont :: Renderer -> Int -> V3 Float -> FilePath -> IO Font
loadTTFont r fontSize (V3 fontR fontG fontB) filepath = do
  -- Create local copy of freetype library... this will free itself once it
  -- goes out of scope...
  ft_library <- alloca $ \p -> do { runFreeType $ ft_Init_FreeType p; peek p }

  -- Load the font
  ft_face <- withCString filepath $ \cstr -> alloca $ \f -> do
    runFreeType $ ft_New_Face ft_library cstr 0 f
    peek f

  -- Set the pixel size
  runFreeType $ ft_Set_Pixel_Sizes ft_face 0 (toEnum . fromEnum $ fontSize)

  -- Figure out the width and height of the bitmap that we need...
  (texW, texH) <- let nextPower2 = (shiftL 1) . (+ 1) . logBase2
                      updateWH (x, y) = (nextPower2 x, nextPower2 y)
                   in updateWH <$> foldM (analyzeGlyph ft_face) (0, 0) charString

  -- Create a texture to store all of the glyphs
  texZeroA <- ((newArray (1, texW*texH) 0) :: IO (StorableArray Int Word8))
  tex <- withStorableArray texZeroA $ \ptr ->
    mkTexture r ptr (fromIntegral <$> V2 texW texH) Alpha8

  -- Place each glyph into the texture
  foldM_ (uploadGlyph r ft_face tex) 0 charString

  -- Generate info for our rendering
  advOffs <- mapM (getGlyphAdvanceOffset ft_face) charString
  let advances = map (fmap (flip div 64) . fst) advOffs
      offsets = map snd advOffs

  sizes <- mapM (analyzeGlyph ft_face (0, 0)) charString
  let texOffsets = snd $ mapAccumL (\a (w, _) -> (a + w, V2 a 0)) 0 sizes
      sizesV = map (\(x, y) -> V2 x y) sizes
      fontColor = V4 fontR fontG fontB 1

  Just s <- loadAnimatedSpriteWithMask r tex sizesV texOffsets
  return $ mkFont (changeSpriteColor fontColor s) charString advances offsets

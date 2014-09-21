module Lambency.Font (
  Font,
  loadSystemFont,
  loadTTFont,
  renderUIString,
) where

--------------------------------------------------------------------------------
import Control.Monad

import Data.Array.Storable
import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Word

import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Graphics.Rendering.FreeType.Internal.Bitmap

import Lambency.Render
import Lambency.Sprite
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Utils

import Linear

import Paths_lambency
import System.FilePath
--------------------------------------------------------------------------------

newtype Font = Font { getGlyph :: Char -> Maybe SpriteFrame }

renderUIString :: Font -> String -> V2 Float -> GameMonad ()
renderUIString _ "" _ = return ()
renderUIString font str pos = let
  glyphSize :: Char -> V2 Int
  glyphSize c =
    case (getGlyph font c) of
      Nothing -> zero
      Just f -> spriteSize f

  glyphSizes :: [V2 Int]
  glyphSizes = map glyphSize str

  positions :: [V2 Float]
  positions = let
    helper _ _ [] = []
    helper (V2 cx cy) (V2 lx _) ((V2 x y):xs) =
      let nextPos = V2 (cx + (fromIntegral lx)) cy
      in nextPos : (helper nextPos (V2 x y) xs)
   in
    helper pos (head glyphSizes) (tail glyphSizes)
  in do
   mapM_ (\(c, p) ->
           case (getGlyph font c) of
                 Nothing -> return ()
                 Just f ->
                   let (V2 sx sy) = fmap ((*0.5) . fromIntegral) $ spriteSize f
                       xf = nonuniformScale (V3 sx sy 1) $ identity
                       ro = xformObject xf $ frameRO f
                   in addRenderUIAction p ro)
     (zip str positions)

loadFont :: Sprite -> [(Char, V2 Int, V2 Int)] -> IO (Font)
loadFont sprite charInfo = return . Font $ \c -> charMap >>= (Map.lookup c)
  where
    charMap = do
      return $ Map.fromList $
        zip (map (\(c', _, _) -> c') charInfo) (cyclicToList $ getFrames sprite)

charString :: [Char]
charString = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
             ++ "[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

loadSystemFont :: IO (Font)
loadSystemFont = let
  systemOffsets = [V2 x 0 | x <- [0,14..]]
  systemSizes = repeat (V2 13 24)

  charInfo = zip3 charString systemOffsets systemSizes
  offsets = map (\(_, x, _) -> x) charInfo
  sizes = map (\(_, _, x) -> x) charInfo
  in do
    fontTexFile <- getDataFileName $ "font" <.> "png"
    Just s <- loadAnimatedSprite fontTexFile sizes offsets
    loadFont s charInfo

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

uploadGlyph :: FT_Face -> Texture -> Int -> Char -> IO (Int)
uploadGlyph ft_face tex widthAccum c = do
  runFreeType $ ft_Load_Char ft_face (cvt c) ft_LOAD_RENDER
  g <- peek $ glyph ft_face
  bm <- peek $ bitmap g
  updateTexture
    tex
    (buffer bm)
    (cvt widthAccum, 0) $
    (cvt $ width bm, cvt $ rows bm)
  return (widthAccum + (cvt $ width bm))
  where
    cvt :: (Enum a, Enum b) => a -> b
    cvt = toEnum . fromEnum

loadTTFont :: FilePath -> Int -> IO (Font)
loadTTFont filepath fontSize = do
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
  (texW, texH) <- foldM (analyzeGlyph ft_face) (0, 0) charString

  -- Create a texture to store all of the glyphs
  texZeroA <- ((newArray (1, texW*texH) 0) :: IO (StorableArray Int Word8))
  tex <- withStorableArray texZeroA $ \ptr ->
    initializeTexture ptr (fromIntegral texW, fromIntegral texH) Alpha8

  -- Place each glyph into the texture
  foldM_ (uploadGlyph ft_face tex) 0 charString

  -- Generate info for our rendering
  sizes <- mapM (analyzeGlyph ft_face (0, 0)) charString

  -- Use info to generate our 3-tuple
  let offsets = snd $ mapAccumL (\a (w, _) -> (a + w, V2 a 0)) 0 sizes
      sizesV = map (\(x, y) -> V2 x y) sizes
      charInfo = zip3 charString offsets sizesV

  Just s <- loadAnimatedSpriteWithTexture tex sizesV offsets
  loadFont s charInfo

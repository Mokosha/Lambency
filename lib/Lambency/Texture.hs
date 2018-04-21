module Lambency.Texture (
  textureSize,
  createSolidTexture,
  loadTexture,
  destroyTexture,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import Lambency.Types
import qualified Lambency.Renderer.OpenGL.Texture as OpenGL

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP

-- import System.Directory
import Data.Array.Storable
import Data.Word
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString as BS

import Linear.V2
import Linear.V4

import System.FilePath (takeExtension)
--------------------------------------------------------------------------------
textureSize :: Texture -> V2 Int
textureSize (Texture (OpenGLTexHandle _ sz) _) = getTextureSize sz
textureSize (RenderTexture (OpenGLTexHandle _ sz) _) = getTextureSize sz


loadTextureFromPNGorTGA :: Renderer -> JP.DynamicImage -> IO Texture
loadTextureFromPNGorTGA r (JP.ImageRGBA8 (JP.Image width height dat)) = do
  Vector.unsafeWith dat $ \ptr ->
    mkTexture r ptr (fromIntegral <$> V2 width height) RGBA8
loadTextureFromPNGorTGA r (JP.ImageRGB8 (JP.Image width height dat)) = do
  Vector.unsafeWith dat $ \ptr ->
    mkTexture r ptr (fromIntegral <$> V2 width height) RGB8
loadTextureFromPNGorTGA _ _ = error "Unknown PNG or TGA color type"

loadTextureFromPNG :: Renderer -> FilePath -> IO (Maybe Texture)
loadTextureFromPNG r filename = do
  pngBytes <- BS.readFile filename
  case JP.decodePng pngBytes of
    Left str -> do
      putStrLn $ "Error loading PNG file: " ++ str
      return Nothing
    Right img -> Just <$> loadTextureFromPNGorTGA r img

loadTextureFromTGA :: Renderer -> FilePath -> IO (Maybe Texture)
loadTextureFromTGA r filename = do
  tgaBytes <- BS.readFile filename
  case JP.decodeTga tgaBytes of
    Left str -> do
      putStrLn $ "Error loading TGA file: " ++ str
      return Nothing
    Right img -> Just <$> loadTextureFromPNGorTGA r img

flipY :: JP.Pixel a => JP.Image a -> JP.Image a
flipY img =
  let w = JP.imageWidth img
      h = JP.imageHeight img
      genPixel x y = JP.pixelAt img x (h - y - 1)
  in JP.generateImage genPixel w h

loadTextureFromJPG :: Renderer -> FilePath -> IO (Maybe Texture)
loadTextureFromJPG r filename = do
  jpgBytes <- BS.readFile filename
  jpgImg <- case JP.decodeJpeg jpgBytes of
    Left str -> do
      putStrLn $ "Error loading JPG file: " ++ str
      return Nothing
    Right img -> return (Just img)
  case jpgImg of
    Nothing -> return Nothing
    Just img -> do
      case img of
        (JP.ImageYCbCr8 i) ->
          let (JP.ImageRGB8 (JP.Image width height dat)) =
                JP.ImageRGB8 (JP.convertImage $ flipY i)
          in do
            tex <- Vector.unsafeWith dat $ \ptr ->
              mkTexture r ptr (fromIntegral <$> V2 width height) RGB8
            return $ Just tex
        _ -> return Nothing

createSolidTexture :: Renderer -> V4 Word8 -> IO Texture
createSolidTexture rend (V4 r g b a) = do
  carr <- newListArray (0 :: Integer, 3) [r, g, b, a]
  withStorableArray carr (\ptr -> mkTexture rend ptr (pure 1) RGBA8)

data ImageType
  = ImageType'PNG
  | ImageType'JPG
  | ImageType'TGA
  deriving (Show, Eq, Ord, Enum)

-- !FIXME! might do better to introspect on the bytes and figure out
-- what kind of image it is... does JuicyPixels do this?
determineImageType :: FilePath -> Maybe ImageType
determineImageType = fromExtension . takeExtension
  where
    fromExtension ".png" = Just ImageType'PNG
    fromExtension ".PNG" = Just ImageType'PNG
    fromExtension ".jpg" = Just ImageType'JPG
    fromExtension ".JPG" = Just ImageType'JPG
    fromExtension ".jpeg" = Just ImageType'JPG
    fromExtension ".JPEG" = Just ImageType'JPG
    fromExtension ".tga" = Just ImageType'TGA
    fromExtension ".TGA" = Just ImageType'TGA
    fromExtension _ = Nothing

loadTextureWithType :: Renderer -> FilePath -> Maybe ImageType -> IO (Maybe Texture)
loadTextureWithType _ fp Nothing = do
  putStrLn $ "WARNING: Unsupported image type: " ++ fp
  return Nothing
loadTextureWithType r filename (Just ImageType'PNG) = loadTextureFromPNG r filename
loadTextureWithType r filename (Just ImageType'JPG) = loadTextureFromJPG r filename
loadTextureWithType r filename (Just ImageType'TGA) = loadTextureFromTGA r filename

loadTexture :: Renderer -> FilePath -> IO (Maybe Texture)
loadTexture r filename =
  let imageType = determineImageType filename
  in loadTextureWithType r filename imageType

destroyTexture :: Texture -> IO ()
destroyTexture (Texture (OpenGLTexHandle h _) _) = OpenGL.destroyTexture h
destroyTexture (RenderTexture (OpenGLTexHandle h _) (OpenGLFBOHandle fboh)) =
  OpenGL.destroyTexture h >> OpenGL.destroyFBO fboh

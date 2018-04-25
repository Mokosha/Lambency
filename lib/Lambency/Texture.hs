module Lambency.Texture (
  textureSize,
  createSolidTexture,
  loadTexture,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Monad.Reader

import Lambency.ResourceLoader
import Lambency.Types

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP

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

loadTextureFromPNGorTGA :: JP.DynamicImage -> ResourceLoader Texture
loadTextureFromPNGorTGA (JP.ImageRGBA8 (JP.Image width height dat)) = do
  r <- ask
  runLoaderWith (Vector.unsafeWith dat) $ \ptr ->
    mkTexture r ptr (fromIntegral <$> V2 width height) RGBA8
loadTextureFromPNGorTGA (JP.ImageRGB8 (JP.Image width height dat)) = do
  r <- ask
  runLoaderWith (Vector.unsafeWith dat) $ \ptr ->
    mkTexture r ptr (fromIntegral <$> V2 width height) RGB8
loadTextureFromPNGorTGA _ = error "Unknown PNG or TGA color type"

loadTextureFromPNG :: FilePath -> ResourceLoader (Maybe Texture)
loadTextureFromPNG filename = do
  pngBytes <- liftIO $ BS.readFile filename
  case JP.decodePng pngBytes of
    Left str -> do
      liftIO $ putStrLn $ "Error loading PNG file: " ++ str
      return Nothing
    Right img -> Just <$> loadTextureFromPNGorTGA img

loadTextureFromTGA :: FilePath -> ResourceLoader (Maybe Texture)
loadTextureFromTGA filename = do
  tgaBytes <- liftIO $ BS.readFile filename
  case JP.decodeTga tgaBytes of
    Left str -> do
      liftIO $ putStrLn $ "Error loading TGA file: " ++ str
      return Nothing
    Right img -> Just <$> loadTextureFromPNGorTGA img

flipY :: JP.Pixel a => JP.Image a -> JP.Image a
flipY img =
  let w = JP.imageWidth img
      h = JP.imageHeight img
      genPixel x y = JP.pixelAt img x (h - y - 1)
  in JP.generateImage genPixel w h

loadTextureFromJPG :: FilePath -> ResourceLoader (Maybe Texture)
loadTextureFromJPG filename =  do
  jpgImg <- liftIO $ do
    jpgBytes <- BS.readFile filename
    case JP.decodeJpeg jpgBytes of
      Left str -> do
        putStrLn $ "Error loading JPG file: " ++ str
        return Nothing
      Right img -> return (Just img)
  case jpgImg of
    Just (JP.ImageYCbCr8 i) -> do
      let (JP.ImageRGB8 (JP.Image width height dat)) =
            JP.ImageRGB8 (JP.convertImage $ flipY i)
      runLoaderWith (Vector.unsafeWith dat) $ \ptr -> do
        r <- ask
        Just <$> mkTexture r ptr (fromIntegral <$> V2 width height) RGB8
    _ -> return Nothing

createSolidTexture :: V4 Word8 -> ResourceLoader Texture
createSolidTexture (V4 r g b a) = do
  carr <- liftIO $ newListArray (0 :: Integer, 3) [r, g, b, a]
  runLoaderWith (withStorableArray carr) $ \ptr -> do
    rend <- ask
    mkTexture rend ptr (pure 1) RGBA8

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

loadTextureWithType :: FilePath -> Maybe ImageType
                    -> ResourceLoader (Maybe Texture)
loadTextureWithType fp Nothing = do
  liftIO $ putStrLn $ "WARNING: Unsupported image type: " ++ fp
  return Nothing
loadTextureWithType filename (Just ImageType'PNG) = loadTextureFromPNG filename
loadTextureWithType filename (Just ImageType'JPG) = loadTextureFromJPG filename
loadTextureWithType filename (Just ImageType'TGA) = loadTextureFromTGA filename

loadTexture :: FilePath -> ResourceLoader (Maybe Texture)
loadTexture filename =
  let imageType = determineImageType filename
  in loadTextureWithType filename imageType

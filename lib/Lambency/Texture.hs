module Lambency.Texture (
  getGLTexObj,
  textureSize,
  isRenderTexture,
  initializeTexture,
  createSolidTexture,
  createDepthTexture,
  loadTexture,
  destroyTexture,
  updateTexture,
  bindRenderTexture,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Types

import qualified Codec.Picture as JP
import qualified Codec.Picture.Types as JP

-- import System.Directory
import Foreign.Ptr
import Data.Array.Storable
import Data.Word
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString as BS

import Linear.V2
import Linear.V4

import System.FilePath (takeExtension)
--------------------------------------------------------------------------------

kShadowMapSize :: GL.GLsizei
kShadowMapSize = 1024

getGLTexObj :: Texture -> GL.TextureObject
getGLTexObj (Texture (OpenGLTexHandle h _) _) = h
getGLTexObj (RenderTexture (OpenGLTexHandle h _) _) = h

getGLTexFmt :: Texture -> TextureFormat
getGLTexFmt (Texture _ fmt) = fmt
getGLTexFmt (RenderTexture _ _) =
  error "Render textures don't have a texture format"

fmt2glpfmt :: TextureFormat -> GL.PixelFormat
fmt2glpfmt RGBA8 = GL.RGBA
fmt2glpfmt RGB8 = GL.RGB
fmt2glpfmt Alpha8 = GL.Alpha

internalglpfmt :: GL.PixelFormat -> GL.PixelInternalFormat
internalglpfmt GL.RGBA = GL.RGBA8
internalglpfmt GL.RGB = GL.RGB8
internalglpfmt GL.Alpha = GL.Alpha8
internalglpfmt _ =
  error "We don't know the data used for this pixelformat"

isRenderTexture :: Texture -> Bool
isRenderTexture (Texture _ _) = False
isRenderTexture (RenderTexture _ _) = True

bindRenderTexture :: Texture -> IO ()
bindRenderTexture (Texture _ _) = return ()
bindRenderTexture (RenderTexture _ (OpenGLFBOHandle h)) = do
  GL.bindFramebuffer GL.Framebuffer GL.$= h
  GL.viewport GL.$= (GL.Position 0 0, GL.Size kShadowMapSize kShadowMapSize)

textureSize :: Texture -> V2 Int
textureSize (Texture (OpenGLTexHandle _ sz) _) = getTextureSize sz
textureSize (RenderTexture (OpenGLTexHandle _ sz) _) = getTextureSize sz

destroyTexture :: Texture -> IO ()
destroyTexture (Texture (OpenGLTexHandle h _) fmt) = do
  putStrLn $ concat ["Destroying ", show fmt, " texture: ", show h]
  GL.deleteObjectName h
destroyTexture (RenderTexture (OpenGLTexHandle h _) (OpenGLFBOHandle fboh)) = do
  putStrLn $ "Destroying framebuffer object."
  GL.deleteObjectName h
  GL.deleteObjectName fboh

initializeTexture :: Ptr a -> (Word32, Word32) -> TextureFormat -> IO (Texture)
initializeTexture ptr (w, h) fmt = do
  handle <- GL.genObjectName
  GL.textureBinding GL.Texture2D GL.$= Just handle

  let glfmt = fmt2glpfmt fmt
      size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
      pd = GL.PixelData glfmt GL.UnsignedByte ptr
  GL.texImage2D GL.Texture2D GL.NoProxy 0 (internalglpfmt glfmt) size 0 pd
  GL.generateMipmap' GL.Texture2D
  GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.Repeat)
  GL.textureFunction GL.$= GL.Replace

  putStrLn $ concat [
    "Loaded ", show fmt,
    " texture with dimensions ", show (w, h),
    ": ", show handle]
  return $ flip Texture fmt
         $ OpenGLTexHandle handle (TexSize $ fromEnum <$> V2 w h)

updateTexture :: Texture -> Ptr a -> (Word32, Word32) -> (Word32, Word32) -> IO ()
updateTexture (RenderTexture _ _) _ _ _ = putStrLn "Cannot update render texture"
updateTexture tex ptr (x, y) (w, h) = do
  GL.textureBinding GL.Texture2D GL.$= Just (getGLTexObj tex)

  let pd = GL.PixelData (fmt2glpfmt $ getGLTexFmt tex) GL.UnsignedByte ptr
      size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
      pos = GL.TexturePosition2D (fromIntegral x) (fromIntegral y)
  GL.texSubImage2D GL.Texture2D 0 pos size pd

loadTextureFromPNGorTGA :: JP.DynamicImage -> IO (Texture)
loadTextureFromPNGorTGA (JP.ImageRGBA8 (JP.Image width height dat)) = do
  Vector.unsafeWith dat $ \ptr ->
    initializeTexture ptr (fromIntegral width, fromIntegral height) RGBA8
loadTextureFromPNGorTGA (JP.ImageRGB8 (JP.Image width height dat)) = do
  Vector.unsafeWith dat $ \ptr ->
    initializeTexture ptr (fromIntegral width, fromIntegral height) RGB8
loadTextureFromPNGorTGA _ = error "Unknown PNG or TGA color type"

loadTextureFromPNG :: FilePath -> IO (Maybe Texture)
loadTextureFromPNG filename = do
  pngBytes <- BS.readFile filename
  case JP.decodePng pngBytes of
    Left str -> do
      putStrLn $ "Error loading PNG file: " ++ str
      return Nothing
    Right img -> Just <$> loadTextureFromPNGorTGA img

loadTextureFromTGA :: FilePath -> IO (Maybe Texture)
loadTextureFromTGA filename = do
  tgaBytes <- BS.readFile filename
  case JP.decodeTga tgaBytes of
    Left str -> do
      putStrLn $ "Error loading TGA file: " ++ str
      return Nothing
    Right img -> Just <$> loadTextureFromPNGorTGA img

flipY :: JP.Pixel a => JP.Image a -> JP.Image a
flipY img =
  let w = JP.imageWidth img
      h = JP.imageHeight img
      genPixel x y = JP.pixelAt img x (h - y - 1)
  in JP.generateImage genPixel w h

loadTextureFromJPG :: FilePath -> IO(Maybe Texture)
loadTextureFromJPG filename = do
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
              initializeTexture ptr (fromIntegral width, fromIntegral height) RGB8
            return $ Just tex
        _ -> return Nothing

createDepthTexture :: IO Texture
createDepthTexture = do
  handle <- GL.genObjectName
  GL.textureBinding GL.Texture2D GL.$= Just handle
  GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
  GL.textureCompareMode GL.Texture2D GL.$= (Just GL.Lequal)
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.DepthComponent'
                (GL.TextureSize2D kShadowMapSize kShadowMapSize) 0
                (GL.PixelData GL.DepthComponent GL.UnsignedInt nullPtr)

  putStrLn "Creating framebuffer object..."
  rbHandle <- GL.genObjectName
  GL.bindFramebuffer GL.Framebuffer GL.$= rbHandle
  GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D handle 0
  GL.drawBuffer GL.$= GL.NoBuffers
  GL.readBuffer GL.$= GL.NoBuffers
  GL.get (GL.framebufferStatus GL.Framebuffer) >>=
    putStrLn . ((++) "Checking framebuffer status...") . show

  GL.depthMask GL.$= GL.Enabled
  GL.depthFunc GL.$= Just GL.Lequal
  GL.cullFace GL.$= Just GL.Back
  GL.bindFramebuffer GL.Framebuffer GL.$= GL.defaultFramebufferObject

  let shadowMapSize = TexSize $ fromEnum <$> V2 kShadowMapSize kShadowMapSize
  return $ RenderTexture (OpenGLTexHandle handle shadowMapSize)
         $ OpenGLFBOHandle rbHandle

createSolidTexture :: V4 Word8 -> IO(Texture)
createSolidTexture (V4 r g b a) = do
  carr <- newListArray (0 :: Integer, 3) [r, g, b, a]
  withStorableArray carr (\ptr -> initializeTexture ptr (1, 1) RGBA8)

data ImageType
  = ImageType'PNG
  | ImageType'JPG
  | ImageType'TGA
  deriving (Show, Eq, Ord, Enum)

determineImageType :: FilePath -> IO (Maybe ImageType)
determineImageType filename = do
  -- !FIXME! might do better to introspect on the bytes and figure out
  -- what kind of image it is... does JuicyPixels do this?
  return $ fromExtension $ takeExtension filename
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

loadTextureWithType :: FilePath -> Maybe ImageType -> IO (Maybe Texture)
loadTextureWithType fp Nothing = do
  putStrLn $ "WARNING: Unsupported image type: " ++ fp
  return Nothing
loadTextureWithType filename (Just ImageType'PNG) = loadTextureFromPNG filename
loadTextureWithType filename (Just ImageType'JPG) = loadTextureFromJPG filename
loadTextureWithType filename (Just ImageType'TGA) = loadTextureFromTGA filename

loadTexture :: FilePath -> IO (Maybe Texture)
loadTexture filename = determineImageType filename >>= (loadTextureWithType filename)

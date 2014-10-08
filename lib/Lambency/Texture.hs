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
  clearRenderTexture,
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Lambency.Types

import qualified Codec.Picture as JP

import Control.Monad (unless)

-- import System.Directory
import Foreign.Ptr
import Data.Array.Storable
import Data.Array.Unboxed
import Data.Word
import qualified Data.Vector.Storable as Vector
import qualified Data.ByteString as BS

import Linear.V2

import System.FilePath (takeExtension)
--------------------------------------------------------------------------------

kShadowMapSize :: GL.GLsizei
kShadowMapSize = 1024

getGLTexObj :: Texture -> GL.TextureObject
getGLTexObj (Texture (TexHandle h _) _) = h
getGLTexObj (RenderTexture (TexHandle h _) _) = h

getGLTexFmt :: Texture -> TextureFormat
getGLTexFmt (Texture _ fmt) = fmt
getGLTexFmt (RenderTexture _ _) = error "Render textures don't have a texture format"

fmt2glpfmt :: TextureFormat -> GL.PixelFormat
fmt2glpfmt RGBA8 = GL.RGBA
fmt2glpfmt RGB8 = GL.RGB
fmt2glpfmt Alpha8 = GL.Alpha

internalglpfmt :: GL.PixelFormat -> GL.PixelInternalFormat
internalglpfmt GL.RGBA = GL.RGBA8
internalglpfmt GL.RGB = GL.RGB8
internalglpfmt GL.Alpha = GL.Alpha8
internalglpfmt _ = error "We don't know the data used for this pixelformat"

isRenderTexture :: Texture -> Bool
isRenderTexture (Texture _ _) = False
isRenderTexture (RenderTexture _ _) = True

bindRenderTexture :: Texture -> IO ()
bindRenderTexture (Texture _ _) = return ()
bindRenderTexture (RenderTexture _ h) = do
  GL.bindFramebuffer GL.Framebuffer GL.$= h
  GL.viewport GL.$= (GL.Position 0 0, GL.Size kShadowMapSize kShadowMapSize)

textureSize :: Texture -> V2 Int
textureSize (Texture (TexHandle _ sz) _) = getTextureSize sz
textureSize (RenderTexture (TexHandle _ sz) _) = getTextureSize sz

clearRenderTexture :: IO ()
clearRenderTexture = do
  let depthfile = "depth.png"
--  exists <- doesFileExist depthfile
--  unless exists $ do
  unless True $ do
    GL.flush
    arr <- newArray_ ((0, 0), (fromIntegral $ kShadowMapSize - 1, fromIntegral $ kShadowMapSize - 1))
    withStorableArray arr (\ptr -> do
      GL.readPixels (GL.Position 0 0)
        (GL.Size kShadowMapSize kShadowMapSize)
        (GL.PixelData GL.DepthComponent GL.Float ptr)
      GL.flush)
    farr <- (freeze :: StorableArray (Int, Int) Float -> IO (UArray (Int, Int) Float)) arr
    let img = JP.generateImage
              (\x y -> (round :: Float -> Word16) $ 65535 * (farr ! (x, y)))
              (fromIntegral kShadowMapSize) (fromIntegral kShadowMapSize)

        smallest :: Integer
        smallest = fromIntegral $ Vector.minimum (JP.imageData img)

        largest :: Integer
        largest = fromIntegral $ Vector.maximum (JP.imageData img)

        modulate :: Word16 -> Word16
        modulate x = fromIntegral $
                     (65535 * ((fromIntegral :: Integral a => a -> Integer) x - smallest))
                     `div`
                     (largest - smallest)

    JP.writePng depthfile $ JP.pixelMap modulate img
  GL.bindFramebuffer GL.Framebuffer GL.$= GL.defaultFramebufferObject
  (Just m) <- GLFW.getCurrentContext
  (szx, szy) <- GLFW.getFramebufferSize m
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

destroyTexture :: Texture -> IO ()
destroyTexture (Texture (TexHandle h _) _) = GL.deleteObjectName h
destroyTexture (RenderTexture (TexHandle h _) fboh) = do
  GL.deleteObjectName h
  GL.deleteObjectName fboh

initializeTexture :: Ptr a -> (Word32, Word32) -> TextureFormat -> IO (Texture)
initializeTexture ptr (w, h) fmt = do
  handle <- GL.genObjectName
  GL.activeTexture GL.$= GL.TextureUnit 0
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

  putStrLn $ "Loaded " ++ (show fmt) ++ "texture with dimensions " ++ (show (w, h))
  return $ Texture (TexHandle handle $ TexSize $ fmap fromEnum (V2 w h)) fmt

updateTexture :: Texture -> Ptr a -> (Word32, Word32) -> (Word32, Word32) -> IO ()
updateTexture (RenderTexture _ _) _ _ _ = putStrLn "Cannot update render texture"
updateTexture tex ptr (x, y) (w, h) = do
  GL.textureBinding GL.Texture2D GL.$= Just (getGLTexObj tex)

  let pd = GL.PixelData (fmt2glpfmt $ getGLTexFmt tex) GL.UnsignedByte ptr
      size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
      pos = GL.TexturePosition2D (fromIntegral x) (fromIntegral y)
  GL.texSubImage2D GL.Texture2D 0 pos size pd

loadTextureFromPNG :: FilePath -> IO(Maybe Texture)
loadTextureFromPNG filename = do
  pngBytes <- BS.readFile filename
  pngImg <- case JP.decodePng pngBytes of
    Left str -> do
      putStrLn $ "Error loading PNG file: " ++ str
      return Nothing
    Right img -> return (Just img)
  case pngImg of
    Nothing -> return Nothing
    Just img -> do
      case img of
        (JP.ImageRGBA8 (JP.Image width height dat)) -> do
          tex <- Vector.unsafeWith dat $ \ptr ->
            initializeTexture ptr (fromIntegral width, fromIntegral height) RGBA8
          return $ Just tex
        (JP.ImageRGB8 (JP.Image width height dat)) -> do
          tex <- Vector.unsafeWith dat $ \ptr ->
            initializeTexture ptr (fromIntegral width, fromIntegral height) RGB8
          return $ Just tex
        _ -> return Nothing

createDepthTexture :: IO (Texture)
createDepthTexture = do
  handle <- GL.genObjectName
  GL.activeTexture GL.$= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D GL.$= Just handle
  GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing), GL.Nearest)
  GL.textureCompareMode GL.Texture2D GL.$= (Just GL.Greater)
  GL.depthTextureMode GL.Texture2D GL.$= GL.Intensity
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.DepthComponent32
    (GL.TextureSize2D kShadowMapSize kShadowMapSize) 0 $ GL.PixelData GL.DepthComponent GL.UnsignedInt nullPtr

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

  return $ RenderTexture (TexHandle handle $ TexSize $ fmap fromEnum (V2 kShadowMapSize kShadowMapSize)) rbHandle

createSolidTexture :: (Word8, Word8, Word8, Word8) -> IO(Texture)
createSolidTexture (r, g, b, a) = do
  carr <- newListArray (0 :: Integer, 3) [r, g, b, a]
  withStorableArray carr (\ptr -> initializeTexture ptr (1, 1) RGBA8)

data ImageType =
  ImageType'PNG
  deriving (Show, Eq, Ord, Enum)

determineImageType :: FilePath -> IO (Maybe ImageType)
determineImageType filename = do
  -- !FIXME! might do better to introspect on the bytes and figure out
  -- what kind of image it is... does JuicyPixels do this?
  return $ fromExtension $ takeExtension filename
  where
    fromExtension ".png" = Just ImageType'PNG
    fromExtension _ = Nothing

loadTextureWithType :: FilePath -> Maybe ImageType -> IO (Maybe Texture)
loadTextureWithType _ Nothing = return Nothing
loadTextureWithType filename (Just ImageType'PNG) = loadTextureFromPNG filename

loadTexture :: FilePath -> IO (Maybe Texture)
loadTexture filename = determineImageType filename >>= (loadTextureWithType filename)

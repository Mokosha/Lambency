module Graphics.Rendering.Lambency.Texture (
  Texture,
  TextureFormat(..),
  FBOHandle,
  TextureHandle,
  getHandle,
  getTextureCamera,
  createFramebufferObject,
  createSolidTexture,
  createDepthTexture,
  loadTextureFromPNG,
  destroyTexture,
  bindRenderTexture,
  clearRenderTexture,
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import qualified Codec.Image.PNG as PNG
import qualified Graphics.Pgm as PGM

import Graphics.Rendering.Lambency.Camera

import Control.Monad (unless)

import System.Directory
import Foreign.Ptr
import Data.Array.Storable
import Data.Array.Unboxed
import Data.Word
--------------------------------------------------------------------------------

type FBOHandle = GL.FramebufferObject
type TextureHandle = GL.TextureObject
data TextureFormat = RGBA8
                   | RGB8
                     deriving(Show, Eq)

data Texture = Texture TextureHandle TextureFormat
             | RenderTexture TextureHandle FBOHandle Camera
               deriving(Show, Eq)

getHandle :: Texture -> TextureHandle
getHandle (Texture h _) = h
getHandle (RenderTexture h _ _) = h

getTextureCamera :: Texture -> Maybe (Camera, FBOHandle)
getTextureCamera (Texture _ _) = Nothing
getTextureCamera (RenderTexture _ h cam) = Just (cam, h)

fmt2glpfmt :: TextureFormat -> GL.PixelFormat
fmt2glpfmt RGBA8 = GL.RGBA
fmt2glpfmt RGB8 = GL.RGB

bindRenderTexture :: FBOHandle -> IO ()
bindRenderTexture h = do
  GL.bindFramebuffer GL.Framebuffer GL.$= h
  GL.viewport GL.$= (GL.Position 0 0, GL.Size 512 512)

clearRenderTexture :: IO ()
clearRenderTexture = do
  let depthfile = "depth.pgm"
  exists <- doesFileExist depthfile
  unless exists $ do
    GL.flush
    arr <- newArray_ ((0, 0), (511, 511))
    withStorableArray arr (\ptr -> do
      GL.readPixels (GL.Position 0 0)
        (GL.Size 512 512)
        (GL.PixelData GL.DepthComponent GL.Float ptr)
      GL.flush)
    farr <- (freeze :: StorableArray (Int, Int) Float -> IO (UArray (Int, Int) Float)) arr
    PGM.arrayToFile depthfile (amap ((round :: Float -> Word16) . (*65535)) farr)
  GL.bindFramebuffer GL.Framebuffer GL.$= GL.defaultFramebufferObject
  GL.viewport GL.$= (GL.Position 0 0, GL.Size 640 480)

destroyTexture :: Texture -> IO ()
destroyTexture (Texture h _) = GL.deleteObjectName h
destroyTexture (RenderTexture h fboh _) = do
  GL.deleteObjectName h
  GL.deleteObjectName fboh

createFramebufferObject :: TextureFormat -> IO (Texture)
createFramebufferObject fmt = do
  handle <- GL.genObjectName
  return $ Texture handle fmt

initializeTexture :: Ptr a -> (Word32, Word32) -> TextureFormat -> IO(Texture)
initializeTexture ptr (w, h) fmt = do
  handle <- GL.genObjectName
  GL.activeTexture GL.$= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D GL.$= Just handle

  let size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
      pd = GL.PixelData (fmt2glpfmt fmt) GL.UnsignedByte ptr
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 pd
  GL.generateMipmap' GL.Texture2D
  GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.Repeat)

  putStrLn $ "Loaded texture with dimensions " ++ (show (w, h))
  return $ Texture handle fmt

loadTextureFromPNG :: FilePath -> IO(Maybe Texture)
loadTextureFromPNG filename = do
  pngFile <- PNG.loadPNGFile filename
  pngImg <- case pngFile of
    Left str -> do
      putStrLn $ "Error loading PNG file: " ++ str
      return Nothing
    Right img -> return (Just img)
  case pngImg of
    Nothing -> return Nothing
    Just img -> do
      tex <- withStorableArray (PNG.imageData img) (\ptr ->
        if (PNG.hasAlphaChannel img) then
          initializeTexture ptr (PNG.dimensions img) RGBA8
        else
          initializeTexture ptr (PNG.dimensions img) RGB8)
      
      return $ Just tex

createDepthTexture :: Camera -> IO (Texture)
createDepthTexture cam = do
  handle <- GL.genObjectName
  GL.activeTexture GL.$= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D GL.$= Just handle
  GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
  GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest, Nothing), GL.Nearest)
  GL.depthTextureMode GL.Texture2D GL.$= GL.Intensity
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.DepthComponent32
    (GL.TextureSize2D 512 512) 0 $ GL.PixelData GL.DepthComponent GL.UnsignedInt nullPtr

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

  return $ RenderTexture handle rbHandle cam

createSolidTexture :: (Word8, Word8, Word8, Word8) -> IO(Texture)
createSolidTexture (r, g, b, a) = do
  carr <- newListArray (0 :: Integer, 3) [r, g, b, a]
  withStorableArray carr (\ptr -> initializeTexture ptr (1, 1) RGBA8)

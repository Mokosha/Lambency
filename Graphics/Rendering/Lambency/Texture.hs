module Graphics.Rendering.Lambency.Texture (
  Texture,
  TextureFormat(..),
  TextureHandle,
  getHandle,
  createFramebufferObject,
  createSolidTexture,
  createDepthTexture,
  loadTextureFromPNG
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import qualified Codec.Image.PNG as PNG

import Graphics.Rendering.Lambency.Camera

import Foreign.Ptr
import Data.Array.Storable
import Data.Word
--------------------------------------------------------------------------------

type FBOHandle = GL.FramebufferObject
type TextureHandle = GL.TextureObject
data TextureFormat = RGBA8
                   | RGB8
data Texture = Texture TextureHandle TextureFormat
             | RenderTexture TextureHandle FBOHandle Camera

getHandle :: Texture -> TextureHandle
getHandle (Texture h _) = h
getHandle (RenderTexture h _ _) = h

fmt2glpfmt :: TextureFormat -> GL.PixelFormat
fmt2glpfmt RGBA8 = GL.RGBA
fmt2glpfmt RGB8 = GL.RGB

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
  GL.bindFramebuffer GL.DrawFramebuffer GL.$= rbHandle
  GL.framebufferTexture2D GL.DrawFramebuffer GL.DepthAttachment GL.Texture2D handle 0
  status <- GL.get $ GL.framebufferStatus GL.DrawFramebuffer
  putStrLn $ "Checking framebuffer status..." ++ (show status)
  GL.bindFramebuffer GL.DrawFramebuffer GL.$= GL.defaultFramebufferObject

  return $ RenderTexture handle rbHandle cam

createSolidTexture :: (Word8, Word8, Word8, Word8) -> IO(Texture)
createSolidTexture (r, g, b, a) = do
  carr <- newListArray (0 :: Integer, 3) [r, g, b, a]
  withStorableArray carr (\ptr -> initializeTexture ptr (1, 1) RGBA8)

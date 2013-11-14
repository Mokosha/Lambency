module Graphics.Rendering.Lambency.Texture (
  Texture,
  TextureFormat(..),
  TextureHandle,
  getHandle,
  createFramebufferObject,
  createSolidTexture,
  loadTextureFromPNG
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import qualified Codec.Image.PNG as PNG

import Foreign.Ptr
import Data.Array.Storable
import Data.Word
--------------------------------------------------------------------------------

type TextureHandle = GL.TextureObject
data TextureFormat = RGBA8
                   | RGB8
data Texture = Texture TextureHandle TextureFormat

getHandle :: Texture -> TextureHandle
getHandle (Texture h _) = h

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

  GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
  GL.generateMipmap GL.Texture2D GL.$= GL.Enabled
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

createSolidTexture :: (Word8, Word8, Word8, Word8) -> IO(Texture)
createSolidTexture (r, g, b, a) = do
  carr <- newListArray (0 :: Integer, 3) [r, g, b, a]
  withStorableArray carr (\ptr -> initializeTexture ptr (1, 1) RGBA8)

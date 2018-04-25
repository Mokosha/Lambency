module Lambency.Renderer.OpenGL.Texture (
  getGLTexObj,
  isRenderTexture,
  initializeTexture,
  createDepthTexture,
  updateTexture,
  bindRenderTexture,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Monad.Writer

import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Types hiding (Renderer(..))

-- import System.Directory
import Foreign.Ptr
import Data.Word

import Linear.V2
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

destroyTexture :: GL.TextureObject -> IO ()
destroyTexture h = do
  putStrLn $ concat ["Destroying texture: ", show h]
  GL.deleteObjectName h

destroyFBO :: GL.FramebufferObject -> IO ()
destroyFBO fboh = do
  putStrLn "Destroying framebuffer object."
  GL.deleteObjectName fboh

initializeTexture :: Ptr a -> V2 Word32 -> TextureFormat -> ResourceLoader Texture
initializeTexture ptr (V2 w h) fmt = do
  handle <- liftIO GL.genObjectName
  tell $ destroyTexture handle

  liftIO $ do
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

updateTexture :: Texture -> Ptr a -> V2 Word32 -> V2 Word32 -> IO ()
updateTexture (RenderTexture _ _) _ _ _ = putStrLn "Cannot update render texture"
updateTexture tex ptr (V2 x y) (V2 w h) = do
  GL.textureBinding GL.Texture2D GL.$= Just (getGLTexObj tex)

  let pd = GL.PixelData (fmt2glpfmt $ getGLTexFmt tex) GL.UnsignedByte ptr
      size = GL.TextureSize2D (fromIntegral w) (fromIntegral h)
      pos = GL.TexturePosition2D (fromIntegral x) (fromIntegral y)
  GL.texSubImage2D GL.Texture2D 0 pos size pd

createDepthTexture :: V2 Word32 -> ResourceLoader Texture
createDepthTexture (V2 w h) = do
  handle <- liftIO $ GL.genObjectName
  
  liftIO $ do
    GL.textureBinding GL.Texture2D GL.$= Just handle
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.ClampToEdge)
    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureCompareMode GL.Texture2D GL.$= (Just GL.Lequal)
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.DepthComponent'
                  (GL.TextureSize2D (fromIntegral w) (fromIntegral h)) 0
                  (GL.PixelData GL.DepthComponent GL.UnsignedInt nullPtr)

  rbHandle <- liftIO $ GL.genObjectName
  liftIO $ do
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

  tell $ destroyFBO rbHandle
  tell $ destroyTexture handle

  liftIO $ putStrLn $ "Created FBO :" ++ show rbHandle
        >> putStrLn $ concat
                    [ "  with texture of dimensions ", show (w, h)
                    , ": ", show handle
                    ]

  let shadowMapSize = TexSize $ fromEnum <$> V2 w h
  return $ RenderTexture (OpenGLTexHandle handle shadowMapSize)
         $ OpenGLFBOHandle rbHandle

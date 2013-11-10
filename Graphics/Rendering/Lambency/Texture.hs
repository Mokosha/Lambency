module Graphics.Rendering.Lambency.Texture (
  TextureFormat(..),
  TextureHandle,
  createFramebufferObject
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
--------------------------------------------------------------------------------

data TextureFormat = RGBA8
                   | DEPTH8
                   | DEPTH24

type TextureHandle = GL.TextureObject

createFramebufferObject :: TextureFormat -> IO (TextureHandle)
createFramebufferObject fmt = GL.genObjectName

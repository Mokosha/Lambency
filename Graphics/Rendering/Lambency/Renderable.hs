module Graphics.Rendering.Lambency.Renderable (
  BoundRender(..),
  RenderPath(..),
  RenderObject(..),
  Renderable(..),
  createBasicRO
  ) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Material
import Graphics.Rendering.Lambency.Vertex
import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture

import Data.Array.IO
import Data.Array.Storable
import Data.Int
import Foreign.Storable
import Foreign.Ptr
--------------------------------------------------------------------------------

data BoundRender = BoundRender ShaderVar TextureHandle RenderPath
data RenderPath = RenderPath Material [BoundRender]

constructRenderPath :: Material -> IO(RenderPath)
constructRenderPath (Material shdr vars inputs) = do
  renderInputs <- mapM bindRender inputs
  return $ RenderPath (Material shdr vars inputs) renderInputs
  where bindRender :: MaterialRenderInput -> IO(BoundRender)
        bindRender (MaterialRenderInput uniform fmt mat) = do
          handle <- createFramebufferObject fmt
          path <- constructRenderPath mat
          return $ BoundRender uniform handle path

data RenderObject = RenderObject {
  renderPath :: RenderPath,
  render :: IO ()
}

createBasicRO :: [Vertex] -> [Int16] -> IO (RenderObject)
createBasicRO vs idxs =
  let
    flts :: [Float]
    flts = vs >>= toFloats
  in do
    vbo <- setupBuffer GL.ArrayBuffer flts
    ibo <- setupBuffer GL.ElementArrayBuffer idxs
    path <- constructRenderPath =<< createSimpleMaterial
    return $ RenderObject {
      renderPath = path,
      render = renderTris vbo ibo $ fromIntegral (length idxs)
    }
  where
    ptrsize :: (Storable a) => [a] -> GL.GLsizeiptr
    ptrsize [] = toEnum 0
    ptrsize xs = toEnum $ length xs * (sizeOf $ head xs)

    setupBuffer :: (Storable a) => GL.BufferTarget -> [a] -> IO( GL.BufferObject )
    setupBuffer tgt xs = do
      buf <- GL.genObjectName
      GL.bindBuffer tgt GL.$= (Just buf)
      varr <- newListArray (0, length xs - 1) xs
      withStorableArray varr (\ptr ->
        GL.bufferData tgt GL.$= (ptrsize xs, ptr, GL.StaticDraw))
      return buf

    renderTris :: GL.BufferObject -> GL.BufferObject -> GL.NumArrayIndices -> IO ()
    renderTris vbo ibo nIndices = let
      vadesc = GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)
      in do
        -- Bind appropriate buffers
        GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
        GL.vertexAttribPointer (GL.AttribLocation 0) GL.$= (GL.ToFloat, vadesc)

        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

        -- Render
        GL.drawElements GL.Triangles nIndices GL.UnsignedShort nullPtr


class Renderable a where
  createRenderObject :: a -> IO (RenderObject)

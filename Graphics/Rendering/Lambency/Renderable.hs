module Graphics.Rendering.Lambency.Renderable (
  RenderObject(..),
  Renderable(..),
  assignMaterial,
  switchMaterialTexture,
  createBasicRO
  ) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Material
import Graphics.Rendering.Lambency.Texture
import Graphics.Rendering.Lambency.Vertex

import Data.Array.IO
import Data.Array.Storable
import Data.Int
import Foreign.Storable
import Foreign.Ptr
--------------------------------------------------------------------------------

data RenderObject = RenderObject {
  material :: Material,
  render :: IO ()
}

assignMaterial :: RenderObject -> Material -> RenderObject
assignMaterial o m = (\ro -> ro { material = m }) o

switchMaterialTexture :: RenderObject -> String -> Texture -> RenderObject
switchMaterialTexture ro name tex =
  (\o -> o { material = switchTexture (material ro) name tex }) ro

createBasicRO :: [Vertex] -> [Int16] -> IO (RenderObject)
createBasicRO [] _ = do
  mat <- createSimpleMaterial
  return $ RenderObject {
    material = mat,
    render = return ()
  }
createBasicRO (v:vs) idxs =
  let
    flts :: [Float]
    flts = (v:vs) >>= toFloats
  in do
    vbo <- setupBuffer GL.ArrayBuffer flts
    ibo <- setupBuffer GL.ElementArrayBuffer idxs
    mat <- createSimpleMaterial
    return $ RenderObject {
      material = mat,
      render = do
        let f = if isTextured v then renderTexturedTris else renderTris
        f vbo ibo $ fromIntegral (length idxs)
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

    renderTexturedTris :: GL.BufferObject -> GL.BufferObject -> GL.NumArrayIndices -> IO ()
    renderTexturedTris vbo ibo nIndices = let
      posdesc = GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float)
      uvdesc = GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)
      in do
        -- Bind appropriate buffers
        GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
        GL.vertexAttribPointer (GL.AttribLocation 0) GL.$= (GL.ToFloat, posdesc)
        GL.vertexAttribPointer (GL.AttribLocation 1) GL.$= (GL.ToFloat, uvdesc)

        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

        -- Render
        GL.drawElements GL.Triangles nIndices GL.UnsignedShort nullPtr


class Renderable a where
  createRenderObject :: a -> IO (RenderObject)

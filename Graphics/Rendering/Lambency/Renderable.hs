module Graphics.Rendering.Lambency.Renderable (
  RenderObject(..),
  Renderable(..),
  assignMaterial,
  switchMaterialTexture,
  createBasicRO
  ) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Shader
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
      render = vertexRenderer mat vbo ibo $ fromIntegral (length idxs)
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

    vertexRenderer :: Material -> (GL.BufferObject -> GL.BufferObject -> GL.NumArrayIndices -> IO ())
    vertexRenderer mat = case v of
      (TVertex3 _ _) -> renderTTris (lu "position") (lu "texCoord")
      (OTVertex3 _ _ _) -> renderOTTris (lu "position") (lu "norm") (lu "texCoord")
      _ -> renderTris (lu "position")
     where
       lu :: String -> GL.AttribLocation
       lu name = case getMaterialVar mat name of
         Attribute _ loc -> loc
         Uniform _ _ -> GL.AttribLocation (-1)

    renderTris :: GL.AttribLocation -> GL.BufferObject -> GL.BufferObject -> GL.NumArrayIndices -> IO ()
    renderTris posLoc vbo ibo nIndices = let
      vadesc = GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)
      in do
        -- Bind appropriate buffers
        GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
        GL.vertexAttribPointer posLoc GL.$= (GL.ToFloat, vadesc)

        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

        -- Render
        GL.drawElements GL.Triangles nIndices GL.UnsignedShort nullPtr

    renderTTris :: GL.AttribLocation -> GL.AttribLocation ->
                   GL.BufferObject -> GL.BufferObject ->
                   GL.NumArrayIndices ->
                   IO ()
    renderTTris posLoc texCoordLoc vbo ibo nIndices = let
      posdesc = GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float)
      uvdesc = GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)
      in do
        -- Bind appropriate buffers
        GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
        GL.vertexAttribPointer posLoc GL.$= (GL.ToFloat, posdesc)
        GL.vertexAttribPointer texCoordLoc GL.$= (GL.ToFloat, uvdesc)

        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

        -- Render
        GL.drawElements GL.Triangles nIndices GL.UnsignedShort nullPtr

    renderOTTris :: GL.AttribLocation -> GL.AttribLocation -> GL.AttribLocation ->
                    GL.BufferObject -> GL.BufferObject ->
                    GL.NumArrayIndices ->
                    IO ()
    renderOTTris posLoc normLoc texCoordLoc vbo ibo nIndices = let
      posdesc = GL.VertexArrayDescriptor 3 GL.Float 32 (nullPtr :: Ptr Float)
      normdesc = GL.VertexArrayDescriptor 3 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 12)
      uvdesc = GL.VertexArrayDescriptor 2 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 24)
      in do
        -- Bind appropriate buffers
        GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
        GL.vertexAttribPointer posLoc GL.$= (GL.ToFloat, posdesc)
        GL.vertexAttribPointer normLoc GL.$= (GL.ToFloat, normdesc)
        GL.vertexAttribPointer texCoordLoc GL.$= (GL.ToFloat, uvdesc)

        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

        -- Render
        GL.drawElements GL.Triangles nIndices GL.UnsignedShort nullPtr

class Renderable a where
  createRenderObject :: a -> IO (RenderObject)

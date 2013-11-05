module Graphics.Rendering.Lambency.Mesh (
  makeTriangle,
  makeCube
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Vertex
import Data.Vect.Float
import Control.Applicative

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Ptr

import Data.Int

--------------------------------------------------------------------------------

data Mesh = Mesh { vertices :: [Vertex],
                   indices :: [Int16] }

makeTriangle :: Mesh
makeTriangle = Mesh {
  vertices = mkVertex3 . mkVec3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)],
  indices = [0, 1, 2]
}

makeCube :: Mesh
makeCube = Mesh {
  vertices = mkVertex3 . mkVec3 <$> [ 
    -- Front face
    (-1.0, -1.0,  1.0),
    ( 1.0, -1.0,  1.0),
    ( 1.0,  1.0,  1.0),
    (-1.0,  1.0,  1.0),

    -- Back face
    (-1.0, -1.0, -1.0),
    (-1.0,  1.0, -1.0),
    ( 1.0,  1.0, -1.0),
    ( 1.0, -1.0, -1.0),

    -- Top face
    (-1.0,  1.0, -1.0),
    (-1.0,  1.0,  1.0),
    ( 1.0,  1.0,  1.0),
    ( 1.0,  1.0, -1.0),

    -- Bottom face
    (-1.0, -1.0, -1.0),
    ( 1.0, -1.0, -1.0),
    ( 1.0, -1.0,  1.0),
    (-1.0, -1.0,  1.0),

    -- Right face
    ( 1.0, -1.0, -1.0),
    ( 1.0,  1.0, -1.0),
    ( 1.0,  1.0,  1.0),
    ( 1.0, -1.0,  1.0),

    -- Left face
    (-1.0, -1.0, -1.0),
    (-1.0, -1.0,  1.0),
    (-1.0,  1.0,  1.0),
    (-1.0,  1.0, -1.0)
  ],
  
  indices = [
    0,  1,  2,      0,  2,  3,    -- front
    4,  5,  6,      4,  6,  7,    -- back
    8,  9,  10,     8,  10, 11,   -- top
    12, 13, 14,     12, 14, 15,   -- bottom
    16, 17, 18,     16, 18, 19,   -- right
    20, 21, 22,     20, 22, 23    -- left
  ]
}

renderMesh :: RenderObject -> IO ()
renderMesh ro =
  let vadesc = GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float) in
  do
    -- Bind appropriate buffers
    GL.bindBuffer GL.ArrayBuffer GL.$= (Just $ vertexBufferObject ro)
    GL.vertexAttribPointer (GL.AttribLocation 0) GL.$= (GL.ToFloat, vadesc)

    GL.bindBuffer GL.ElementArrayBuffer GL.$= (Just $ indexBufferObject ro)

    -- Render
    GL.drawElements GL.Triangles (nIndices ro) GL.UnsignedShort nullPtr

instance Renderable Mesh where
  createRenderObject m = do
    ro <- createBasicRO (vertices m) (indices m) renderMesh
    return ro

module Graphics.Rendering.Lambency.Renderable (
  RenderObject,
  Renderable,
  createROWithVertices,
  createRenderObject,
  render
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Vertex
import Graphics.Rendering.Lambency.Shader

import Paths_lambency

import Data.Array.IO
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable

--------------------------------------------------------------------------------

data RenderObject = RenderObject {
  shaderProgram :: Maybe Shader,
  vertexBufferObject :: GL.BufferObject
}

createROWithVertices :: [Vertex] -> IO (RenderObject)
createROWithVertices vs =
  let
    flts :: [Float]
    flts = vs >>= toFloats
    ptrsize :: (Storable a) => [a] -> GL.GLsizeiptr
    ptrsize [] = toEnum 0
    ptrsize xs = toEnum $ length flts * (sizeOf $ head xs)
  in
   do
     [vbo] <- GL.genObjectNames 1
     GL.bindBuffer GL.ArrayBuffer GL.$= (Just vbo)
     varr <- newListArray (0, length flts - 1) flts
     withStorableArray varr (\ptr ->
       GL.bufferData GL.ArrayBuffer GL.$= (ptrsize flts, ptr, GL.StaticDraw))

     defaultVertexShader <- getDataFileName "simple.vs"
     defaultFragmentShader <- getDataFileName "simple.fs"
     prg <- loadShader (Just defaultVertexShader) (Just defaultFragmentShader) Nothing
     return RenderObject { shaderProgram = prg,
                           vertexBufferObject = vbo }

addShaderToRenderObject :: RenderObject -> Shader -> RenderObject
addShaderToRenderObject ro shdr =
  RenderObject { shaderProgram = Just shdr,
                 vertexBufferObject = (vertexBufferObject ro) }

render :: RenderObject -> IO ()
render ro =
  let
    vloc = GL.AttribLocation 0
    vadesc = GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)
  in
   do
     GL.currentProgram GL.$= (shaderProgram ro)
     GL.vertexAttribArray vloc GL.$= GL.Enabled
     GL.bindBuffer GL.ArrayBuffer GL.$= (Just $ vertexBufferObject ro)
     GL.vertexAttribPointer vloc GL.$= (GL.ToFloat, vadesc)
     GL.drawArrays GL.Triangles 0 3
     GL.vertexAttribArray vloc GL.$= GL.Disabled

class Renderable a where
  createRenderObject :: a -> IO (RenderObject)


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

import Data.Array.IO
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable

import Control.Applicative
import Data.Monoid
--------------------------------------------------------------------------------

data RenderObject = RenderObject {
  vertexBufferObject :: GL.BufferObject
}

createROWithVertices :: [Vertex] -> IO (RenderObject)
createROWithVertices vs =
  let
    flts = mconcat $ toFloats <$> vs
    ptrsize [] = toEnum 0
    ptrsize xs = toEnum $ length flts * (sizeOf $ head xs)
  in
   do
     [vbo] <- GL.genObjectNames 1
     GL.bindBuffer GL.ArrayBuffer GL.$= (Just vbo)
     varr <- newListArray (0, length flts - 1) flts
     withStorableArray varr (\ptr ->
       GL.bufferData GL.ArrayBuffer GL.$= (ptrsize flts, ptr, GL.StaticDraw))
     return RenderObject { vertexBufferObject = vbo }

class Renderable a where
  createRenderObject :: a -> IO (RenderObject)

render :: RenderObject -> IO ()
render ro =
  let
    vloc = GL.AttribLocation 0
  in
   do
     GL.vertexAttribArray vloc GL.$= GL.Enabled
     GL.bindBuffer GL.ArrayBuffer GL.$= (Just $ vertexBufferObject ro)
     GL.vertexAttribPointer vloc GL.$= (GL.KeepIntegral, GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float))
     putStrLn "Crash"
     GL.drawArrays GL.Triangles 0 3
     putStrLn "No crash"
     GL.vertexAttribArray vloc GL.$= GL.Disabled
    

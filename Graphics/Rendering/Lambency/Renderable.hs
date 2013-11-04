module Graphics.Rendering.Lambency.Renderable (
  Material(..),
  RenderObject(..),
  Renderable(..),
  createROWithVertices,
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Vertex
import Graphics.Rendering.Lambency.Shader

import Paths_lambency

import Data.Array.IO
import Data.Array.Storable
import Foreign.Storable

--------------------------------------------------------------------------------

data Material =
  SimpleMaterial { shaderProgram :: Maybe Shader,
                   beforeRender :: IO (),
                   afterRender :: IO () }

createSimpleMaterial :: IO(Material)
createSimpleMaterial = do
  defaultVertexShader <- getDataFileName "simple.vs"
  defaultFragmentShader <- getDataFileName "simple.fs"
  prg <- loadShader (Just defaultVertexShader) (Just defaultFragmentShader) Nothing
  return $ SimpleMaterial {
    shaderProgram = prg,
    beforeRender = do
      GL.currentProgram GL.$= prg
      GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled,
    afterRender = do
      GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Disabled
    }

data RenderObject = RenderObject {
  material :: Material,
  nVerts :: GL.NumComponents,
  vertexBufferObject :: GL.BufferObject,
  render :: RenderObject -> IO ()
}

createROWithVertices :: [Vertex] -> (RenderObject -> IO ()) -> IO (RenderObject)
createROWithVertices vs renderFunc =
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
     simpleMaterial <- createSimpleMaterial
     return RenderObject { material = simpleMaterial,
                           nVerts = fromIntegral (length vs),
                           vertexBufferObject = vbo,
                           render = renderFunc }

{---
addShaderToRenderObject :: RenderObject -> Shader -> RenderObject
addShaderToRenderObject ro shdr =
  RenderObject { shaderProgram = Just shdr,
                 vertexBufferObject = (vertexBufferObject ro) }
---}

class Renderable a where
  createRenderObject :: a -> IO (RenderObject)


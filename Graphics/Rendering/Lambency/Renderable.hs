module Graphics.Rendering.Lambency.Renderable (
  Material(..),
  RenderObject(..),
  Renderable(..),
  createBasicRO,
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Vertex
import Graphics.Rendering.Lambency.Shader

import Paths_lambency

import Data.Array.IO
import Data.Array.Storable
import Data.Int
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
  nIndices :: GL.NumComponents,
  indexBufferObject :: GL.BufferObject,
  vertexBufferObject :: GL.BufferObject,
  render :: RenderObject -> IO ()
}

createBasicRO :: [Vertex] -> [Int16] -> (RenderObject -> IO ()) -> IO (RenderObject)
createBasicRO vs idxs renderFunc =
  let
    flts :: [Float]
    flts = vs >>= toFloats
  in
   do
     vbo <- setupBuffer GL.ArrayBuffer flts
     ibo <- setupBuffer GL.ElementArrayBuffer idxs
     simpleMaterial <- createSimpleMaterial
     return RenderObject { material = simpleMaterial,
                           nIndices = fromIntegral (length idxs),
                           indexBufferObject = ibo,
                           vertexBufferObject = vbo,
                           render = renderFunc }
  where
    ptrsize :: (Storable a) => [a] -> GL.GLsizeiptr
    ptrsize [] = toEnum 0
    ptrsize xs = toEnum $ length xs * (sizeOf $ head xs)

    setupBuffer :: (Storable a) => GL.BufferTarget -> [a] -> IO( GL.BufferObject )
    setupBuffer tgt xs = do
      [buf] <- GL.genObjectNames 1
      GL.bindBuffer tgt GL.$= (Just buf)
      varr <- newListArray (0, length xs - 1) xs
      withStorableArray varr (\ptr ->
        GL.bufferData tgt GL.$= (ptrsize xs, ptr, GL.StaticDraw))
      return buf

{---
addShaderToRenderObject :: RenderObject -> Shader -> RenderObject
addShaderToRenderObject ro shdr =
  RenderObject { shaderProgram = Just shdr,
                 vertexBufferObject = (vertexBufferObject ro) }
---}

class Renderable a where
  createRenderObject :: a -> IO (RenderObject)


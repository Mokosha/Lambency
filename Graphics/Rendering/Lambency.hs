module Graphics.Rendering.Lambency (
  initLambency,
  
  RenderObject,
  createRenderObject,
  
  Renderable,
  render,
  
  makeTriangle,

  Shader,
  loadShader
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Mesh
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader

--------------------------------------------------------------------------------

initLambency :: IO ()
initLambency = do
  vaos <- GL.genObjectNames 1
  case vaos of
    vao : _ -> do
      let vaoBind = GL.bindVertexArrayObject
        in
       vaoBind GL.$= (Just vao)
    _ -> return ()
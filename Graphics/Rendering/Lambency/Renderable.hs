module Graphics.Rendering.Lambency.Renderable (
  RenderObject,
  Renderable,
  renderObjectWithVertices,
  createRenderObject,
  render
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Vertex

--------------------------------------------------------------------------------

data RenderObject = RenderObject {
  vertexArrayObject :: GL.VertexArrayObject,
  vertices :: [Vertex]
}

renderObjectWithVertices :: [Vertex] -> IO (RenderObject)
renderObjectWithVertices vs = do
  vao <- GL.genObjectNames 1
  return RenderObject { vertexArrayObject = (head vao), vertices = vs }

class Renderable a where
  createRenderObject :: a -> IO (RenderObject)

render :: RenderObject -> IO ()
render ro = do return ()

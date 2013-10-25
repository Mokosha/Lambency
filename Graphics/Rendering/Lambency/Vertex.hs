module Graphics.Rendering.Lambency.Vertex (
  Vertex,
  mkVertex3
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import Data.Vect.Float

--------------------------------------------------------------------------------

data Vertex = SimpleVertex2 (GL.Vertex2 GL.GLfloat)
            | SimpleVertex3 (GL.Vertex3 GL.GLfloat)

mkVertex3 :: Vec3 -> Vertex
mkVertex3 v = SimpleVertex3 $ GL.Vertex3 (realToFrac $ _1 v) (realToFrac $ _2 v) (realToFrac $ _3 v)

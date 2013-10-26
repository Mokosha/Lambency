module Graphics.Rendering.Lambency.Vertex (
  Vertex,
  toFloats,
  mkVertex3
  ) where

--------------------------------------------------------------------------------

import Data.Vect.Float

--------------------------------------------------------------------------------

data Vertex = SimpleVertex2 Vec2
            | SimpleVertex3 Vec3

toFloats :: Vertex -> [Float]
toFloats (SimpleVertex2 v) = [_1 v, _2 v]
toFloats (SimpleVertex3 v) = [_1 v, _2 v, _3 v]

mkVertex3 :: Vec3 -> Vertex
mkVertex3 v = SimpleVertex3 v

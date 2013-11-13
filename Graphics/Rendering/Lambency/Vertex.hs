module Graphics.Rendering.Lambency.Vertex (
  Vertex,
  isTextured,
  toFloats,
  mkVertex3,
  mkTVertex3
  ) where

--------------------------------------------------------------------------------

import Data.Vect.Float

--------------------------------------------------------------------------------

data Vertex = SimpleVertex2 Vec2
            | SimpleVertex3 Vec3
            | TexturedVertex3 Vec3 Vec2

toFloats :: Vertex -> [Float]
toFloats (SimpleVertex2 v) = [_1 v, _2 v]
toFloats (SimpleVertex3 v) = [_1 v, _2 v, _3 v]
toFloats (TexturedVertex3 p uv) = [_1 p, _2 p, _3 p, _1 uv, _2 uv]

isTextured :: Vertex -> Bool
isTextured (TexturedVertex3 _ _) = True
isTextured _ = False

mkVertex3 :: Vec3 -> Vertex
mkVertex3 = SimpleVertex3

mkTVertex3 :: Vec3 -> Vec2 -> Vertex
mkTVertex3 = TexturedVertex3

module Graphics.Rendering.Lambency.Vertex (
  Vertex(..),
  isTextured,
  toFloats,
  ) where

--------------------------------------------------------------------------------

import Data.Vect.Float

--------------------------------------------------------------------------------

data Vertex = Vertex2 Vec2
            | Vertex3 Vec3
            | TVertex3 Vec3 Vec2
            | OTVertex3 Vec3 Vec3 Vec2

toFloats :: Vertex -> [Float]
toFloats (Vertex2 v) = [_1 v, _2 v]
toFloats (Vertex3 v) = [_1 v, _2 v, _3 v]
toFloats (TVertex3 p uv) = [_1 p, _2 p, _3 p, _1 uv, _2 uv]
toFloats (OTVertex3 p n uv) = [_1 p, _2 p, _3 p, _1 n, _2 n, _3 n, _1 uv, _2 uv]

isTextured :: Vertex -> Bool
isTextured (TVertex3 _ _) = True
isTextured (OTVertex3 _ _ _) = True
isTextured _ = False

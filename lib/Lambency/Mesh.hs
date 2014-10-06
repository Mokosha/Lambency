module Lambency.Mesh (
  Mesh(..),
  triangle,
  cube,
  plane,
  quad,
) where

--------------------------------------------------------------------------------

import Lambency.Render
import Lambency.Vertex
import Control.Applicative

import Data.Int

import Linear.V2
import Linear.V3

--------------------------------------------------------------------------------

type Vec2f = V2 Float
type Vec3f = V3 Float

data Mesh a = Mesh { vertices :: [a],
                     indices :: [Int16] }
              deriving (Show)

mkV3 :: (Float, Float, Float) -> V3 Float
mkV3 (a, b, c) = V3 a b c

triangle :: Mesh Vertex3
triangle = Mesh {
  vertices = mkVertex3 . mkV3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)],
  indices = [0, 1, 2]
}

cube :: Mesh OTVertex3
cube = Mesh {
  vertices = zipWith3 mkNormTexVertex3 (mkV3 <$> [
    -- Front face
    (-1.0, -1.0,  1.0),
    ( 1.0, -1.0,  1.0),
    ( 1.0,  1.0,  1.0),
    (-1.0,  1.0,  1.0),

    -- Back face
    (-1.0, -1.0, -1.0),
    (-1.0,  1.0, -1.0),
    ( 1.0,  1.0, -1.0),
    ( 1.0, -1.0, -1.0),

    -- Top face
    (-1.0,  1.0, -1.0),
    (-1.0,  1.0,  1.0),
    ( 1.0,  1.0,  1.0),
    ( 1.0,  1.0, -1.0),

    -- Bottom face
    (-1.0, -1.0, -1.0),
    ( 1.0, -1.0, -1.0),
    ( 1.0, -1.0,  1.0),
    (-1.0, -1.0,  1.0),

    -- Right face
    ( 1.0, -1.0, -1.0),
    ( 1.0,  1.0, -1.0),
    ( 1.0,  1.0,  1.0),
    ( 1.0, -1.0,  1.0),

    -- Left face
    (-1.0, -1.0, -1.0),
    (-1.0, -1.0,  1.0),
    (-1.0,  1.0,  1.0),
    (-1.0,  1.0, -1.0)
  ])
  -- Normals
  (concat [ replicate 4 (V3 0 0 1),
            replicate 4 (V3 0 0 (-1)),
            replicate 4 (V3 0 1 0),
            replicate 4 (V3 0 (-1) 0),
            replicate 4 (V3 1 0 0),
            replicate 4 (V3 (-1) 0 0)])
  -- Texture Coordinates
  ((concat . (replicate 6)) [V2 0 0, V2 1 0, V2 1 1, V2 0 1]),

  indices = concat [[x, x+1, x+2, x, x+2, x+3] | x <- [0,4..20]]
}

plane :: Mesh OTVertex3
plane = Mesh {
  vertices = zipWith3 mkNormTexVertex3
             [V3 x 0 z | z <- [(-1),(-0.9)..1], x <- [(-1),(-0.9)..1]]
             (replicate (21*21) (V3 0 1 0))
             [V2 u v | v <- [0,0.05..1], u <- [0,0.05..1]],
  indices = concat [quadAt x y | y <- [0..19], x <- [0..19]]
}
  where quadAt x y =
          [idxOf x y, idxOf x (y+1), idxOf (x+1) y,
           idxOf (x+1) y, idxOf x (y+1), idxOf (x+1) (y+1)]
        idxOf x y = y * 21 + x

quad :: Mesh TVertex3
quad = Mesh {
    vertices = zipWith mkTexVertex3 (map texToVert texcoords) texcoords,
    indices = [0, 2, 1, 1, 2, 3]
  }
  where
    texcoords :: [ Vec2f ]
    texcoords = [ V2 x y | x <- [0, 1], y <- [1, 0] ]

    texToVert :: Vec2f -> Vec3f
    texToVert (V2 x y) = V3 x (1 - y) 0


instance Vertex a => Renderable (Mesh a) where
  createRenderObject m mat = createBasicRO (vertices m) (indices m) mat

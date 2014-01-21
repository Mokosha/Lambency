module Graphics.Rendering.Lambency.Mesh (
  Mesh,
  triangle,
  cube,
  plane,
  quad,
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Vertex
import Data.Vect.Float
import Control.Applicative

import Data.Int

--------------------------------------------------------------------------------

data Mesh = Mesh { vertices :: [Vertex],
                   indices :: [Int16] }

triangle :: Mesh
triangle = Mesh {
  vertices = mkVertex3 . mkVec3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)],
  indices = [0, 1, 2]
}

cube :: Mesh
cube = Mesh {
  vertices = zipWith3 mkNormTexVertex3 (mkVec3 <$> [ 
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
  (concat [ replicate 4 (Vec3 0 0 1),
            replicate 4 (Vec3 0 0 (-1)),
            replicate 4 (Vec3 0 1 0),
            replicate 4 (Vec3 0 (-1) 0),
            replicate 4 (Vec3 1 0 0),
            replicate 4 (Vec3 (-1) 0 0)])
  -- Texture Coordinates
  ((concat . (replicate 6)) [Vec2 0 0, Vec2 1 0, Vec2 1 1, Vec2 0 1]),

  indices = concat [[x, x+1, x+2, x, x+2, x+3] | x <- [0,4..20]]
}

plane :: Mesh
plane = Mesh {
  vertices = zipWith3 mkNormTexVertex3
             [Vec3 x 0 z | z <- [(-1),(-0.9)..1], x <- [(-1),(-0.9)..1]]
             (replicate (21*21) (Vec3 0 1 0))
             [Vec2 u v | v <- [0,0.05..1], u <- [0,0.05..1]],
  indices = concat [quadAt x y | y <- [0..19], x <- [0..19]]
}
  where quadAt x y =
          [idxOf x y, idxOf x (y+1), idxOf (x+1) y,
           idxOf (x+1) y, idxOf x (y+1), idxOf (x+1) (y+1)]
        idxOf x y = y * 21 + x

quad :: Mesh
quad = Mesh {
  vertices = zipWith mkTexVertex3 (map texToVert texcoords) texcoords,
  indices = [0, 2, 1, 1, 2, 3]
  }
  where
    texcoords :: [ Vec2 ]
    texcoords = [ Vec2 x y | x <- [0, 1], y <- [0, 1] ]

    texToVert :: Vec2 -> Vec3
    texToVert (Vec2 x y) = Vec3 (x * 2 - 1) (y * 2 - 1) 0


instance Renderable Mesh where
  createRenderObject m mat = createBasicRO (vertices m) (indices m) mat

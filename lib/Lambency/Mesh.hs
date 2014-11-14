module Lambency.Mesh (
  Mesh(..),

  genNormalsV3,
  genNormalsTV3,

  genTexCoords,

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
import qualified Data.Map as Map

import Linear
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

type Triangle = (Vec3f, Vec3f, Vec3f)

mkTris :: Vertex a => (a -> Vec3f) -> Mesh a -> Map.Map a [Triangle]
mkTris posFn mesh = mkTrisFn (indices mesh) Map.empty
  where
    vertMap = Map.fromList $ zip [0,1..] (vertices mesh)

    mkTrisFn [] m = m
    mkTrisFn (x : y : z : rest) m =
      let xv = vertMap Map.! x
          yv = vertMap Map.! y
          zv = vertMap Map.! z
          t = (posFn xv, posFn yv, posFn zv)
          miniMap = Map.fromList $ zip [xv, yv, zv] $ repeat [t]
      in mkTrisFn rest $ Map.unionWith (++) miniMap m

    mkTrisFn _ _ = error "Not a list of triangle indices!"

genNormals :: Map.Map a [Triangle] -> Map.Map a Vec3f
genNormals = Map.map genNormal
  where
    triNormal :: Triangle -> Vec3f
    triNormal (v1, v2, v3) =
      let x = v2 ^-^ v1
          y = v3 ^-^ v1
      in signorm $ x `cross` y
    
    genNormal :: [Triangle] -> Vec3f
    genNormal = signorm . foldl1 (^+^) . map triNormal

genNormalsV3 :: Mesh Vertex3 -> Mesh OVertex3
genNormalsV3 mesh =
  let genOVertex :: Map.Map Vertex3 Vec3f -> Map.Map Vertex3 OVertex3
      genOVertex = Map.mapWithKey addNormalV3

      ovMap = genOVertex . genNormals . mkTris getVertex3Position $ mesh
  in
   mesh { vertices = map (ovMap Map.!) $ vertices mesh }

genNormalsTV3 :: Mesh TVertex3 -> Mesh OTVertex3
genNormalsTV3 mesh =
  let genOTVertex :: Map.Map TVertex3 Vec3f -> Map.Map TVertex3 OTVertex3
      genOTVertex = Map.mapWithKey addNormalTV3

      otvMap = genOTVertex . genNormals . mkTris getTexVertex3Position $ mesh
  in
   mesh { vertices = map (otvMap Map.!) $ vertices mesh }

-- !FIXME! Actually properly generate texture coordinates... this is kind
-- of embarassing
genTexCoords :: Mesh OVertex3 -> Mesh OTVertex3
genTexCoords (Mesh verts idxs) = Mesh (map (flip addTexCoordOV3 zero) verts) idxs

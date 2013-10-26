module Graphics.Rendering.Lambency.Mesh (
  makeTriangle
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Vertex
import Data.Vect.Float
import Control.Applicative

--------------------------------------------------------------------------------

data Mesh = Mesh { vertices :: [Vec3] }
-----          | NMesh { vertices :: [Vec3], normals :: [Vec3] } deriving (Show)

instance Renderable Mesh where
  createRenderObject m = do
    ro <- createROWithVertices $ map mkVertex3 $ vertices m
    return ro

makeTriangle :: Mesh
makeTriangle = Mesh{vertices = mkVec3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)]}

module Graphics.Rendering.Lambency.Mesh (
  makeTriangle
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Vertex
import Data.Vect.Float
import Control.Applicative

--------------------------------------------------------------------------------

data Mesh = Mesh { vertices :: [Vec3] }
          | NMesh { vertices :: [Vec3], normals :: [Vec3] } deriving (Show)

instance Renderable Mesh where
  createRenderObject Mesh { vertices = vs } = do
    ro <- createROWithVertices $ map mkVertex3 vs
    return ro
  createRenderObject NMesh { vertices = vs,
               normals = ns } = do
    ro <- createROWithVertices $ map mkVertex3 vs
    return ro

makeTriangle :: Mesh
makeTriangle = Mesh{vertices = mkVec3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)]}

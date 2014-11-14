module Lambency.Loaders (
  OBJ.OBJInfo(..),
  OBJ.getOBJInfo,

  loadV3,
  loadOV3,
  loadTV3,
  loadOTV3,
) where

--------------------------------------------------------------------------------
import Lambency.Mesh
import Lambency.Vertex

import qualified Lambency.Loaders.OBJLoader as OBJ
--------------------------------------------------------------------------------

loadV3 :: FilePath -> IO (Mesh Vertex3)
loadV3 = OBJ.loadV3

loadOV3 :: FilePath -> IO (Mesh OVertex3)
loadOV3 = OBJ.loadOV3

loadTV3 :: FilePath -> IO (Mesh TVertex3)
loadTV3 = OBJ.loadTV3

loadOTV3 :: FilePath -> IO (Mesh OTVertex3)
loadOTV3 = OBJ.loadOTV3

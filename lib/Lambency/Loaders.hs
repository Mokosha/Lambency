module Lambency.Loaders (
  OBJ.OBJInfo(..),
  OBJ.getOBJInfo,

  loadV3,
  loadOV3,
  loadTV3,
  loadOTV3,
) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad

import Lambency.Mesh
import Lambency.Vertex

import qualified Lambency.Loaders.OBJLoader as OBJ
--------------------------------------------------------------------------------

getOBJMeshes :: OBJ.OBJOutput a -> [Mesh a]
getOBJMeshes (OBJ.OBJOutput _ ms) = snd <$> ms

loadV3 :: FilePath -> IO [Mesh Vertex3]
loadV3 = liftM getOBJMeshes . OBJ.loadV3

loadOV3 :: FilePath -> IO [Mesh OVertex3]
loadOV3 = liftM getOBJMeshes . OBJ.loadOV3

loadTV3 :: FilePath -> IO [Mesh TVertex3]
loadTV3 = liftM getOBJMeshes . OBJ.loadTV3

loadOTV3 :: FilePath -> IO [Mesh OTVertex3]
loadOTV3 = liftM getOBJMeshes . OBJ.loadOTV3

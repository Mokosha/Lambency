module Graphics.Rendering.Lambency.Loaders.OBJLoader (
  loadOBJ
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Mesh
import Graphics.Rendering.Lambency.Vertex

import Data.List as List
import Data.Map as Map
import Data.Vect.Float

--------------------------------------------------------------------------------

type OBJVertex = Vec3
type OBJVertexList = [OBJVertex]

type OBJTexCoord = Vec2
type OBJTexCoordList = [OBJTexCoord]

emptyTexCoords :: OBJTexCoordList -> Bool
emptyTexCoords [] = True
emptyTexCoords _ = False

type OBJNormal = Normal3
type OBJNormalList = [OBJNormal]

emptyNormals :: OBJNormalList -> Bool
emptyNormals [] = True
emptyNormals _ = False

type OBJIndex = (Int, Maybe Int, Maybe Int) -- derives Eq, Ord
type OBJFace = [OBJIndex]
type OBJFaceList = [OBJFace]

data OBJGeometry = OBJGeometry {
  objVerts :: OBJVertexList,
  objTexCoords :: OBJTexCoordList,
  objNormals :: OBJNormalList,
  objFaces :: OBJFaceList
}

simpleObj2Mesh :: OBJVertexList -> OBJFaceList -> Mesh
simpleObj2Mesh verts faces = triangle -- TODO

normalObj2Mesh :: OBJVertexList -> OBJNormalList -> OBJFaceList -> Mesh
normalObj2Mesh verts normals faces = triangle -- TODO

texturedObj2Mesh :: OBJVertexList -> OBJTexCoordList -> OBJFaceList -> Mesh
texturedObj2Mesh verts texcoords faces = triangle -- TODO

obj2Mesh :: OBJGeometry -> Mesh
obj2Mesh (OBJGeometry {objVerts=vs, objTexCoords=tcs, objNormals=ns, objFaces=fs})
  | emptyTexCoords tcs && (emptyNormals ns) = simpleObj2Mesh vs fs
  | emptyTexCoords tcs = normalObj2Mesh vs ns fs
  | emptyNormals ns = texturedObj2Mesh vs tcs fs
  | otherwise = triangle -- TODO

loadOBJ :: FilePath -> IO (Mesh)
loadOBJ filepath = return triangle -- TODO

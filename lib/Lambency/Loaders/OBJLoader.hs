module Lambency.Loaders.OBJLoader (
  OBJInfo(..),

  OBJGeometry,
  OBJOutput(..),

  obj2V3Mesh,
  obj2OV3Mesh,
  obj2TV3Mesh,
  obj2OTV3Mesh,

  loadOBJ,
) where

--------------------------------------------------------------------------------
import Lambency.Loaders.Utils
import Lambency.Mesh
import Lambency.Vertex

import qualified Data.Map as Map

import Data.Char (toLower)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Text (pack)

import Data.Array.Unboxed (UArray, listArray, (!))

import Control.Monad.State.Strict as State

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative hiding (many, (<|>))
#endif

import Text.Parsec
import Text.Parsec.Text (Parser)

import Linear.Metric
import Linear.Vector
import Linear.V2
import Linear.V3
--------------------------------------------------------------------------------

type OBJVertex = Vec3f
type OBJVertexList = [OBJVertex]

type OBJTexCoord = Vec2f
type OBJTexCoordList = [OBJTexCoord]

-- type OBJNormal = Normal3
type OBJNormal = Vec3f
type OBJNormalList = [OBJNormal]

type OBJIndex = (Int, Maybe Int, Maybe Int) -- derives Eq, Ord
type OBJIndexList = [OBJIndex]
type OBJFace = OBJIndexList
type OBJFaceList = [OBJFace]

data OBJGeometry = OBJGeometry {
  objVerts :: OBJVertexList,
  objTexCoords :: OBJTexCoordList,
  objNormals :: OBJNormalList,
  objFaces :: OBJFaceList
} deriving (Show)

data OBJInfo = OBJInfo {
  mtlLib :: FilePath,
  mtlName :: String,
  numVerts :: Int,
  numTexCoords :: Int,
  numNormals :: Int,
  numFaces :: Int
} deriving (Show, Ord, Eq)

data OBJOutput = OBJOutput {
  objMtlLib :: FilePath,
  objMeshes :: [(OBJInfo, OBJGeometry)]
}

convertNegativeFaces :: OBJInfo -> OBJFace -> OBJFace
convertNegativeFaces geomLen = map (convertNegativeIndex geomLen)
  where
    convert :: Int -> Int -> Int
    convert len x
      | x < 0 = len + x + 1
      | otherwise = x

    convertNegativeIndex :: OBJInfo -> OBJIndex -> OBJIndex
    convertNegativeIndex info (p, tc, n) =
      (convert (numVerts info) p,
       liftM (convert . numTexCoords $ info) tc,
       liftM (convert . numNormals $ info) n)

reverseGeometry :: OBJGeometry -> OBJGeometry
reverseGeometry geom = OBJGeometry {
  objVerts = reverse $ objVerts geom,
  objTexCoords = reverse $ objTexCoords geom,
  objNormals = reverse $ objNormals geom,
  objFaces = objFaces geom
  }

triangulate :: OBJFaceList -> OBJIndexList
triangulate fs = let
  tglte :: OBJFace -> [OBJFace] -> [OBJFace]
  tglte f faces
    | length f <= 3 = f : faces
    | otherwise =
      case f of
        (i1 : i2 : i3 : rest) -> tglte (i1 : i3 : rest) ([i1, i2, i3] : faces)
        _ -> error "Wat"
  in
   concat . concat $ map (flip tglte []) fs

mkVec2fLookup :: [Vec2f] -> (Int -> Vec2f)
mkVec2fLookup vecs = let
  arr :: UArray Int Float
  arr = listArray (1, length vecs * 2) $
        concat $ map (\(V2 x y) -> [x, y]) vecs

  in (\i -> V2 (arr ! (2*i - 1)) (arr ! (2 * i)))

mkVec3fLookup :: [Vec3f] -> (Int -> Vec3f)
mkVec3fLookup vecs = let
  arr :: UArray Int Float
  arr = listArray (1, length vecs * 3) $
        concat $ map (\(V3 x y z) -> [x, y, z]) vecs

  in \i -> V3 (arr ! (3*i - 2)) (arr ! (3*i - 1)) (arr ! (3 * i))

genIdxMap' :: Vertex a => (OBJIndex -> a) -> OBJIndexList -> Map.Map OBJIndex (Int, a) -> Int ->
             Map.Map OBJIndex (Int, a)
genIdxMap' _ [] m _ = m
genIdxMap' f (idx : rest) m nVerts =
  case Map.lookup idx m of
    Just _ -> genIdxMap' f rest m nVerts
    Nothing -> genIdxMap' f rest (Map.insert idx (nVerts, f idx) m) (nVerts + 1)

genIdxMap :: Vertex a => (OBJIndex -> a) -> OBJIndexList -> Map.Map OBJIndex (Int, a)
genIdxMap f idxs = genIdxMap' f idxs Map.empty 0

genMesh :: Vertex a => (OBJIndex -> a) -> OBJIndexList -> Mesh a
genMesh f idxs = let
  idxMap = genIdxMap f idxs
  in Mesh {
    vertices = map snd $ sortBy (comparing fst) $ Map.elems idxMap,
    indices = map (fromIntegral . fst . (idxMap Map.!)) idxs
  }

normalObj2Mesh :: OBJVertexList -> OBJNormalList -> OBJFaceList -> Mesh OVertex3
normalObj2Mesh verts normals faces = let
  ns = mkVec3fLookup normals
  vs = mkVec3fLookup verts

  idx2Vertex :: OBJIndex -> OVertex3
  idx2Vertex (x, _, Just n) = mkNormVertex3 (vs x) (ns n)
  idx2Vertex i = error $ "Ill formatted OV3 index: " ++ (show i)

  in genMesh idx2Vertex (triangulate faces)

texturedObj2Mesh :: OBJVertexList -> OBJTexCoordList -> OBJFaceList -> Mesh TVertex3
texturedObj2Mesh verts texcoords faces = let
  tcs = mkVec2fLookup texcoords
  vs = mkVec3fLookup verts

  idx2Vertex :: OBJIndex -> TVertex3
  idx2Vertex (x, Just tc, _) = mkTexVertex3 (vs x) (tcs tc)
  idx2Vertex i = error $ "Ill formatted TV3 index: " ++ (show i)

  in genMesh idx2Vertex (triangulate faces)

normTexturedObj2Mesh :: OBJVertexList -> OBJTexCoordList -> OBJNormalList -> OBJFaceList ->
                        Mesh OTVertex3
normTexturedObj2Mesh verts texcoords normals faces = let
  ns = mkVec3fLookup normals
  tcs = mkVec2fLookup texcoords
  vs = mkVec3fLookup verts

  idx2Vertex :: OBJIndex -> OTVertex3
  idx2Vertex (x, Just tc, Just n) = mkNormTexVertex3 (vs x) (ns n) (tcs tc)
  idx2Vertex i = error $ "Ill formatted OTV3 index: " ++ (show i)

  in genMesh idx2Vertex (triangulate faces)

obj2V3Mesh :: OBJGeometry -> Mesh Vertex3
obj2V3Mesh (OBJGeometry {objVerts=vs, objTexCoords=_, objNormals=_, objFaces=fs}) =
  let vfn = mkVec3fLookup vs
      idx2Vertex (x, _, _) = mkVertex3 (vfn x)
  in genMesh idx2Vertex (triangulate fs)

obj2OV3Mesh :: OBJGeometry -> Mesh OVertex3
obj2OV3Mesh (OBJGeometry {objVerts=vs, objTexCoords=_, objNormals=ns, objFaces = fs})
  | null ns = normalObj2Mesh vs (repeat zero) fs
  | otherwise = normalObj2Mesh vs ns fs

obj2TV3Mesh :: OBJGeometry -> Mesh TVertex3
obj2TV3Mesh (OBJGeometry {objVerts=vs, objTexCoords=uvs, objNormals=_, objFaces = fs})
  | null uvs = texturedObj2Mesh vs (repeat zero) fs
  | otherwise = texturedObj2Mesh vs uvs fs

obj2OTV3Mesh :: OBJGeometry -> Mesh OTVertex3
obj2OTV3Mesh (OBJGeometry {objVerts=vs, objTexCoords=uvs, objNormals=ns, objFaces = fs})
  | (null ns) && (null uvs) =
    normTexturedObj2Mesh vs (repeat zero) (repeat zero) fs
  -- !FIXME! Do we want to generate normals here maybe?
  | null ns = normTexturedObj2Mesh vs uvs (repeat zero) fs
  | null uvs = normTexturedObj2Mesh vs (repeat zero) ns fs
  | otherwise = normTexturedObj2Mesh vs uvs ns fs

data Command = Normal Vec3f
             | Position Vec3f
             | TexCoord Vec2f
             | Face OBJFace
             | MtlLib FilePath
             | Group String
             | MtlName String
             deriving (Show, Eq, Ord)

initialGeom :: OBJGeometry
initialGeom = OBJGeometry [] [] [] []

initialInfo :: OBJInfo
initialInfo = OBJInfo "" "" 0 0 0 0

checkInfo :: OBJFaceList -> OBJInfo -> OBJInfo
checkInfo [] x = x
checkInfo faces info =
  let hasTexCoords = any (any (\(_, x, _) -> isJust x)) faces
      hasNormals = any (any (\(_, _, x) -> isJust x)) faces

      fixedTCInfo
        | hasTexCoords = info
        | otherwise = info { numTexCoords = 0 }

      fixedInfo
        | hasNormals = fixedTCInfo
        | otherwise = fixedTCInfo { numNormals = 0 }

  in fixedInfo

addCommand :: Command -> State.State (OBJInfo, OBJGeometry) [(OBJInfo, OBJGeometry)]
addCommand (Normal n) = do
  (info, geom) <- get
  let newInfo = info { numNormals = numNormals info + 1 }
      newGeom = geom { objNormals = signorm n : (objNormals geom) }
  put (newInfo, newGeom)
  return []

addCommand (Position p) = do
  (info, geom) <- get
  let newInfo = info { numVerts = numVerts info + 1 }
      newGeom = geom { objVerts = p : (objVerts geom) }
  put (newInfo, newGeom)
  return []

addCommand (TexCoord tc) = do
  (info, geom) <- get
  let newInfo = info { numTexCoords = numTexCoords info + 1 }
      newGeom = geom { objTexCoords = tc : (objTexCoords geom) }
  put (newInfo, newGeom)
  return []

addCommand (Face f) = do
  (info, geom) <- get
  let newInfo = info { numFaces = numFaces info + 1 }
      newGeom = geom { objFaces = convertNegativeFaces info f : (objFaces geom) }
  put (newInfo, newGeom)
  return []

addCommand (MtlLib lib) = do
  (info, geom) <- get
  let newInfo = info { mtlLib = lib }
  put (newInfo, geom)
  return []

addCommand (Group _) = return []

addCommand (MtlName mtl) = do
  (info, geom) <- get
  let newInfo = info { mtlName = map toLower mtl }
  case objFaces geom of
    [] -> put (newInfo, geom) >> return []
    faces -> do
      let newGeom = geom { objFaces = [] }
          newInfo' = newInfo { numFaces = 0 }
      put (newInfo', newGeom)
      return [(checkInfo faces info, reverseGeometry geom)]

-- addCommand c = error $ "Lambency.Loaders.OBJLoader (addCommand): Unable to handle command " ++ show c

runCommands :: [Command] -> [(OBJInfo, OBJGeometry)]
runCommands cmds =
  let (pairs, (lastInfo, lastGeom)) = State.runState (mapM addCommand cmds) (initialInfo, initialGeom)
  in (checkInfo (objFaces lastGeom) lastInfo, reverseGeometry lastGeom) : (concat pairs)

parseFile :: Parser [(OBJInfo, OBJGeometry)]
parseFile = let

  ignoreRestOfLine :: Parser ()
  ignoreRestOfLine = many (noneOf "\r\n") >> endOfLine >> return ()

  comment :: Parser ()
  comment = char '#' >> ignoreRestOfLine

  -- FIXME -- 
  errata :: Parser ()
  errata = (oneOf "os" >> ignoreRestOfLine)

  blankLine :: Parser ()
  blankLine = (endOfLine <|>
               (skipMany1 (tab <|> char ' ') >> endOfLine)) >> return ()

  vert :: Parser Command
  vert = do
    v <- char 'v' >>
         ((many1 space >> (Position <$> vector3))
          <|> (char 'n' >> (Normal <$> vector3))
          <|> (char 't' >> (TexCoord <$> vector2)))
    _ <- many (noneOf "\r\n")
    return v

  integer :: Parser Int
  integer = do
    skipMany (tab <|> char ' ')
    m <- option 1 $ do
      _ <- char '-'
      return (-1)
    v <- many1 digit
    return $ m * (read v)

  index :: Parser OBJIndex
  index = do
    skipMany (tab <|> char ' ')
    idx <- integer
    (tc, n) <- (do _ <- char '/'
                   mtc <- option Nothing $ Just <$> integer
                   mn <- (char '/' >> (Just <$> integer)) <|> (return Nothing)
                   return (mtc, mn))
               <|>
               (return (Nothing, Nothing))
    skipMany (tab <|> char ' ')
    return (idx, tc, n)

  face :: Parser Command
  face = do
    idxs <- char 'f' >> (many1 index)
    _ <- many (noneOf "\r\n")
    return $ Face idxs

  mtllib :: Parser Command
  mtllib = MtlLib <$> (string "mtllib" >> spaces >> many1 (noneOf " #\n\r\t"))

  usemtl :: Parser Command
  usemtl = MtlName <$> (string "usemtl" >> spaces >> many1 (noneOf " #\n\r\t"))

  meshGroup :: Parser Command
  meshGroup = Group <$> (char 'g' >> spaces >> many1 (noneOf " #\n\r\t"))

  command :: Parser Command
  command = vert <|> face <|> mtllib <|> usemtl <|> meshGroup

  ignorableLines :: Parser ()
  ignorableLines = many (errata <|> comment <|> blankLine) >> return ()

  parseLine :: Parser Command
  parseLine = do
    v <- command
    ignorableLines
    return v

  in do
    ignorableLines
    cmds <- many1 parseLine
    _ <- try (ignorableLines >> eof) >> return ()
    return $ runCommands cmds

parseOBJ :: ([(OBJInfo, OBJGeometry)] -> a) -> FilePath -> IO a
parseOBJ fn filepath = do
  s <- readFile filepath
  case parse parseFile filepath (pack s) of
    Left x -> error $ show x
    Right [] -> error "Lambency.Loaders.OBJLoader (parseOBJ): OBJ file had no meshes!"
    Right xs -> return $ fn xs

genOutput :: [(OBJInfo, OBJGeometry)] -> OBJOutput
genOutput [] = OBJOutput "" []
genOutput o@((info', _):_) =
  let mtllib = mtlLib info'

      pruneOutput (info, _) = mtlLib info /= mtllib

      addOutput [] result = result
      addOutput (x : rest) result =
        if pruneOutput x
        then addOutput rest result
        else addOutput rest (x : result)

  in OBJOutput mtllib (addOutput o [])

loadOBJ :: FilePath -> IO OBJOutput
loadOBJ = parseOBJ genOutput

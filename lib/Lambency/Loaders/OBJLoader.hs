module Lambency.Loaders.OBJLoader (
  loadV3,
  loadOV3,
  loadTV3,
  loadOTV3,
) where

--------------------------------------------------------------------------------

import Lambency.Mesh
import Lambency.Vertex

import qualified Data.Map as Map

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (pack)

import Data.Array.Unboxed (UArray, listArray, (!))

import Control.Monad.State.Strict as State
import Control.Applicative hiding (many, (<|>))

import Text.Parsec
import Text.Parsec.Text (Parser)

import Linear.Metric
import Linear.Vector
import Linear.V2
import Linear.V3
--------------------------------------------------------------------------------

type Vec2f = V2 Float
type Vec3f = V3 Float

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

type OBJGeometryLength = (Int, Int, Int) -- nVerts, nTexCoords, nNormals

convertNegativeFaces :: OBJGeometryLength -> OBJFace -> OBJFace
convertNegativeFaces geomLen = map (convertNegativeIndex geomLen)
  where
    convert :: Int -> Int -> Int
    convert len x
      | x < 0 = len + x + 1
      | otherwise = x
    
    convertNegativeIndex :: OBJGeometryLength -> OBJIndex -> OBJIndex
    convertNegativeIndex (nVerts, nTexCoords, nNormals) (p, tc, n) =
      (convert nVerts p, liftM (convert nTexCoords) tc, liftM (convert nNormals) n)

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

simpleObj2Mesh :: OBJVertexList -> OBJFaceList -> Mesh Vertex3
simpleObj2Mesh verts faces = Mesh {
  vertices = map mkVertex3 verts,
  indices = map (\(x, _, _) -> fromIntegral x) $ triangulate faces
}

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

genMesh :: Vertex a => OBJIndexList -> (OBJIndex -> a) -> Mesh a
genMesh idxs f = let
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
  idx2Vertex i = error $ "Ill formatted index: " ++ (show i)

  in genMesh (triangulate faces) idx2Vertex

texturedObj2Mesh :: OBJVertexList -> OBJTexCoordList -> OBJFaceList -> Mesh TVertex3
texturedObj2Mesh verts texcoords faces = let
  tcs = mkVec2fLookup texcoords
  vs = mkVec3fLookup verts

  idx2Vertex :: OBJIndex -> TVertex3
  idx2Vertex (x, Just tc, _) = mkTexVertex3 (vs x) (tcs tc)
  idx2Vertex i = error $ "Ill formatted index: " ++ (show i)

  in genMesh (triangulate faces) idx2Vertex

normTexturedObj2Mesh :: OBJVertexList -> OBJTexCoordList -> OBJNormalList -> OBJFaceList ->
                        Mesh OTVertex3
normTexturedObj2Mesh verts texcoords normals faces = let
  ns = mkVec3fLookup normals
  tcs = mkVec2fLookup texcoords
  vs = mkVec3fLookup verts

  idx2Vertex :: OBJIndex -> OTVertex3
  idx2Vertex (x, Just tc, Just n) = mkNormTexVertex3 (vs x) (ns n) (tcs tc)
  idx2Vertex i = error $ "Ill formatted index: " ++ (show i)

  in genMesh (triangulate faces) idx2Vertex

obj2V3Mesh :: OBJGeometry -> Mesh Vertex3
obj2V3Mesh (OBJGeometry {objVerts=vs, objTexCoords=_, objNormals=_, objFaces=fs}) =
  simpleObj2Mesh vs fs

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
             deriving (Show, Eq, Ord)

parseFile :: Parser OBJGeometry
parseFile = let

  float :: Parser Float
  float = do
    spaces
    sign <- option 1 $ do s <- oneOf "+-"
                          return $ if s == '-' then (-1.0) else 1.0
    t <- option "0" $ many digit
    _ <- if t == [] then (char '.') else ((try $ char '.') <|> (return ' '))
    d <- option "0" $ many1 digit
    let
      denom :: Float
      denom = if d == "0" then 1.0 else (fromIntegral $ length d)
    e <- option "0" $ char 'e' >> (many1 digit)

    return $ ((read t) + ((read d) / (10 ** denom))) * (10 ** (read e)) * sign

  vector2 :: Parser Vec2f
  vector2 = V2 <$> float <*> float

  vector3 :: Parser Vec3f
  vector3 = V3 <$> float <*> float <*> float

  ignoreRestOfLine :: Parser ()
  ignoreRestOfLine = many (noneOf "\r\n") >> newline >> return ()

  comment :: Parser ()
  comment = char '#' >> ignoreRestOfLine

  -- FIXME -- 
  errata :: Parser ()
  errata = try (string "mtllib" >> ignoreRestOfLine) <|>
           try (string "usemtl" >> ignoreRestOfLine) <|>
           (oneOf "osg" >> ignoreRestOfLine)

  blankLine :: Parser ()
  blankLine = (newline <|>
               (skipMany1 (tab <|> char ' ') >> newline)) >> return ()

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

  command :: Parser Command
  command = vert <|> face

  ignorableLines :: Parser ()
  ignorableLines = many (errata <|> comment <|> blankLine) >> return ()

  parseLine :: Parser Command
  parseLine = do
    v <- command
    ignorableLines
    return v

  initialGeom = OBJGeometry {
    objVerts = [],
    objTexCoords = [],
    objNormals = [],
    objFaces = []
    }

  addCommand :: OBJGeometry -> Command -> State.State OBJGeometryLength OBJGeometry
  addCommand g (Normal n) = do
    (nVerts, nTexCoords, nNormals) <- State.get
    State.put (nVerts, nTexCoords, nNormals + 1)
    return $ g { objNormals = signorm n : (objNormals g) }

  addCommand g (Position p) = do
    (nVerts, nTexCoords, nNormals) <- State.get
    State.put (nVerts + 1, nTexCoords, nNormals)
    return $ g { objVerts = p : (objVerts g) }

  addCommand g (TexCoord tc) = do
    (nVerts, nTexCoords, nNormals) <- State.get
    State.put (nVerts, nTexCoords + 1, nNormals)
    return $ g { objTexCoords = tc : (objTexCoords g) }

  addCommand g (Face f) = do
    lengths <- State.get
    return $ g { objFaces = convertNegativeFaces lengths f : (objFaces g) }

  runCommands :: [Command] -> State.State OBJGeometryLength OBJGeometry
  runCommands cmds = reverseGeometry <$> foldM addCommand initialGeom cmds

  in do
    ignorableLines
    vals <- many1 parseLine
    _ <- try (ignorableLines >> eof) >> return ()
    return $ State.evalState (runCommands vals) (0, 0, 0)

loadOBJ :: Vertex a => (OBJGeometry -> Mesh a) -> FilePath -> IO (Mesh a)
loadOBJ gen filepath = let
  parseOBJ :: String -> OBJGeometry
  parseOBJ s =
    case parse parseFile filepath (pack s) of
      Left x -> error $ show x
      Right y -> y
  in
   gen . parseOBJ <$> readFile filepath

loadV3 :: FilePath -> IO (Mesh Vertex3)
loadV3 = loadOBJ obj2V3Mesh

loadOV3 :: FilePath -> IO (Mesh OVertex3)
loadOV3 = loadOBJ obj2OV3Mesh

loadTV3 :: FilePath -> IO (Mesh TVertex3)
loadTV3 = loadOBJ obj2TV3Mesh

loadOTV3 :: FilePath -> IO (Mesh OTVertex3)
loadOTV3 = loadOBJ obj2OTV3Mesh

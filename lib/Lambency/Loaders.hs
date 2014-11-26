module Lambency.Loaders (
  loadOBJ,
  loadOBJWithDefaultMaterial,
) where

--------------------------------------------------------------------------------
import Control.Applicative

import qualified Data.Map as Map

import Lambency.Material
import Lambency.Mesh
import Lambency.Render
import Lambency.Types

import qualified Lambency.Loaders.OBJLoader as OBJ
import qualified Lambency.Loaders.MTLLoader as MTL

import System.Directory (doesFileExist)
import System.FilePath
--------------------------------------------------------------------------------

genMtlMap :: FilePath -> [MTL.MTL] -> IO (Map.Map String Material)
genMtlMap baseDir mtls = Map.fromList <$> mapM cvtMtl mtls
  where
    cvtMtl mtl = do
      m <- MTL.mkMaterial baseDir mtl
      return (MTL.mtlName mtl, m)

lookupMtl :: Map.Map String Material -> Maybe Material -> String -> Material
lookupMtl mtlmap defaultMtl str =
  case Map.lookup str mtlmap of
    Nothing ->
      case defaultMtl of
        Nothing -> error "Lambency.Loaders (lookupMtl): No default material and no stored material"
        Just m -> m
    Just m -> m

mesh2RenderObj :: (String -> Material) -> (OBJ.OBJInfo, OBJ.OBJGeometry) -> IO (RenderObject)
mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ 0 0 _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2V3Mesh geom
  in
   case usesTextures m of
     False ->
       case isUnlit m of
         True -> createRenderObject mesh m
         False -> createRenderObject (genNormalsV3 mesh) m
     True ->
       case isUnlit m of
         True -> createRenderObject (genTexCoordsV3 mesh) m
         False -> createRenderObject (genTexCoordsOV3 . genNormalsV3 $ mesh) m

mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ _ 0 _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2TV3Mesh geom
  in
   case isUnlit m of
     False -> createRenderObject (genNormalsTV3 mesh) m
     True -> createRenderObject mesh m

mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ 0 _ _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2OV3Mesh geom
  in
   case usesTextures m of
     False -> createRenderObject mesh m
     True -> createRenderObject (genTexCoordsOV3 mesh) m

mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ _ _ _, geom) = createRenderObject (OBJ.obj2OTV3Mesh geom) (mtlFn mtl)

loadOBJWithDefaultMaterial :: FilePath -> (Maybe Material) -> IO [RenderObject]
loadOBJWithDefaultMaterial fp defaultMtl = do
  objExists <- doesFileExist fp
  if not objExists then error ("OBJ file " ++ fp ++ " not found") else return ()

  let baseDir = takeDirectory fp
  OBJ.OBJOutput mtllib meshes <- OBJ.loadOBJ fp

  let mtlFile = if null mtllib then "" else baseDir </> mtllib
  mtlExists <- doesFileExist mtlFile
  mtls <- if not mtlExists
          then
            case defaultMtl of
              Nothing -> error ("MTL file " ++ mtlFile ++ " not found and no default specified")
              Just _ -> return []
          else MTL.loadMTL mtlFile

  mtlmap <- genMtlMap baseDir mtls
  mapM (mesh2RenderObj $ lookupMtl mtlmap defaultMtl) meshes

loadOBJ :: FilePath -> IO [RenderObject]
loadOBJ fp = loadOBJWithDefaultMaterial fp Nothing

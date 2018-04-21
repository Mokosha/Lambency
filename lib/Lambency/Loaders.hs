module Lambency.Loaders (
  loadOBJ,
  loadOBJWithDefaultMaterial,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import qualified Data.Map as Map

import Lambency.Material
import Lambency.Mesh
import Lambency.Types

import qualified Lambency.Loaders.OBJLoader as OBJ
import qualified Lambency.Loaders.MTLLoader as MTL

import System.Directory (doesFileExist)
import System.FilePath
--------------------------------------------------------------------------------

genMtlMap :: Renderer -> FilePath -> [MTL.MTL] -> IO (Map.Map String Material)
genMtlMap r baseDir mtls = Map.fromList <$> mapM cvtMtl mtls
  where
    cvtMtl mtl = do
      m <- MTL.mkMaterial r baseDir mtl
      return (MTL.mtlName mtl, m)

lookupMtl :: Map.Map String Material -> Maybe Material -> String -> Material
lookupMtl mtlmap defaultMtl str =
  case Map.lookup str mtlmap of
    Nothing ->
      case defaultMtl of
        Nothing -> error "Lambency.Loaders (lookupMtl): No default material and no stored material"
        Just m -> m
    Just m -> m

mesh2RenderObj :: Renderer -> (String -> Material) -> (OBJ.OBJInfo, OBJ.OBJGeometry)
               -> IO RenderObject
mesh2RenderObj r mtlFn (OBJ.OBJInfo _ mtl _ 0 0 _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2V3Mesh geom
  in
   case usesTextures m of
     False ->
       case isUnlit m of
         True -> createRenderObject r mesh m
         False -> createRenderObject r (genNormalsV3 mesh) m
     True ->
       case isUnlit m of
         True -> createRenderObject r (genTexCoordsV3 mesh) m
         False -> createRenderObject r (genTexCoordsOV3 . genNormalsV3 $ mesh) m

mesh2RenderObj r mtlFn (OBJ.OBJInfo _ mtl _ _ 0 _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2TV3Mesh geom
  in
   case isUnlit m of
     False -> createRenderObject r (genNormalsTV3 mesh) m
     True -> createRenderObject r mesh m

mesh2RenderObj r mtlFn (OBJ.OBJInfo _ mtl _ 0 _ _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2OV3Mesh geom
  in
   case usesTextures m of
     False -> createRenderObject r mesh m
     True -> createRenderObject r (genTexCoordsOV3 mesh) m

mesh2RenderObj r mtlFn (OBJ.OBJInfo _ mtl _ _ _ _, geom) =
  createRenderObject r (OBJ.obj2OTV3Mesh geom) (mtlFn mtl)

loadOBJWithDefaultMaterial :: Renderer -> FilePath -> (Maybe Material)
                           -> IO [RenderObject]
loadOBJWithDefaultMaterial r fp defaultMtl = do
  objExists <- doesFileExist fp
  if not objExists then error ("OBJ file " ++ fp ++ " not found") else return ()

  let baseDir = takeDirectory fp
  OBJ.OBJOutput mtllib meshes <- OBJ.loadOBJ fp

  let mtlFile = if null mtllib then "" else baseDir </> mtllib
  mtlExists <- doesFileExist mtlFile
  mtls <- if not mtlExists
          then
            case defaultMtl of
              Nothing -> error $ concat
                         [ "MTL file "
                         , mtlFile
                         , " not found and no default specified"
                         ]
              Just _ -> return []
          else MTL.loadMTL mtlFile

  mtlmap <- genMtlMap r baseDir mtls
  mapM (mesh2RenderObj r $ lookupMtl mtlmap defaultMtl) meshes

loadOBJ :: Renderer -> FilePath -> IO [RenderObject]
loadOBJ r fp = loadOBJWithDefaultMaterial r fp Nothing

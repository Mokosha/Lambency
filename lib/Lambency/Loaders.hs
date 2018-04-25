module Lambency.Loaders (
  loadOBJ,
  loadOBJWithDefaultMaterial,
) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Monad.Reader

import qualified Data.Map as Map
import Data.Monoid

import Lambency.Material
import Lambency.Mesh
import Lambency.Renderer
import Lambency.Types

import qualified Lambency.Loaders.OBJLoader as OBJ
import qualified Lambency.Loaders.MTLLoader as MTL

import System.Directory (doesFileExist)
import System.FilePath
--------------------------------------------------------------------------------

genMtlMap :: FilePath -> [MTL.MTL] -> ResourceLoader (Map.Map String Material)
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
        Nothing -> error $ "Lambency.Loaders (lookupMtl): "
                        <> "No default material and no stored material"
        Just m -> m
    Just m -> m

mesh2RenderObj :: (String -> Material) -> (OBJ.OBJInfo, OBJ.OBJGeometry)
               -> ResourceLoader RenderObject
mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ 0 0 _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2V3Mesh geom
  in case usesTextures m of
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
  in case isUnlit m of
    False -> createRenderObject (genNormalsTV3 mesh) m
    True -> createRenderObject mesh m

mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ 0 _ _, geom) =
  let m = mtlFn mtl
      mesh = OBJ.obj2OV3Mesh geom
  in case usesTextures m of
    False -> createRenderObject mesh m
    True -> createRenderObject (genTexCoordsOV3 mesh) m

mesh2RenderObj mtlFn (OBJ.OBJInfo _ mtl _ _ _ _, geom) =
  createRenderObject (OBJ.obj2OTV3Mesh geom) (mtlFn mtl)

loadOBJWithDefaultMaterial :: FilePath -> (Maybe Material)
                           -> ResourceLoader [RenderObject]
loadOBJWithDefaultMaterial fp defaultMtl = do
  objExists <- liftIO $ doesFileExist fp
  if not objExists then error ("OBJ file " ++ fp ++ " not found") else return ()

  let baseDir = takeDirectory fp
  OBJ.OBJOutput mtllib meshes <- liftIO $ OBJ.loadOBJ fp

  let mtlFile = if null mtllib then "" else baseDir </> mtllib
  mtlExists <- liftIO $ doesFileExist mtlFile
  mtls <- if mtlExists
          then liftIO $ MTL.loadMTL mtlFile
          else
            case defaultMtl of
              Nothing -> error $ concat
                         [ "MTL file "
                         , mtlFile
                         , " not found and no default specified"
                         ]
              Just _ -> return []

  mtlmap <- genMtlMap baseDir mtls
  mapM (mesh2RenderObj $ lookupMtl mtlmap defaultMtl) meshes

loadOBJ :: FilePath -> ResourceLoader [RenderObject]
loadOBJ fp = loadOBJWithDefaultMaterial fp Nothing

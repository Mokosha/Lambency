module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import qualified Data.Map as Map

import System.Directory
import System.FilePath
import Paths_lambency_examples

import GHC.Float (double2Float)
---------------------------------------------------------------------------------

data CubeDemoObject = Triangle

rotateCamera :: Double -> LR.Camera -> LR.Camera
rotateCamera dt cam =
  let newPos = actU (rotU vec3Y (double2Float dt)) (LR.getCamPos cam)
      newDir = (mkNormal . neg) newPos
  in flip LR.setCamDir newDir $ LR.setCamPos cam newPos

stillCamera :: Double -> LR.Camera -> LR.Camera
stillCamera = flip const

kCamPos :: Vec3
kCamPos = Vec3 4 3 3

kCamDir :: Normal3
kCamDir = (mkNormal . neg) kCamPos

kCamUp :: Normal3
kCamUp = mkNormal vec3Y

demoCam :: LR.GameCamera
demoCam = LR.GameCamera
          (LR.mkOrthoCamera kCamPos kCamDir kCamUp (-10) 10 10 (-10) 0.1 1000.0)
          rotateCamera

demoSVMap :: Map.Map LR.ShaderVar (CubeDemoObject -> LR.Camera -> LR.ShaderValue)
demoSVMap =
  Map.insert (LR.Uniform LR.Matrix4Ty "mvpMatrix")
             (\_ c -> LR.Matrix4Val $ LR.getViewProjMatrix c)
  Map.empty

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  ro <- LR.createRenderObject LR.makeCube
  mtex <- LR.loadTextureFromPNG =<< (getDataFileName $ "crate" <.> "png")
--  mtex <- getDataFileName "crate1_diffuse.png" >>= LR.loadTextureFromPNG
  case mtex of
    Nothing -> return ()
    Just tex -> do
      mat <- LR.createTexturedMaterial tex
      let triObj = LR.GameObject {
            LR.renderObject = Just (LR.assignMaterial ro mat),
            LR.gameObject = Triangle,
            LR.objSVMap = demoSVMap,
            LR.update = (\t a -> Just a),
            LR.collide = (\a as -> Just a)}
      case m of
        (Just win) -> L.run win demoCam [triObj]
        Nothing -> return ()
  L.destroyWindow m

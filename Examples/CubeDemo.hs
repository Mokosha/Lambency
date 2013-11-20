module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import Data.Maybe (fromJust)
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

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  ro <- LR.createRenderObject LR.makeCube
  (Just tex) <- LR.loadTextureFromPNG =<< (getDataFileName $ "crate" <.> "png")
  let mvpSV = (LR.getMaterialVar (LR.material ro) "mvpMatrix")
      mSV = (LR.getMaterialVar (LR.material ro) "m2wMatrix")
      svMap = Map.union
              (Map.singleton mvpSV (\_ c -> LR.Matrix4Val $ LR.getViewProjMatrix c))
              (Map.singleton mSV (\_ c -> LR.Matrix4Val one))
      triObj = LR.GameObject {
        LR.renderObject = Just (LR.switchMaterialTexture ro "diffuseTex" tex),
        LR.gameObject = Triangle,
        LR.objSVMap = svMap,
        LR.update = (\t a -> Just a),
        LR.collide = (\a as -> Just a)}
  case m of
    (Just win) -> L.run win demoCam [triObj]
    Nothing -> return ()
  L.destroyWindow m

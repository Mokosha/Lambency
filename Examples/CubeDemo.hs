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

data CubeDemoObject = DemoObject Float Vec3 UnitQuaternion

demoCam :: LR.Camera
demoCam = LR.mkPerspCamera
           -- Pos           Dir              Up
           ((-15) *& vec3Z) (mkNormal vec3Z) (mkNormal vec3Y)
           (pi / 4) (4.0 / 3.0)
           -- near far
           0.1 1000.0

demoSVMap :: LR.RenderObject -> Map.Map LR.ShaderVar (CubeDemoObject -> LR.Camera -> LR.ShaderValue)
demoSVMap ro = Map.fromList [
  (lu "mvpMatrix", updateMVPMatrix),
  (lu "m2wMatrix", updateModelMatrix)]
  where
    updateModelMatrix :: CubeDemoObject -> LR.Camera -> LR.ShaderValue
    updateModelMatrix (DemoObject scale pos rot) c =
      LR.Matrix4Val $ LR.sprToMatrix scale pos rot

    updateMVPMatrix :: CubeDemoObject -> LR.Camera -> LR.ShaderValue
    updateMVPMatrix obj c = LR.Matrix4Val $ model .*. (LR.getViewProjMatrix c)
      where (LR.Matrix4Val model) = updateModelMatrix obj c
      
    lu = LR.getMaterialVar (LR.material ro)

planeObj :: IO (LR.GameObject CubeDemoObject)
planeObj = do
  mat <- LR.createSpotlightMaterial . Just =<< (LR.createSolidTexture (128, 128, 128, 255))
  ro <- LR.createRenderObject LR.makePlane mat
  return LR.GameObject {
    LR.renderObject = Just ro,
    LR.gameObject = DemoObject 10 (Vec3 0 (-2) 0) unitU,
    LR.objSVMap = demoSVMap ro,
    LR.update = \_ o _ -> Just o
  }

cubeObj :: IO (LR.GameObject CubeDemoObject)
cubeObj = do
  ro <- (getDataFileName $ "crate" <.> "png") >>=
        LR.loadTextureFromPNG >>=
        LR.createSpotlightMaterial >>=
        LR.createRenderObject LR.makeCube
  return LR.GameObject {
    LR.renderObject = Just ro,
    LR.gameObject = DemoObject 1 zero $ rotU (Vec3 1 0 1) 0.6,
    LR.objSVMap = demoSVMap ro,
    LR.update = \t obj _ -> Just $ rotateObj t obj
  }
  where 
  rotateObj dt (DemoObject s p u) = DemoObject s p $ u .*. (rotU vec3Y $ double2Float dt)

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  objs <- sequence [cubeObj, planeObj]
  case m of
    (Just win) -> L.run win (LR.GameCamera demoCam $ flip const) objs
    Nothing -> return ()
  L.destroyWindow m

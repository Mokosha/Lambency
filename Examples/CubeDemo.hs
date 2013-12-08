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

type CubeDemoObject = LR.Transform

demoCam :: LR.Camera
demoCam = LR.mkPerspCamera
           -- Pos           Dir              Up
           ((-15) *& vec3Z) (mkNormal (vec3Z)) (mkNormal vec3Y)
           (pi / 4) (4.0 / 3.0)
           -- near far
           0.1 1000.0

planeObj :: LR.Material -> IO (LR.GameObject CubeDemoObject)
planeObj mat = do
  ro <- LR.createRenderObject LR.makePlane mat
  return $ LR.mkStaticObject xform xform (Just ro)
  where xform = LR.uniformScale 10 $
                LR.translate (Vec3 0 (-2) 0) $
                LR.identityXForm

cubeObj :: LR.Material -> IO (LR.GameObject CubeDemoObject)
cubeObj mat = do
  (Just tex) <- getDataFileName ("crate" <.> "png") >>= LR.loadTextureFromPNG
  ro <- LR.createRenderObject LR.makeCube (LR.switchTexture mat "diffuseTex" tex)
  return LR.Object {
    LR.location = id,
    LR.renderObject = Just ro,
    LR.gameObject = LR.rotate (rotU (Vec3 1 0 1) 0.6) LR.identityXForm,
    LR.objSVMap = Map.empty,
    LR.update = \t obj _ -> Just $ rotateObj t obj
  }
  where
    rotateObj :: Double -> CubeDemoObject -> CubeDemoObject
    rotateObj dt = LR.rotateWorld (rotU vec3Y $ double2Float dt)

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  mat <- LR.createSpotlightMaterial . Just =<< (LR.createSolidTexture (128, 128, 128, 255))
  objs <- sequence [cubeObj mat, planeObj mat]
  case m of
    (Just win) -> L.run win (LR.mkDebugCam demoCam) objs
    Nothing -> return ()
  L.destroyWindow m

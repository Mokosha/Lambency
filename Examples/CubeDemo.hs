module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import GHC.Float (double2Float)
---------------------------------------------------------------------------------

data CubeDemoObject = Triangle

kInitialCameraPosition :: Vec3
kInitialCameraPosition = Vec3 0 0 5

kInitialCameraDirection :: UnitQuaternion
kInitialCameraDirection =
  LR.quatFromVecs (mkNormal . neg $ kInitialCameraPosition) (toNormalUnsafe . neg $ vec3Z)

kInitialOrthoCamera :: LR.CameraType
kInitialOrthoCamera = LR.mkOrthoCamera (toNormalUnsafe vec3Y) (-10) 10 10 (-10) 0.1 1000.0

rotatingCamera :: LR.GameCamera
rotatingCamera = LR.GameCamera LR.GameObject {
  LR.position = kInitialCameraPosition,
  LR.orientation = kInitialCameraDirection,
  LR.renderObject = Nothing,
  LR.gameObject = kInitialOrthoCamera,
  LR.update = \t a -> let newPos = actU (rotU vec3Y (double2Float t)) (LR.position a)
                          newDir = (mkNormal . neg) newPos
                          (LR.GameCamera newCam) = flip LR.setDirection newDir $ LR.setPosition (LR.GameCamera a) newPos
                      in Just newCam,
  LR.collide = (\a as -> Just a)
}

stationaryCamera :: LR.GameCamera
stationaryCamera = LR.GameCamera LR.GameObject {
  LR.position = kInitialCameraPosition,
  LR.orientation = kInitialCameraDirection,
  LR.renderObject = Nothing,
  LR.gameObject = kInitialOrthoCamera,
  LR.update = \t a -> Just a,
  LR.collide = (\a as -> Just a)
}

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  renderTriangle <- LR.createRenderObject LR.makeTriangle
  let triObj = LR.GameObject {
        LR.position = Vec3 0 0 0,
        LR.orientation = unitU,
        LR.renderObject = Just renderTriangle,
        LR.gameObject = Triangle,
        LR.update = (\t a -> Just a),
        LR.collide = (\a as -> Just a)}
  case m of
    (Just win) -> L.run win rotatingCamera [triObj]
    Nothing -> return ()
  L.destroyWindow m

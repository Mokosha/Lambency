module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import GHC.Float (double2Float)
---------------------------------------------------------------------------------

data CubeDemoObject = Triangle

rotatingCamera :: LR.GameCamera
rotatingCamera = LR.GameCamera LR.GameObject {
  LR.position = Vec3 4 3 3,
  LR.orientation = LR.quatFromVecs (mkNormal . neg $ Vec3 0.01 0 0.9) (toNormalUnsafe . neg $ vec3Z),
  LR.renderObject = Nothing,
  LR.gameObject = LR.mkOrthoCamera (toNormalUnsafe vec3Y) (-10) 10 10 (-10) 0.1 1000.0,
  LR.update = \t a -> let newPos = actU (rotU vec3Y (double2Float t)) (LR.position a)
                          newDir = (mkNormal . neg) newPos
                          (LR.GameCamera newCam) = flip LR.setDirection newDir $ LR.setPosition (LR.GameCamera a) newPos
                      in Just newCam,
  LR.collide = (\a as -> Just a)
}

main :: IO ()
main = let
  pos = Vec3 4 3 3
  dir = (mkNormal . neg) pos
  in do
    m <- L.makeWindow 640 480 "Cube Demo"
    let triangle = LR.makeTriangle
    renderTri <- LR.createRenderObject triangle
    let triObj = LR.GameObject {
          LR.position = Vec3 0 0 0,
          LR.orientation = unitU,
          LR.renderObject = Just renderTri,
          LR.gameObject = Triangle,
          LR.update = (\t a -> Just a),
          LR.collide = (\a as -> Just a)
      }
    case m of
      (Just win) -> L.run win rotatingCamera [triObj]
      Nothing -> return ()
    L.destroyWindow m

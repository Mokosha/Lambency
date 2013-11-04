module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

---------------------------------------------------------------------------------

data CubeDemoObject = Triangle

main :: IO ()
main = let
  pos = Vec3 4 3 3
  dir = (mkNormal . neg) pos
  camera =
    (flip LR.setDirection) dir
    $ LR.setPosition LR.simpleOrthoCamera pos
  in do
    m <- L.makeWindow 640 480 "Cube Demo"
    let triangle = LR.makeTriangle
    renderTri <- LR.createRenderObject triangle
    let triObj = LR.GameObject {
          LR.baseObject = LR.BaseObject {
             LR.position = Vec3 0 0 0,
             LR.orientation = unitU,
             LR.renderObj = Just renderTri },
          LR.gameObject = Triangle,
          LR.update = (\t a -> Just a),
          LR.collide = (\a as -> Just a)
      }
    case m of
      (Just win) -> L.run win camera [triObj]
      Nothing -> return ()
    L.destroyWindow m

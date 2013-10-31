module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float

---------------------------------------------------------------------------------

main :: IO ()
main = let
  pos = Vec3 0 4 (-3)
  -- dir = toNormalUnsafe $ Vec3 0 0 1
  dir = (mkNormal . neg) pos
  camera =
    (flip LR.setDirection) dir
    $ LR.setPosition LR.simpleOrthoCamera pos
  in do
    m <- L.makeWindow 640 480 "Cube Demo"
    let triangle = LR.makeTriangle
    renderTri <- LR.createRenderObject triangle
    case m of
      (Just win) -> L.run win camera [renderTri]
      Nothing -> return ()
    L.destroyWindow m

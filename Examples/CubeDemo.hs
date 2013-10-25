module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

---------------------------------------------------------------------------------

main :: IO ()
main = do
  let triangle = LR.makeTriangle
  renderTri <- LR.createRenderObject triangle
  m <- L.makeWindow 640 480 "Cube Demo"
  case m of
    (Just win) -> L.run win [renderTri]
    Nothing -> return ()
  L.destroyWindow m

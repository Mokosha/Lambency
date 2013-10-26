module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

---------------------------------------------------------------------------------

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  let triangle = LR.makeTriangle
  renderTri <- LR.createRenderObject triangle
  case m of
    (Just win) -> L.run win [renderTri]
    Nothing -> return ()
  L.destroyWindow m

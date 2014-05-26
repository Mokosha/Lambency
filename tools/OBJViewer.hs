module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import System.FilePath

import Linear.Vector
import Linear.V3

import qualified Control.Wire as W
---------------------------------------------------------------------------------

initialCam :: LR.Camera
initialCam = LR.mkPerspCamera
             -- Pos           Dir              Up
             ((-15) *^ LR.localForward) (LR.localForward) (LR.localUp)
             (pi / 4) (4.0 / 3.0)
             -- near far
             0.1 1000.0

cam :: LR.GameWire () LR.Camera
cam = LR.mkViewerCam initialCam

mkOBJ :: FilePath -> IO (LR.RenderObject)
mkOBJ objfile = do
  tex <- LR.createSolidTexture (67, 128, 67, 255)
--  mesh <- LR.loadOBJ objfile
  ro <- LR.createRenderObject (LR.cube) (LR.createTexturedMaterial tex)
  return ro

initGame :: FilePath -> IO (LR.Game ())
initGame objfile = do
  obj <- mkOBJ objfile
  let lightPos = 10 *^ (V3 (-1) 1 0)
  spotlight <- LR.createSpotlight lightPos (negate lightPos) 0
  return $ LR.Game { LR.staticLights = [LR.setAmbient (V3 0.5 0.5 0.5) spotlight],
                     LR.staticGeometry = [(LR.identity, obj)],
                     LR.mainCamera = cam,
                     LR.dynamicLights = [],
                     LR.gameLogic = W.mkId }

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "OBJ Viewer"
  game <- initGame ""
  case m of
    (Just win) -> L.run win () game
    Nothing -> return ()
  L.destroyWindow m

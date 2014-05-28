module Main (main) where

--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import System.Environment

import qualified Linear.Quaternion as Quat
import Linear.Vector
import Linear.V3

import Control.Monad.RWS.Strict
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
cam = LR.mkFixedCam initialCam

mkOBJ :: FilePath -> IO (LR.RenderObject)
mkOBJ objfile = do
  tex <- LR.createSolidTexture (67, 128, 67, 255)
  mesh <- LR.loadOBJ objfile
  ro <- LR.createRenderObject mesh (LR.createTexturedMaterial tex)
  return ro

controlWire :: LR.RenderObject -> LR.GameWire a a
controlWire ro = LR.mkObject ro (xForm LR.identity)
  where
    xForm :: LR.Transform -> LR.GameWire a LR.Transform
    xForm xf = W.mkGenN $ \_ -> do
      ipt <- get
      let rotate = L.isButtonPressed GLFW.MouseButton'1 ipt
          newxf = if rotate then rotation ipt else xf
      put $ L.resetCursorPos ipt
      return (Right newxf, xForm newxf)
      where
        rotation :: L.Input -> LR.Transform
        rotation ipt = case (L.cursor ipt) of
          Just (mx, my) -> flip LR.rotateWorld xf $
                           foldl1 (*) [
                             Quat.axisAngle LR.localUp (-asin mx),
                             Quat.axisAngle LR.localRight $ asin my]
          Nothing -> xf

initGame :: FilePath -> IO (LR.Game ())
initGame objfile = do
  obj <- mkOBJ objfile
  let lightPos = 10 *^ (V3 0 1 (-1))
  spotlight <- LR.createSpotlight lightPos (negate lightPos) 0
  return $ LR.Game { LR.staticLights = [LR.setAmbient (V3 0.5 0.5 0.5) spotlight],
                     LR.staticGeometry = [],
                     LR.mainCamera = cam,
                     LR.dynamicLights = [],
                     LR.gameLogic = controlWire obj }

main :: IO ()
main = do
  args <- getArgs
  let
    objfile :: FilePath
    objfile = case args of
        (x : []) -> x
        _ -> error "Usage: lobjview OBJFILE"
  m <- L.makeWindow 640 480 "OBJ Viewer"
  game <- initGame objfile
  case m of
    (Just win) -> L.run win () game
    Nothing -> return ()
  L.destroyWindow m

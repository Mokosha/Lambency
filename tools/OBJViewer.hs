module Main (main) where

--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import qualified Lambency as L

import System.Environment

import qualified Linear.Quaternion as Quat
import Linear.Vector
import Linear.V3

import Control.Monad.RWS.Strict
import qualified Control.Wire as W
---------------------------------------------------------------------------------

initialCam :: L.Camera
initialCam = L.mkPerspCamera
             -- Pos           Dir              Up
             ((-15) *^ L.localForward) (L.localForward) (L.localUp)
             (pi / 4) (4.0 / 3.0)
             -- near far
             0.1 1000.0

cam :: L.GameWire () L.Camera
cam = L.mkFixedCam initialCam

mkOBJ :: FilePath -> IO (L.RenderObject)
mkOBJ objfile = do
  tex <- L.createSolidTexture (67, 128, 67, 255)
  mesh <- L.loadOV3 objfile
  ro <- L.createRenderObject mesh (L.createTexturedMaterial tex)
  return ro

controlWire :: L.RenderObject -> L.GameWire a a
controlWire ro = L.mkObject ro (xForm L.identity)
  where
    xForm :: L.Transform -> L.GameWire a L.Transform
    xForm xf = W.mkGenN $ \_ -> do
      ipt <- get
      let rotate = L.isButtonPressed GLFW.MouseButton'1 ipt
          newxf = if rotate then rotation ipt else xf
      put $ L.resetCursorPos ipt
      return (Right newxf, xForm newxf)
      where
        rotation :: L.Input -> L.Transform
        rotation ipt = case (L.cursor ipt) of
          Just (mx, my) -> flip L.rotateWorld xf $
                           foldl1 (*) [
                             Quat.axisAngle L.localUp (-asin mx),
                             Quat.axisAngle L.localRight $ asin my]
          Nothing -> xf

initGame :: FilePath -> IO (L.Game ())
initGame objfile = do
  obj <- mkOBJ objfile
  let lightPos = 10 *^ (V3 0 1 (-1))
  spotlight <- L.createSpotlight lightPos (negate lightPos) 0
  return $ L.Game { L.staticLights = [L.setAmbient (V3 0.5 0.5 0.5) spotlight],
                     L.staticGeometry = [],
                     L.mainCamera = cam,
                     L.dynamicLights = [],
                     L.gameLogic = controlWire obj }

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

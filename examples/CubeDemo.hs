module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad.Reader

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import System.FilePath
import Paths_lambency_examples

import Linear.Vector
import Linear.V2
import Linear.V3
import qualified Linear.Quaternion as Quat

import qualified Control.Wire as W
import FRP.Netwire.Analyze
---------------------------------------------------------------------------------

initialCam :: L.Camera
initialCam = L.mkPerspCamera
             -- Pos           Dir              Up
             ((-15) *^ L.localForward) (L.localForward) (L.localUp)
             (pi / 4) (4.0 / 3.0)
             -- near far
             0.1 1000.0

demoCam :: L.GameWire () L.Camera
demoCam = L.mkDebugCam initialCam

mkPlane :: IO (L.Transform, L.RenderObject)
mkPlane = do
  tex <- L.createSolidTexture (128, 128, 128, 255)
  ro <- L.createRenderObject L.plane (L.createTexturedMaterial tex)
  return (xform, ro)
  where xform = L.uniformScale 10 $
                L.translate (V3 0 (-2) 0) $
                L.identity

mkBunny:: IO (L.Transform, L.RenderObject)
mkBunny = do
  tex <- L.createSolidTexture (67, 128, 67, 255)
  mesh <- getDataFileName ("bunnyN" <.> "obj") >>= L.loadOTV3
  ro <- L.createRenderObject mesh (L.createTexturedMaterial tex)
  return (xform, ro)
  where xform = L.rotate (Quat.axisAngle (V3 0 1 0) pi) $
                L.translate (V3 (-4) (-4.8) (-5)) $
                L.identity

cubeWire :: IO (L.GameWire () ())
cubeWire = do
  sound <- getDataFileName ("stereol" <.> "wav") >>= L.loadSound
  (Just tex) <- getDataFileName ("crate" <.> "png") >>= L.loadTexture
  mesh <- getDataFileName ("cube" <.> "obj") >>= L.loadOTV3
  ro <- L.createRenderObject mesh (L.createTexturedMaterial tex)
  return $ playSound sound 3.0 W.>>> (L.mkObject ro (rotate initial))
  where
    playSound :: L.Sound -> Float -> L.GameWire a a
    playSound sound p = L.pulseSound sound W.>>> (W.for p) W.-->
                        playSound sound p

    rotate :: L.Transform -> L.GameWire a L.Transform
    rotate xform =
      W.mkPure (\t _ -> let
                   rotation = Quat.axisAngle L.localUp $ 3.0 * (W.dtime t)
                   newxform = L.rotateWorld rotation xform
                   in (Right newxform, rotate newxform))

    initial :: L.Transform
    initial = L.rotate (Quat.axisAngle (V3 1 0 1) 0.6) $
              L.uniformScale 2.0 $
              L.identity

frameWire :: L.Font -> L.GameWire a a
frameWire font = (W.mkId W.&&& (lastRenderTime W.>>> sAvg 5)) W.>>> renderWire
  where
    lastRenderTime :: L.GameWire a Float
    lastRenderTime = W.mkGen_ $ \_ -> do
      lastPicoSeconds <- ask
      return . Right $ fromIntegral lastPicoSeconds / 1000000000.0
    
    renderWire :: L.GameWire (a, Float) a
    renderWire = W.mkGen_ $ \(v, fps) -> do
      L.renderUIString font ("Frame Time (ms): " ++ (show fps)) (V2 10 10)
      return $ Right v

loadGame :: IO (L.Game ())
loadGame = do
  sysFont <- getDataFileName ("kenpixel" <.> "ttf") >>= flip L.loadTTFont 18
  plane <- mkPlane
  bunny <- mkBunny
  cube <- cubeWire
  let lightPos = 10 *^ (V3 (-1) 1 0)
      gameWire = cube W.>>>
                 (frameWire sysFont) W.>>>
                 (L.quitWire GLFW.Key'Q)
  spotlight <- L.createSpotlight lightPos (negate lightPos) 0
  return $ L.Game { L.staticLights = [spotlight],
                    L.staticGeometry = [plane, bunny],
                    L.mainCamera = demoCam,
                    L.dynamicLights = [],
                    L.gameLogic = gameWire}

main :: IO ()
main = L.runWindow 640 480 "Cube Demo" () loadGame

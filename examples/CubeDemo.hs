module Main (main) where

--------------------------------------------------------------------------------
import Prelude hiding ((.))

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

import Control.Monad.Reader

#if __GLASGOW_HASKELL__ <= 708
import Data.Traversable (sequenceA)
#endif

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import System.FilePath
import Paths_lambency

import Linear.Vector
import Linear.V2
import Linear.V3
import qualified Linear.Quaternion as Quat

import Control.Wire ((.))
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
demoCam = L.mkFreeCam initialCam

mkPlane :: IO (L.RenderObject, L.Transform)
mkPlane = do
  ro <- L.createRenderObject L.plane (L.diffuseColoredMaterial $ V3 0.5 0.5 0.5)
  return (ro, xform)
  where xform = L.uniformScale 10 $
                L.translate (V3 0 (-2) 0) $
                L.identity

mkBunny:: IO [(L.RenderObject, L.Transform)]
mkBunny = do
  objFile <- getDataFileName ("examples" </> "bunnyN" <.> "obj")
  ros <- L.loadOBJWithDefaultMaterial objFile $ Just (L.shinyColoredMaterial $ V3 0.26 0.5 0.26)
  return $ (\ro -> (ro, xform)) <$> ros
  where xform = L.rotate (Quat.axisAngle (V3 0 1 0) pi) $
                L.translate (V3 (-4) (-4.8) (-5)) $
                L.identity

cubeWire :: IO (L.GameWire () ())
cubeWire = do
  sound <- getDataFileName ("examples" </> "stereol" <.> "wav") >>= L.loadSound
  (Just tex) <- getDataFileName ("examples" </> "crate" <.> "png") >>= L.loadTexture
  objFile <- getDataFileName ("examples" </> "cube" <.> "obj")
  ros <- L.loadOBJWithDefaultMaterial objFile $ Just (L.diffuseTexturedMaterial tex)
  return $
    playSound sound 3.0 W.>>>
    (sequenceA $ (\ro -> L.mkObject ro (rotate initial)) <$> ros) W.>>>
    (pure ())
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

lightWire :: L.Light -> L.GameWire () L.Light
lightWire initial = (W.timeF W.>>>) $ W.mkSF_ $ \t ->
  let Just (V3 _ py pz) = L.getLightPosition initial
      newPos = V3 (sin(t) * 10) py pz
  in L.setLightPosition newPos $
     L.setLightDirection (negate newPos) initial

loadGame :: IO (L.Game ())
loadGame = do
  sysFont <- L.loadTTFont 18 (V3 1 0 0) =<<
             getDataFileName ("examples" </> "kenpixel" <.> "ttf")
  plane <- uncurry L.staticObject <$> mkPlane
  bunny <- foldl (W.>>>) W.mkId <$> map (uncurry L.staticObject) <$> mkBunny
  cube <- cubeWire
  let gameWire = L.quitWire GLFW.Key'Q . frameWire sysFont . cube . bunny . plane
      lightPos = 5 *^ (V3 (-2) 1 0)
      lightParams = L.mkLightParams (V3 0.15 0.15 0.15) (V3 1.0 1.0 1.0) 1.0
  shadowLight <- L.addShadowMap $
                 L.spotlight lightParams lightPos (negate lightPos) (pi/4)
  return $ L.Game { L.mainCamera = demoCam,
                    L.dynamicLights = [lightWire shadowLight],
                    L.gameLogic = gameWire}

main :: IO ()
main = L.withWindow 640 480 "Cube Demo" $ L.loadAndRun () loadGame

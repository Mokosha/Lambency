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

import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Data.Word

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import System.FilePath
import Paths_lambency

import Linear.Vector
import Linear.V3
import Linear.V4
import qualified Linear.Quaternion as Quat

import Control.Wire ((.))
import qualified Control.Wire as W
import FRP.Netwire.Analyze
import qualified Yoga as Y
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
  ros <- L.loadOBJWithDefaultMaterial objFile
         $ Just (L.shinyColoredMaterial $ V3 0.26 0.5 0.26)
  return $ (\ro -> (ro, xform)) <$> ros
  where xform = L.rotate (Quat.axisAngle (V3 0 1 0) pi) $
                L.translate (V3 (-4) (-4.8) (-5)) $
                L.identity

cubeWire :: L.GameWire () ()
cubeWire =
  L.bracketResource loadSound L.unloadSound $ \sound ->
  L.bracketResource loadTex L.destroyTexture $ \tex ->
  L.bracketResource (loadMeshes tex) (traverse_ L.unloadRenderObject) $ \ros ->
    playSound sound 3.0 W.>>>
    (sequenceA $ (\ro -> L.mkObject ro (rotate initial)) <$> ros) W.>>>
    pure ()
  where
    loadMeshes :: L.Texture -> IO [L.RenderObject]
    loadMeshes t = do
      objFile <- getDataFileName ("examples" </> "cube" <.> "obj")
      L.loadOBJWithDefaultMaterial objFile $ Just (L.diffuseTexturedMaterial t)

    loadTex :: IO L.Texture
    loadTex =
      liftM fromJust $
      getDataFileName ("examples" </> "crate" <.> "png") >>= L.loadTexture

    loadSound :: IO L.Sound
    loadSound =
      getDataFileName ("examples" </> "stereol" <.> "wav") >>= L.loadSound

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

lightWire :: L.Light -> L.GameWire () L.Light
lightWire initial = (W.timeF W.>>>) $ W.mkSF_ $ \t ->
  let Just (V3 _ py pz) = L.getLightPosition initial
      newPos = V3 (sin(t) * 10) py pz
  in L.setLightPosition newPos $
     L.setLightDirection (negate newPos) initial

uiWire :: Monoid a => L.Font -> L.GameWire a a
uiWire font = L.screen [
  L.hbox [renderTime, L.glue],
  L.glue,
  L.hbox [L.glue, button]
  ]
  where
    background = let
        blue :: V4 Word8
        blue = V4 0 0 255 255

        yellow :: V4 Word8
        yellow = V4 255 255 0 255
      in L.WidgetState {
        L.idleLogic = L.colorRenderer blue W.mkId,
        L.eventHandlers =
          [L.WidgetEvent'OnKeyDown GLFW.Key'U $ L.colorRenderer yellow W.mkId]
        }

    button = L.Widget
             $ ($ background)
             $ Y.withMargin Y.Edge'All 10.0
             $ Y.exact 10.0 10.0

    lastRenderTime :: L.GameWire a Float
    lastRenderTime = W.mkGen_ $ \_ -> do
      lastPicoSeconds <- L.lastFrameTime <$> ask
      return . Right $ fromIntegral lastPicoSeconds / 1000000000.0

    avgRenderTimeWire :: L.GameWire a String
    avgRenderTimeWire =
      ("Frame Time (ms): " ++) . show <$> (lastRenderTime W.>>> sAvg 5)

    frameRenderState = let
      idle = L.dynamicTextRenderer font $ W.mkId W.&&& avgRenderTimeWire
      in L.WidgetState idle []

    renderTime = L.Widget
                 $ ($ frameRenderState)
                 $ Y.withMargin Y.Edge'Left 5.0
                 $ Y.withMargin Y.Edge'Top 15.0
                 $ Y.exact 300.0 50.0

loadGame :: IO (L.Game ())
loadGame = do
  sysFont <- L.loadTTFont 18 (V3 1 0 0) =<<
             getDataFileName ("examples" </> "kenpixel" <.> "ttf")
  plane <- uncurry L.staticObject <$> mkPlane
  bunny <- foldl (W.>>>) W.mkId <$> map (uncurry L.staticObject) <$> mkBunny
  let gameWire = L.quitWire GLFW.Key'Q . cubeWire . bunny . plane
      lightPos = 5 *^ (V3 (-2) 1 0)
      lightParams = L.mkLightParams (V3 0.15 0.15 0.15) (V3 1.0 1.0 1.0) 1.0
  shadowLight <- L.addShadowMap $
                 L.spotlight lightParams lightPos (negate lightPos) (pi/4)
  return $ L.Game { L.mainCamera = demoCam,
                    L.dynamicLights = [lightWire shadowLight],
                    L.gameLogic = gameWire W.>>> uiWire sysFont }

main :: IO ()
main = L.withWindow 640 480 "Cube Demo" $ L.loadAndRun () loadGame

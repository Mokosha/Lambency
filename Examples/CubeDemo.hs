module Main (main) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.Lambency as L
import qualified Graphics.Rendering.Lambency as LR

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import Data.Traversable (sequenceA)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import System.Directory
import System.FilePath
import Paths_lambency_examples

import Control.Arrow
import qualified Control.Wire as W

import GHC.Float (double2Float)
---------------------------------------------------------------------------------

type CubeDemoObject = LR.Transform

demoCam :: LR.GameWire LR.Camera
demoCam = LR.mkDebugCam $ LR.mkPerspCamera
           -- Pos           Dir              Up
           ((-15) *& vec3Z) (mkNormal vec3Z) (mkNormal vec3Y)
           (pi / 4) (4.0 / 3.0)
           -- near far
           0.1 1000.0

planeWire :: IO (LR.GameWire [LR.GameObject])
planeWire = do
  tex <- LR.createSolidTexture (128, 128, 128, 255)
  ro <- LR.createRenderObject LR.plane (LR.createTexturedMaterial tex)
  return $ LR.mkStaticObject ro xform
  where xform = LR.uniformScale 10 $
                LR.translate (Vec3 0 (-2) 0) $
                LR.identity

cubeWire :: IO (LR.GameWire [LR.GameObject])
cubeWire = do
  (Just tex) <- getDataFileName ("crate" <.> "png") >>= LR.loadTextureFromPNG
  ro <- LR.createRenderObject LR.cube (LR.createTexturedMaterial tex)
  return $ LR.mkObject ro (rotate initial)
  where
    rotate :: Monad m => LR.Transform -> W.Wire LR.Timestep e m a LR.Transform
    rotate xform =
      W.mkPure (\ts _ -> let
                   W.Timed dt () = ts ()
                   newxform = LR.rotateWorld (rotU vec3Y dt) xform

                   in (Right newxform, rotate newxform))

    initial :: LR.Transform
    initial = LR.rotate (rotU (Vec3 1 0 1) 0.6) LR.identity

initGame :: IO (LR.Game)
initGame = do
  [cube, plane] <- sequence [cubeWire, planeWire]
  let lightPos = 10 *& (Vec3 (-1) 1 0)
  spotlight <- LR.createSpotlight lightPos (mkNormal $ neg lightPos) 0
  return (demoCam, [W.mkConst $ Right spotlight], [cube, plane])

main :: IO ()
main = do
  m <- L.makeWindow 640 480 "Cube Demo"
  game <- initGame
  case m of
    (Just win) -> L.run win game
    Nothing -> return ()
  L.destroyWindow m

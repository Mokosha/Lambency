module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative
import qualified Control.Wire as W

import Data.List (intercalate)

import qualified Graphics.UI.GLFW as GLFW

import FRP.Netwire.Input

import qualified Lambency as L

import qualified Linear.Quaternion as Quat
import Linear.Vector
import Linear.V3

import System.Directory (doesFileExist)
import System.Environment
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
  exists <- doesFileExist objfile
  if not exists then error ("OBJ file " ++ objfile ++ " not found") else return ()
  tex <- L.createSolidTexture (67, 128, 67, 255)
  mesh <- L.loadOV3 objfile
  ro <- L.createRenderObject mesh (L.createTexturedMaterial tex)
  return ro

controlWire :: L.RenderObject -> L.GameWire a a
controlWire ro = L.mkObject ro (xForm L.identity)
  where
    feedback :: L.GameWire (a, b) (b, b)
    feedback = W.arr $ \(_, x) -> (x, x)

    inputWire :: L.GameWire a (Float, Float)
    inputWire = (mousePressed GLFW.MouseButton'1 W.>>> mouseMickies) W.<|> (pure (0, 0))

    rotationFn :: (Float, Float) -> Quat.Quaternion Float
    rotationFn (0, 0) = Quat.axisAngle L.localRight 0
    rotationFn (mx, my) =
      foldl1 (*) [
        Quat.axisAngle L.localUp (-asin mx),
        Quat.axisAngle L.localRight $ asin my]

    rotation :: L.GameWire (Float, Float) (Quat.Quaternion Float)
    rotation = W.arr rotationFn

    handleQuat :: L.GameWire (Quat.Quaternion Float, L.Transform) L.Transform
    handleQuat = W.arr $ uncurry L.rotateWorld

    xForm :: L.Transform -> L.GameWire a L.Transform
    xForm initialXF =
      W.loop $ (W.second $ (inputWire W.>>> rotation) W.&&& W.mkId W.>>>
                handleQuat W.>>>
                (W.delay initialXF)) W.>>> feedback
      where

loadGame :: FilePath -> IO (L.Game ())
loadGame objfile = do
  obj <- mkOBJ objfile
  let lightPos = 10 *^ (V3 0 1 (-1))
  spotlight <- L.createSpotlight lightPos (negate lightPos) 0
  return $ L.Game { L.staticLights = [L.setAmbient (V3 0.5 0.5 0.5) spotlight],
                     L.staticGeometry = [],
                     L.mainCamera = cam,
                     L.dynamicLights = [],
                     L.gameLogic = controlWire obj W.>>> (L.quitWire GLFW.Key'Q) }

handleArgs :: [FilePath] -> Either String FilePath
handleArgs [] = Left "Usage: lobjview OBJFILE"
handleArgs (x : []) = Right x
handleArgs (_ : xs) = Left $ "Unrecognized arguments: " ++ (intercalate " " xs)

main :: IO ()
main = do
  objfile <- pure handleArgs <*> getArgs
  case objfile of
    Right file -> L.runWindow 640 480 "OBJ Viewer" () (loadGame file)
    Left err -> putStrLn err

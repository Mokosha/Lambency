module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative
import qualified Control.Wire as W

import Control.Monad.Writer

import Data.List (intercalate)

import qualified Graphics.UI.GLFW as GLFW

import FRP.Netwire.Input

import qualified Lambency as L
import Linear

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
cam = L.mkViewerCam initialCam zero

wireframeToggle :: L.GameWire a a
wireframeToggle = (keyDebounced GLFW.Key'W W.>>> toggleWireframe True) W.<|> W.mkId
  where
    toggleWireframe wireframe = W.mkGenN $ \x -> do
      tell [L.WireframeAction wireframe]
      return (Right x, toggleWireframe $ not wireframe)

mkOBJ :: FilePath -> IO (L.RenderObject)
mkOBJ objfile = do
  exists <- doesFileExist objfile
  if not exists then error ("OBJ file " ++ objfile ++ " not found") else return ()
  objInfo <- L.getOBJInfo objfile
  let material = L.diffuseColoredMaterial $ V3 0.26 0.5 0.26
  case objInfo of
    (L.OBJInfo _ 0 0 _) -> do
      mesh <- L.genTexCoords . L.genNormalsV3 <$> L.loadV3 objfile
      L.createRenderObject mesh material
    (L.OBJInfo _ _ 0 _) -> do
      mesh <- L.genNormalsTV3 <$> L.loadTV3 objfile
      L.createRenderObject mesh material
    (L.OBJInfo _ 0 _ _) -> do
      mesh <- L.genTexCoords <$> L.loadOV3 objfile
      L.createRenderObject mesh material
    _ -> do
      mesh <- L.loadOTV3 objfile
      L.createRenderObject mesh material

controlWire :: L.RenderObject -> L.GameWire a a
controlWire ro = L.mkObject ro (pure L.identity) W.>>> wireframeToggle

loadGame :: FilePath -> IO (L.Game ())
loadGame objfile = do
  obj <- mkOBJ objfile
  let lightPos = 10 *^ (V3 0 1 (-1))
      lightParams = L.mkLightParams (V3 0.5 0.5 0.5) (V3 1.0 1.0 1.0) 1.0
      light = L.spotlight lightParams lightPos (negate lightPos) (pi/2)
  return $ L.Game { L.staticLights = [light],
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
  objfile <- handleArgs <$> getArgs
  case objfile of
    Right file -> L.runWindow 640 480 "OBJ Viewer" () (loadGame file)
    Left err -> putStrLn err

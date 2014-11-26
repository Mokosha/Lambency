module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative
import qualified Control.Wire as W

import Control.Monad.Writer

import Data.Traversable (sequenceA)
import Data.List (intercalate)

import qualified Graphics.UI.GLFW as GLFW

import FRP.Netwire.Input

import qualified Lambency as L
import Linear hiding (trace)

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

controlWire :: [L.RenderObject] -> L.GameWire a [a]
controlWire ros = sequenceA $ (\ro -> L.mkObject ro (pure L.identity) W.>>> wireframeToggle) <$> ros

loadGame :: FilePath -> IO (L.Game ())
loadGame objfile = do
  obj <- L.loadOBJWithDefaultMaterial objfile $ Just (L.shinyColoredMaterial $ V3 0.26 0.5 0.26)
  putStrLn $ "OBJ contained " ++ (show $ length obj) ++ " meshes."
  let lightParams = L.mkLightParams (V3 0.5 0.5 0.5) (V3 1.0 1.0 1.0) 1.0
      light = L.dirlight lightParams (V3 0 (-1) 1)
  return $ L.Game { L.staticLights = [light],
                    L.staticGeometry = [],
                    L.mainCamera = cam,
                    L.dynamicLights = [],
                    L.gameLogic = controlWire obj W.>>> (L.quitWire GLFW.Key'Q) W.>>> pure () }

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

module Main (main) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Prelude hiding ((.))
import Control.Wire ((.))
import qualified Control.Wire as W

import Control.Monad.Writer

#if __GLASGOW_HASKELL__ <= 708
import Data.Traversable (sequenceA)
#endif
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
             0.1 10000.0

cam :: L.GameWire () L.Camera
cam = startCam (makeViewer initialCam) makeViewer makeFree
  where
    makeViewer c = L.mkViewerCam c zero
    makeFree c = L.mkFreeCam c
    startCam :: L.GameWire a L.Camera
                -> (L.Camera -> L.GameWire a L.Camera)
                -> (L.Camera -> L.GameWire a L.Camera)
                -> (L.GameWire a L.Camera)
    startCam camWire mkThisCam mkThatCam = W.mkGen $ \dt _ -> do
      (Right nextCam, nextCamWire) <- W.stepWire camWire dt (Right undefined)
      toggle <- keyIsPressed GLFW.Key'F
      if toggle then
        do
          setCursorMode CursorMode'Enabled
          releaseKey GLFW.Key'F
          return (Right nextCam, startCam (mkThatCam nextCam) mkThatCam mkThisCam)
        else return (Right nextCam, startCam nextCamWire mkThisCam mkThatCam)

wireframeToggle :: L.GameWire a a
wireframeToggle = (keyDebounced GLFW.Key'V W.>>> toggleWireframe True) W.<|> W.mkId
  where
    toggleWireframe :: Bool -> L.GameWire a a
    toggleWireframe wireframe = W.mkGenN $ \x -> do
      tell [L.WireframeAction wireframe]
      return (Right x, toggleWireframe $ not wireframe)

controlWire :: [L.RenderObject] -> L.GameWire a [a]
controlWire ros = sequenceA $ (\ro -> L.mkObject ro (pure L.identity) W.>>> wireframeToggle) <$> ros

camLight :: L.GameWire L.Camera L.Light
camLight = W.mkSF_ $ \c ->
  let dir = L.getCamDir c
      up = L.getCamUp c
      right = dir `cross` up

      lightParams = L.mkLightParams (V3 0.5 0.5 0.5) (V3 1.0 1.0 1.0) 1.0
      lightDir = negate $ signorm $ right ^+^ up

  in L.dirlight lightParams lightDir

loadGame :: FilePath -> IO (L.Game ())
loadGame objfile = do
  obj <- L.loadOBJWithDefaultMaterial objfile $
         Just (L.shinyColoredMaterial $ V3 0.26 0.5 0.26)
  putStrLn $ "OBJ contained " ++ (show $ length obj) ++ " meshes."
  return $ L.Game { L.mainCamera = cam,
                    L.dynamicLights = [cam W.>>> camLight],
                    L.gameLogic = pure () . L.quitWire GLFW.Key'Q . controlWire obj }

handleArgs :: [FilePath] -> Either String FilePath
handleArgs [] = Left "Usage: lobjview OBJFILE"
handleArgs (x : []) = Right x
handleArgs (_ : xs) = Left $ "Unrecognized arguments: " ++ (intercalate " " xs)

main :: IO ()
main = do
  objfile <- handleArgs <$> getArgs
  case objfile of
    Right file -> L.withWindow 640 480 "OBJ Viewer" $
                  L.loadAndRun () (loadGame file)
    Left err -> putStrLn err

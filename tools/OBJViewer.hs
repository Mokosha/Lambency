module Main (main) where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Prelude hiding ((.), id)
import Control.Wire hiding (right)

#if __GLASGOW_HASKELL__ <= 708
import Data.Traversable (sequenceA)
#endif
import Data.List (intercalate)
import Data.Foldable (traverse_)

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

cam :: L.ContWire () L.Camera
cam = startCam (makeViewer initialCam) makeViewer makeFree
  where
    makeViewer c = L.mkViewerCam c zero
    makeFree c = L.mkFreeCam c
    startCam :: L.ContWire a L.Camera
             -> (L.Camera -> L.ContWire a L.Camera)
             -> (L.Camera -> L.ContWire a L.Camera)
             -> (L.ContWire a L.Camera)
    startCam camWire mkThisCam mkThatCam = L.mkContWire $ \dt _ -> do
      (nextCam, nextCamWire) <- L.stepContWire camWire dt undefined
      toggle <- keyIsPressed GLFW.Key'F
      if toggle then
        do
          setCursorMode CursorMode'Enabled
          releaseKey GLFW.Key'F
          return (nextCam, startCam (mkThatCam nextCam) mkThatCam mkThisCam)
        else return (nextCam, startCam nextCamWire mkThisCam mkThatCam)

wireframeToggle :: L.GameWire a a
wireframeToggle = (keyDebounced GLFW.Key'W >>> toggle True) <|> mkId
  where
    toggle :: Bool -> L.GameWire a a
    toggle wireframe = mkGenN $ \x -> do
      L.toggleWireframe wireframe
      return (Right x, toggle $ not wireframe)

controlWire :: [L.RenderObject] -> L.GameWire a [a]
controlWire ros = sequenceA $ (\ro -> L.mkObject ro (pure L.identity) >>> wireframeToggle) <$> ros

camLight :: L.ContWire L.Camera L.Light
camLight = arr $ \c ->
  let dir = L.getCamDir c
      up = L.getCamUp c
      right = dir `cross` up

      lightParams = L.mkLightParams (V3 0.5 0.5 0.5) (V3 1.0 1.0 1.0) 1.0
      lightDir = negate $ signorm $ right ^+^ up

  in L.dirlight lightParams lightDir

loadObj :: L.Renderer -> FilePath -> IO [L.RenderObject]
loadObj r objfile = do
  obj <- L.loadOBJWithDefaultMaterial r objfile $
         Just (L.shinyColoredMaterial $ V3 0.26 0.5 0.26)
  putStrLn $ "OBJ contained " ++ (show $ length obj) ++ " meshes."
  return obj

viewerWire :: FilePath -> L.ContWire (a, Bool) (Maybe ())
viewerWire file =
  L.bracketResource (flip loadObj file) (traverse_ L.unloadRenderObject)
    $ L.withResource
    $ \ros -> pure () . controlWire ros

viewer :: FilePath -> L.Game ()
viewer file = L.Game cam [cam >>> camLight]
            $ viewerWire file
            . (id &&&
               ((pure True . L.quitWire GLFW.Key'Q) `L.withDefault` pure False))

handleArgs :: [FilePath] -> Either String FilePath
handleArgs [] = Left "Usage: lobjview OBJFILE"
handleArgs (x : []) = Right x
handleArgs (_ : xs) = Left $ "Unrecognized arguments: " ++ (intercalate " " xs)

main :: IO ()
main = do
  objfile <- handleArgs <$> getArgs
  case objfile of
    Right file -> L.runOpenGL 640 480 "OBJ Viewer" () (viewer file)
    Left err -> putStrLn err

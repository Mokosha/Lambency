module Lambency.Render (
  renderROs,  
) where

--------------------------------------------------------------------------------

import Lambency.Camera
import Lambency.Material
import Lambency.Renderable
import Lambency.Shader
import Lambency.Texture
import Lambency.Transform
import Lambency.Types

import Data.List (partition, sortBy)
import qualified Data.Map as Map

import qualified Graphics.Rendering.OpenGL as GL

import Linear.Matrix
import Linear.V4

--------------------------------------------------------------------------------

place :: Transform -> Camera -> RenderObject -> RenderObject
place xf cam ro = let
  model :: M44 Float
  model = xform2Matrix xf

  sm :: ShaderMap
  sm = Map.fromList [
    ("mvpMatrix", Matrix4Val $ model !*! (getViewProjMatrix cam)),
    ("m2wMatrix", Matrix4Val $ model)]
  in
   ro { material = Map.union sm (material ro) }

renderLight :: [RenderObject] -> Light -> IO ()
renderLight ros (Light shdr shdrmap msm) = do
  case msm of
    Nothing -> return ()
    Just (Shadow shadowShdr shadowMap) -> do
      bindRenderTexture shadowMap
      clearBuffers
      beforeRender shadowShdr
      -- Right now the MVP matrix of each object is for the main camera, so
      -- we need to replace it with the combination from the model matrix
      -- and the shadow VP...
      mapM_
        (\ro -> do
            let
              mat :: ShaderValue -> Mat4f
              mat (Matrix4Val m) = m
              mat _ = eye4
              lightMVP = (mat $ material ro Map.! "m2wMatrix") !*!
                         (mat $ shdrmap Map.! "shadowVP")
              newmap = Map.insert "mvpMatrix" (Matrix4Val lightMVP) shdrmap
            (render ro) shadowShdr (Map.union newmap (material ro))) ros
      afterRender shadowShdr
      clearRenderTexture
  beforeRender shdr
  mapM_ (\ro -> (render ro) shdr (Map.union (material ro) shdrmap)) ros
  afterRender shdr

renderLights :: [RenderObject] -> [Light] -> IO ()
renderLights [] _ = return ()
renderLights ros lights = mapM_ (renderLight ros) lights

renderROs :: [(Transform, RenderObject)] -> Camera -> [Light] -> IO ()
renderROs ros cam lights = let
  camDist :: RenderObject -> Float
  camDist ro = let (Matrix4Val (V4 _ _ (V4 _ _ _ z) _)) = getMaterialVar (material ro) "mvpMatrix" in z

  (trans, opaque) = partition (\ro -> Transparent `elem` (flags ro)) $
                    sortBy (\ro1 ro2 -> compare (camDist ro1) (camDist ro2)) $
                    map (\(xf, ro) -> place xf cam ro) ros
  in do
    -- !FIXME! This should be moved to the camera...
    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
    clearBuffers

    GL.depthFunc GL.$= Just GL.Lequal
    renderLights opaque lights
    GL.depthFunc GL.$= Nothing
    renderLights (reverse trans) lights

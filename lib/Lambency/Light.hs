module Lambency.Light (
  createSpotlight,
  createNoLight,
  setAmbient,
  renderLight
) where

--------------------------------------------------------------------------------

import Lambency.Camera
import Lambency.Renderable
import Lambency.Shader
import Lambency.Texture
import Lambency.Types

import Linear.Matrix
import Linear.Metric
import Linear.V3

import qualified Data.Map as Map
--------------------------------------------------------------------------------

createSpotlight :: Vec3f -> Vec3f -> Float -> IO (Light)
createSpotlight pos dirvec ang = do
  shdr <- createSpotlightShader
  depthTex <- createDepthTexture
  minShdr <- createMinimalShader
  -- !FIXME! The camera fovy should depend on the cosoffset
  let dir = signorm dirvec
      lightCam = mkPerspCamera pos dir (V3 0 1 0) (pi / 4) 1 0.1 500.0
      shdrMap = Map.fromList [
        ("shadowVP", Matrix4Val $ getViewProjMatrix lightCam),
        ("shadowMap", TextureVal depthTex),
        ("lightDir", Vector3Val dir),
        ("lightPos", Vector3Val pos),
        ("lightCosCutoff", FloatVal ang),
        ("ambient", Vector3Val $ V3 0.15 0.15 0.15)]
  return $ Light shdr shdrMap (Just $ Shadow minShdr depthTex)

setAmbient :: Vec3f -> Light -> Light
setAmbient color (Light shdr shdrMap shadow) =
  Light shdr (Map.insert "ambient" (Vector3Val color) shdrMap) shadow

createNoLight :: IO (Light)
createNoLight = do
  shdr <- createTransparentShader
  return $ Light shdr (Map.singleton "alpha" $ FloatVal 0.5) Nothing

renderLight :: Light -> [RenderObject] -> IO ()
renderLight (Light shdr shdrmap msm) ros = do
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
  mapM_ (\ro -> (render ro) shdr (Map.union shdrmap (material ro))) ros
  afterRender shdr

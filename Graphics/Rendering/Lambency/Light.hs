module Graphics.Rendering.Lambency.Light (
  createSpotlight,
  renderLight
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Camera
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture
import Graphics.Rendering.Lambency.Types

import Data.Vect.Float
import qualified Data.Map as Map
--------------------------------------------------------------------------------

createSpotlight :: Vec3 -> Normal3 -> Float -> IO (Light)
createSpotlight pos dir ang = do
  shdr <- createSpotlightShader
  depthTex <- createDepthTexture
  minShdr <- createMinimalShader
  -- !FIXME! The camera fovy should depend on the cosoffset
  let lightCam = mkPerspCamera pos dir (mkNormal vec3Y) (pi / 4) 1 0.1 500.0
      shdrMap = Map.fromList [
        ("shadowVP", Matrix4Val $ getViewProjMatrix lightCam),
        ("shadowMap", TextureVal depthTex),
        ("lightDir", Vector3Val $ fromNormal dir),
        ("lightPos", Vector3Val pos),
        ("lightCosCutoff", FloatVal ang),
        ("ambient", Vector3Val $ Vec3 0.15 0.15 0.15)]
  return $ Light shdr shdrMap (Just $ Shadow minShdr depthTex)

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
              mat :: ShaderValue -> Mat4
              mat (Matrix4Val m) = m
              mat _ = one
              lightMVP = (mat $ material ro Map.! "m2wMatrix") .*.
                         (mat $ shdrmap Map.! "shadowVP")
              newmap = Map.insert "mvpMatrix" (Matrix4Val lightMVP) shdrmap
            (render ro) shadowShdr (Map.union newmap (material ro))) ros
      afterRender shadowShdr
      clearRenderTexture
  beforeRender shdr
  mapM_ (\ro -> (render ro) shdr (Map.union shdrmap (material ro))) ros
  afterRender shdr

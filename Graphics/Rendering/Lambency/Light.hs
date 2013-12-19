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
  let lightCam = mkPerspCamera pos dir (mkNormal vec3Z) (pi / 4) 1 0.1 500.0
      shdrMap = Map.fromList [
        ("shadowVP", Matrix4Val $ getViewProjMatrix lightCam),
        ("shadowMap", (TextureVal depthTex)),
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
      renderShdr shadowShdr
      clearRenderTexture
  renderShdr shdr
  
  where
    renderShdr :: Shader -> IO ()
    renderShdr s = do
      beforeRender s
      mapM_ (renderObj s shdrmap) ros
      afterRender s

    renderObj :: Shader -> ShaderMap -> RenderObject -> IO ()
    renderObj s sm ro = do
      (render ro) s (Map.union sm (material ro))


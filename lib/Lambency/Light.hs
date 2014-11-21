module Lambency.Light (
  spotlight,
  createNoLight,
  createFontLight,
  setAmbient,
) where

--------------------------------------------------------------------------------

import Lambency.Shader
import Lambency.Texture
import Lambency.Types

import qualified Data.Map as Map

import Linear.Matrix
import Linear.Metric
import Linear.V3

--------------------------------------------------------------------------------

mkLightParams :: Vec3f -> Vec3f -> Float -> LightParams
mkLightParams = LightParams

spotlight :: LightParams -> Vec3f -> Vec3f -> Float -> Light
spotlight params pos dir ang =
  Light {
    lightParams = params,
    lightType = SpotLight dir pos ang,
    lightShadowMap = Nothing
  }

addShadowMap :: Light -> IO (Light)
addShadowMap l = do
  depthTex <- createDepthTexture
  minShdr <- createMinimalShader
  return $ l { lightShadowMap = (Just $ ShadowMap minShdr depthTex) }
{--
setAmbient :: Vec3f -> Light -> Light
setAmbient color (Light params lightTy shadow) =
  Light ( (Map.insert "ambient" (Vector3Val color) shdrMap) shadow

createNoLight :: IO (Light)
createNoLight = let
  shdrMap = Map.fromList [
    ("alpha", FloatVal 1.0),
    ("texCoordMatrix", Matrix3Val eye3)]
  in do
    shdr <- createTransparentShader
    return $ Light shdr shdrMap Nothing

createFontLight :: IO (Light)
createFontLight = let
  shdrMap = Map.fromList [
    ("color", Vector3Val (V3 1 1 1)),
    ("alpha", FloatVal 1.0),
    ("texCoordMatrix", Matrix3Val eye3)]
  in do
    shdr <- createFontShader
    return $ Light shdr shdrMap Nothing  
--}

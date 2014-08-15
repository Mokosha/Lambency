module Lambency.Light (
  createSpotlight,
  createNoLight,
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

createSpotlight :: Vec3f -> Vec3f -> Float -> IO (Light)
createSpotlight pos dirvec ang = do
  shdr <- createSpotlightShader
  depthTex <- createDepthTexture
  minShdr <- createMinimalShader
  -- !FIXME! The camera fovy should depend on the cosoffset
  let dir = signorm dirvec
      shdrMap = Map.fromList [
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
createNoLight = let
  shdrMap = Map.fromList [
    ("alpha", FloatVal 1.0),
    ("texCoordMatrix", Matrix2Val eye2)]
  in do
    shdr <- createTransparentShader
    return $ Light shdr shdrMap Nothing

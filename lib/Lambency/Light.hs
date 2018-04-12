{-# LANGUAGE RecordWildCards #-}
module Lambency.Light (
  getLightVarName,
  getLightShaderVars,

  mkLightParams,
  
  spotlight,
  dirlight,
  pointlight,

  addShadowMap,
  removeShadowMap,

  getLightPosition,
  setLightPosition,

  getLightDirection,
  setLightDirection,
  
  setLightAmbient,
  setLightColor,
  setLightIntensity,
) where

--------------------------------------------------------------------------------
import Lambency.Texture
import Lambency.Shader
import Lambency.Types

import qualified Data.Map as Map

import Linear.Metric
import Linear.V3
--------------------------------------------------------------------------------

mkLightVar :: String -> ShaderValue -> LightVar a
mkLightVar n v = LightVar (n, v)

mkLightVar3f :: String -> (V3 Float) -> LightVar (V3 Float)
mkLightVar3f n v = mkLightVar n (Vector3Val v)

mkLightVarf :: String -> Float -> LightVar Float
mkLightVarf n f = mkLightVar n (FloatVal f)

mkLightParams :: Vec3f -> Vec3f -> Float -> LightParams
mkLightParams a c i =
  LightParams
  (mkLightVar3f "lightAmbient" a)
  (mkLightVar3f "lightColor" c)
  (mkLightVarf "lightIntensity" i)

getLightShaderVars :: Light -> UniformMap
getLightShaderVars (Light params ty _) =
  let mkShdrVarPair :: LightVar a -> (String, ShaderValue)
      mkShdrVarPair (LightVar x) = x

      getTypeVars (SpotLight x y z) =
        [mkShdrVarPair x, mkShdrVarPair y, mkShdrVarPair z]
      getTypeVars (DirectionalLight dir) = [mkShdrVarPair dir]
      getTypeVars (PointLight pos) = [mkShdrVarPair pos]

      getParamVars (LightParams a c i) =
        [mkShdrVarPair a, mkShdrVarPair c, mkShdrVarPair i]
  in
   Map.fromList $ getTypeVars ty ++ getParamVars params

spotlight :: LightParams -> Vec3f -> Vec3f -> Float -> Light
spotlight params pos dir ang =
  Light {
    lightParams = params,
    lightType =
      SpotLight
      (mkLightVar3f "spotlightDir" $ signorm dir)
      (mkLightVar3f "spotlightPos" pos)
      (mkLightVarf "spotlightCosCutoff" $ cos ang),
    lightShadowMap = Nothing
  }

dirlight :: LightParams -> Vec3f -> Light
dirlight params dir =
  Light {
    lightParams = params,
    lightType = DirectionalLight (mkLightVar3f "dirlightDir" $ signorm dir),
    lightShadowMap = Nothing
  }

pointlight :: LightParams -> Vec3f -> Light
pointlight params pos =
  Light {
    lightParams = params,
    lightType = PointLight (mkLightVar3f "pointlightPos" pos),
    lightShadowMap = Nothing
  }

addShadowMap :: Light -> IO (Light)
addShadowMap l = do
  depthTex <- createDepthTexture
  return $ l { lightShadowMap = (Just (ShadowMap depthTex, ShadowTechnique'Simple)) }

removeShadowMap :: Light -> IO ()
removeShadowMap l = case lightShadowMap l of
  Nothing -> return ()
  Just (ShadowMap t, _) -> destroyTexture t

getLightPosition :: Light -> Maybe (V3 Float)
getLightPosition (Light _ (SpotLight {..}) _) =
  let LightVar (_, Vector3Val v) = spotLightPos
  in Just v
getLightPosition (Light _ (PointLight {..}) _) =
  let LightVar (_, Vector3Val v) = pointLightPos
  in Just v
getLightPosition _ = Nothing

setLightPosition :: Vec3f -> Light -> Light
setLightPosition pos (Light params (SpotLight {..}) shdw) =
  let LightVar (name, _) = spotLightPos
      newPos = LightVar (name, Vector3Val pos)
  in Light params (SpotLight spotLightDir newPos spotLightCosCutoff) shdw
setLightPosition pos (Light params (PointLight {..}) shdw) =
  let LightVar (name, _) = pointLightPos
      newPos = LightVar (name, Vector3Val pos)
  in Light params (PointLight newPos) shdw
setLightPosition _ light = light -- Silently do nothing for lights that have no position...

getLightDirection :: Light -> Maybe (V3 Float)
getLightDirection (Light _ (SpotLight {..}) _) =
  let LightVar (_, Vector3Val v) = spotLightDir in Just v
getLightDirection (Light _ (DirectionalLight {..}) _) =
  let LightVar (_, Vector3Val v) = dirLightDir in Just v
getLightDirection _ = Nothing

setLightDirection :: Vec3f -> Light -> Light
setLightDirection dir (Light params (SpotLight {..}) shdw) =
  let LightVar (name, _) = spotLightDir
      newDir = LightVar (name, Vector3Val $ signorm dir)
  in Light params (SpotLight newDir spotLightPos spotLightCosCutoff) shdw
setLightDirection dir (Light params (DirectionalLight {..}) shdw) =
  let LightVar (name, _) = dirLightDir
      newDir = LightVar (name, Vector3Val $ signorm dir)
  in Light params (DirectionalLight newDir) shdw
setLightDirection _ light = light -- Silently do nothing for lights that have no direction...

setLightAmbient :: Vec3f -> Light -> Light
setLightAmbient color (Light params lightTy shadow) =
  let newColor = (mkLightVar3f "lightAmbient" color)
  in Light (params { ambientColor = newColor}) lightTy shadow

setLightColor :: Vec3f -> Light -> Light
setLightColor color (Light params lightTy shadow) =
  let newColor = (mkLightVar3f "lightColor" color)
  in Light (params { lightColor = newColor}) lightTy shadow

setLightIntensity :: Float -> Light -> Light
setLightIntensity intensity (Light params lightTy shadow) =
  let newi = (mkLightVarf "lightIntensity" intensity)
  in Light (params { lightIntensity = newi}) lightTy shadow

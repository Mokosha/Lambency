{-# LANGUAGE RecordWildCards #-}
module Lambency.Material (
  getMatVarName,

  defaultBlinnPhong,
  defaultMaskedSprite,

  materialShaderVars,

  createSimpleMaterial,
  shinyTexturedMaterial,
  diffuseTexturedMaterial,
  shinyColoredMaterial,
  diffuseColoredMaterial,
  
  updateMaterialVar3mf,
  updateMaterialVar3vf,
  updateMaterialVar4vf,
  updateMaterialVarf,
  updateMaterialVarTex,

  maskedSpriteMaterial,
  texturedSpriteMaterial,

  isUnlit,
  isDefined,
  usesTextures
) where

--------------------------------------------------------------------------------
import Lambency.Types

import qualified Data.Map as Map

import Linear
--------------------------------------------------------------------------------

getMatVarName :: MaterialVar a -> String
getMatVarName (MaterialVar (n, _)) = n

defaultBlinnPhong :: Material
defaultBlinnPhong = BlinnPhongMaterial {
  diffuseReflectivity = MaterialVar ("diffuseColor", Just $ Vector3Val $ V3 1 1 1),
  diffuseMap = MaterialVar ("diffuseMap", Nothing),
  specularExponent = MaterialVar ("specularExponent", Just $ FloatVal 10.0),
  specularReflectivity = MaterialVar ("specularColor", Just $ Vector3Val $ V3 1 1 1),
  specularMap = MaterialVar ("specularMap", Nothing),
  ambientReflectivity = MaterialVar ("ambientColor", Just $ Vector3Val $ V3 1 1 1),
  reflectionInfo = Nothing,
  normalMod = Nothing
}

defaultMaskedSprite :: Material
defaultMaskedSprite = MaskedSpriteMaterial {
  spriteMaskColor = MaterialVar ("spriteMaskColor", Just $ Vector4Val $ V4 0 0 0 1),
  spriteMaskMatrix = MaterialVar ("spriteMaskMatrix", Nothing),
  spriteMask = MaterialVar ("spriteMask", Nothing)
  }

matVarToList :: MaterialVar a -> [(String, ShaderValue)]
matVarToList (MaterialVar (_, Nothing)) = []
matVarToList (MaterialVar (name, Just val)) = [(name, val)]

materialShaderVars :: Material -> ShaderMap
materialShaderVars (BlinnPhongMaterial{..}) =
  Map.fromList $ concat
  [ matVarToList diffuseReflectivity,
    matVarToList diffuseMap,
    matVarToList specularExponent,
    matVarToList specularReflectivity,
    matVarToList specularMap,
    matVarToList ambientReflectivity,
    case reflectionInfo of
      Just info -> concat $ [
        matVarToList $ indexOfRefraction info,
        matVarToList $ reflectionMap info,
        matVarToList $ sharpness info
        ]
      Nothing -> [],
    case normalMod of
      Just (BumpMap var) -> matVarToList var
      Just (NormalMap var) -> matVarToList var
      Nothing -> []
  ]

materialShaderVars (TexturedSpriteMaterial{..}) =
  Map.fromList $ concat
  [ matVarToList spriteTextureMatrix,
    matVarToList spriteTexture,
    matVarToList spriteAlpha
  ]
  
materialShaderVars (MaskedSpriteMaterial{..}) =
  Map.fromList $ concat
  [ matVarToList spriteMaskMatrix,
    matVarToList spriteMask,
    matVarToList spriteMaskColor
  ]

materialShaderVars MinimalMaterial = Map.empty
materialShaderVars _ = error "Lambency.Material (materialShaderVars): Not implemented!"

createSimpleMaterial :: Material
createSimpleMaterial = defaultBlinnPhong

shinyTexturedMaterial :: Texture -> Material
shinyTexturedMaterial tex =
  defaultBlinnPhong { diffuseMap = updateMaterialVarTex tex $ diffuseMap defaultBlinnPhong }

diffuseTexturedMaterial :: Texture -> Material
diffuseTexturedMaterial tex =
  defaultBlinnPhong { diffuseMap = updateMaterialVarTex tex $ diffuseMap defaultBlinnPhong,
                      specularExponent =
                        let MaterialVar (name, _) = specularExponent defaultBlinnPhong
                        in MaterialVar (name, Nothing)}

shinyColoredMaterial :: V3 Float -> Material
shinyColoredMaterial color =
  defaultBlinnPhong { diffuseReflectivity = updateMaterialVar3vf color $ diffuseReflectivity defaultBlinnPhong }

diffuseColoredMaterial :: V3 Float -> Material
diffuseColoredMaterial color =
  defaultBlinnPhong { diffuseReflectivity = updateMaterialVar3vf color $ diffuseReflectivity defaultBlinnPhong,
                      specularExponent =
                        let MaterialVar (name, _) = specularExponent defaultBlinnPhong
                        in MaterialVar (name, Nothing) }

maskedSpriteMaterial :: Texture -> Material
maskedSpriteMaterial tex =
  defaultMaskedSprite { spriteMaskMatrix = MaterialVar ("spriteMaskMatrix", Just $ Matrix3Val $ eye3),
                        spriteMask = MaterialVar ("spriteMask", Just $ TextureVal tex) }

texturedSpriteMaterial :: Texture -> Material
texturedSpriteMaterial tex =
  TexturedSpriteMaterial { spriteTextureMatrix = MaterialVar ("spriteMaskMatrix", Just $ Matrix3Val $ eye3),
                           spriteTexture = MaterialVar ("spriteMask", Just $ TextureVal tex),
                           spriteAlpha = MaterialVar ("spriteAlpha", Just $ FloatVal 1) }

updateMaterialVar3mf :: M33 Float -> MaterialVar (M33 Float) -> MaterialVar (M33 Float)
updateMaterialVar3mf x (MaterialVar (n, _)) = MaterialVar (n, Just $ Matrix3Val x)

updateMaterialVar3vf :: V3 Float -> MaterialVar (V3 Float) -> MaterialVar (V3 Float)
updateMaterialVar3vf x (MaterialVar (n, _)) = MaterialVar (n, Just $ Vector3Val x)

updateMaterialVar4vf :: V4 Float -> MaterialVar (V4 Float) -> MaterialVar (V4 Float)
updateMaterialVar4vf x (MaterialVar (n, _)) = MaterialVar (n, Just $ Vector4Val x)

updateMaterialVarf :: Float -> MaterialVar Float -> MaterialVar Float
updateMaterialVarf x (MaterialVar (n, _)) = MaterialVar (n, Just $ FloatVal x)

updateMaterialVarTex :: Texture -> MaterialVar Texture -> MaterialVar Texture
updateMaterialVarTex x (MaterialVar (n, _)) = MaterialVar (n, Just $ TextureVal x)

isDefined :: MaterialVar a -> Bool
isDefined (MaterialVar (_, Nothing)) = False
isDefined _ = True

isUnlit :: Material -> Bool
isUnlit (BlinnPhongMaterial {..}) = False
isUnlit _ = True

usesTextures :: Material -> Bool
usesTextures (BlinnPhongMaterial {..}) =
  isDefined diffuseMap ||
  isDefined specularMap ||
  (case normalMod of
      Just (BumpMap v) -> isDefined v
      Just (NormalMap v) -> isDefined v
      Nothing -> False)
usesTextures (TexturedSpriteMaterial {..}) = isDefined spriteTexture
usesTextures (MaskedSpriteMaterial {..}) = isDefined spriteMask
usesTextures _ = False

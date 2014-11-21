{-# LANGUAGE RecordWildCards #-}
module Lambency.Material (
  defaultBlinnPhong,

  materialShaderVars,
  
  createSimpleMaterial,
  createTexturedMaterial,
  
  updateMaterialVar3mf,
  updateMaterialVar3vf,
  updateMaterialVar4vf,
  updateMaterialVarf,

  maskedSpriteMaterial,
  texturedSpriteMaterial,

  isUnlit,
  isDefined,
  usesTextures
) where

--------------------------------------------------------------------------------
import Lambency.Types

import Linear
--------------------------------------------------------------------------------

defaultBlinnPhong :: Material
defaultBlinnPhong = BlinnPhongMaterial {
  diffuseReflectivity = MaterialVar ("diffuseColor", Just $ Vector3Val $ V3 1 1 1),
  diffuseMap = MaterialVar ("diffuseMap", Nothing),
  specularExponent = MaterialVar ("specularExponent", Nothing),
  specularReflectivity = MaterialVar ("specularColor", Just $ Vector3Val $ V3 1 1 1),
  specularMap = MaterialVar ("specularMap", Nothing),
  ambientReflectivity = MaterialVar ("ambientColor", Just $ Vector3Val $ V3 1 1 1),
  reflectionInfo = Nothing,
  normalMod = MaterialVar ("normalMap", Nothing)
}

materialShaderVars :: Material -> ShaderMap
materialShaderVars _ = error "Lambency.Material (materialShaderVars): Not implemented!"

createSimpleMaterial :: Material
createSimpleMaterial = defaultBlinnPhong

createTexturedMaterial :: Texture -> Material
createTexturedMaterial tex =
  defaultBlinnPhong { diffuseMap = MaterialVar ("diffuseMap", Just $ TextureVal tex) }

updateMaterialVar3mf :: M33 Float -> MaterialVar (M33 Float) -> MaterialVar (M33 Float)
updateMaterialVar3mf x (MaterialVar (n, _)) = MaterialVar (n, Just $ Matrix3Val x)

updateMaterialVar3vf :: V3 Float -> MaterialVar (V3 Float) -> MaterialVar (V3 Float)
updateMaterialVar3vf x (MaterialVar (n, _)) = MaterialVar (n, Just $ Vector3Val x)

updateMaterialVar4vf :: V4 Float -> MaterialVar (V4 Float) -> MaterialVar (V4 Float)
updateMaterialVar4vf x (MaterialVar (n, _)) = MaterialVar (n, Just $ Vector4Val x)

updateMaterialVarf :: Float -> MaterialVar Float -> MaterialVar Float
updateMaterialVarf x (MaterialVar (n, _)) = MaterialVar (n, Just $ FloatVal x)

maskedSpriteMaterial :: Texture -> Material
maskedSpriteMaterial tex =
  MaskedSpriteMaterial { spriteMaskColor = MaterialVar ("spriteMaskColor", Just $ Vector4Val $ V4 0 0 0 1),
                         spriteMaskMatrix = MaterialVar ("spriteMaskMatrix", Just $ Matrix3Val $ eye3),
                         spriteMask = MaterialVar ("spriteMask", Just $ TextureVal tex) }

texturedSpriteMaterial :: Texture -> Material
texturedSpriteMaterial tex =
  TexturedSpriteMaterial { spriteTextureMatrix = MaterialVar ("spriteMaskMatrix", Just $ Matrix3Val $ eye3),
                           spriteTexture = MaterialVar ("spriteMask", Just $ TextureVal tex),
                           spriteAlpha = MaterialVar ("spriteAlpha", Just $ FloatVal 1) }

isDefined :: MaterialVar a -> Bool
isDefined (MaterialVar (_, Nothing)) = False
isDefined _ = True

isUnlit :: Material -> Bool
isUnlit (BlinnPhongMaterial {..}) = False
isUnlit _ = True

usesTextures :: Material -> Bool
usesTextures (BlinnPhongMaterial {..}) =
  isDefined diffuseMap || isDefined specularMap || isDefined normalMod
usesTextures (TexturedSpriteMaterial {..}) = isDefined spriteTexture
usesTextures (MaskedSpriteMaterial {..}) = isDefined spriteMask
usesTextures _ = False

{-# LANGUAGE RecordWildCards #-}
module Lambency.Material (
  defaultBlinnPhong,

  createSimpleMaterial,
  createTexturedMaterial,
  createMaskedMaterial,

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

createSimpleMaterial :: Material
createSimpleMaterial = defaultBlinnPhong

createTexturedMaterial :: Texture -> Material
createTexturedMaterial tex =
  defaultBlinnPhong { diffuseMap = MaterialVar ("diffuseMap", Just $ TextureVal tex) }

createMaskedMaterial :: Texture -> Material
createMaskedMaterial tex =
  MaskedSpriteMaterial { spriteColor = MaterialVar ("spriteColor", Just $ Vector3Val $ V3 0 0 0),
                         spriteMaskMatrix = MaterialVar ("spriteMaskMatrix", Just $ Matrix3Val $ eye3),
                         spriteMask = MaterialVar ("spriteMask", Just $ TextureVal tex) }

isDefined :: MaterialVar a -> Bool
isDefined (MaterialVar (_, Nothing)) = False
isDefined _ = True

usesTextures :: Material -> Bool
usesTextures (BlinnPhongMaterial {..}) =
  isDefined diffuseMap || isDefined specularMap || isDefined normalMod
usesTextures (TexturedSpriteMaterial {..}) = isDefined spriteTexture
usesTextures (MaskedSpriteMaterial {..}) = isDefined spriteMask

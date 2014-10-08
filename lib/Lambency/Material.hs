module Lambency.Material (
  getShaderMap,
  createSimpleMaterial,
  createTexturedMaterial,
  createMaskedMaterial,
  setMaterialColor,
  switchTexture
) where

--------------------------------------------------------------------------------
import Lambency.Types
import Lambency.Texture

import qualified Data.Map as Map
import Linear.V3
--------------------------------------------------------------------------------

getShaderMap :: Material -> ShaderMap
getShaderMap = id

createSimpleMaterial :: IO(Material)
createSimpleMaterial =
  createSolidTexture (255, 0, 255, 255) >>= (return . createTexturedMaterial)

createTexturedMaterial :: Texture -> Material
createTexturedMaterial tex = Map.singleton "diffuseTex" (TextureVal tex)

createMaskedMaterial :: Texture -> Material
createMaskedMaterial tex = Map.singleton "maskTex" (TextureVal tex)

setMaterialColor :: V3 Float -> Material -> Material
setMaterialColor c = Map.insert "color" (Vector3Val c)

switchTexture :: Material -> String -> Texture -> Material
switchTexture shdrMap name tex = Map.adjust (\_ -> (TextureVal tex)) name shdrMap

module Lambency.Material (
  getShaderMap,
  createSimpleMaterial,
  createTexturedMaterial,
  switchTexture
) where

--------------------------------------------------------------------------------
import Lambency.Types
import Lambency.Texture

import qualified Data.Map as Map
--------------------------------------------------------------------------------

getShaderMap :: Material -> ShaderMap
getShaderMap = id

createSimpleMaterial :: IO(Material)
createSimpleMaterial =
  createSolidTexture (255, 0, 255, 255) >>= (return . createTexturedMaterial)

createTexturedMaterial :: Texture -> Material
createTexturedMaterial tex = Map.singleton "diffuseTex" (TextureVal tex)

switchTexture :: Material -> String -> Texture -> Material
switchTexture shdrMap name tex = Map.adjust (\_ -> (TextureVal tex)) name shdrMap

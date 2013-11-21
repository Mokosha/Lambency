module Graphics.Rendering.Lambency.Material (
  Material(..),
  getShader,
  getShaderMap,
  createSimpleMaterial,
  createTexturedMaterial,
  createSpotlightMaterial,
  getMaterialVar,
  switchTexture,
  beforeRender,
  afterRender
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture

import Data.Vect.Float

import qualified Data.Map as Map
--------------------------------------------------------------------------------

-- Material consists of a shader and the variables specified by the
-- engine for the shader.
data Material = Material Shader ShaderMap

getShader :: Material -> Shader
getShader (Material s _) = s

getShaderMap :: Material -> ShaderMap
getShaderMap (Material _ m) = m

getMaterialVar :: Material -> String -> ShaderVar
getMaterialVar m = (Map.!) $ (getShaderVars . getShader) m

createSimpleMaterial :: IO(Material)
createSimpleMaterial =
  createSolidTexture (255, 0, 255, 255) >>= createTexturedMaterial

createTexturedMaterial :: Texture -> IO(Material)
createTexturedMaterial tex = do
  shdr <- createSimpleShader
  let varMap = getShaderVars shdr
      shdrMap = Map.singleton (varMap Map.! "sampler") (TextureVal $ getHandle tex)
  return $ Material shdr shdrMap

createSpotlightMaterial :: Maybe Texture -> IO(Material)
createSpotlightMaterial mtex = do
  shdr <- createSpotlightShader
  t <- case mtex of
    Nothing -> createSolidTexture (255, 0, 255, 255)
    Just tex -> return tex
  let varMap = getShaderVars shdr
      shdrMap = Map.fromList [
        (varMap Map.! "diffuseTex", TextureVal $ getHandle t),
        (varMap Map.! "lightDir", Vector3Val $ Vec3 0.57735 (-0.57735) 0.57735),
        (varMap Map.! "ambient", Vector3Val $ Vec3 0.15 0.15 0.15)]
  return $ Material shdr shdrMap

switchTexture :: Material -> String -> Texture -> Material
switchTexture (Material shdr shdrMap) name tex =
  let shdrVar = (getShaderVars shdr) Map.! name
      shdrVal = TextureVal $ getHandle tex
  in
   Material shdr $ Map.adjust (\_ -> shdrVal) shdrVar shdrMap

beforeRender :: Material -> IO ()
beforeRender (Material shdr _) = do
  -- Enable the program
  GL.currentProgram GL.$= Just (getProgram shdr)

  -- Enable each vertex attribute that this material needs
  mapM_ enableAttribute $ (Map.elems . getShaderVars) shdr
  where enableAttribute :: ShaderVar -> IO ()
        enableAttribute v = case v of
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Enabled

afterRender :: Material -> IO ()
afterRender (Material shdr _) = do
  -- Disable each vertex attribute that this material needs
  mapM_ disableAttribute $ (Map.elems . getShaderVars) shdr
  where disableAttribute :: ShaderVar -> IO ()
        disableAttribute v = case v of
          Uniform TextureTy _ -> do
            GL.activeTexture GL.$= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D GL.$= Nothing
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Disabled

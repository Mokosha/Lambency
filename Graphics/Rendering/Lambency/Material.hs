module Graphics.Rendering.Lambency.Material (
  Material(..),
  getShader,
  getShaderMap,
  createSimpleMaterial,
  createTexturedMaterial,
  switchTexture,
  beforeRender,
  afterRender
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture

import qualified Data.Map as Map
--------------------------------------------------------------------------------

-- Material consists of a shader and the variables specified by the
-- engine for the shader.
data Material = Material Shader ShaderMap

getShader :: Material -> Shader
getShader (Material s _) = s

getShaderMap :: Material -> ShaderMap
getShaderMap (Material _ m) = m

createSimpleMaterial :: IO(Material)
createSimpleMaterial =
  createSolidTexture (255, 0, 255, 255) >>= createTexturedMaterial

createTexturedMaterial :: Texture -> IO(Material)
createTexturedMaterial tex = do
  shdr <- createSimpleShader
  let shdrMap = Map.singleton (Uniform TextureTy "sampler") (TextureVal $ getHandle tex)
  return $ Material shdr shdrMap

switchTexture :: Material -> String -> Texture -> Material
switchTexture (Material shdr shdrMap) name tex =
  let shdrVar = Uniform TextureTy name
      shdrVal = TextureVal $ getHandle tex
  in
   Material shdr $ Map.adjust (\_ -> shdrVal) shdrVar shdrMap

beforeRender :: Material -> IO ()
beforeRender (Material shdr _) = do
  -- Enable the program
  GL.currentProgram GL.$= Just (getProgram shdr)

  -- Enable each vertex attribute that this material needs
  mapM_ enableAttribute (getShaderVars shdr)
  where enableAttribute :: ShaderVar -> IO ()
        enableAttribute v = case v of
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Enabled

afterRender :: Material -> IO ()
afterRender (Material shdr _) = do
  -- Disable each vertex attribute that this material needs
  mapM_ disableAttribute (getShaderVars shdr)
  where disableAttribute :: ShaderVar -> IO ()
        disableAttribute v = case v of
          Uniform TextureTy _ -> do
            GL.activeTexture GL.$= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D GL.$= Nothing
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Disabled

module Graphics.Rendering.Lambency.Material (
  Material(..),
  getShader,
  createSimpleMaterial,
  beforeRender,
  afterRender
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Shader
import qualified Data.Map as Map
--------------------------------------------------------------------------------

-- Material consists of a shader and the variables specified by the
-- engine for the shader.
data Material = Material Shader ShaderMap

getShader :: Material -> Shader
getShader (Material s _) = s

createSimpleMaterial :: IO(Material)
createSimpleMaterial = do
  shdr <- createSimpleShader
  return $ Material
    shdr
    Map.empty

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
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Disabled

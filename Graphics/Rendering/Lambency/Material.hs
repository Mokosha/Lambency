module Graphics.Rendering.Lambency.Material (
  Material(..),
  MaterialRenderInput(..),
  shaderVars,
  createSimpleMaterial,
  beforeRender,
  afterRender
) where

--------------------------------------------------------------------------------
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture

import Paths_lambency

import Data.Maybe
--------------------------------------------------------------------------------

data MaterialRenderInput = MaterialRenderInput UniformVar TextureFormat Material
data Material = Material Shader [ShaderVar] [MaterialRenderInput]

shaderVars :: Material -> [ShaderVar]
shaderVars (Material _ vars _) = vars

createSimpleMaterial :: IO(Material)
createSimpleMaterial = do
  defaultVertexShader <- getDataFileName "simple.vs"
  defaultFragmentShader <- getDataFileName "simple.fs"
  prg <- loadShader (Just defaultVertexShader) (Just defaultFragmentShader) Nothing
  return $ Material
    (fromJust prg)
    [Uniform $ UniformVar Matrix4Ty "mvpMatrix",
     Attribute $ AttribVar FloatListTy (GL.AttribLocation 0)]
    []

beforeRender :: Material -> IO ()
beforeRender (Material shdr vars _) = do
  -- Enable the program
  GL.currentProgram GL.$= Just shdr

  -- Enable each vertex attribute that this material needs
  mapM_ enableAttribute vars
  where enableAttribute :: ShaderVar -> IO ()
        enableAttribute v = case v of
          Uniform _ -> return ()
          Attribute (AttribVar _ loc) -> GL.vertexAttribArray loc GL.$= GL.Enabled

afterRender :: Material -> IO ()
afterRender mat = do
  -- Disable each vertex attribute that this material needs
  mapM_ disableAttribute (shaderVars mat)
  where disableAttribute :: ShaderVar -> IO ()
        disableAttribute v = case v of
          Uniform _ -> return ()
          Attribute (AttribVar _ loc) -> GL.vertexAttribArray loc GL.$= GL.Disabled

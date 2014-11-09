module Lambency.Shader (
  getProgram,
  getShaderVars,
  isUniform,
  getUniforms,
  setUniformVar,
  createMinimalShader,
  createSimpleShader,
  createTransparentShader,
  createFontShader,
  createSpotlightShader,
  destroyShader,
  beforeRender, afterRender
) where

--------------------------------------------------------------------------------

import Lambency.Vertex
import Lambency.Texture
import Lambency.Types

import qualified Lambency.Shader.Var as I
import qualified Lambency.Shader.Expr as I
import qualified Lambency.Shader.Program as I
import qualified Lambency.Shader.OpenGL as I

import Paths_lambency

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Control.Monad as M

import Data.Maybe (catMaybes)
import Linear.Matrix
import Linear.V3

import System.FilePath
import Foreign.Marshal.Utils
import Foreign.Ptr
--------------------------------------------------------------------------------

type ShaderVarMap = Map.Map String ShaderVar

getProgram :: Shader -> GL.Program
getProgram (Shader prg _) = prg

getShaderVars :: Shader -> ShaderVarMap
getShaderVars (Shader _ vars) = vars

isUniform :: ShaderVar -> Bool
isUniform (Uniform _ _) = True
isUniform _ = False

getUniforms :: Shader -> ShaderVarMap
getUniforms = (Map.filter isUniform) . getShaderVars

setUniformVar :: ShaderVar -> ShaderValue -> IO ()
setUniformVar (Uniform Matrix4Ty (GL.UniformLocation loc)) (Matrix4Val mat) = do
  with mat $ \ptr ->
    GLRaw.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 Float)))

setUniformVar (Uniform Matrix3Ty (GL.UniformLocation loc)) (Matrix3Val mat) = do
  with mat $ \ptr ->
    GLRaw.glUniformMatrix3fv loc 1 0 (castPtr (ptr :: Ptr (M33 Float)))

setUniformVar (Uniform Matrix2Ty (GL.UniformLocation loc)) (Matrix2Val mat) = do
  with mat $ \ptr ->
    GLRaw.glUniformMatrix2fv loc 1 0 (castPtr (ptr :: Ptr (M22 Float)))

setUniformVar (Uniform (TextureTy unit) loc) (TextureVal tex) = do
  GL.activeTexture GL.$= (GL.TextureUnit unit)
  GL.textureBinding GL.Texture2D GL.$= Just (getGLTexObj tex)
  GL.uniform loc GL.$= (GL.TextureUnit unit)

setUniformVar (Uniform FloatTy loc) (FloatVal f) = do
  GL.uniform loc GL.$= GL.Index1 ((realToFrac f) :: GL.GLfloat)

setUniformVar (Uniform Vector3Ty loc) (Vector3Val (V3 x y z)) = do
  GL.uniform loc GL.$= GL.Vertex3 (f x) (f y) (f z)
  where
    f :: Float -> GL.GLfloat
    f = realToFrac

setUniformVar (Attribute _ _) _ = return ()
setUniformVar (Uniform ty _) _ = ioError $ userError $ "Uniform not supported: " ++ (show ty)

getShaderExt :: GL.ShaderType -> String
getShaderExt GL.FragmentShader = ".frag"
getShaderExt GL.VertexShader = ".vert"
getShaderExt _ = ".glsl"

getShaderForExt :: String -> Maybe GL.ShaderType
getShaderForExt ".frag" = Just GL.FragmentShader
getShaderForExt ".vert" = Just GL.VertexShader
getShaderForExt _ = Nothing

getShaderPath :: String -> GL.ShaderType -> IO(FilePath)
getShaderPath name ty = getDataFileName $ "shaders" </> name <.> (getShaderExt ty)

loadProgram :: [FilePath] -> IO(Maybe GL.Program)
loadProgram paths = do
  sids <- mapM compileShader paths
  case (catMaybes sids) of
    [] -> return Nothing
    ids -> do
      prg <- GL.createProgram
      mapM_ (GL.attachShader prg) ids
      GL.linkProgram prg
      return $ Just prg
  where
    compileShader :: FilePath -> IO(Maybe GL.Shader)
    compileShader fp = do
      putStrLn $ "Compiling " ++ fp
      fileSrc <- BS.readFile fp
      msid <- case (getShaderForExt . takeExtension) fp of
        Just shdrTy -> return . Just =<< GL.createShader shdrTy
        Nothing -> return Nothing
      case msid of
        Just sid -> do
          GL.shaderSourceBS sid GL.$= fileSrc
          GL.compileShader sid
          shaderLog <- GL.get $ GL.shaderInfoLog sid
          M.unless (shaderLog == []) $ putStrLn shaderLog
          return (Just sid)
        Nothing -> return Nothing

lookupShaderVar :: GL.Program -> ShaderVarTy -> String -> IO (String, Maybe ShaderVar)
lookupShaderVar prg ty name = do
  uloc <- GL.get $ GL.uniformLocation prg name
  if uloc == (GL.UniformLocation (-1)) then do
    aloc <- GL.get $ GL.attribLocation prg name
    if aloc == (GL.AttribLocation (-1)) then
      return (name, Nothing)
      else
      return $ (name, Just $ Attribute ty aloc)
    else
    return $ (name, Just $ Uniform ty uloc)

extractVars :: [IO(String, Maybe ShaderVar)] -> IO ShaderVarMap
extractVars iomvars = M.liftM constructMap $ sequence iomvars
  where
    constructMap :: [(String, Maybe ShaderVar)] -> ShaderVarMap
    constructMap vs = Map.fromList (gatherValid vs)

    gatherValid :: [(String, Maybe ShaderVar)] -> [(String, ShaderVar)]
    gatherValid vs = catMaybes $ map (\(n, msv) -> case msv of Nothing -> Nothing
                                                               Just sv -> Just (n, sv)) vs
createSimpleShader :: IO (Shader)
createSimpleShader = do
  vs <- getShaderPath "simple" GL.VertexShader
  fs <- getShaderPath "simple" GL.FragmentShader
  (Just prg) <- loadProgram [vs, fs]
  vars <- extractVars $ map (uncurry (lookupShaderVar prg))
    [(FloatListTy, "position"),
     (FloatListTy, "texCoord"),
     (TextureTy 0, "diffuseTex"),
     (Matrix4Ty, "mvpMatrix"),
     (Matrix3Ty, "texCoordMatrix")]
  return $ Shader prg vars

{--
createTransparentShader :: IO (Shader)
createTransparentShader = do
  vs <- getShaderPath "simple" GL.VertexShader
  fs <- getShaderPath "simple-trans" GL.FragmentShader
  (Just prg) <- loadProgram [vs, fs]
  vars <- extractVars $ map (uncurry (lookupShaderVar prg))
    [(FloatListTy, "position"),
     (FloatListTy, "texCoord"),
     (FloatTy, "alpha"),
     (TextureTy 0, "diffuseTex"),
     (Matrix4Ty, "mvpMatrix"),
     (Matrix3Ty, "texCoordMatrix")]
  return $ Shader prg vars

attribute vec3 position;
attribute vec2 texCoord;

uniform mat4 mvpMatrix;
uniform mat3 texCoordMatrix;

varying vec2 uv;

void main() {
  vec3 uvp = texCoordMatrix * vec3(texCoord, 1.0);
  uv = (uvp / uvp.z).xy; // Perspective correction

  gl_Position = mvpMatrix * vec4(position, 1.0);
}

varying vec2 uv;

uniform sampler2D diffuseTex;
uniform float alpha;

void main() {
  gl_FragColor = texture2D(diffuseTex, uv);
  gl_FragColor.a *= alpha;
}
--}

createTransparentShader :: IO (Shader)
createTransparentShader = I.generateOpenGLShader shdr
  where
    shdr = I.compileProgram texVertex3Ty vshdr fshdr

    vshdr :: I.ShaderCode TVertex3 Vertex2
    vshdr = I.ShdrCode $ do
      position <- I.getInput3f 0
      texCoord <- I.getInput2f 1

      mvpMatrix <- I.newUniformVar "mvpMatrix" I.matrix4Ty
      texCoordMatrix <- I.newUniformVar "texCoordMatrix" I.matrix3Ty

      uvp <- I.setE I.vector3fTy $
             I.xform3f (I.mkVarExpr texCoordMatrix) $
             I.mkVec3f_21 (I.mkVarExpr texCoord) (I.mkConstf 1)

      uv <- I.setE I.vector2fTy $
            I.finishSwizzleV . I._y_ . I._x_ . I.swizzle3D $
            I.div3f (I.mkVarExpr uvp) ((I.finishSwizzleS . I._z_ . I.swizzle3D) (I.mkVarExpr uvp))

      out_pos <- I.setE I.vector4fTy $
                 I.xform4f (I.mkVarExpr mvpMatrix) $
                 I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)

      let output = I.addVertexPosition out_pos $
                   I.addCustomOVar uv I.emptyO

      return output

    fshdr :: I.ShaderCode Vertex2 ()
    fshdr = I.ShdrCode $ do
      uv <- I.getInput2f 0

      diffuseTex <- I.newUniformVar "diffuseTex" I.sampler2DTy
      alpha <- I.newUniformVar "alpha" I.floatTy

      texColor <- I.setE I.vector4fTy $ I.sample2D (I.mkVarExpr diffuseTex) (I.mkVarExpr uv)
      out_color <- I.setE I.vector4fTy $ I.mkVec4f_31
                   ((I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D) (I.mkVarExpr texColor)) $
                   I.multf ((I.finishSwizzleS . I._w_ . I.swizzle4D) (I.mkVarExpr texColor)) (I.mkVarExpr alpha)

      let output = I.addFragmentColor out_color I.emptyO
      return output

createFontShader :: IO (Shader)
createFontShader = do
  vs <- getShaderPath "simple" GL.VertexShader
  fs <- getShaderPath "font" GL.FragmentShader
  (Just prg) <- loadProgram [vs, fs]
  vars <- extractVars $ map (uncurry (lookupShaderVar prg))
    [(FloatListTy, "position"),
     (FloatListTy, "texCoord"),
     (Vector3Ty, "color"),
     (FloatTy, "alpha"),
     (TextureTy 0, "maskTex"),
     (Matrix4Ty, "mvpMatrix"),
     (Matrix3Ty, "texCoordMatrix")]
  return $ Shader prg vars

createSpotlightShader :: IO (Shader)
createSpotlightShader = do
  vs <- getShaderPath "standard" GL.VertexShader
  fs <- getShaderPath "spotlight" GL.FragmentShader
  (Just prg) <- loadProgram [vs, fs]
  vars <- extractVars $ map (uncurry (lookupShaderVar prg))
    [(FloatListTy, "position"),
     (FloatListTy, "texCoord"),
     (FloatListTy, "normal"),
     (Matrix4Ty, "m2wMatrix"),
     (Matrix4Ty, "mvpMatrix"),
     (TextureTy 0, "shadowMap"),
     (TextureTy 1, "diffuseTex"),
     (Vector3Ty, "ambient"),
     (Vector3Ty, "lightPos"),
     (Vector3Ty, "lightDir"),
     (FloatTy, "lightCosCutoff"),
     (Matrix4Ty, "shadowVP")]
  return $ Shader prg vars

createMinimalShader :: IO (Shader)
createMinimalShader = do
  vs <- getShaderPath "minimal" GL.VertexShader
  fs <- getShaderPath "minimal" GL.FragmentShader
  (Just prg) <- loadProgram [vs, fs]
  vars <- extractVars $ map (uncurry (lookupShaderVar prg))
    [(FloatListTy, "position"),
     (Matrix4Ty, "mvpMatrix")]
  return $ Shader prg vars

destroyShader :: Shader -> IO ()
destroyShader (Shader prog _) = GL.deleteObjectName prog

beforeRender :: Shader -> IO ()
beforeRender shdr = do
  -- Enable the program
  GL.currentProgram GL.$= Just (getProgram shdr)

  -- Enable each vertex attribute that this material needs
  mapM_ enableAttribute $ (Map.elems . getShaderVars) shdr
  where enableAttribute :: ShaderVar -> IO ()
        enableAttribute v = case v of
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Enabled

afterRender :: Shader -> IO ()
afterRender shdr = do
  -- Disable each vertex attribute that this material needs
  mapM_ disableAttribute $ (Map.elems . getShaderVars) shdr
  where disableAttribute :: ShaderVar -> IO ()
        disableAttribute v = case v of
          Uniform (TextureTy unit) _ -> do
            GL.activeTexture GL.$= GL.TextureUnit unit
            GL.textureBinding GL.Texture2D GL.$= Nothing
          Uniform _ _ -> return ()
          Attribute _ loc -> GL.vertexAttribArray loc GL.$= GL.Disabled

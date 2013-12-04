module Graphics.Rendering.Lambency.Shader (
  Shader,
  ShaderVarTy(..),
  ShaderValue(..),
  ShaderVar(..),
  ShaderMap,
  getProgram,
  getShaderVars,
  isUniform,
  getUniforms,
  setUniformVar,
  createMinimalShader,
  createSimpleShader,
  createSpotlightShader,
  destroyShader,
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Texture
import Graphics.Rendering.Lambency.Utils

import Paths_lambency

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Data.Vect.Float

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Control.Monad as M

import Data.Maybe (catMaybes)
import Data.Array.IO
import Data.Array.Storable

import System.FilePath
--------------------------------------------------------------------------------

data ShaderVarTy = Matrix3Ty
                 | Matrix4Ty
                 | Matrix3ListTy
                 | Matrix4ListTy
                 | Vector3Ty
                 | Vector4Ty
                 | Vector3ListTy
                 | Vector4ListTy
                 | IntTy
                 | IntListTy
                 | FloatTy
                 | FloatListTy
                 | TextureTy GLRaw.GLuint
                 deriving (Show, Eq)

data ShaderValue = Matrix3Val Mat3
                 | Matrix4Val Mat4
                 | Matrix3ListVal [Mat3]
                 | Matrix4ListVal [Mat4]
                 | Vector3Val Vec3
                 | Vector4Val Vec4
                 | Vector3ListVal [Vec3]
                 | Vector4ListVal [Vec4]
                 | IntVal Int
                 | IntListVal [Int]
                 | FloatVal Float
                 | FloatListVal [Float]
                 | TextureVal Texture
                 deriving (Show)

cmpm3 :: Mat3 -> Mat3 -> Bool
cmpm3 (Mat3 x y z) (Mat3 a b c) =
  compareClose x a && compareClose y b && compareClose z c

cmpm4 :: Mat4 -> Mat4 -> Bool
cmpm4 (Mat4 x y z w) (Mat4 a b c d) =
  compareClose x a && compareClose y b && compareClose z c && compareClose w d

instance Eq ShaderValue where
  (Matrix3Val a) == (Matrix3Val b) = cmpm3 a b
  (Matrix4Val a) == (Matrix4Val b) = cmpm4 a b
  (Matrix3ListVal a) == (Matrix3ListVal b) = all id $ map (uncurry cmpm3) $ zip a b
  (Matrix4ListVal a) == (Matrix4ListVal b) = all id $ map (uncurry cmpm4) $ zip a b
  (Vector3Val a) == (Vector3Val b) = compareClose a b
  (Vector4Val a) == (Vector4Val b) = compareClose a b
  (Vector3ListVal a) == (Vector3ListVal b) = all id $ map (uncurry compareClose) $ zip a b
  (Vector4ListVal a) == (Vector4ListVal b) = all id $ map (uncurry compareClose) $ zip a b
  (IntVal a) == (IntVal b) = a == b
  (IntListVal a) == (IntListVal b) = a == b
  (FloatVal a) == (FloatVal b) = a == b
  (FloatListVal a) == (FloatListVal b) = a == b
  (TextureVal a) == (TextureVal b) = a == b
  _ == _ = False

data ShaderVar = Uniform ShaderVarTy GL.UniformLocation
               | Attribute ShaderVarTy GL.AttribLocation
               deriving (Show, Eq)

type ShaderVarMap = Map.Map String ShaderVar

instance Ord ShaderVar where
  s1 `compare` s2 = (show s1) `compare` (show s2)

type ShaderMap = Map.Map ShaderVar ShaderValue
data Shader = Shader GL.Program ShaderVarMap deriving(Show, Eq)

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
setUniformVar (Uniform Matrix4Ty (GL.UniformLocation loc)) (Matrix4Val mat)  = do
  arr <- newListArray (0 :: Int, 15) (map realToFrac (destructMat4 mat))
  withStorableArray arr (\ptr -> GLRaw.glUniformMatrix4fv loc 1 0 ptr)

setUniformVar (Uniform (TextureTy unit) loc) (TextureVal tex) = do
  GL.activeTexture GL.$= (GL.TextureUnit unit)
  GL.textureBinding GL.Texture2D GL.$= Just (getHandle tex)
  GL.uniform loc GL.$= (GL.TextureUnit unit)

setUniformVar (Uniform Vector3Ty loc) (Vector3Val (Vec3 x y z)) = do
  GL.uniform loc GL.$= GL.Vertex3 (f x) (f y) (f z)
  where
    f :: Float -> GL.GLfloat
    f = realToFrac

setUniformVar _ _ = ioError $ userError "Uniform not supported"

getShaderExt :: GL.ShaderType -> String
getShaderExt GL.FragmentShader = ".frag"
getShaderExt GL.VertexShader = ".vert"
getShaderExt _ = ".glsl"

getShaderForExt :: String -> Maybe GL.ShaderType
getShaderForExt ".frag" = Just GL.FragmentShader
getShaderForExt ".vert" = Just GL.VertexShader
getShaderForExt _ = Nothing

getShaderPath :: String -> GL.ShaderType -> IO(FilePath)
getShaderPath name ty = getDataFileName $ name <.> (getShaderExt ty)

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

lookupShaderVar :: GL.Program -> ShaderVarTy -> String -> IO((String, Maybe ShaderVar))
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
    constructMap vs = foldl (\m (n, sv) -> Map.insert n sv m) Map.empty (gatherValid vs)

    gatherValid :: [(String, Maybe ShaderVar)] -> [(String, ShaderVar)]
    gatherValid vs = catMaybes $ map (\(n, msv) -> case msv of Nothing -> Nothing
                                                               Just sv -> Just (n, sv)) vs
createSimpleShader :: IO (Shader)
createSimpleShader = do
  (Just prg) <- loadProgram =<< mapM (getShaderPath "simple") [GL.VertexShader, GL.FragmentShader]
  vars <- extractVars $ map (uncurry (lookupShaderVar prg))
    [(FloatListTy, "position"),
     (FloatListTy, "texCoord"),
     (TextureTy 0, "diffuseTex"),
     (Matrix4Ty, "mvpMatrix")]
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

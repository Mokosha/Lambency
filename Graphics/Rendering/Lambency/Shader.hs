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
  createSimpleShader
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Texture
import Graphics.Rendering.Lambency.Utils

import Paths_lambency

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Data.Vect.Float

import qualified Data.ByteString as BS
import qualified Control.Monad as M

import Data.Maybe (fromJust)
import Data.Array.IO
import Data.Array.Storable

import qualified Data.Map as Map
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
                 | TextureTy
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
                 | TextureVal TextureHandle
                 deriving (Show)

data ShaderVar = Uniform ShaderVarTy String
               | Attribute ShaderVarTy GL.AttribLocation
               deriving (Show, Eq)

instance Ord ShaderVar where
  s1 `compare` s2 = (show s1) `compare` (show s2)

type ShaderMap = Map.Map ShaderVar ShaderValue
data Shader = Shader GL.Program [ShaderVar]

getProgram :: Shader -> GL.Program
getProgram (Shader prg _) = prg

getShaderVars :: Shader -> [ShaderVar]
getShaderVars (Shader _ vars) = vars

isUniform :: ShaderVar -> Bool
isUniform (Uniform _ _) = True
isUniform _ = False

getUniforms :: Shader -> [ShaderVar]
getUniforms = (filter isUniform) . getShaderVars

setUniformVar :: ShaderVar -> ShaderValue -> IO ()
setUniformVar (Uniform Matrix4Ty varName) (Matrix4Val mat)  = do
  mprg <- GL.get GL.currentProgram
  case mprg of
    Nothing -> return ()
    Just prg -> do
      (GL.UniformLocation matLoc) <- GL.get $ GL.uniformLocation prg varName
      if matLoc == (-1) then return ()
        else do
        matArr <- newListArray (0 :: Int, 15) (map realToFrac (destructMat4 mat))
        withStorableArray matArr (\ptr -> GLRaw.glUniformMatrix4fv matLoc 1 0 ptr)

setUniformVar (Uniform TextureTy varName) (TextureVal tex) = do
  mprg <- GL.get GL.currentProgram
  case mprg of
    Nothing -> return ()
    Just prg -> do
      texLoc <- GL.get $ GL.uniformLocation prg varName
      if texLoc == (GL.UniformLocation (-1)) then return ()
        else do
        GL.activeTexture GL.$= (GL.TextureUnit 0)
        GL.textureBinding GL.Texture2D GL.$= Just tex
        GL.uniform texLoc GL.$= (GL.TextureUnit 0)

setUniformVar _ _ = ioError $ userError "Uniform not supported"

loadProgram :: (Maybe FilePath) -> (Maybe FilePath) -> (Maybe FilePath) -> IO(Maybe GL.Program)
loadProgram vss fss gss = do
  vsid <- compileShader GL.VertexShader vss
  fsid <- compileShader GL.FragmentShader fss
  gsid <- compileShader GL.GeometryShader gss
  case (vsid, fsid, gsid) of
    (Nothing, Nothing, Nothing) -> return Nothing
    (_, _, _) -> do
      prg <- GL.createProgram
      let
        attachShader :: Maybe GL.Shader -> IO ()
        attachShader shdr = case shdr of
            Nothing -> return ()
            Just s -> GL.attachShader prg s
        in do
        attachShader vsid
        attachShader fsid
        attachShader gsid
      GL.linkProgram prg
      return $ Just prg
  where
   compileShader :: GL.ShaderType -> (Maybe FilePath) -> IO(Maybe GL.Shader)
   compileShader _ Nothing = return Nothing
   compileShader sTy (Just fp) = do
     putStrLn $ "Compiling " ++ (show sTy) ++ ": " ++ fp
     sid <- GL.createShader sTy
     fileSrc <- BS.readFile fp
     GL.shaderSourceBS sid GL.$= fileSrc
     GL.compileShader sid
     shaderLog <- GL.get $ GL.shaderInfoLog sid
     M.unless (shaderLog == "") $ putStrLn shaderLog
     return (Just sid)

createSimpleShader :: IO (Shader)
createSimpleShader = do
  defaultVertexShader <- getDataFileName "simple.vs"
  defaultFragmentShader <- getDataFileName "simple.fs"
  prg <- loadProgram (Just defaultVertexShader) (Just defaultFragmentShader) Nothing
  return $ Shader (fromJust prg)
    [Attribute FloatListTy (GL.AttribLocation 0),
     Attribute FloatListTy (GL.AttribLocation 1),
     Uniform TextureTy "sampler",
     Uniform Matrix4Ty "mvpMatrix"]

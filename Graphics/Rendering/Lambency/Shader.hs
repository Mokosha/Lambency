module Graphics.Rendering.Lambency.Shader (
  Shader,
  ShaderVarTy(..),
  ShaderVarValue(..),
  ShaderVar(..),
  isUniform,
  getUniforms,
  setUniformVar,
  loadShader
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Texture
import Graphics.Rendering.Lambency.Utils

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import qualified Data.ByteString as BS

import Data.Vect.Float

import Data.Array.IO
import Data.Array.Storable
--------------------------------------------------------------------------------

type Shader = GL.Program

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

data ShaderVarValue = Matrix3Val Mat3
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

isUniform :: ShaderVar -> Bool
isUniform (Uniform _ _) = True
isUniform _ = False

getUniforms :: [ShaderVar] -> [ShaderVar]
getUniforms vs = do
  us <- filter isUniform vs
  (\x -> case x of (Uniform _ _) -> [x]
                   _ -> []) us

setUniformVar :: ShaderVar -> ShaderVarValue -> IO ()
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

setUniformVar _ _ = ioError $ userError "Uniform not supported"

loadShader :: (Maybe FilePath) -> (Maybe FilePath) -> (Maybe FilePath) -> IO(Maybe Shader)
loadShader vss fss gss = do
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
   compileShader sTy ss =
     case ss
       of Nothing -> return Nothing
          (Just fp) -> do
            putStrLn $ "Compiling " ++ (show sTy) ++ ": " ++ fp
            sid <- GL.createShader sTy
            fileSrc <- BS.readFile fp
            GL.shaderSourceBS sid GL.$= fileSrc
            GL.compileShader sid
            shaderLog <- GL.get $ GL.shaderInfoLog sid
            if shaderLog == "" then return () else putStrLn shaderLog
            return (Just sid)

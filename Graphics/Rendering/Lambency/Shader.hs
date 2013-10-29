module Graphics.Rendering.Lambency.Shader (
  Shader,
  loadShader
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as BS

import qualified Graphics.Rendering.Lambency.Utils as Utils

--------------------------------------------------------------------------------

type Shader = GL.Program

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
            putStrLn =<< (GL.get $ GL.shaderInfoLog sid)
            return (Just sid)

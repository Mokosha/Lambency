{-# LANGUAGE RecordWildCards #-}
module Lambency.Shader (
  getProgram,
  getShaderVars,
  isUniform,
  getUniforms,
  setUniformVar,
  destroyShader,
  beforeRender, afterRender,

  getLightVarName,
  compileMaterial,
  compileUnlitMaterial

) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad

import Data.Maybe
import qualified Data.Map as Map

import Lambency.Material
import Lambency.Texture
import Lambency.Types

import qualified Lambency.Shader.Expr as I
import qualified Lambency.Shader.Base as I
import qualified Lambency.Shader.OpenGL as I
import qualified Lambency.Shader.Program as I
import qualified Lambency.Shader.Var as I

import Linear.Matrix
import Linear.V3
import Linear.V4

import Foreign.Marshal.Utils
import Foreign.Ptr

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
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

setUniformVar (Uniform (ShadowMapTy unit) loc) (ShadowMapVal sm) =
  setUniformVar (Uniform (TextureTy unit) loc) (TextureVal $ getShadowmapTexture sm)

setUniformVar (Uniform FloatTy loc) (FloatVal f) = do
  GL.uniform loc GL.$= GL.Index1 ((realToFrac f) :: GL.GLfloat)

setUniformVar (Uniform Vector3Ty loc) (Vector3Val (V3 x y z)) = do
  GL.uniform loc GL.$= GL.Vertex3 (f x) (f y) (f z)
  where
    f :: Float -> GL.GLfloat
    f = realToFrac

setUniformVar (Uniform Vector4Ty loc) (Vector4Val (V4 x y z w)) = do
  GL.uniform loc GL.$= GL.Vertex4 (f x) (f y) (f z) (f w)
  where
    f :: Float -> GL.GLfloat
    f = realToFrac

setUniformVar (Attribute _ _) _ = return ()
setUniformVar (Uniform ty _) _ = ioError $ userError $ "Uniform not supported: " ++ (show ty)

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

----------------------------------------

vertMinimal :: I.ShaderCode
vertMinimal = I.ShdrCode $ do
  position <- I.getInput3f "position"
  mvpMatrix <- I.newUniformVar "mvpMatrix" I.matrix4Ty
  out_pos <- I.setE I.vector4fTy $
             I.xform4f (I.mkVarExpr mvpMatrix) $
             I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)
  return $ I.addVertexPosition out_pos I.emptyO
  
fragMinimal :: I.ShaderCode
fragMinimal = I.ShdrCode $ return I.emptyO

createMinimalShader :: IO (Shader)
createMinimalShader =
  I.generateOpenGLShader $ I.compileProgram vertMinimal fragMinimal

--------------------

{-- !TODO! We should use dithering on non-specular non-textured materials, although
 -- this is kind of hard (read: work) to interleave into the current shader system
 -- with little benefit

dither :: I.ShaderVar Float -> I.ShaderVar Float -> I.ShaderContext i (I.Expr Float)
dither v r = do
  val <- I.setE I.floatTy $ I.multf (I.mkVarExpr v) (I.mkConstf 255)
  ival <- I.setE I.floatTy $ I.floorf (I.mkVarExpr val)
  diff <- I.setE I.floatTy $ I.subf (I.mkVarExpr val) (I.mkVarExpr ival)
  return $
    I.divf
    (I.addf
     (I.mkVarExpr ival)
     (I.castBoolToFloat $ I.ltf (I.mkVarExpr r) (I.mkVarExpr diff)))
    (I.mkConstf 255.0)

dither3 :: I.ShaderVar (V3 Float) -> I.ShaderVar (V2 Float) -> I.ShaderContext i (I.Expr (V3 Float))
dither3 v seed = do
  r <- I.setE I.floatTy $ I.fractf $
       I.multf (I.mkConstf 43758.5453) $ I.sinf $
       I.dot2f (I.mkVarExpr seed) (I.mkConstVec2f $ V2 12.9898 78.233)

  vx <- I.setE I.floatTy $ (I.finishSwizzleS . I._x_ . I.swizzle3D) (I.mkVarExpr v)
  vy <- I.setE I.floatTy $ (I.finishSwizzleS . I._y_ . I.swizzle3D) (I.mkVarExpr v)
  vz <- I.setE I.floatTy $ (I.finishSwizzleS . I._z_ . I.swizzle3D) (I.mkVarExpr v)

  I.mkVec3f_111 <$> dither vx r <*> dither vy r <*> dither vz r
--}

handleUnlitVertex :: MaterialVar Texture -> MaterialVar (M33 Float) -> I.ShaderCode
handleUnlitVertex texture texMatrix = I.ShdrCode $ do
  uv <-
    case isDefined texture of
      False -> I.newVar "dummyUV" I.vector2fTy
      True -> do
        case isDefined texMatrix  of
          False -> I.getInput2f "texCoord"
          True -> do
            tc <- I.getInput2f "texCoord"
            tcMat <- I.newUniformVar (getMatVarName texMatrix) I.matrix3Ty

            -- Transform texture coordinates by the matrix
            tcTrans <- I.setE I.vector3fTy $
                       I.xform3f (I.mkVarExpr tcMat) $
                       I.mkVec3f_21 (I.mkVarExpr tc) (I.mkConstf 1.0)

            -- Perspective divide...
            I.setE I.vector2fTy $
              I.finishSwizzleV . I._y_ . I._x_ . I.swizzle3D $
              I.div3f (I.mkVarExpr tcTrans) $
              I.finishSwizzleS . I._z_ . I.swizzle3D $
              (I.mkVarExpr tcTrans)

  position <- I.getInput3f "position"
  mvpMatrix <- I.newUniformVar "mvpMatrix" I.matrix4Ty
  out_pos <- I.setE I.vector4fTy $
             I.xform4f (I.mkVarExpr mvpMatrix) $
             I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)

  let output = I.addVertexPosition out_pos $
               if isDefined texture
               then I.addCustomOVar "uv" uv $ I.emptyO
               else I.emptyO
  return output

genUnlitVertexShader :: Material -> I.ShaderCode
genUnlitVertexShader (MaskedSpriteMaterial {..}) = handleUnlitVertex spriteMask spriteMaskMatrix
genUnlitVertexShader (TexturedSpriteMaterial {..}) = handleUnlitVertex spriteTexture spriteTextureMatrix
genUnlitVertexShader m =
  error $ "Lambency.Shader (genLitVertexShader): Cannot generate unlit vertex shader for material: " ++ show m

genUnlitFragmentShader :: Material -> I.ShaderCode
genUnlitFragmentShader (MaskedSpriteMaterial{..}) =
  let lookupMaskValue tex = do
        guard (isDefined tex)

        let MaterialVar (name, Just _) = tex

        sampler <- I.newUniformVar name I.sampler2DTy
        uv <- I.getInput2f "uv"
        I.setE I.floatTy $
          I.finishSwizzleS . I._w_ . I.swizzle4D $
          I.sample2D (I.mkVarExpr sampler) (I.mkVarExpr uv)

  in I.ShdrCode $ do
    -- Just determine the final color
    maskedValue <- lookupMaskValue spriteMask <|> (I.setE I.floatTy $ I.mkConstf 1.0)

    color <- case spriteMaskColor of
      MaterialVar (_, Nothing) ->
        error "Lambency.Shader (genUnlitFragmentShader): Masked material must have color!"
      MaterialVar (name, _) -> I.newUniformVar name I.vector4fTy

    outColor <- I.setE I.vector4fTy $
                I.mkVec4f_31 (I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $ I.mkVarExpr color) $
                I.multf (I.mkVarExpr maskedValue) $
                I.finishSwizzleS . I._w_ . I.swizzle4D $
                I.mkVarExpr color

    return $ I.addFragmentColor outColor I.emptyO

genUnlitFragmentShader (TexturedSpriteMaterial {..}) =
  let lookupTextureValue tex = do
        guard (isDefined tex)

        let MaterialVar (name, Just _) = tex

        sampler <- I.newUniformVar name I.sampler2DTy
        uv <- I.getInput2f "uv"
        I.setE I.vector4fTy $ I.sample2D (I.mkVarExpr sampler) (I.mkVarExpr uv)

  in I.ShdrCode $ do
    -- Just determine the final color
    color <- lookupTextureValue spriteTexture <|> (I.setE I.vector4fTy $ I.mkConstVec4f $ V4 1 1 1 1 )

    alpha <- case spriteAlpha of
      MaterialVar (_, Nothing) -> I.setE I.floatTy $ I.mkConstf 1.0
      MaterialVar (name, _) -> I.newUniformVar name I.floatTy

    outColor <- I.setE I.vector4fTy $
                I.mkVec4f_31 (I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $ I.mkVarExpr color) $
                I.multf (I.mkVarExpr alpha) $
                I.finishSwizzleS . I._w_ . I.swizzle4D $
                I.mkVarExpr color

    return $ I.addFragmentColor outColor I.emptyO

genUnlitFragmentShader m =
  error $ "Lambency.Shader (genLitVertexShader): Cannot generate unlit fragment shader for material: " ++ show m

getLightVarName :: LightVar a -> String
getLightVarName (LightVar (n, _)) = n

genLitVertexShader :: Material -> I.ShaderCode
genLitVertexShader mat
  | isUnlit mat = error "Lambency.Shader (genLitVertexShader): Generating lit vertex shader for unlit material!"
  | otherwise = I.ShdrCode $ do
    -- The actual fragment position needs to be set in order
    -- to do lighting calculations based on world space coordinates.
    position <- I.getInput3f "position"
    m2wMatrix <- I.newUniformVar "m2wMatrix" I.matrix4Ty
    pos <- I.setE I.vector3fTy $
           I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
           I.xform4f (I.mkVarExpr m2wMatrix) $
           I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)

    -- Since the material is lit, it should also have a normal
    normal <- I.getInput3f "normal"
    norm <- I.setE I.vector3fTy $
            I.normalize3f $
            I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
            I.xform4f (I.mkVarExpr m2wMatrix) $
            I.mkVec4f_31 (I.mkVarExpr normal) (I.mkConstf 0)

    -- If we're using textures then we should also check for texture
    -- coordinates
    let hasTexCoords = usesTextures mat
    uv <-
      if hasTexCoords
      then I.getInput2f "texCoord"
      else I.newVar "dummyUV" I.vector2fTy

    -- All vertices need to be transformed based on the
    -- MVP matrix being passed in...
    mvpMatrix <- I.newUniformVar "mvpMatrix" I.matrix4Ty
    out_pos <- I.setE I.vector4fTy $
               I.xform4f (I.mkVarExpr mvpMatrix) $
               I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)

    let output = I.addVertexPosition out_pos $
                 I.addCustomOVar "position" pos $
                 I.addCustomOVar "normal" norm $
                 if hasTexCoords
                 then I.addCustomOVar "uv" uv $ I.emptyO
                 else I.emptyO

    return output

getDiffuseColor :: Material -> I.ShaderContext (I.ShaderVar (V3 Float))
getDiffuseColor (BlinnPhongMaterial {..}) = do
  reflectivity <- case diffuseReflectivity of
    MaterialVar (_, Nothing) -> I.setE I.vector3fTy $ I.mkConstVec3f (V3 0 0 0)
    MaterialVar (name, Just _) -> I.newUniformVar name I.vector3fTy
  case diffuseMap of
    MaterialVar (_, Nothing) -> return reflectivity
    MaterialVar (name, Just _) -> do
      tex <- I.newUniformVar name I.sampler2DTy
      uv <- I.getInput2f "uv"
      color <- I.setE I.vector3fTy $
               I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
               I.sample2D (I.mkVarExpr tex) $
               I.mkVarExpr uv

      I.setE I.vector3fTy $
        I.mult3f (I.mkVarExpr color) (I.mkVarExpr reflectivity)

getDiffuseColor (TexturedSpriteMaterial {..}) = error "Lambency.Material (getDiffuseColor): Not implemented!"
getDiffuseColor (MaskedSpriteMaterial {..}) = error "Lambency.Material (getDiffuseColor): Not implemented!"
getDiffuseColor m = error $ "Lambency.Material (getDiffuseColor): Not supported for material: " ++ show m

type SpecularInfo = Maybe (I.ShaderVar (V3 Float), I.ShaderVar Float)

getSpecularColor :: Material -> I.ShaderContext (SpecularInfo)
getSpecularColor (BlinnPhongMaterial {..}) = do
  reflectivity <- case specularReflectivity of
    MaterialVar (_, Nothing) -> return Nothing
    MaterialVar (name, Just _) -> Just <$> I.newUniformVar name I.vector3fTy

  -- We must have specular reflectivity to use specular color
  guard $ isJust reflectivity

  -- We don't need a specular map, but if we have one then make sure to
  -- multiply the appropriate value with the reflectivity
  applyMap <- case specularMap of
    MaterialVar (_, Nothing) -> return reflectivity
    MaterialVar (name, Just _) -> do
      -- !FIXME! we should tailor the lookup of the specular values
      -- based on what kind of texture we're passing here...
      tex <- I.newUniformVar name I.sampler2DTy
      uv <- I.getInput2f "uv"
      color <- I.setE I.vector3fTy $
               I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
               I.sample2D (I.mkVarExpr tex) $
               I.mkVarExpr uv

      case reflectivity of
        Just refl -> Just <$> (I.setE I.vector3fTy $
                               I.mult3f (I.mkVarExpr color) (I.mkVarExpr refl))
        Nothing -> return $ Just color

  if not (isJust applyMap)
    then error "Lambency.Shader (getSpecularColor): This shouldn't happen"
    else return ()

  -- Finally, we have to have a specular exponent. If we don't, then
  -- we definitely don't have any specular activity
  guard $ isDefined specularExponent
  case specularExponent of
    MaterialVar (_, Nothing) -> return Nothing
    MaterialVar (name, Just _) -> do
      e <- I.newUniformVar name I.floatTy
      return $ (\x -> (x, e)) <$> applyMap

getSpecularColor _ = error "Lambency.Material (getSpecularColor): Only Blinn-Phong materials contain specular!"

getAmbientColor :: LightParams -> Material -> I.ShaderContext (I.ShaderVar (V3 Float))
getAmbientColor (LightParams{..}) (BlinnPhongMaterial {..}) = do
  reflectivity <- case ambientReflectivity of
    MaterialVar (_, Nothing) -> I.setE I.vector3fTy $ I.mkConstVec3f (V3 1 1 1)
    MaterialVar (name, _) -> I.newUniformVar name I.vector3fTy

  color <- I.newUniformVar (getLightVarName ambientColor) I.vector3fTy
  I.setE I.vector3fTy $
    I.mult3f (I.mkVarExpr color) (I.mkVarExpr reflectivity)  

getAmbientColor _ _ = error "Lambency.Material (getAmbientColor): Only Blinn-Phong materials contain ambient!"

handleNormalMaps :: Material -> I.ShaderVar (V3 Float) -> I.ShaderContext (I.ShaderVar (V3 Float))
handleNormalMaps (BlinnPhongMaterial {..}) norm =
  case reflectionInfo of
    Nothing -> I.setE I.vector3fTy $ I.normalize3f (I.mkVarExpr norm)
    _ -> error "Lambency.Material (handleNormalMaps): Not implemented"
handleNormalMaps _ _ = error "Lambency.Material (handleNormalMaps): Only Blinn-Phong materials can use normal maps!"

modulateLight :: I.ShaderVar (Float) -> I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) ->
                 I.ShaderContext (I.ShaderVar (V3 Float))
modulateLight intensity lightColor materialColor =
  I.setE I.vector3fTy $
  I.mult3f (I.mkVarExpr materialColor) $
  I.scale3f (I.mkVarExpr lightColor) $
  I.mkVarExpr intensity

blinnPhongLightColor :: LightParams -> I.ShaderContext (I.ShaderVar (V3 Float))
blinnPhongLightColor (LightParams {..}) = do
  color <- I.newUniformVar (getLightVarName lightColor) I.vector3fTy
  intensity <- I.newUniformVar (getLightVarName lightIntensity) I.floatTy

  I.setE I.vector3fTy $ I.scale3f (I.mkVarExpr color) (I.mkVarExpr intensity)

blinnPhongNDotH :: I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) ->
                   I.ShaderContext (I.ShaderVar Float)
blinnPhongNDotH lightDir pos norm = do
  eyePos <- I.newUniformVar "eyePos" I.vector3fTy

  ed <- I.setE I.vector3fTy $
        I.sub3f (I.mkVarExpr eyePos) (I.mkVarExpr pos)

  hVec <- I.setE I.vector3fTy $
          I.normalize3f $
          I.add3f (I.mkVarExpr ed) (I.mkVarExpr lightDir)

  I.setE I.floatTy $
    I.maxf (I.mkConstf 0) $
    I.dot3f (I.mkVarExpr hVec) (I.mkVarExpr norm)

blinnPhongLighting :: Light ->
                      I.ShaderVar (V3 Float) -> SpecularInfo ->
                      I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) ->
                      I.ShaderContext (I.ShaderVar (V3 Float))
blinnPhongLighting (Light params (SpotLight {..}) _) diffuseColor specularInfo pos norm = do

  -- !FIXME! We might be able to calculate this in the vertex shader
  -- to increase performance
  lightPos <- I.newUniformVar (getLightVarName spotLightPos) I.vector3fTy
  spotDir <- I.setE I.vector3fTy $
             I.normalize3f $
             I.sub3f (I.mkVarExpr pos) (I.mkVarExpr lightPos)

  lightDir <- I.newUniformVar (getLightVarName spotLightDir) I.vector3fTy
  spotDot <- I.setE I.floatTy $
             I.dot3f (I.mkVarExpr spotDir) (I.mkVarExpr lightDir)

  outColor <- I.setE I.vector3fTy $ I.mkConstVec3f (V3 0 0 0)

  cosCutoff <- I.newUniformVar (getLightVarName spotLightCosCutoff) I.floatTy
  flip (I.ifThen (I.gtf (I.mkVarExpr spotDot) (I.mkVarExpr cosCutoff))) (return ()) $ do

    lColor <- blinnPhongLightColor params

    ld <- I.setE I.vector3fTy $
          I.neg3f (I.mkVarExpr spotDir)

    nDotL <- I.setE I.floatTy $
             I.maxf (I.mkConstf 0) $
             I.dot3f (I.mkVarExpr ld) (I.mkVarExpr norm)

    diffuseLight <- modulateLight nDotL lColor diffuseColor
    case specularInfo of
      Nothing -> do
        I.assignE outColor $ I.mkVarExpr diffuseLight

      Just (color, e) -> do
        nDotH <- blinnPhongNDotH ld pos norm
        nDotHpow <- I.setE I.floatTy $ I.powf (I.mkVarExpr nDotH) (I.mkVarExpr e)
        specularLight <- modulateLight nDotHpow lColor color
        I.assignE outColor $ I.add3f (I.mkVarExpr diffuseLight) (I.mkVarExpr specularLight)

  return outColor
  
blinnPhongLighting (Light params (DirectionalLight {..}) _) diffuseColor specularInfo pos norm = do
  lightDir <- I.newUniformVar (getLightVarName dirLightDir) I.vector3fTy

  lColor <- blinnPhongLightColor params

  ld <- I.setE I.vector3fTy $
        I.neg3f (I.mkVarExpr lightDir)

  nDotL <- I.setE I.floatTy $
           I.maxf (I.mkConstf 0) $
           I.dot3f (I.mkVarExpr ld) (I.mkVarExpr norm)

  diffuseLight <- modulateLight nDotL lColor diffuseColor

  case specularInfo of
    Nothing -> return diffuseLight
    Just (color, e) -> do
      nDotH <- blinnPhongNDotH ld pos norm
      nDotHpow <- I.setE I.floatTy $ I.powf (I.mkVarExpr nDotH) (I.mkVarExpr e)
      specularLight <- modulateLight nDotHpow lColor color
      I.setE I.vector3fTy $
        I.add3f (I.mkVarExpr diffuseLight) $
        I.mkVarExpr specularLight

blinnPhongLighting (Light params (PointLight {..}) _) diffuseColor specularInfo pos norm = do
  -- !FIXME! We might be able to calculate this in the vertex shader
  -- to increase performance
  lightPos <- I.newUniformVar (getLightVarName pointLightPos) I.vector3fTy
  ld <- I.setE I.vector3fTy $
        I.sub3f (I.mkVarExpr lightPos) (I.mkVarExpr pos)

  lColor <- blinnPhongLightColor params

  nDotL <- I.setE I.floatTy $
           I.maxf (I.mkConstf 0) $
           I.dot3f (I.mkVarExpr ld) (I.mkVarExpr norm)

  diffuseLight <- modulateLight nDotL lColor diffuseColor

  case specularInfo of
    Nothing -> return diffuseLight
    Just (color, e) -> do
      nDotH <- blinnPhongNDotH ld pos norm
      nDotHpow <- I.setE I.floatTy $ I.powf (I.mkVarExpr nDotH) (I.mkVarExpr e)
      specularLight <- modulateLight nDotHpow lColor color
      I.setE I.vector3fTy $
        I.add3f (I.mkVarExpr diffuseLight) $
        I.mkVarExpr specularLight
  
genShadowFragment :: I.ShaderContext (I.ShaderVar Float)
genShadowFragment = do
  pos <- I.getInput3f "position"
  
  shadowMap <- I.newUniformVar "shadowMap" I.sampler2DTy
  shadowVP <- I.newUniformVar "shadowVP" I.matrix4Ty

  lightPersp <- I.setE I.vector4fTy $
                I.xform4f (I.mkVarExpr shadowVP) $
                I.mkVec4f_31 (I.mkVarExpr pos) (I.mkConstf 1)

  I.assignE lightPersp $
    I.div4f (I.mkVarExpr lightPersp) $
    I.finishSwizzleS . I._w_ . I.swizzle4D $
    I.mkVarExpr lightPersp

  I.assignE lightPersp $
    I.add4f (I.mkVec4f_1111 (I.mkConstf 0.5) (I.mkConstf 0.5) (I.mkConstf 0.5) (I.mkConstf 0.5)) $
    I.scale4f (I.mkVarExpr lightPersp) (I.mkConstf 0.5)

  -- !FIXME! Do a better bias here...
  objDepth <- I.setE I.floatTy $
              I.subf (I.finishSwizzleS . I._z_ . I.swizzle4D $ I.mkVarExpr lightPersp) (I.mkConstf 0.0001)
  shdwDepth <- I.setE I.floatTy $
               I.finishSwizzleS . I._z_ . I.swizzle4D $
               I.sample2D (I.mkVarExpr shadowMap) $
               I.finishSwizzleV . I._y_ . I._x_ . I.swizzle4D $
               I.mkVarExpr lightPersp

  I.setE I.floatTy $ I.castBoolToFloat $ I.gtf (I.mkVarExpr shdwDepth) (I.mkVarExpr objDepth)

genLitFragShader :: Light -> Material -> I.ShaderCode
genLitFragShader light mat = I.ShdrCode $ do

  pos <- I.getInput3f "position"
  norm <- I.getInput3f "normal"

  diffuseColor <- getDiffuseColor mat
  specularColor <- getSpecularColor mat <|> return Nothing

  litFragment <- handleNormalMaps mat norm >>=
                 blinnPhongLighting light diffuseColor specularColor pos

  ambientColor <- getAmbientColor (lightParams light) mat
  finalColor <- I.setE I.vector3fTy $
                I.add3f (I.mkVarExpr litFragment) $
                I.mult3f (I.mkVarExpr ambientColor) (I.mkVarExpr diffuseColor)

  -- !TODO! materials should be able to define opacity...
  let alpha = I.mkConstf 1.0

  outColor <- I.setE I.vector4fTy $
              I.mkVec4f_31 (I.mkVarExpr finalColor) alpha

  return $ I.addFragmentColor outColor I.emptyO

genShadowedFragShader :: Light -> Material -> I.ShaderCode
genShadowedFragShader light mat = I.ShdrCode $ do

  pos <- I.getInput3f "position"
  norm <- I.getInput3f "normal"

  diffuseColor <- getDiffuseColor mat
  specularColor <- getSpecularColor mat <|> return Nothing

  litFragment <- handleNormalMaps mat norm >>=
                 blinnPhongLighting light diffuseColor specularColor pos

  shadow <- genShadowFragment

  ambientColor <- getAmbientColor (lightParams light) mat
  finalColor <- I.setE I.vector3fTy $
                I.add3f (I.mult3f (I.mkVarExpr ambientColor) (I.mkVarExpr diffuseColor)) $
                I.scale3f (I.mkVarExpr litFragment) $
                I.mkVarExpr shadow

  -- !TODO! materials should be able to define opacity...
  let alpha = I.mkConstf 1.0

  outColor <- I.setE I.vector4fTy $
              I.mkVec4f_31 (I.mkVarExpr finalColor) alpha

  return $ I.addFragmentColor outColor I.emptyO

compileMaterial :: Light -> Material -> Maybe ShadowMap -> IO (Shader)
compileMaterial light mat Nothing
  | isUnlit mat = compileUnlitMaterial mat
  | otherwise =
    let vshdr = genLitVertexShader mat
        fshdr = genLitFragShader light mat
    in I.generateOpenGLShader $ I.compileProgram vshdr fshdr
compileMaterial light mat (Just _)
  | isUnlit mat = compileUnlitMaterial mat
  | otherwise =
    let vshdr = genLitVertexShader mat
        fshdr = genShadowedFragShader light mat
    in I.generateOpenGLShader $ I.compileProgram vshdr fshdr

compileUnlitMaterial :: Material -> IO (Shader)
compileUnlitMaterial NoMaterial =
  error "Lambency.Shader (compileUnlitMaterial): Cannot compile non-material!"
compileUnlitMaterial MinimalMaterial = createMinimalShader
compileUnlitMaterial mat
  | (not.isUnlit) mat = error "Lambency.Shader (compileUnlitMaterial): Material requires light!"
  | otherwise = I.generateOpenGLShader $ I.compileProgram vshdr fshdr
  where
    vshdr = genUnlitVertexShader mat
    fshdr = genUnlitFragmentShader mat

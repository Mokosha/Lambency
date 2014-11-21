{-# LANGUAGE RecordWildCards #-}
module Lambency.Material (
  createSimpleMaterial,
  createTexturedMaterial,
  createMaskedMaterial,

  compileMaterial
) where

--------------------------------------------------------------------------------
import Lambency.Types
import Lambency.Mesh
import Lambency.Vertex

import qualified Lambency.Shader.Expr as I
import qualified Lambency.Shader.Base as I
import qualified Lambency.Shader.OpenGL as I
import qualified Lambency.Shader.Program as I
import qualified Lambency.Shader.Var as I

import Control.Applicative

import qualified Data.Map as Map

import Linear
--------------------------------------------------------------------------------

defaultBlinnPhong :: Material
defaultBlinnPhong = BlinnPhongMaterial {
  diffuseReflectivity = MatDefault,
  diffuseMap = MatNothing,
  specularExponent = MatNothing,
  specularReflectivity = MatNothing,
  specularMap = MatNothing,
  ambientReflectivity = MatDefault,
  reflectionInfo = MatNothing,
  normalMod = MatNothing
}

createSimpleMaterial :: Material
createSimpleMaterial = defaultBlinnPhong

createTexturedMaterial :: Texture -> Material
createTexturedMaterial tex =
  defaultBlinnPhong { diffuseReflectivity = MatNothing,
                      diffuseMap = MatJust tex }

createMaskedMaterial :: Texture -> Material
createMaskedMaterial tex =
  defaultBlinnPhong { diffuseReflectivity = MatNothing,
                      diffuseMap = MatJust tex }

usesTextures :: Material -> Bool
usesTextures (BlinnPhongMaterial {..}) =
  (diffuseMap /= MatNothing) &&
  (specularMap /= MatNothing) &&
  (normalMod /= MatNothing)

genLitVertexShader :: Material -> I.ShaderCode a b
genLitVertexShader mat = I.ShdrCode $ do
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
  uv <- case usesTextures mat of
    False -> I.newVar "dummyUV" I.vector2fTy
    True -> I.getInput2f "texCoord"

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

getDiffuseColor :: Material -> I.ShaderContext i (I.ShaderVar (V3 Float))
getDiffuseColor (BlinnPhongMaterial {..}) = do
  reflectivity <- case diffuseReflectivity of
    MatNothing -> I.setE I.vector3fTy $ I.mkConstVec3f (V3 0 0 0)
    MatDefault -> I.setE I.vector3fTy $ I.mkConstVec3f (V3 1 1 1)
    MatJust _ -> I.newUniformVar "diffuseColor" I.vector3fTy
  case diffuseMap of
    MatNothing -> return reflectivity
    MatDefault -> return reflectivity
    MatJust _ -> do
      tex <- I.newUniformVar "diffuseMap" I.sampler2DTy
      uv <- I.getInput2f "uv"
      color <- I.setE I.vector3fTy $
               I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
               I.sample2D (I.mkVarExpr tex) $
               I.mkVarExpr uv

      I.setE I.vector3fTy $
        I.mult3f (I.mkVarExpr color) (I.mkVarExpr reflectivity)

type SpecularInfo = Maybe (I.ShaderVar (V3 Float), I.ShaderVar Float)

getSpecularColor :: Material -> I.ShaderContext i (SpecularInfo)
getSpecularColor (BlinnPhongMaterial {..}) = do
  reflectivity <- case specularReflectivity of
    MatNothing -> return Nothing
    MatDefault -> Just <$> (I.setE I.vector3fTy $ I.mkConstVec3f $ V3 1 1 1)
    MatJust _ -> Just <$> I.newUniformVar "specularColor" I.vector3fTy
  applyMap <- case specularMap of
    MatNothing -> return reflectivity
    MatDefault -> return reflectivity
    MatJust _ -> do
      -- !FIXME! we should tailor the lookup of the specular values
      -- based on what kind of texture we're passing here...
      tex <- I.newUniformVar "specularMap" I.sampler2DTy
      uv <- I.getInput2f "uv"
      color <- I.setE I.vector3fTy $
               I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
               I.sample2D (I.mkVarExpr tex) $
               I.mkVarExpr uv

      case reflectivity of
        Just refl -> Just <$> (I.setE I.vector3fTy $
                               I.mult3f (I.mkVarExpr color) (I.mkVarExpr refl))
        Nothing -> return $ Just color

  case specularExponent of
    MatNothing -> return Nothing
    MatDefault -> do
      defaultExponent <- I.setE I.floatTy $ I.mkConstf 1.0
      return $ (\x -> (x, defaultExponent)) <$> applyMap
    MatJust _ -> do
      e <- I.newUniformVar "specularExponent" I.floatTy
      return $ (\x -> (x, e)) <$> applyMap

getAmbientColor :: Material -> I.ShaderContext i (I.ShaderVar (V3 Float))
getAmbientColor (BlinnPhongMaterial {..}) = do
  reflectivity <- case specularReflectivity of
    MatNothing -> I.setE I.vector3fTy $ I.mkConstVec3f (V3 1 1 1)
    MatDefault -> I.setE I.vector3fTy $ I.mkConstVec3f (V3 1 1 1)
    MatJust _ -> I.newUniformVar "ambientColor" I.vector3fTy

  color <- I.newUniformVar "ambient" I.vector3fTy
  I.setE I.vector3fTy $
    I.mult3f (I.mkVarExpr color) (I.mkVarExpr reflectivity)  

handleNormalMaps :: Material -> I.ShaderVar (V3 Float) -> I.ShaderContext i (I.ShaderVar (V3 Float))
handleNormalMaps (BlinnPhongMaterial {..}) norm =
  case reflectionInfo of
    MatNothing -> return norm
    MatDefault -> return norm
    MatJust _ -> error "Lambency.Material (handleNormalMaps): Not implemented"

modulateLight :: I.ShaderVar (Float) -> I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) ->
                 I.ShaderContext i (I.ShaderVar (V3 Float))
modulateLight intensity lightColor materialColor =
  I.setE I.vector3fTy $
  I.mult3f (I.mkVarExpr materialColor) $
  I.scale3f (I.mkVarExpr lightColor) $
  I.mkVarExpr intensity

blinnPhongLightColor :: I.ShaderContext i (I.ShaderVar (V3 Float))
blinnPhongLightColor = do
  color <- I.newUniformVar "lightColor" I.vector3fTy
  intensity <- I.newUniformVar "lightIntensity" I.floatTy

  I.setE I.vector3fTy $ I.scale3f (I.mkVarExpr color) (I.mkVarExpr intensity)

blinnPhongNDotH :: I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) ->
                   I.ShaderContext i (I.ShaderVar Float)
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

blinnPhongLighting :: LightEnum ->
                      I.ShaderVar (V3 Float) -> SpecularInfo ->
                      I.ShaderVar (V3 Float) -> I.ShaderVar (V3 Float) ->
                      I.ShaderContext i (I.ShaderVar (V3 Float))
blinnPhongLighting SpotLightTy diffuseColor specularInfo pos norm = do

  -- !FIXME! We might be able to calculate this in the vertex shader
  -- to increase performance
  lightPos <- I.newUniformVar "lightPos" I.vector3fTy
  spotDir <- I.setE I.vector3fTy $
             I.sub3f (I.mkVarExpr pos) (I.mkVarExpr lightPos)

  lightDir <- I.newUniformVar "lightDir" I.vector3fTy
  spotDot <- I.setE I.floatTy $
             I.dot3f (I.mkVarExpr spotDir) (I.mkVarExpr lightDir)

  outColor <- I.setE I.vector3fTy $ I.mkConstVec3f (V3 0 0 0)

  cosCutoff <- I.newUniformVar "lightCosCutoff" I.floatTy
  flip (I.ifThen (I.gtf (I.mkVarExpr spotDot) (I.mkVarExpr cosCutoff))) (return ()) $ do

    lColor <- blinnPhongLightColor

    ld <- I.setE I.vector3fTy $
          I.neg3f (I.mkVarExpr spotDir)

    nDotL <- I.setE I.floatTy $
             I.maxf (I.mkConstf 0) $
             I.dot3f (I.mkVarExpr ld) (I.mkVarExpr norm)

    diffuseLight <- modulateLight nDotL lColor diffuseColor
    case specularInfo of
      Nothing -> do
        I.assignE outColor $ I.mkVarExpr diffuseLight

      Just (color, exponent) -> do
        nDotH <- blinnPhongNDotH ld pos norm
        nDotHpow <- I.setE I.floatTy $
                    I.powf (I.mkVarExpr nDotH) (I.mkVarExpr exponent)
        specularLight <- modulateLight nDotHpow lColor color
        I.assignE outColor $ I.add3f (I.mkVarExpr diffuseLight) (I.mkVarExpr specularLight)

  return outColor
  
blinnPhongLighting DirectionalLightTy diffuseColor specularInfo pos norm = do
  lightDir <- I.newUniformVar "lightDir" I.vector3fTy

  lColor <- blinnPhongLightColor

  ld <- I.setE I.vector3fTy $
        I.neg3f (I.mkVarExpr lightDir)

  nDotL <- I.setE I.floatTy $
           I.maxf (I.mkConstf 0) $
           I.dot3f (I.mkVarExpr ld) (I.mkVarExpr norm)

  diffuseLight <- modulateLight nDotL lColor diffuseColor

  case specularInfo of
    Nothing -> return diffuseLight
    Just (color, exponent) -> do
      nDotH <- blinnPhongNDotH ld pos norm
      nDotHpow <- I.setE I.floatTy $
                  I.powf (I.mkVarExpr nDotH) (I.mkVarExpr exponent)
      specularLight <- modulateLight nDotHpow lColor color
      I.setE I.vector3fTy $
        I.add3f (I.mkVarExpr diffuseLight) $
        I.mkVarExpr specularLight

blinnPhongLighting PointLightTy diffuseColor specularInfo pos norm = do
  -- !FIXME! We might be able to calculate this in the vertex shader
  -- to increase performance
  lightPos <- I.newUniformVar "lightPos" I.vector3fTy
  ld <- I.setE I.vector3fTy $
        I.sub3f (I.mkVarExpr lightPos) (I.mkVarExpr pos)

  lColor <- blinnPhongLightColor

  nDotL <- I.setE I.floatTy $
           I.maxf (I.mkConstf 0) $
           I.dot3f (I.mkVarExpr ld) (I.mkVarExpr norm)

  diffuseLight <- modulateLight nDotL lColor diffuseColor

  case specularInfo of
    Nothing -> return diffuseLight
    Just (color, exponent) -> do
      nDotH <- blinnPhongNDotH ld pos norm
      nDotHpow <- I.setE I.floatTy $
                  I.powf (I.mkVarExpr nDotH) (I.mkVarExpr exponent)
      specularLight <- modulateLight nDotHpow lColor color
      I.setE I.vector3fTy $
        I.add3f (I.mkVarExpr diffuseLight) $
        I.mkVarExpr specularLight
  
blinnPhongLighting x _ _ _ _ = error $ "Lambency.Material (genLighting): Not implemented light type: " ++ show x

genLitFragment :: LightEnum -> Material -> I.ShaderContext i (I.ShaderVar (V3 Float))
genLitFragment lightTy mat = do
  pos <- I.getInput3f "position"
  norm <- I.getInput3f "normal"
  
  diffuseColor <- getDiffuseColor mat
  specularColor <- getSpecularColor mat

  norm' <- handleNormalMaps mat norm

  blinnPhongLighting lightTy diffuseColor specularColor pos norm'

genShadowFragment :: I.ShaderContext i (I.ShaderVar Float)
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

  I.setE I.floatTy $ I.castBoolToFloat $ I.gtf (I.mkVarExpr objDepth) (I.mkVarExpr shdwDepth)

genLitFragShader :: LightEnum -> Material -> I.ShaderCode a b
genLitFragShader lightTy mat = I.ShdrCode $ do

  litFragment <- genLitFragment lightTy mat
  
  ambientColor <- getAmbientColor mat
  finalColor <- I.setE I.vector3fTy $
                I.add3f (I.mkVarExpr litFragment) (I.mkVarExpr ambientColor)

  -- !TODO! materials should be able to define opacity...
  let alpha = I.mkConstf 1.0

  outColor <- I.setE I.vector4fTy $
              I.mkVec4f_31 (I.mkVarExpr finalColor) alpha

  return $ I.addFragmentColor outColor I.emptyO

genShadowedFragShader :: LightEnum -> Material -> I.ShaderCode a b
genShadowedFragShader lightTy mat = I.ShdrCode $ do

  litFragment <- genLitFragment lightTy mat

  shadow <- genShadowFragment

  ambientColor <- getAmbientColor mat
  finalColor <- I.setE I.vector3fTy $
                I.add3f (I.mkVarExpr ambientColor) $
                I.scale3f (I.mkVarExpr litFragment) $
                I.mkVarExpr shadow

  -- !TODO! materials should be able to define opacity...
  let alpha = I.mkConstf 1.0

  outColor <- I.setE I.vector4fTy $
              I.mkVec4f_31 (I.mkVarExpr finalColor) alpha

  return $ I.addFragmentColor outColor I.emptyO

compileMaterial :: Vertex a => Mesh a -> Material -> IO (CompiledMaterial)
compileMaterial mesh mat =
  let lightCombos = [(x, y) | x <- ([minBound..maxBound] :: [LightEnum]), y <- [True, False]]
      genLightShdr p@(ty, True) =
        let vshdr = genLitVertexShader mat
            fshdr = genShadowedFragShader ty mat
        in (\x -> (p, x)) <$> (I.generateOpenGLShader $ I.compileProgram undefined vshdr fshdr)
      genLightShdr p@(ty, False) =
        let vshdr = genLitVertexShader mat
            fshdr = genLitFragShader ty mat
        in (\x -> (p, x)) <$> (I.generateOpenGLShader $ I.compileProgram undefined vshdr fshdr)
  in CompiledLitMaterial . Map.fromList <$> mapM genLightShdr lightCombos

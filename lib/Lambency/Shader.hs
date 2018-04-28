{-# LANGUAGE RecordWildCards #-}
module Lambency.Shader (
  getLightVarName,
  compileMaterial,
  compileUnlitMaterial
) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad

import Data.Maybe

import Lambency.Material
import Lambency.Types

import qualified Lambency.Shader.Expr as I
import qualified Lambency.Shader.Base as I
import qualified Lambency.Shader.Program as I
import qualified Lambency.Shader.Var as I

import Linear.Matrix
import Linear.V3
import Linear.V4

--------------------------------------------------------------------------------

type ShaderCompiler = I.Shader -> IO Shader

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

createMinimalShader :: ShaderCompiler -> IO Shader
createMinimalShader = ($ I.compileProgram vertMinimal fragMinimal)

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
genUnlitVertexShader (MaskedSpriteMaterial {..}) =
  handleUnlitVertex spriteMask spriteMaskMatrix
genUnlitVertexShader (TexturedSpriteMaterial {..}) =
  handleUnlitVertex spriteTexture spriteTextureMatrix
genUnlitVertexShader m =
  error $ concat [ "Lambency.Shader (genLitVertexShader): "
                 , "Cannot generate unlit vertex shader for material: "
                 , show m
                 ]

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
        error $ concat [ "Lambency.Shader (genUnlitFragmentShader): "
                       , "Masked material must have color!"
                       ]
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

  shadowMap <- I.newUniformVar "shadowMap" I.shadow2DTy
  shadowVP <- I.newUniformVar "shadowVP" I.matrix4Ty

  lightPersp <- I.setE I.vector4fTy $
                I.xform4f (I.mkVarExpr shadowVP) $
                I.mkVec4f_31 (I.mkVarExpr pos) (I.mkConstf 1)

  -- !FIXME! Do a better bias here...
  I.assignE lightPersp $
    I.mkVec4f_211
    (I.finishSwizzleV . I._y_ . I._x_ . I.swizzle4D $ I.mkVarExpr lightPersp)
    (I.subf
     (I.finishSwizzleS . I._z_ . I.swizzle4D $ I.mkVarExpr lightPersp)
     (I.mkConstf 0.001))
    (I.finishSwizzleS . I._w_ . I.swizzle4D $ I.mkVarExpr lightPersp)

  I.assignE lightPersp
    $ I.div4f (I.mkVarExpr lightPersp)
    $ I.finishSwizzleS . I._w_ . I.swizzle4D
    $ I.mkVarExpr lightPersp

  I.assignE lightPersp
    $ I.add4f (I.mkVec4f_1111 (I.mkConstf 0.5)
                              (I.mkConstf 0.5)
                              (I.mkConstf 0.5)
                              (I.mkConstf 0.5))
    $ I.scale4f (I.mkVarExpr lightPersp) (I.mkConstf 0.5)

  I.setE I.floatTy $
    I.finishSwizzleS . I._z_ . I.swizzle4D $
    I.shadow2D (I.mkVarExpr shadowMap) $
    I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
    I.mkVarExpr lightPersp

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
  finalColor <- I.setE I.vector3fTy
              $ I.add3f (I.mult3f (I.mkVarExpr ambientColor)
                                  (I.mkVarExpr diffuseColor))
              $ I.scale3f (I.mkVarExpr litFragment)
              $ I.mkVarExpr shadow

  -- !TODO! materials should be able to define opacity...
  let alpha = I.mkConstf 1.0

  outColor <- I.setE I.vector4fTy $
              I.mkVec4f_31 (I.mkVarExpr finalColor) alpha

  return $ I.addFragmentColor outColor I.emptyO

compileMaterial :: ShaderCompiler -> Light -> Material -> Maybe ShadowMap
                -> IO Shader
compileMaterial c light mat Nothing
  | isUnlit mat = compileUnlitMaterial c mat
  | otherwise =
    let vshdr = genLitVertexShader mat
        fshdr = genLitFragShader light mat
    in c $ I.compileProgram vshdr fshdr
compileMaterial c light mat (Just _)
  | isUnlit mat = compileUnlitMaterial c mat
  | otherwise =
    let vshdr = genLitVertexShader mat
        fshdr = genShadowedFragShader light mat
    in c $ I.compileProgram vshdr fshdr

compileUnlitMaterial :: ShaderCompiler -> Material -> IO Shader
compileUnlitMaterial _ NoMaterial =
  error "Lambency.Shader (compileUnlitMaterial): Cannot compile non-material!"
compileUnlitMaterial c MinimalMaterial = createMinimalShader c
compileUnlitMaterial c mat
  | (not.isUnlit) mat = error
                      $ concat [ "Lambency.Shader (compileUnlitMaterial): "
                               , "Material requires light!"
                               ]
  | otherwise = c $ I.compileProgram vshdr fshdr
  where
    vshdr = genUnlitVertexShader mat
    fshdr = genUnlitFragmentShader mat

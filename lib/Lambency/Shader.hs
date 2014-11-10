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
import Control.Applicative

import Data.Maybe (catMaybes)
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4

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

vertMinimal :: I.ShaderCode Vertex3 ()
vertMinimal = I.ShdrCode $ do
  position <- I.getInput3f 0
  mvpMatrix <- I.newUniformVar "mvpMatrix" I.matrix4Ty
  out_pos <- I.setE I.vector4fTy $
             I.xform4f (I.mkVarExpr mvpMatrix) $
             I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)
  let output = I.addVertexPosition out_pos I.emptyO
  return output

{--

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

--}

vertSimple :: I.ShaderCode TVertex3 Vertex2
vertSimple = I.ShdrCode $ do
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

{--

  attribute vec3 position;
  attribute vec3 normal;
  attribute vec2 texCoord;
  
  uniform mat4 mvpMatrix;
  uniform mat4 m2wMatrix;
  
  varying vec3 pos;
  varying vec3 norm;
  varying vec2 uv;
  
  void main() {

    pos = (m2wMatrix * vec4(position, 1.0)).xyz;
    norm = normalize((m2wMatrix * vec4(normal, 0)).xyz);
    uv = texCoord;
  
    gl_Position = mvpMatrix * vec4(position, 1.0);
  }

--}
  
vertStandard :: I.ShaderCode OTVertex3 OTVertex3
vertStandard = I.ShdrCode $ do
  position <- I.getInput3f 0
  normal <- I.getInput3f 1
  texCoord <- I.getInput2f 2

  mvpMatrix <- I.newUniformVar "mvpMatrix" I.matrix4Ty
  m2wMatrix <- I.newUniformVar "m2wMatrix" I.matrix4Ty

  pos <- I.setE I.vector3fTy $
         I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
         I.xform4f (I.mkVarExpr m2wMatrix) $
         I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)
         
  norm <- I.setE I.vector3fTy $
          I.normalize3f $
          I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
          I.xform4f (I.mkVarExpr m2wMatrix) $
          I.mkVec4f_31 (I.mkVarExpr normal) (I.mkConstf 0)

  out_pos <- I.setE I.vector4fTy $
             I.xform4f (I.mkVarExpr mvpMatrix) $
             I.mkVec4f_31 (I.mkVarExpr position) (I.mkConstf 1)

  let output = I.addVertexPosition out_pos $
               I.addCustomOVar pos $
               I.addCustomOVar norm $
               I.addCustomOVar texCoord I.emptyO

  return output
  
--------------------

fragMinimal :: I.ShaderCode a ()
fragMinimal = I.ShdrCode $ return I.emptyO

{--

  varying vec2 uv;

  uniform sampler2D diffuseTex;

  void main() {
    gl_FragColor = texture2D(diffuseTex, uv);
  }

--}

fragSimple :: I.ShaderCode Vertex2 ()
fragSimple = I.ShdrCode $ do
  uv <- I.getInput2f 0
  diffuseTex <- I.newUniformVar "diffuseTex" I.sampler2DTy
  out_color <- I.setE I.vector4fTy $ I.sample2D (I.mkVarExpr diffuseTex) (I.mkVarExpr uv)
  return $ I.addFragmentColor out_color I.emptyO

{--  
  varying vec2 uv;

  uniform sampler2D diffuseTex;
  uniform float alpha;

  void main() {
    gl_FragColor = texture2D(diffuseTex, uv);
    gl_FragColor.a *= alpha;
  }

--}

fragSimpleTrans :: I.ShaderCode Vertex2 ()
fragSimpleTrans = I.ShdrCode $ do
  uv <- I.getInput2f 0

  diffuseTex <- I.newUniformVar "diffuseTex" I.sampler2DTy
  alpha <- I.newUniformVar "alpha" I.floatTy

  texColor <- I.setE I.vector4fTy $ I.sample2D (I.mkVarExpr diffuseTex) (I.mkVarExpr uv)
  out_color <- I.setE I.vector4fTy $ I.mkVec4f_31
               ((I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D) (I.mkVarExpr texColor)) $
               I.multf ((I.finishSwizzleS . I._w_ . I.swizzle4D) (I.mkVarExpr texColor)) (I.mkVarExpr alpha)

  let output = I.addFragmentColor out_color I.emptyO
  return output

{--

  varying vec2 uv;

  uniform sampler2D maskTex;
  uniform float alpha;
  uniform vec3 color;

  ovoid main() {
    float al = alpha * texture2D(maskTex, uv).a;
    gl_FragColor = vec4(color, al);
  }

--}

fragFont :: I.ShaderCode Vertex2 ()
fragFont = I.ShdrCode $ do
  uv <- I.getInput2f 0

  maskTex <- I.newUniformVar "maskTex" I.sampler2DTy
  alpha <- I.newUniformVar "alpha" I.floatTy
  color <- I.newUniformVar "color" I.vector3fTy

  al <- I.setE I.floatTy $
        I.multf (I.mkVarExpr alpha) $
        I.finishSwizzleS . I._w_ . I.swizzle4D $
        I.sample2D (I.mkVarExpr maskTex) (I.mkVarExpr uv)

  out_color <- I.setE I.vector4fTy $ I.mkVec4f_31 (I.mkVarExpr color) (I.mkVarExpr al)

  return $ I.addFragmentColor out_color I.emptyO

{--

  varying vec2 uv;
  varying vec3 norm;
  varying vec3 pos;

  uniform sampler2D diffuseTex;

  uniform vec3 ambient;
  uniform vec3 lightPos;
  uniform vec3 lightDir;
  uniform float lightCosCutoff;

  uniform sampler2DShadow shadowMap;
  uniform mat4 shadowVP;

  float rnd(vec2 co)
  {
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
  }

  // Fix values from their floating point representation
  // to what they should be in the interval [0,255] based
  // on a stochastic dithering pattern where the dithering
  // is done based on the proximity to an integer value in
  // the aforementioned interval.
  float dither(float v, float r) {
    float val = v * 255.0;
    float ival = floor(val);
    float diff = val - ival;
    return (ival + float(r < diff)) / 255.0;
  }

  vec3 dither3(vec3 v, vec2 seed) {
    float r = rnd(seed);
    return vec3(dither(v.x, r), dither(v.y, r), dither(v.z, r));
  }

  float bias(float d) {
    return d - 0.0001;
  }

  void main() {
  
    vec4 lightPersp = shadowVP * vec4(pos, 1);
    lightPersp /= lightPersp.w;
    lightPersp = lightPersp * 0.5 + 0.5;
    lightPersp.z = bias(lightPersp.z);
    float shadow = float(shadow2D(shadowMap, lightPersp.xyz));

    vec3 p2l = pos-lightPos;
    float dist = length(p2l);
    p2l = normalize(p2l);

    float cosToPt = dot(lightDir, p2l);
    float spot = clamp(cosToPt, lightCosCutoff, 1.0);
  
    const vec3 color = vec3(1.0, 1.0, 1.0);
    vec3 lightColor = vec3(0, 0, 0);
    if(spot > 0.0) {
      lightColor += color*(spot/(0.1*dist));
    }

    // diffuse
    lightColor *= max(0.0, dot(-lightDir, norm)) * (1.0 - 0.5*shadow);

    // ambient
    lightColor += ambient;
    vec3 finalColor = lightColor*texture2D(diffuseTex, uv).xyz;
    finalColor = dither3(finalColor, uv);

    gl_FragColor = vec4(finalColor, 1.0);
  }

--}

fragSpotlight :: I.ShaderCode OTVertex3 ()
fragSpotlight = I.ShdrCode $ do
  pos <- I.getInput3f 0
  norm <- I.getInput3f 1
  uv <- I.getInput2f 2

  diffuseTex <- I.newUniformVar "diffuseTex" I.sampler2DTy

  ambient <- I.newUniformVar "ambient" I.vector3fTy
  lightPos <- I.newUniformVar "lightPos" I.vector3fTy
  lightDir <- I.newUniformVar "lightDir" I.vector3fTy
  lightCosCutoff <- I.newUniformVar "lightCosCutoff" I.floatTy

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

  objDepth <- I.setE I.floatTy $
              I.subf (I.finishSwizzleS . I._z_ . I.swizzle4D $ I.mkVarExpr lightPersp) (I.mkConstf 0.0001)
  shdwDepth <- I.setE I.floatTy $
               I.finishSwizzleS . I._x_ . I.swizzle4D $
               I.sample2D (I.mkVarExpr shadowMap) $
               I.finishSwizzleV . I._y_ . I._x_ . I.swizzle4D $
               I.mkVarExpr lightPersp

  -- Ideally we'd like to cast the bool to a float here...
  shadow <- I.setE I.floatTy $ I.mkConstf 0
  I.ifThen (I.gtf (I.mkVarExpr shdwDepth) (I.mkVarExpr objDepth))
    (I.assignE shadow (I.mkConstf 1.0))
    (return ())

  posToLight <- I.setE I.vector3fTy $ I.sub3f (I.mkVarExpr pos) (I.mkVarExpr lightPos)
  distToLight <- I.setE I.floatTy $ I.length3f (I.mkVarExpr posToLight)

  I.assignE posToLight $ I.div3f (I.mkVarExpr posToLight) (I.mkVarExpr distToLight)

  cosToPoint <- I.setE I.floatTy $ I.dot3f (I.mkVarExpr lightDir) (I.mkVarExpr posToLight)
  spot <- I.setE I.floatTy $ I.clampf (I.mkVarExpr cosToPoint) (I.mkVarExpr lightCosCutoff) (I.mkConstf 1)

  let color = I.mkConstVec3f (V3 1 1 1)
  lightColor <- I.setE I.vector3fTy $ I.mkConstVec3f (V3 0 0 0)

  let modLight = I.assignE lightColor $
                 I.add3f (I.mkVarExpr lightColor) $
                 I.scale3f color $
                 I.divf (I.mkVarExpr spot) $
                 I.multf (I.mkConstf 0.1) (I.mkVarExpr distToLight)

  I.ifThen (I.gtf (I.mkVarExpr spot) (I.mkConstf 0)) modLight (return ())

  I.assignE lightColor $
    I.scale3f (I.mkVarExpr lightColor) $
    I.multf (I.maxf (I.mkConstf 0) (I.dot3f (I.neg3f (I.mkVarExpr lightDir)) (I.mkVarExpr norm))) $
    I.subf (I.mkConstf 1) (I.multf (I.mkConstf 0.5) (I.mkVarExpr shadow))

  I.assignE lightColor $ I.add3f (I.mkVarExpr lightColor) (I.mkVarExpr ambient)
  finalColor <- I.setE I.vector3fTy $
                I.mult3f (I.mkVarExpr lightColor) $
                I.finishSwizzleV . I._z_ . I._y_ . I._x_ . I.swizzle4D $
                I.sample2D (I.mkVarExpr diffuseTex) (I.mkVarExpr uv)

  dither3 finalColor uv >>= I.assignE finalColor
  out_color <- I.setE I.vector4fTy $ I.mkVec4f_31 (I.mkVarExpr finalColor) (I.mkConstf 1)

  return $ I.addFragmentColor out_color I.emptyO
  
  where

    dither :: I.ShaderVar Float -> I.ShaderVar Float -> I.ShaderContext i (I.Expr Float)
    dither v r = do
      val <- I.setE I.floatTy $ I.multf (I.mkVarExpr v) (I.mkVarExpr r)
      ival <- I.setE I.floatTy $ I.floorf (I.mkVarExpr val)
      diff <- I.setE I.floatTy $ I.subf (I.mkVarExpr val) (I.mkVarExpr ival)
      return $
        I.divf
        (I.addf
         (I.mkVarExpr ival)
         (I.castBoolToFloat $ I.gtf (I.mkVarExpr r) (I.mkVarExpr diff)))
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

createSimpleShader :: IO (Shader)
createSimpleShader =
  I.generateOpenGLShader $ I.compileProgram texVertex3Ty vertSimple fragSimple

createTransparentShader :: IO (Shader)
createTransparentShader =
  I.generateOpenGLShader $ I.compileProgram texVertex3Ty vertSimple fragSimpleTrans

createFontShader :: IO (Shader)
createFontShader =
  I.generateOpenGLShader $ I.compileProgram texVertex3Ty vertSimple fragFont

createSpotlightShader :: IO (Shader)
createSpotlightShader =
  I.generateOpenGLShader $ I.compileProgram normTexVertex3Ty vertStandard fragSpotlight

createMinimalShader :: IO (Shader)
createMinimalShader =
  I.generateOpenGLShader $ I.compileProgram vertex3Ty vertMinimal fragMinimal

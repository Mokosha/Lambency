module Lambency.Render (
  Renderable(..),
  RenderState, initialRenderState,
  RenderContext,
  emptyRenderActions,
  clearBuffers,
  createBasicRO,
  xformObject,
  performRenderActions,
  addRenderAction,
  addRenderUIAction,
  addClippedRenderAction,
  addTransformedRenderAction,
) where

--------------------------------------------------------------------------------

import Lambency.Camera
import Lambency.Material
import Lambency.Light
import Lambency.Shader
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Vertex

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Monad.State
import Control.Concurrent.STM

import Data.Array.IO
import Data.Array.Storable
import Data.Bits (complement)
import Data.Hashable
import Data.Int
import Data.List (partition)
import qualified Data.Map as Map

import Foreign.Storable
import Foreign.Ptr

import GHC.Exts (groupWith)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Linear hiding (trace, identity)

import System.IO.Unsafe
--------------------------------------------------------------------------------

data RenderStateFlag
  = RenderState'DepthOnly
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data RenderState = RenderState {
  currentRenderXF :: Transform,
  currentRenderVars :: ShaderMap,
  currentRenderFlags :: [RenderStateFlag]
}

type RenderContext = StateT RenderState IO

initialRenderState :: RenderState
initialRenderState = RenderState identity Map.empty []

emptyRenderActions :: RenderActions
emptyRenderActions = RenderActions {
  renderScene = RenderObjects [],
  renderUI = RenderObjects []
}

data ShaderCache = ShaderCache {
  unlitShaders :: Map.Map Int Shader, -- Parameterized by material hashes
  litShaders :: Map.Map Int Shader -- Parameterized by (material, light) hashes
}

shaderCache :: TVar ShaderCache
shaderCache = unsafePerformIO $ newTVarIO (ShaderCache Map.empty Map.empty)

addLitShader :: Int -> Shader -> IO ()
addLitShader h shdr = do
  atomically $ modifyTVar' shaderCache $ \cache ->
    cache { litShaders = Map.insert h shdr (litShaders cache) }

addUnlitShader :: Int -> Shader -> IO ()
addUnlitShader h shdr = do
  atomically $ modifyTVar' shaderCache $ \cache ->
    cache { unlitShaders = Map.insert h shdr (unlitShaders cache) }

lookupLitShader :: Material -> Light -> Maybe ShadowMap -> IO (Shader)
lookupLitShader mat light shadowmap = do
  let litHash = hash (mat, light, (\_ -> "__has_shadowmap__") <$> shadowmap)
  cache <- readTVarIO shaderCache
  let litShdrs = litShaders cache
  case Map.lookup litHash litShdrs of
    Just s -> return s
    Nothing -> do
      putStrLn $ "Compiling lit material..."
      -- putStrLn $ "Compiling lit material: " ++ show mat
      shdr <- compileMaterial light mat shadowmap
      addLitShader litHash shdr
      return shdr

lookupUnlitShader :: Material -> IO (Shader)
lookupUnlitShader mat = do
  let minimalHash = hash mat
  cache <- readTVarIO shaderCache
  let unlitShdrs = unlitShaders cache
  case Map.lookup minimalHash unlitShdrs of
    Just s -> return s
    Nothing -> do
      putStrLn $ "Compiling unlit material..."
      -- putStrLn $ "Compiling unlit material: " ++ show mat
      shdr <- compileUnlitMaterial mat
      addUnlitShader minimalHash shdr
      return shdr

lookupMinimalShader :: IO (Shader)
lookupMinimalShader = lookupUnlitShader MinimalMaterial

clearBuffers :: IO ()
clearBuffers = GL.clear [GL.ColorBuffer, GL.DepthBuffer, GL.StencilBuffer]

ptrsize :: (Storable a) => [a] -> GL.GLsizeiptr
ptrsize [] = toEnum 0
ptrsize xs = toEnum $ length xs * (sizeOf $ head xs)

setupBuffer :: (Storable a) => GL.BufferTarget -> [a] -> IO ( GL.BufferObject )
setupBuffer tgt xs = do
  buf <- GL.genObjectName
  GL.bindBuffer tgt GL.$= (Just buf)
  varr <- newListArray (0, length xs - 1) xs
  withStorableArray varr (\ptr -> GL.bufferData tgt GL.$= (ptrsize xs, ptr, GL.StaticDraw))
  return buf

createBasicRO :: (Vertex a) => [a] -> [Int32] -> Material -> IO (RenderObject)

-- If there's no vertices, then there's nothing to render...
createBasicRO [] _ _ = do
  return $ RenderObject {
    material = NoMaterial,
    render = \_ _ -> return (),
    flags = []
  }

createBasicRO verts@(v:_) idxs mat =
  let
    bindShaderVertexAttributes :: Shader -> IO ()
    bindShaderVertexAttributes shdr = let

      -- Lookup the location for the given attribute name in the shader program
      lu :: String -> GL.AttribLocation
      lu name =
        case Map.lookup name (getShaderVars shdr) of
           Nothing -> GL.AttribLocation (-1)
           Just var -> case var of
             Uniform _ _ -> GL.AttribLocation (-1)
             Attribute _ loc -> loc
      in do
        mapM_ (\(loc, desc) -> GL.vertexAttribPointer loc GL.$= (GL.ToFloat, desc)) $
          zip (map lu $ getAttribNames v) (vertexAttributesToOpenGL . getVertexAttributes $ v)

    -- Takes as input an array of vertices and indices and returns a function
    -- that renders the vertices for a given shader and shader variable mapping
    createRenderFunc :: GL.BufferObject -> GL.BufferObject ->
                        GL.NumArrayIndices -> Shader -> ShaderMap -> IO ()
    createRenderFunc vbo ibo nIndices shdr shdrmap = do

        -- Set all uniforms for this shader
        let shdrVars = getShaderVars shdr
        mapM_ (\(k, sv) -> case Map.lookup k shdrVars of
          Nothing -> return ()
          Just shdrVar -> setUniformVar shdrVar sv) (Map.toList shdrmap)

        -- Bind appropriate buffers
        GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
        bindShaderVertexAttributes shdr

        GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

        -- Render
        GL.drawElements GL.Triangles nIndices GL.UnsignedInt nullPtr

  in do
    vbo <- setupBuffer GL.ArrayBuffer verts
    ibo <- setupBuffer GL.ElementArrayBuffer idxs
    return $ RenderObject {
      -- materialVars = mat,
      -- !FIXME! These should be the variables extracted from the
      -- material after being compiled into shader code...
      material = mat,
      render = createRenderFunc vbo ibo $ fromIntegral (length idxs),
      flags = []
    }

class Renderable a where
  createRenderObject :: a -> Material -> IO (RenderObject)

updateMatrices :: String -> ShaderValue -> ShaderValue -> ShaderValue
updateMatrices "mvpMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices "m2wMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices _ v1 _ = v1

groupROsByMaterial :: [RenderObject] -> [[RenderObject]]
groupROsByMaterial = groupWith (hash . material)

renderROsWithShader :: [RenderObject] -> Shader -> ShaderMap -> IO ()
renderROsWithShader ros shdr shdrmap = do
  beforeRender shdr
  flip mapM_ ros $ \ro -> do
    let matVars = materialShaderVars $ material ro
    (render ro) shdr $ Map.union matVars shdrmap
  afterRender shdr

renderLitROs :: [RenderObject] -> Light -> Maybe ShadowMap -> ShaderMap -> IO ()
renderLitROs ros light sm shdrmap =
  let vars = Map.union shdrmap $ getLightShaderVars light
      renderWithShdr [] = return ()
      renderWithShdr rs@(ro : _) = do
        let m = material ro
        shdr <- lookupLitShader m light sm
        -- !FIXME! Make sure that the ro's vertex type has the same attributes that
        -- the shader expects
        renderROsWithShader rs shdr vars
  in mapM_ renderWithShdr $ groupROsByMaterial ros

renderUnlitROs :: [RenderObject] -> ShaderMap -> IO ()
renderUnlitROs ros shdrmap =
  let renderWithShdr [] = return ()
      renderWithShdr rs@(ro : _) = do
        let m = material ro
        shdr <- lookupUnlitShader m
        -- !FIXME! Make sure that the ro's vertex type has the same attributes that
        -- the shader expects
        renderROsWithShader rs shdr shdrmap
  in mapM_ renderWithShdr $ groupROsByMaterial ros

appendCamXForm :: Camera -> ShaderMap -> ShaderMap
appendCamXForm cam sm' =
  let sm :: ShaderMap
      sm = Map.singleton "mvpMatrix" (Matrix4Val $ getViewProjMatrix cam)
  in Map.unionWithKey updateMatrices sm' sm

appendXform :: Transform -> ShaderMap -> ShaderMap
appendXform xform sm' = let
  matrix :: M44 Float
  matrix = xform2Matrix xform

  sm :: ShaderMap
  sm = Map.fromList [
    ("mvpMatrix", Matrix4Val matrix),
    ("m2wMatrix", Matrix4Val matrix)]
  in
   Map.unionWithKey updateMatrices sm sm'

xformObject :: Transform -> RenderObject -> RenderObject
xformObject xform ro =
  ro { render = \shr -> (render ro) shr . appendXform xform }

place :: Camera -> RenderObject -> RenderObject
place cam ro =
  ro { render = \shr -> (render ro) shr . appendCamXForm cam }

divideAndRenderROs :: [RenderObject] -> Camera -> Maybe Light -> RenderContext ()
divideAndRenderROs [] _ _ = return ()
divideAndRenderROs ros cam light = let
  placeAndDivideObjs :: RenderContext ([RenderObject], [RenderObject])
  placeAndDivideObjs = do
    xf <- currentRenderXF <$> get
    return $
      partition (\ro -> Transparent `elem` (flags ro)) $
      -- sortBy (\ro1 ro2 -> compare (camDist ro1) (camDist ro2)) $
      map (place cam . xformObject xf) ros

  renderFn :: [RenderObject] -> (Maybe GL.ComparisonFunction) -> RenderContext ()
  renderFn [] _ = return ()
  renderFn rs d = do
    vars <- currentRenderVars <$> get
    liftIO $ do
      GL.depthFunc GL.$= d
      case light of
        Nothing -> renderUnlitROs rs vars
        Just l -> do
          case Map.lookup "shadowMap" vars of
            Just (ShadowMapVal sm) -> renderLitROs rs l (Just sm) vars
            _ -> renderLitROs rs l Nothing vars
  in do
    addRenderVar "eyePos" (Vector3Val $ getCamPos cam)

    st <- get
    (trans, opaque) <- placeAndDivideObjs
    let currentFlags = currentRenderFlags st

    case RenderState'DepthOnly `elem` currentFlags of
      True -> liftIO $ do
        GL.depthFunc GL.$= (Just GL.Lequal)
        shdr <- lookupMinimalShader
        renderROsWithShader opaque shdr (currentRenderVars st)
      False -> do
        renderFn opaque (Just GL.Lequal)
        renderFn (reverse trans) Nothing

withRenderFlag :: RenderStateFlag -> RenderContext () -> RenderContext ()
withRenderFlag flag action = do
  st <- get
  withStateT (\s -> s { currentRenderFlags = flag : (currentRenderFlags s) }) action
  put st

addRenderVar :: String -> ShaderValue -> RenderContext ()
addRenderVar name val = do
  st <- get
  let vars = currentRenderVars st
  put $ st { currentRenderVars = Map.insert name val vars }

mkLightCam :: LightType -> Camera
-- !FIXME! light fov should be based on spotlight cos cutoff...
mkLightCam (SpotLight dir' pos' cosCutoff') =
  let LightVar (_, Vector3Val pos) = pos'
      LightVar (_, Vector3Val dir) = dir'
      LightVar (_, FloatVal cco) = cosCutoff'
      ang = acos cco
  in mkPerspCamera pos dir (V3 0 1 0) (2 * ang) 1 0.1 500.0

mkLightCam c = error $ "Lambency.Render (mkLightCam): Camera for light type unsupported: " ++ show c

renderLight :: RenderAction -> Camera -> Maybe Light -> RenderContext ()
renderLight act cam (Just (Light params lightTy (Just (shadowMap, _)))) = do
  (lcam, shadowVP) <- liftIO $ do
    bindRenderTexture $ getShadowmapTexture shadowMap
    clearBuffers
    -- Right now the MVP matrix of each object is for the main camera, so
    -- we need to replace it with the combination from the model matrix
    -- and the shadow VP...
    let lightCam = mkLightCam lightTy
    return (lightCam, Matrix4Val $ getViewProjMatrix lightCam)
  withRenderFlag RenderState'DepthOnly $ renderLight act lcam Nothing
  liftIO clearRenderTexture
  addRenderVar "shadowMap" (ShadowMapVal shadowMap)
  addRenderVar "shadowVP" shadowVP
  renderLight act cam (Just $ Light params lightTy Nothing)

-- If there's no clipped geometry then we're not rendering anything...
renderLight (RenderClipped (RenderObjects []) _) _ _ = return ()
renderLight (RenderClipped clip action) camera light = do
  liftIO $ do
    -- Disable stencil test, and drawing into the color and depth buffers
    -- Enable writing to stencil buffer, and always write to it.
    GL.stencilTest GL.$= GL.Enabled
    GL.depthMask GL.$= GL.Disabled
    GL.colorMask GL.$= (GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled)
    GL.stencilFunc GL.$= (GL.Never, 1, complement 0)
    GL.stencilOp GL.$= (GL.OpReplace, GL.OpKeep, GL.OpKeep)

    -- Draw our clip
    GL.stencilMask GL.$= (complement 0)
    GL.clear [GL.StencilBuffer]

  renderLight clip camera light

  -- Enable drawing to the color and depth buffers, and disable drawing
  -- to the stencil buffer
  liftIO $ do
    GL.depthMask GL.$= GL.Enabled
    GL.colorMask GL.$= (GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled)
    GL.stencilMask GL.$= 0

    -- There is a one in the stencil buffer where there was clipped geometry.
    -- To only render where we have clipped stuff, we should set the stencil func
    -- to test for equality
    GL.stencilFunc GL.$= (GL.Equal, 1, complement 0)

  -- Draw our clipped stuff
  renderLight action camera light  -- !

  -- Finally, disable the stencil test
  liftIO $ GL.stencilTest GL.$= GL.Disabled

renderLight (RenderTransformed xf act) camera light = do
  oldState <- get
  withStateT (\st -> st { currentRenderXF = transform xf (currentRenderXF st) }) $
    renderLight act camera light
  put oldState

renderLight (RenderObjects ros') camera light = do
  let ros = filter (not . elem Text . flags) ros'
  divideAndRenderROs ros camera light

renderLight (RenderCons act1 act2) camera light = do
  renderLight act1 camera light
  renderLight act2 camera light

renderText :: RenderAction -> Camera -> RenderContext ()
renderText (RenderObjects objs) camera = do
  let ros = filter ((elem Text) . flags) objs
  divideAndRenderROs ros camera Nothing

renderText (RenderTransformed xf act) camera = do
  oldState <- get
  withStateT (\st -> st { currentRenderXF = transform xf (currentRenderXF st) }) $
    renderText act camera
  put oldState
renderText (RenderClipped _ act) camera = do
  -- !FIXME! ignoring clipped text...
  -- liftIO $ putStrLn "Warning: Unable to render clipped text!"
  renderText act camera
renderText (RenderCons act1 act2) camera = renderText act1 camera >> renderText act2 camera

performRenderActions :: [Light] -> Camera -> RenderActions -> RenderContext ()
performRenderActions lights camera actions = do
  case lights of
    [] -> renderLight (renderScene actions) camera Nothing
    _ -> mapM_ (\l -> renderLight (renderScene actions) camera (Just l)) lights
  cam <- liftIO $ do
    -- !FIXME! This should be stored in the camera...? Why
    -- are we querying IO here? =(
    (Just win) <- GLFW.getCurrentContext
    (szx, szy) <- GLFW.getWindowSize win
    -- Setup ortho camera and nolight for ui crap
    return $
      mkOrthoCamera
      zero (negate localForward) localUp          -- The three axes
      0 (fromIntegral szx) (fromIntegral szy) 0   -- Match the screen size
      0.01 50.0                                   -- The near and far planes
  renderLight (renderUI actions) cam Nothing
  renderText (renderUI actions) cam

type AppendObjectFn = RenderObject -> RenderAction -> RenderAction

appendObj :: AppendObjectFn
appendObj obj (RenderObjects objs) = RenderObjects (obj : objs)
appendObj obj (RenderClipped clip act) = RenderClipped clip (appendObj obj act)
appendObj obj (RenderCons act1 act2) = RenderCons act1 (appendObj obj act2)
appendObj obj (RenderTransformed xf act) = RenderTransformed xf (appendObj obj act)

appendSceneWith :: AppendObjectFn -> RenderObject -> RenderActions -> RenderActions
appendSceneWith fn obj acts = acts { renderScene = fn obj (renderScene acts) }

appendUIWith :: AppendObjectFn -> RenderObject -> RenderActions -> RenderActions
appendUIWith fn obj acts = acts { renderUI = fn obj (renderUI acts) }

embedNewAction :: RenderAction -> RenderAction -> RenderAction
embedNewAction old new = RenderCons old $ RenderCons new $ RenderObjects []

createClippedAction :: RenderAction -> RenderAction -> RenderAction -> RenderAction
createClippedAction old clip draw = embedNewAction old $ RenderClipped clip draw

-- !FIXME! This would probably be cleaner with lenses
createClippedActions :: RenderActions -> RenderActions -> RenderActions -> RenderActions
createClippedActions old clip draw =
  RenderActions { renderScene = createClippedAction (rs old) (rs clip) (rs draw),
                  renderUI = createClippedAction (ru old) (ru clip) (ru draw) }
  where
    rs = renderScene
    ru = renderUI

addClippedRenderAction :: GameMonad () -> GameMonad a -> GameMonad a
addClippedRenderAction clip draw = do
  -- Get the existing actions
  actions <- lift get

  -- Put empty actions
  lift $ put emptyRenderActions

  -- Get the actions that render the clip
  clipActions <- clip >> lift get

  -- Put back empty actions again
  lift $ put emptyRenderActions

  -- Get the actions that render our clipped geometry
  result <- draw
  renderActions <- lift get

  -- Finally, replace our existing actions with clipped actions
  lift $ put $ createClippedActions actions clipActions renderActions
  return result

createTransformedAction :: Transform -> RenderAction -> RenderAction -> RenderAction
createTransformedAction xf old new = embedNewAction old $ RenderTransformed xf new

createTransformedActions :: Transform -> RenderActions -> RenderActions -> RenderActions
createTransformedActions xf old new =
  RenderActions { renderScene = createTransformedAction xf (rs old) (rs new),
                  renderUI = createTransformedAction xf (ru old) (ru new) }
  where
    rs = renderScene
    ru = renderUI

addTransformedRenderAction :: Transform -> GameMonad a -> GameMonad a
addTransformedRenderAction xf prg = do
  -- Get the actions so far
  actions <- lift get

  -- Put empty actions
  lift $ put emptyRenderActions

  -- Run the actions
  result <- prg

  -- Get the resulting actions
  xfActions <- lift get

  -- Put the transformed actions back
  lift $ put $ createTransformedActions xf actions xfActions
  return result

addRenderAction :: Transform -> RenderObject -> GameMonad ()
addRenderAction xf ro = lift $ modify $ appendSceneWith appendObj (xformObject xf ro)

addRenderUIAction :: V2 Float -> RenderObject -> GameMonad ()
addRenderUIAction (V2 x y) ro = lift $
  modify $ appendUIWith appendObj (xformObject xf ro)
  where
    xf = translate (V3 x y (-1)) identity

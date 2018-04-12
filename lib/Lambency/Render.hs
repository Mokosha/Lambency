module Lambency.Render (
  render,
  createRenderObject,
  addRenderAction,
  addRenderUIAction,
  addClippedRenderAction,
  addTransformedRenderAction,
) where

--------------------------------------------------------------------------------
import qualified Codec.Picture as JP

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif
import Control.Concurrent.STM
import Control.Monad (unless)
import Control.Monad.RWS.Strict
import Control.Monad.State

import Data.Array.IO
import Data.Array.Storable
import Data.Array.Unboxed hiding (indices)
import Data.Bits (complement)
import Data.Function (on)
import Data.Hashable
import Data.Int
import Data.List (partition, sortBy)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as Vector
import Data.Word

import Foreign.Storable
import Foreign.Ptr

import GHC.Exts (groupWith)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Lambency.Camera
import Lambency.Material
import Lambency.Mesh
import Lambency.Light
import Lambency.Shader
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Vertex

import Linear hiding (trace, identity)

import System.Directory
import System.Exit
import System.IO.Unsafe
--------------------------------------------------------------------------------

data RenderStateFlag
  = RenderState'DepthOnly
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data RenderState = RenderState {
  nextStencilValue :: Int,
  currentRenderXF :: Transform,
  currentRenderVars :: UniformMap,
  currentRenderFlags :: [RenderStateFlag]
}

type RenderContext = StateT RenderState IO
type TransparentRenderAction = (Maybe Int, RenderObject)

initialRenderState :: RenderState
initialRenderState = RenderState 1 identity Map.empty []

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
  withStorableArray varr $
    \ptr -> GL.bufferData tgt GL.$= (ptrsize xs, ptr, GL.StaticDraw)
  return buf

createBasicRO :: (Vertex a) => [a] -> [Int32] -> Material -> IO (RenderObject)

-- If there's no vertices, then there's nothing to render...
createBasicRO [] _ _ = do
  return $ RenderObject {
    material = NoMaterial,
    objectVars = Map.empty,
    renderObject = \_ _ -> return (),
    flags = [],
    unloadRenderObject = return ()
  }

createBasicRO verts@(v:_) idxs mat =
  let
    bindShaderVertexAttributes :: Shader -> IO ()
    bindShaderVertexAttributes (Shader _ vars) = let

      -- Lookup the location for the given attribute name in the shader program
      lu :: String -> GL.AttribLocation
      lu name =
        case Map.lookup name vars of
           Nothing -> GL.AttribLocation (-1)
           Just var -> case var of
             Uniform _ _ -> GL.AttribLocation (-1)
             Attribute _ (OpenGLAttributeBinding loc) -> loc

      locattrib = filter ((/= GL.AttribLocation(-1)) . fst) $
                  zip (map lu $ getAttribNames v)
                      (vertexAttributesToOpenGL . getVertexAttributes $ v)
      in flip mapM_ locattrib $ \(loc, desc) ->
          GL.vertexAttribPointer loc GL.$= (GL.ToFloat, desc)

    -- Takes as input an array of vertices and indices and returns a function
    -- that renders the vertices for a given shader and shader variable mapping
    createRenderFunc :: GL.BufferObject -> GL.BufferObject ->
                        GL.NumArrayIndices -> Shader -> UniformMap -> IO ()
    createRenderFunc vbo ibo nIndices shdr@(Shader _ shdrVars) shdrmap = do

        -- Set all uniforms for this shader
        foldM_ setUniformVar 0 $ Map.elems $ setUniformVals shdrmap shdrVars

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
      objectVars = Map.empty,
      renderObject = createRenderFunc vbo ibo $ fromIntegral (length idxs),
      flags = [],
      unloadRenderObject = GL.deleteObjectName vbo >> GL.deleteObjectName ibo
    }

createRenderObject :: Vertex a => Mesh a -> Material -> IO RenderObject
createRenderObject m = createBasicRO (vertices m) (indices m)

updateMatrices :: String -> ShaderValue -> ShaderValue -> ShaderValue
updateMatrices "mvpMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices "m2wMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices _ v1 _ = v1

groupROsByMaterial :: [RenderObject] -> [[RenderObject]]
groupROsByMaterial = groupWith (hash . material)

renderROsWithShader :: [RenderObject] -> Shader -> UniformMap -> IO ()
renderROsWithShader ros shdr shdrmap = do
  beforeRender shdr
  flip mapM_ ros $ \ro -> do
    let matVars = materialShaderVars $ material ro
        uniforms = Map.union (objectVars ro) $ Map.union matVars shdrmap
    renderObject ro shdr uniforms
  afterRender shdr

renderLitROs :: [RenderObject] -> Light -> Maybe ShadowMap -> UniformMap -> IO ()
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

renderUnlitROs :: [RenderObject] -> UniformMap -> IO ()
renderUnlitROs ros shdrmap =
  let renderWithShdr [] = return ()
      renderWithShdr rs@(ro : _) = do
        let m = material ro
        shdr <- lookupUnlitShader m
        -- !FIXME! Make sure that the ro's vertex type has the same attributes that
        -- the shader expects
        renderROsWithShader rs shdr shdrmap
  in mapM_ renderWithShdr $ groupROsByMaterial ros

appendCamXForm :: Camera -> UniformMap -> UniformMap
appendCamXForm cam sm' =
  let sm = Map.singleton "mvpMatrix" (Matrix4Val $ getViewProjMatrix cam)
  in Map.unionWithKey updateMatrices sm' sm

appendXform :: Transform -> UniformMap -> UniformMap
appendXform xform sm' = let
  matrix :: M44 Float
  matrix = xform2Matrix xform

  sm :: UniformMap
  sm = Map.fromList [ ("mvpMatrix", Matrix4Val matrix)
                    , ("m2wMatrix", Matrix4Val matrix)
                    ]
  in
   Map.unionWithKey updateMatrices sm' sm

xformObject :: Transform -> RenderObject -> RenderObject
xformObject xform ro = ro { objectVars = appendXform xform (objectVars ro) }

place :: Camera -> RenderObject -> RenderObject
place cam ro = ro { objectVars = appendCamXForm cam (objectVars ro) }

divideAndRenderROs :: [RenderObject] -> Camera -> Maybe Light
                   -> RenderContext [TransparentRenderAction]
divideAndRenderROs [] _ _ = return []
divideAndRenderROs ros cam light = do
  addRenderVar "eyePos" (Vector3Val $ getCamPos cam)

  st <- get
  let vars = currentRenderVars st
      xf = currentRenderXF st
      (trans, opaque) =
        partition (\ro -> Transparent `elem` (flags ro)) $
        map (place cam . xformObject xf) ros

  liftIO $ GL.depthFunc GL.$= (Just GL.Lequal)
  case RenderState'DepthOnly `elem` (currentRenderFlags st) of
    True -> liftIO $ do
      shdr <- lookupMinimalShader
      renderROsWithShader opaque shdr vars
      return []
    False -> do
      liftIO $ case light of
        Nothing -> renderUnlitROs opaque vars
        Just l -> do
          case Map.lookup "shadowMap" vars of
            Just (ShadowMapVal _ sm) -> renderLitROs opaque l (Just sm) vars
            _ -> renderLitROs opaque l Nothing vars
      return $ fmap (\x -> (Nothing, x)) trans

withRenderFlag :: RenderStateFlag -> RenderContext a -> RenderContext a
withRenderFlag flag action = do
  st <- get
  result <- flip withStateT action $ \s ->
    s { currentRenderFlags = flag : (currentRenderFlags s) }
  put st
  return result

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

mkLightCam c =
  error $ concat [
    "Lambency.Render (mkLightCam): ",
    "Camera for light type unsupported: ",
    show c
  ]

-- TODO: If we have two nested stencil operations, then the second will take
-- precedence. Ideally, we'd like to clip against a range in the stencil buffer,
-- but this will need to work with messing with the mask. For right now I think
-- this is OK.
updateTransparentStencil :: Int -> TransparentRenderAction
                         -> TransparentRenderAction
updateTransparentStencil x (Nothing, ro) = (Just x, ro)
updateTransparentStencil _             y = y

writeDebugShadowMap :: ShadowMap -> IO ()
writeDebugShadowMap (ShadowMap (Texture _ _)) =
  error "Depth maps should only be generated by rendering!"
writeDebugShadowMap (ShadowMap tex) = do
  let depthfile = "depth.png"
      (V2 shadowMapWidth shadowMapHeight) = textureSize tex
  exists <- doesFileExist depthfile
  unless exists $ do
    GL.flush
    arr <- newArray_ ((0, 0),
                      ( fromIntegral $ shadowMapWidth - 1
                      , fromIntegral $ shadowMapHeight - 1
                      )
                     )
    withStorableArray arr (\ptr -> do
      GL.readPixels (GL.Position 0 0)
        (GL.Size (fromIntegral shadowMapWidth) (fromIntegral shadowMapHeight))
        (GL.PixelData GL.DepthComponent GL.Float ptr)
      GL.flush)
    farr <- (freeze :: StorableArray (Int, Int) Float
                    ->    IO (UArray (Int, Int) Float)) arr
    let img = JP.generateImage
              (\x y -> (round :: Float -> Word16) $ 65535 * (farr ! (x, y)))
              (fromIntegral shadowMapWidth) (fromIntegral shadowMapHeight)

        smallest :: Integer
        smallest = fromIntegral $ Vector.minimum (JP.imageData img)

        largest :: Integer
        largest = fromIntegral $ Vector.maximum (JP.imageData img)

        modulate :: Word16 -> Word16
        modulate x =
          fromIntegral $
          (65535 * ((fromIntegral :: Integral a => a -> Integer) x - smallest))
          `div`
          (largest - smallest)

    JP.writePng depthfile $ JP.pixelMap modulate img

resetDefaultFramebuffer :: ShadowMap -> IO ()
resetDefaultFramebuffer sm = do
  -- For debugging, set to False:
  unless True $ writeDebugShadowMap sm

  GL.bindFramebuffer GL.Framebuffer GL.$= GL.defaultFramebufferObject
  (Just m) <- GLFW.getCurrentContext
  (szx, szy) <- GLFW.getFramebufferSize m
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

renderLight :: RenderAction -> Camera -> Maybe Light
            -> RenderContext [TransparentRenderAction]
renderLight act cam (Just (Light params lightTy (Just (shadowMap, _)))) = do
  (lcam, shadowVP) <- liftIO $ do
    bindRenderTexture $ getShadowmapTexture shadowMap
    clearBuffers
    -- Right now the MVP matrix of each object is for the main camera, so
    -- we need to replace it with the combination from the model matrix
    -- and the shadow VP...
    let lightCam = mkLightCam lightTy
    return (lightCam, Matrix4Val $ getViewProjMatrix lightCam)

  -- Render into the depth map. If we rendered anything labeled transparent,
  -- then that's an error, and we'll crash here on an invalid pattern match.
  [] <- withRenderFlag RenderState'DepthOnly $ renderLight act lcam Nothing

  liftIO $ resetDefaultFramebuffer shadowMap
  addRenderVar "shadowMap" (ShadowMapVal undefined shadowMap)
  addRenderVar "shadowVP" shadowVP
  renderLight act cam (Just $ Light params lightTy Nothing)

-- If there's no clipped geometry then we're not rendering anything...
renderLight (RenderClipped (RenderObjects []) _) _ _ = return []
renderLight (RenderClipped clip action) camera light = do
  -- Get and increment the stencil value we need
  -- TODO: Check that stencilVal doesn't exceed what we're allowed to use
  st <- get
  let stencilVal = nextStencilValue st
  put $ st { nextStencilValue = (1 + stencilVal) }

  liftIO $ do
    -- Disable stencil test, and drawing into the color and depth buffers
    -- Enable writing to stencil buffer, and always write to it.
    GL.stencilTest GL.$= GL.Enabled
    GL.depthMask GL.$= GL.Disabled
    GL.colorMask GL.$= (GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled)
    GL.stencilFunc GL.$= (GL.Never, fromIntegral stencilVal, complement 0)
    GL.stencilOp GL.$= (GL.OpReplace, GL.OpKeep, GL.OpKeep)

    -- Draw our clip
    GL.stencilMask GL.$= (complement 0)

  clipResult <- renderLight clip camera light
  case clipResult of
    [] -> return ()
    _ -> liftIO
         $ putStrLn "Warning: Transparent geometry not rendered into clip"

  -- Enable drawing to the color and depth buffers, and disable drawing
  -- to the stencil buffer
  liftIO $ do
    GL.depthMask GL.$= GL.Enabled
    GL.colorMask GL.$= (GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled)
    GL.stencilMask GL.$= 0

    -- We drew a 'stencilVal' in the stencil buffer where there was clipped
    -- geometry. To only render where we have clipped stuff, we should set the
    -- stencil func to test for equality
    GL.stencilFunc GL.$= (GL.Equal, fromIntegral stencilVal, complement 0)

  -- Draw our clipped stuff
  results <- renderLight action camera light  -- !

  -- Finally, disable the stencil test
  liftIO $ GL.stencilTest GL.$= GL.Disabled

  return $ updateTransparentStencil stencilVal <$> results

renderLight (RenderTransformed xf act) camera light = do
  oldState <- get
  result <- flip withStateT (renderLight act camera light) $ \st ->
    st { currentRenderXF = transform xf (currentRenderXF st) }
  put oldState
  return result

renderLight (RenderObjects ros') camera light =
  let ros = filter (not . elem Text . flags) ros'
   in divideAndRenderROs ros camera light

renderLight (RenderCons act1 act2) camera light = do
  r1 <- renderLight act1 camera light
  r2 <- renderLight act2 camera light
  return $ r1 ++ r2

renderText :: RenderAction -> Camera
           -> RenderContext [TransparentRenderAction]
renderText (RenderObjects objs) camera = do
  let ros = filter ((elem Text) . flags) objs
  divideAndRenderROs ros camera Nothing

renderText (RenderTransformed xf act) camera = do
  oldState <- get
  result <- flip withStateT (renderText act camera) $ \st ->
    st { currentRenderXF = transform xf (currentRenderXF st) }
  put oldState
  return result

renderText (RenderClipped _ act) c = do
  -- !FIXME! ignoring clipped text...
  -- liftIO $ putStrLn "Warning: Unable to render clipped text!"
  renderText act c

renderText (RenderCons act1 act2) c = do
  r1 <- renderText act1 c
  r2 <- renderText act2 c
  return $ r1 ++ r2

sortByCamDist :: [TransparentRenderAction] -> [TransparentRenderAction]
sortByCamDist = reverse . sortBy (compare `on` zFromCam)
  where
    zFromCam :: TransparentRenderAction -> Float
    zFromCam (_, ro) =
      let (Just (Matrix4Val mvp)) = Map.lookup "mvpMatrix" $ objectVars ro
          V4 _ _ z _ = mvp !* (V4 0 0 0 1)
       in z

sortAndRenderTransparent :: Maybe Light -> [TransparentRenderAction]
                         -> RenderContext ()
sortAndRenderTransparent _ [] = return ()
sortAndRenderTransparent light acts = do
  liftIO $ GL.depthFunc GL.$= Nothing
  let ros = collapse <$> sortByCamDist acts
  vars <- currentRenderVars <$> get
  case light of
    Nothing -> liftIO $ renderUnlitROs ros vars
    Just l  -> liftIO $ renderLitROs ros l Nothing vars

  where
    collapse :: TransparentRenderAction -> RenderObject
    collapse (Nothing, ro) = ro
    collapse (Just stencil, ro) = ro {
      renderObject = \shdr shdrMap -> do
         -- Enable drawing to the color buffers, and disable drawing to the stencil
         -- and depth buffers
         GL.stencilTest GL.$= GL.Enabled
         GL.depthMask GL.$= GL.Disabled
         GL.colorMask GL.$=
           (GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled)
         GL.stencilMask GL.$= 0

         -- We drew a 'stencilVal' in the stencil buffer where there was clipped
         -- geometry. To only render where we have clipped stuff, we should set
         -- the stencil func to test for equality
         GL.stencilFunc GL.$= (GL.Equal, fromIntegral stencil, complement 0)
         renderObject ro shdr shdrMap

         -- Finally, disable the stencil test
         GL.stencilTest GL.$= GL.Disabled
      }

performRenderActions :: [Light] -> Camera -> RenderActions -> RenderContext ()
performRenderActions lights camera actions = do
  liftIO $ GL.clear [GL.StencilBuffer]
  case lights of
    [] -> renderLight (renderScene actions) camera Nothing
          >>= sortAndRenderTransparent Nothing
    _ -> do
      trans <- forM lights $ renderLight (renderScene actions) camera . Just
      forM_ lights $ \l -> sortAndRenderTransparent (Just l) (head trans)
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
  transUI <- renderLight (renderUI actions) cam Nothing
  transText <- renderText (renderUI actions) cam
  sortAndRenderTransparent Nothing $ transUI ++ transText

render :: GLFW.Window -> [Light] -> Camera -> RenderActions -> IO ()
render win lights cam acts = do
  -- !FIXME! This should be moved to the camera...
  GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
  clearBuffers
  let renderPrg = performRenderActions lights cam acts
  result <- evalStateT renderPrg initialRenderState
  GL.flush
  GLFW.swapBuffers win

  GL.get GL.errors >>= foldM (\() e -> print e >> exitFailure) ()
  return result

-- !FIXME! This would probably be cleaner with lenses
createClippedActions :: RenderActions -> RenderActions -> RenderActions
createClippedActions clip draw =
  RenderActions { renderScene = RenderClipped (rs clip) (rs draw),
                  renderUI = RenderClipped (ru clip) (ru draw) }
  where
    rs = renderScene
    ru = renderUI

addClippedRenderAction :: GameMonad b -> (b -> GameMonad a) -> GameMonad a
addClippedRenderAction clip drawWithClip = GameMonad . RWST $ \cfg input -> do
  -- Get the actions that render the clip
  (clipResult, clipInput, (clipActions, clipRenderActions)) <-
    runRWST (nextFrame clip) cfg input

  -- Get the actions that render our clipped geometry
  (result, finalInput, (finalActions, finalRenderActions)) <-
    runRWST (nextFrame $ drawWithClip clipResult) cfg clipInput

  -- Return with clipped actions
  return (result, finalInput,
          (clipActions ++ finalActions,
           createClippedActions clipRenderActions finalRenderActions))

createTransformedActions :: Transform -> RenderActions -> RenderActions
createTransformedActions xf new =
  RenderActions { renderScene = RenderTransformed xf (renderScene new),
                  renderUI = RenderTransformed xf (renderUI new) }

addTransformedRenderAction :: Transform -> GameMonad a -> GameMonad a
addTransformedRenderAction xf prg = GameMonad . RWST $ \cfg input -> do
  (result, finalInput, (actions, renderActions)) <-
    runRWST (nextFrame prg) cfg input
  let xformedActions = createTransformedActions xf renderActions
  return (result, finalInput, (actions, xformedActions))

addRenderAction :: Transform -> RenderObject -> GameMonad ()
addRenderAction xf ro = GameMonad $
  tell $ ([], RenderActions (RenderObjects [xformObject xf ro]) mempty)

addRenderUIAction :: V2 Float -> RenderObject -> GameMonad ()
addRenderUIAction (V2 x y) ro = GameMonad $
  tell $ ([], RenderActions mempty (RenderObjects [xformObject xf ro]))
  where
    xf = translate (V3 x y (-1)) identity

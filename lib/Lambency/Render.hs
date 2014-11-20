module Lambency.Render (
  Renderable(..),
  RenderConfig,
  mkRenderConfig,
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
import Lambency.Light
import Lambency.Shader
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Vertex

import Control.Monad.RWS.Strict

import Data.Array.IO
import Data.Array.Storable
import Data.Bits (complement)
import Data.Int
import Data.List (partition, sortBy)
import qualified Data.Map as Map

import Foreign.Storable
import Foreign.Ptr

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Linear

--------------------------------------------------------------------------------

data RenderConfig = RenderConfig {
  uiLight :: Light,
  fontLight :: Light
}

mkRenderConfig :: IO (RenderConfig)
mkRenderConfig = do
  ui <- createNoLight
  font <- createFontLight
  return $ RenderConfig ui font

type RenderContext = RWST RenderConfig () Transform IO

emptyRenderActions :: RenderActions
emptyRenderActions = RenderActions {
  renderScene = RenderObjects [],
  renderUI = RenderObjects []
}

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

shaderCache :: TVar (Data.Map Int Shader)
shaderCache = unsafeIO $ newTVarIO (Map.empty)

createBasicRO :: (Vertex a) => [a] -> [Int32] -> Material -> IO (RenderObject)

-- If there's no vertices, then there's nothing to render...
createBasicRO [] _ _ = do
  return $ RenderObject {
    materialVars = Map.empty,
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
                        GL.NumArrayIndices -> (Shader -> ShaderMap -> IO ())
    createRenderFunc vbo ibo nIndices = (\shdr shdrmap -> do

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
        GL.drawElements GL.Triangles nIndices GL.UnsignedInt nullPtr)

  in do
    vbo <- setupBuffer GL.ArrayBuffer verts
    ibo <- setupBuffer GL.ElementArrayBuffer idxs
    return $ RenderObject {
      -- materialVars = mat,
      -- !FIXME! These should be the variables extracted from the
      -- material after being compiled into shader code...
      materialVars = Map.empty,
      render = createRenderFunc vbo ibo $ fromIntegral (length idxs),
      flags = []
    }

class Renderable a where
  createRenderObject :: a -> Material -> IO (RenderObject)

updateMatrices :: String -> ShaderValue -> ShaderValue -> ShaderValue
updateMatrices "mvpMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices "m2wMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices _ v1 _ = v1

place :: Camera -> RenderObject -> RenderObject
place cam ro = let
  sm :: ShaderMap
  sm = Map.singleton "mvpMatrix" (Matrix4Val $ getViewProjMatrix cam)
  in
   ro { materialVars = Map.unionWithKey updateMatrices (materialVars ro) sm }

renderROs :: [RenderObject] -> Shader -> ShaderMap -> IO ()
renderROs ros shdr shdrmap = do
  beforeRender shdr
  flip mapM_ ros $ \ro -> (render ro) shdr $ Map.union (materialVars ro) shdrmap
  afterRender shdr

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

divideAndRenderROs :: [RenderObject] -> Camera -> Light -> IO ()
divideAndRenderROs [] _ _ = return ()
divideAndRenderROs ros cam (Light shdr shdrmap _) = let
  camDist :: RenderObject -> Float
  camDist ro = z
    where 
      (Matrix4Val (V4 _ _ (V4 _ _ _ z) _)) =
        case Map.lookup "mvpMatrix" (materialVars ro)
        of Nothing -> Matrix4Val eye4
           Just x -> x

  (trans, opaque) = partition (\ro -> Transparent `elem` (flags ro)) $
                    sortBy (\ro1 ro2 -> compare (camDist ro1) (camDist ro2)) $
                    map (place cam) ros

  renderFn :: [RenderObject] -> (Maybe GL.ComparisonFunction) -> IO ()
  renderFn [] _ = return ()
  renderFn rs d = do
    GL.depthFunc GL.$= d
    renderROs rs shdr shdrmap

  in do
    renderFn opaque (Just GL.Lequal)
    renderFn (reverse trans) Nothing

renderLight :: RenderAction -> Camera -> Light -> RenderContext ()
renderLight act cam (Light shdr shdrmap (Just (Shadow shadowShdr shadowMap))) = do
  (lcam, shadowShdrMap) <- liftIO $ do
    bindRenderTexture shadowMap
    clearBuffers
    -- Right now the MVP matrix of each object is for the main camera, so
    -- we need to replace it with the combination from the model matrix
    -- and the shadow VP...
    let (Vector3Val pos) = shdrmap Map.! "lightPos"
        (Vector3Val dir) = shdrmap Map.! "lightDir"
        lightCam = mkPerspCamera pos dir (V3 0 1 0) (pi / 4) 1 0.1 500.0
        shadowVP = Matrix4Val $ getViewProjMatrix lightCam
    return (lightCam, Map.insert "shadowVP" shadowVP shdrmap)
  renderLight act lcam (Light shadowShdr shdrmap Nothing)
  liftIO clearRenderTexture
  renderLight act cam (Light shdr shadowShdrMap Nothing)

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
  oldxf <- get
  withRWST (\x s -> (x, transform xf s)) $ renderLight act camera light
  put oldxf

renderLight (RenderObjects ros') camera light = do
  xf <- get
  let ros = map (xformObject xf) $ filter (not . elem Text . flags) ros'
  liftIO $ divideAndRenderROs ros camera light

renderLight (RenderCons act1 act2) camera light = do
  renderLight act1 camera light
  renderLight act2 camera light

renderText :: RenderAction -> Camera -> RenderContext ()
renderText (RenderObjects objs) camera = do
  xf <- get
  config <- ask
  liftIO $
    divideAndRenderROs
    (map (xformObject xf) $ filter ((elem Text) . flags) objs)
    camera
    (fontLight config)
renderText (RenderTransformed xf act) camera = do
  oldxf <- get
  withRWST (\x s -> (x, transform xf s)) $ renderText act camera
  put oldxf
renderText (RenderClipped _ act) camera = do
  -- !FIXME! ignoring clipped text...
  -- liftIO $ putStrLn "Warning: Unable to render clipped text!"
  renderText act camera
renderText (RenderCons act1 act2) camera = renderText act1 camera >> renderText act2 camera

performRenderActions :: [Light] -> Camera -> RenderActions -> RenderContext ()
performRenderActions lights camera actions = do
  mapM_ (\l -> renderLight (renderScene actions) camera l) lights
  config <- ask
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
  renderLight (renderUI actions) cam (uiLight config)
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
addRenderAction xf ro = lift $  modify $ appendSceneWith appendObj (xformObject xf ro)

addRenderUIAction :: V2 Float -> RenderObject -> GameMonad ()
addRenderUIAction (V2 x y) ro = lift $
  modify $ appendUIWith appendObj (xformObject xf ro)
  where
    xf = translate (V3 x y (-1)) identity

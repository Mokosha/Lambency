module Lambency.Render (
  Renderable(..),
  RenderConfig,
  mkRenderConfig,
  RenderContext,
  clearBuffers,
  createBasicRO,
  xformObject,
  performRenderAction,
  addRenderAction,
  addRenderUIAction,
  addClipRenderAction,
  resetClip,
) where

--------------------------------------------------------------------------------

import Lambency.Camera
import Lambency.Light
import Lambency.Material
import Lambency.Shader
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Vertex

import Control.Monad.Reader
import Control.Monad.State.Class

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
  uiLight :: Light
}

mkRenderConfig :: IO (RenderConfig)
mkRenderConfig = do
  ui <- createNoLight
  return $ RenderConfig {
    uiLight = ui
  }

type RenderContext = ReaderT RenderConfig IO

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

createBasicRO :: (Vertex a) => [a] -> [Int16] -> Material -> IO (RenderObject)

-- If there's no vertices, then there's nothing to render...
createBasicRO [] _ _ = do
  return $ RenderObject {
    material = Map.empty,
    render = \_ _ -> return (),
    flags = []
  }

createBasicRO verts@(v:_) idxs mat =
  let
    bindShaderVertexAttributes :: Shader -> IO ()
    bindShaderVertexAttributes shdr = let

      -- Lookup the location for the given attribute name in the shader program
      lu :: String -> GL.AttribLocation
      lu name = let
        svs = getShaderVars shdr
        in
         case Map.lookup name svs of
           Nothing -> GL.AttribLocation (-1)
           Just var -> case var of
             Uniform _ _ -> GL.AttribLocation (-1)
             Attribute _ loc -> loc
      in do
        mapM_ (\(loc, desc) -> GL.vertexAttribPointer loc GL.$= (GL.ToFloat, desc)) $
          zip (map lu $ getAttribNames v) (getOpenGLDescriptors v)

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
        GL.drawElements GL.Triangles nIndices GL.UnsignedShort nullPtr)

  in do
    vbo <- setupBuffer GL.ArrayBuffer verts
    ibo <- setupBuffer GL.ElementArrayBuffer idxs
    return $ RenderObject {
      material = mat,
      render = createRenderFunc vbo ibo $ fromIntegral (length idxs),
      flags = []
    }

class Renderable a where
  createRenderObject :: a -> Material -> IO (RenderObject)

  defaultRenderObject :: a -> IO (RenderObject)
  defaultRenderObject m = createRenderObject m =<< createSimpleMaterial

updateMatrices :: String -> ShaderValue -> ShaderValue -> ShaderValue
updateMatrices "mvpMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices "m2wMatrix" (Matrix4Val m1) (Matrix4Val m2) = Matrix4Val $ m1 !*! m2
updateMatrices _ v1 _ = v1

place :: Camera -> RenderObject -> RenderObject
place cam ro = let
  sm :: ShaderMap
  sm = Map.singleton "mvpMatrix" (Matrix4Val $ getViewProjMatrix cam)
  in
   ro { material = Map.unionWithKey updateMatrices (material ro) sm }

renderROs :: [RenderObject] -> Shader -> ShaderMap -> IO ()
renderROs ros shdr shdrmap = do
  beforeRender shdr
  mapM_ (\ro -> (render ro) shdr (Map.union (material ro) shdrmap)) ros
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
xformObject xform ro = ro {
  render = \shr sm -> do
     let newmap = appendXform xform sm
     (render ro) shr newmap
  }

divideAndRenderROs :: [RenderObject] -> Camera -> Light -> IO ()
divideAndRenderROs [] _ _ = return ()
divideAndRenderROs ros cam (Light shdr shdrmap _) = let
  camDist :: RenderObject -> Float
  camDist ro = z
    where 
      (Matrix4Val (V4 _ _ (V4 _ _ _ z) _)) =
        case Map.lookup "mvpMatrix" (material ro)
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

renderLight (RenderObjects ros) camera light = liftIO $ divideAndRenderROs ros camera light
renderLight (RenderCons act1 act2) camera light = do
  renderLight act1 camera light
  renderLight act2 camera light
renderLight (RenderUI act) _ _ = do
  config <- ask
  cam <- liftIO $ do
    -- !FIXME! This should be stored in the camera...? Why
    -- are we querying IO here? =(
    (Just win) <- GLFW.getCurrentContext
    (szx, szy) <- GLFW.getWindowSize win
    -- Setup ortho camera and nolight for ui crap
    let hx = 0.5 * (fromIntegral szx)
        hy = 0.5 * (fromIntegral szy)
    return $ mkOrthoCamera zero (negate localForward) localUp (-hx) (hx) (hy) (-hy) 0.01 50.0
  renderLight act cam (uiLight config)

performRenderAction :: [Light] -> Camera -> RenderAction -> RenderContext ()
performRenderAction lights camera action = mapM_ (\l -> renderLight action camera l) lights

appendObj :: RenderObject -> RenderAction -> RenderAction
appendObj obj (RenderObjects objs) = RenderObjects (obj : objs)
appendObj obj (RenderClipped clip act) = RenderClipped clip (appendObj obj act)
appendObj obj (RenderCons act1 act2) = RenderCons act1 (appendObj obj act2)
appendObj obj (RenderUI act) = RenderCons (RenderUI act) (RenderObjects [obj])

appendUI :: RenderObject -> RenderAction -> RenderAction
appendUI obj (RenderObjects objs) = RenderCons (RenderObjects objs) (RenderUI (RenderObjects [obj]))
appendUI obj (RenderClipped clip act) = RenderClipped clip (appendUI obj act)
appendUI obj (RenderUI act) = RenderUI (appendObj obj act)
appendUI obj (RenderCons act1 act2) = RenderCons act1 (appendUI obj act2)

addClipRenderAction :: Transform -> RenderObject -> GameMonad ()
addClipRenderAction xf ro = lift $
  modify $ appendClip (xformObject xf ro)
  where
    appendClip :: RenderObject -> RenderAction -> RenderAction
    appendClip obj (RenderClipped clip act) = RenderClipped (appendObj obj clip) act
    appendClip obj (RenderCons act1 act2) = RenderCons act1 (appendClip obj act2)
    appendClip obj (RenderUI act) = RenderUI (appendClip obj act)
    appendClip obj act = RenderCons act (RenderClipped (RenderObjects [obj]) (RenderObjects []))

resetClip :: GameMonad ()
resetClip = lift $ modify resetFn
  where
    resetFn x@(RenderClipped _ _) = RenderCons x (RenderObjects [])
    resetFn (RenderCons act1 act2) = RenderCons act1 (resetFn act2)
    resetFn (RenderUI act) = RenderUI $ resetFn act
    resetFn x = x

addRenderAction :: Transform -> RenderObject -> GameMonad ()
addRenderAction xf ro = lift $
  modify $ appendObj (xformObject xf ro)

addRenderUIAction :: V2 Float -> RenderObject -> GameMonad ()
addRenderUIAction (V2 x y) ro = lift $
  modify $ appendUI (xformObject xf ro)
  where
    xf = translate (V3 x y (-1)) identity

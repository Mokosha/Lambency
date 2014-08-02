module Lambency.Render (
  Renderable(..),
  clearBuffers,
  createBasicRO,
  renderROs,
) where

--------------------------------------------------------------------------------

import Lambency.Camera
import Lambency.Material
import Lambency.Shader
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Vertex

import Data.Array.IO
import Data.Array.Storable
import Data.Int
import Data.List (partition, sortBy)
import qualified Data.Map as Map

import Foreign.Storable
import Foreign.Ptr

import qualified Graphics.Rendering.OpenGL as GL

import Linear.Matrix
import Linear.V4

--------------------------------------------------------------------------------

clearBuffers :: IO ()
clearBuffers = GL.clear [GL.ColorBuffer, GL.DepthBuffer]

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
        mapM_ (\k -> case Map.lookup k shdrVars of
          Nothing -> return ()
          Just shdrVar -> setUniformVar shdrVar (shdrmap Map.! k)) (Map.keys shdrmap)

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

place :: Transform -> Camera -> RenderObject -> RenderObject
place xf cam ro = let
  model :: M44 Float
  model = xform2Matrix xf

  sm :: ShaderMap
  sm = Map.fromList [
    ("mvpMatrix", Matrix4Val $ model !*! (getViewProjMatrix cam)),
    ("m2wMatrix", Matrix4Val $ model)]
  in
   ro { material = Map.union sm (material ro) }

renderLight :: [RenderObject] -> Light -> IO ()
renderLight ros (Light shdr shdrmap msm) = do
  case msm of
    Nothing -> return ()
    Just (Shadow shadowShdr shadowMap) -> do
      bindRenderTexture shadowMap
      clearBuffers
      beforeRender shadowShdr
      -- Right now the MVP matrix of each object is for the main camera, so
      -- we need to replace it with the combination from the model matrix
      -- and the shadow VP...
      mapM_
        (\ro -> do
            let
              mat :: ShaderValue -> Mat4f
              mat (Matrix4Val m) = m
              mat _ = eye4
              lightMVP = (mat $ material ro Map.! "m2wMatrix") !*!
                         (mat $ shdrmap Map.! "shadowVP")
              newmap = Map.insert "mvpMatrix" (Matrix4Val lightMVP) shdrmap
            (render ro) shadowShdr (Map.union newmap (material ro))) ros
      afterRender shadowShdr
      clearRenderTexture
  beforeRender shdr
  mapM_ (\ro -> (render ro) shdr (Map.union (material ro) shdrmap)) ros
  afterRender shdr

renderLights :: [RenderObject] -> [Light] -> IO ()
renderLights [] _ = return ()
renderLights ros lights = mapM_ (renderLight ros) lights

renderROs :: [(Transform, RenderObject)] -> Camera -> [Light] -> IO ()
renderROs ros cam lights = let
  camDist :: RenderObject -> Float
  camDist ro = let (Matrix4Val (V4 _ _ (V4 _ _ _ z) _)) = getMaterialVar (material ro) "mvpMatrix" in z

  (trans, opaque) = partition (\ro -> Transparent `elem` (flags ro)) $
                    sortBy (\ro1 ro2 -> compare (camDist ro1) (camDist ro2)) $
                    map (\(xf, ro) -> place xf cam ro) ros
  in do
    -- !FIXME! This should be moved to the camera...
    GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
    clearBuffers

    GL.depthFunc GL.$= Just GL.Lequal
    renderLights opaque lights
    GL.depthFunc GL.$= Nothing
    renderLights (reverse trans) lights

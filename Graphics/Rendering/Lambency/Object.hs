module Graphics.Rendering.Lambency.Object (
  GameObject(..),
  updateGameObject,
  updateObjs,
  renderCamera
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Camera
import Graphics.Rendering.Lambency.Material
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture

import Data.Maybe (catMaybes)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type Time = Double

data GameObject a = GameObject {
  renderObject :: Maybe RenderObject,
  gameObject :: a,
  objSVMap :: Map.Map String (a -> Camera -> ShaderValue),
  update :: Time -> a -> [a] -> Maybe a
}

updateGameObject :: GameObject a -> a -> GameObject a
updateGameObject go val = (\obj -> obj { gameObject = val }) go

updateObjs :: Time -> [GameObject a] -> [GameObject a]
updateObjs dt objs = catMaybes $
                     zipWith3 (\upd obj go ->
                                case upd dt obj os of
                                  Nothing -> Nothing
                                  Just o -> Just $ updateGameObject go o)
                     upds os objs
  where upds = map update objs
        os = map gameObject objs

renderObj :: Maybe Material -> Camera -> GameObject a -> IO ()
renderObj maybeMat cam obj = let

  setShaderVar :: ShaderMap -> ShaderVar -> IO ()
  setShaderVar _ (Attribute _ _) = return ()
  setShaderVar m v =
    if Map.member v m then
      setUniformVar v $ m Map.! v
    else do
      putStrLn $ "Warning: Shader variable uninitialized: " ++ (show v)
      return ()
  in
   case renderObject obj of
     Nothing -> return ()
     Just ro -> do
       let mat = case maybeMat of
             Nothing -> (material ro)
             Just omat -> omat
       beforeRender mat
       let
         -- compute all of the object defined shader variables
         objMap = Map.map (\f -> f (gameObject obj) cam) (objSVMap obj)
         -- map them to the values requested by the material
         matVars = getShaderVars $ getShader mat
         userMap = Map.mapKeys (\k -> matVars Map.! k) $
                   Map.filterWithKey (\k _ -> Map.member k matVars) objMap
         -- finally, concatenate the results with the predefined shader
         -- variables....
         finalMap = Map.union userMap $ getShaderMap mat
       mapM_ (setShaderVar finalMap) $ Map.elems matVars
       render ro mat
       afterRender mat

renderCamera :: Camera -> [GameObject a] -> IO ()
renderCamera cam objs = do
  sequence_ $
    (catMaybes $ renderObject `map` objs) >>=
    (Map.elems . getShaderMap . material) >>=
    (\x ->
      case x of
        TextureVal tex ->
          case getTextureCamera tex of
            Nothing -> []
            Just (c, h) -> [renderCameraTexture (c, h)]
        _ -> [])
  mapM_ (renderObj Nothing cam) objs
  where
    renderCameraTexture :: (Camera, FBOHandle) -> IO ()
    renderCameraTexture (c, h) = do
      mat <- createSimpleMaterial
      bindRenderTexture h
      clearBuffers
      mapM_ (renderObj (Just mat) c) objs
      clearRenderTexture
      destroyMaterial mat

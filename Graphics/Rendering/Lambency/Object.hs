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

import Data.Maybe (catMaybes)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type Time = Double

data GameObject a = GameObject {
  renderObject :: Maybe RenderObject,
  gameObject :: a,
  objSVMap :: Map.Map ShaderVar (a -> Camera -> ShaderValue),
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

renderCamera :: Camera -> [GameObject a] -> IO ()
renderCamera cam objs = mapM_ renderObj objs
  where renderObj :: GameObject a -> IO ()
        renderObj obj = let

          setShaderVar :: ShaderMap -> ShaderVar -> IO ()
          setShaderVar _ (Attribute _ _) = return ()
          setShaderVar m v =
            if Map.member v m then
              setUniformVar v $ m Map.! v
            else do
              putStrLn $ "Warning: Shader variable uninitialized: " ++ (show v)
              return ()

          setShaderVars :: ShaderMap -> [ShaderVar] -> IO ()
          setShaderVars m = mapM_ (setShaderVar m)

          in
           case renderObject obj of
             Nothing -> return ()
             Just ro -> do
               beforeRender (material ro)
               let objMap = Map.map (\f -> f (gameObject obj) cam) (objSVMap obj)
                   finalMap = Map.union objMap $ (getShaderMap . material) ro
               setShaderVars finalMap $ Map.elems . getShaderVars $ (getShader . material) ro
               render ro
               afterRender (material ro)

module Graphics.Rendering.Lambency.Object (
  GameObject(..),
  updateGameObject,
  updateObjs,
  interactObjs,
  renderCamera
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Camera
import Graphics.Rendering.Lambency.Material
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader

import Control.Applicative

import Data.Maybe (catMaybes)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type Time = Double

data GameObject a = GameObject {
  renderObject :: Maybe RenderObject,
  gameObject :: a,
  objSVMap :: Map.Map ShaderVar (a -> Camera -> ShaderValue),
  update :: Time -> GameObject a -> Maybe (GameObject a),
  collide :: GameObject a -> [GameObject a] -> Maybe (GameObject a)
}

updateGameObject :: GameObject a -> a -> GameObject a
updateGameObject go val = (\obj -> obj { gameObject = val }) go

updateObjs :: Time -> [GameObject a] -> [GameObject a]
updateObjs dt objs = catMaybes $ (\obj -> update obj dt obj) <$> objs

interactObjs :: [GameObject a] -> [GameObject a]
interactObjs allobjs = catMaybes $ (\obj -> (collide obj) obj allobjs) <$> allobjs

renderCamera :: Camera -> [GameObject a] -> IO ()
renderCamera cam objs = mapM_ renderObj objs
  where renderObj :: GameObject a -> IO ()
        renderObj obj = let
          
          setShaderVar :: ShaderVar -> IO ()
          setShaderVar v = case v of
            Attribute _ _ -> return ()
            Uniform _ _ ->
              let eval = (objSVMap obj) Map.! v
                  val = eval (gameObject obj) cam
              in setUniformVar v val

          setShaderVars :: [ShaderVar] -> IO ()
          setShaderVars = mapM_ setShaderVar

          in
           case renderObject obj of
             Nothing -> return ()
             Just ro -> do
               beforeRender (material ro)
               setShaderVars $ getShaderVars $ (getShader . material) ro
               render ro
               afterRender (material ro)

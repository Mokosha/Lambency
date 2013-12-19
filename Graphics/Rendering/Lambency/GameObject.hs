module Graphics.Rendering.Lambency.GameObject (
  placeStaticObject,
  placeObject
) where

--------------------------------------------------------------------------------
import Graphics.Rendering.Lambency.Camera
import Graphics.Rendering.Lambency.Transform
import Graphics.Rendering.Lambency.Types

import Data.Vect.Float
import qualified Data.Map as Map

import Control.Arrow
import Control.Wire

--------------------------------------------------------------------------------

positioned :: Camera -> Transform -> ShaderMap
positioned cam xform = Map.fromList
                    [("mvpMatrix", Matrix4Val $ model .*. (getViewProjMatrix cam)),
                     ("m2wMatrix", Matrix4Val $ model)]
  where model :: Mat4
        model = xform2Matrix xform

placeStaticObject :: Monad m => Transform -> Wire s e m (Camera, RenderObject) RenderObject
placeStaticObject xform = 
  ((arr (flip positioned xform) *** (arr material)) >>> (arr $ uncurry Map.union))
  &&& (arr snd) >>> (arr $ uncurry (\x ro -> ro { material = x }))

placeObject :: Monad m => Wire s e m ((Camera, Transform), RenderObject) RenderObject
placeObject = 
  (((arr $ uncurry positioned) *** (arr material)) >>> (arr $ uncurry Map.union))
  &&& (arr snd) >>> (arr $ uncurry (\x ro -> ro { material = x }))

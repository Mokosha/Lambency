module Graphics.Rendering.Lambency.GameObject (
  mkObject,
  mkStaticObject
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
positioned cam xform = let
  model :: Mat4
  model = xform2Matrix xform
  in Map.fromList
     [("mvpMatrix", Matrix4Val $ model .*. (getViewProjMatrix cam)),
      ("m2wMatrix", Matrix4Val $ model)]

mkObject :: Monad m => RenderObject ->
            Wire s e m a Transform ->
            Wire s e m (Camera, a) RenderObject
mkObject ro xformw = mkId *** xformw >>> (arr $ uncurry positioned) >>>
                     (mkSF_ $ \sm ->
                       (\r -> r { material = Map.union sm (material ro) }) ro)

mkStaticObject :: Monad m => RenderObject -> Transform -> Wire s e m Camera RenderObject
mkStaticObject ro xform = mkSF_ $ \cam ->
  (\r -> r { material = Map.union (positioned cam xform) (material ro) }) ro
  

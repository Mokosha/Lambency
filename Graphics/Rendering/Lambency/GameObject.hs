module Graphics.Rendering.Lambency.GameObject (
  positioned,
  mkObject, mkStaticObject,
--  mkCollider, mkStaticCollider,
  mkStaticLight,
  withVelocity,
  keyPressed
) where

--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import Graphics.UI.Lambency.Input

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
            Wire s e m (Camera, a) GameObject
mkObject ro xformw = mkId *** xformw >>> (arr $ uncurry positioned) >>>
                     (mkSF_ $ \sm -> SimpleObject $
                       (\r -> r { material = Map.union sm (material ro) }) ro)

mkStaticObject :: Monad m => RenderObject -> Transform -> Wire s e m Camera GameObject
mkStaticObject ro xform = mkSF_ $ \cam -> SimpleObject $
  (\r -> r { material = Map.union (positioned cam xform) (material ro) }) ro

--mkCollider :: Monad m => RenderObject -> BoundingVolume ->
--              Wire s e m a Transform -> Wire s e m (Camera, a) GameObject

--mkStaticCollider :: Monad m =>
--                    RenderObject -> BoundingVolume -> Transform ->
--                    Wire s e m Camera GameObject

mkStaticLight :: Monad m => Light -> Wire s e m a GameObject
mkStaticLight l = mkConst $ Right (LightObject l)
  
withVelocity :: Monad m =>
                Transform -> Wire Timestep e m a Vec3 ->
                Wire Timestep e m a Transform
withVelocity xform velWire = velWire >>> (moveXForm xform)
  where moveXForm :: Transform -> Wire Timestep e m Vec3 Transform
        moveXForm xf = mkPure $ \ts vel -> let
          Timed dt () = ts ()
          newxform = translate (dt *& vel) xf
          in (Right newxform, moveXForm newxform)

keyPressed :: (Monad m, Monoid e) => GLFW.Key -> Wire s e m Input Input
keyPressed key = when (isKeyPressed key)

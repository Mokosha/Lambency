module Graphics.Rendering.Lambency.GameObject (
  emptyState,
  positioned,
  rendersWith, collidesWith,
  mkObject, mkStaticObject,
  mkStaticLight,
  withVelocity,
  keyPressed
) where

--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import Graphics.UI.Lambency.Input

import Graphics.Rendering.Lambency.Bounds
import Graphics.Rendering.Lambency.Camera
import Graphics.Rendering.Lambency.Transform
import Graphics.Rendering.Lambency.Types

import Data.Vect.Float
import qualified Data.Map as Map

import Control.Arrow
import Control.Wire
import Control.Monad.State.Class

--------------------------------------------------------------------------------

emptyState :: GameState
emptyState = (mkOrthoCamera
              vec3X
              (toNormalUnsafe vec3Z)
              (toNormalUnsafe vec3Y)
              0 0 0 0 0 0, [], [])

positioned :: Camera -> Transform -> ShaderMap
positioned cam xform = let
  model :: Mat4
  model = xform2Matrix xform
  in Map.fromList
     [("mvpMatrix", Matrix4Val $ model .*. (getViewProjMatrix cam)),
      ("m2wMatrix", Matrix4Val $ model)]

rendersWith :: RenderObject -> GameObject -> GameObject
rendersWith ro (GameObject xf cs) =
  GameObject xf (RenderComponent ro : cs)

collidesWith :: BoundingVolume -> GameObject -> GameObject
collidesWith bv (GameObject xf cs) =
  GameObject xf (CollisionComponent bv : cs)

mkObject :: RenderObject -> GameWire Transform -> GameWire [GameObject]
mkObject ro xfw =
  xfw >>> (mkPure_ $ \xf -> Right $ [GameObject xf [RenderComponent ro]])

mkStaticObject :: RenderObject -> Transform -> GameWire [GameObject]
mkStaticObject ro xform = mkConst $ Right $ [GameObject xform [RenderComponent ro]]

mkStaticLight :: Monad m => Light -> Wire s e m a Light
mkStaticLight l = mkConst $ Right l
  
withVelocity :: Monad m =>
                Transform -> Wire Timestep e m a Vec3 ->
                Wire Timestep e m a Transform
withVelocity xform velWire = velWire >>> (moveXForm xform)
  where moveXForm :: Transform -> Wire Timestep e m Vec3 Transform
        moveXForm xf = mkPure $ \ts vel -> let
          Timed dt () = ts ()
          newxform = translate (dt *& vel) xf
          in (Right newxform, moveXForm newxform)

-- This wire produces the given value when the key is pressed otherwise
-- it inhibits
keyPressed :: GLFW.Key -> GameWire a -> GameWire a
keyPressed key wire =
  wire >>>
  (mkGen_ $ \val -> do
      ipt <- get
      return $
        if (isKeyPressed key ipt) then
          Right val
        else
          Left ()
  )

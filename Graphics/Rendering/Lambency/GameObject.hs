module Graphics.Rendering.Lambency.GameObject (
  mkObject,
  withVelocity,
  pulseSound,
  onEvent,
  keyPressed
) where

--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import Graphics.UI.Lambency.Input
import Graphics.UI.Lambency.Sound

import Graphics.Rendering.Lambency.Transform
import Graphics.Rendering.Lambency.Types

import Data.Vect.Float

import Control.Arrow
import Control.Wire
import Control.Wire.Unsafe.Event
import Control.Monad.State.Class
import Control.Monad.Writer

--------------------------------------------------------------------------------

mkObject :: RenderObject -> GameWire a Transform -> GameWire a a
mkObject ro xfw = mkGen $ \dt val -> do
  (xform, nextWire) <- stepWire xfw dt (Right val)
  case xform of
    Right xf -> do
      censor (Render3DAction xf ro :) $
        return (Right val, mkObject ro nextWire)
    Left i -> do
      return $ (Left i, mkObject ro nextWire)

withVelocity :: (Monad m, Monoid s) =>
                Transform -> Wire (Timed Timestep s) e m a Vec3 ->
                Wire (Timed Timestep s) e m a Transform
withVelocity initial velWire = velWire >>> (moveXForm initial)
  where moveXForm :: (Monad m, Monoid s) =>
                     Transform -> Wire (Timed Timestep s) e m Vec3 Transform
        moveXForm xf = mkPure $ \dt vel -> let
          newxform = translate (dtime dt *& vel) xf
          in (Right newxform, moveXForm newxform)

pulseSound :: Sound -> GameWire a b -> GameWire a b
pulseSound sound wire = mkGen $ \dt val -> do
  (result, nextWire) <- stepWire wire dt (Right val)
  censor (SoundAction sound StartSound :) $ return (result, nextWire)

onEvent :: GameWire a (Event b) -> (b -> OutputAction) ->
           GameWire a c -> GameWire a c
onEvent eventWire actionFn wire = mkGen $ \dt val -> do
  (e, nextE) <- stepWire eventWire dt (Right val)
  (result, nextWire) <- stepWire wire dt (Right val)
  case e of
    Right (Event x) ->
      censor (actionFn x :) $
      return (result, onEvent nextE actionFn nextWire)
    _ -> return (result, onEvent nextE actionFn nextWire)

-- This wire produces the given value when the key is pressed otherwise
-- it inhibits
keyPressed :: GLFW.Key -> GameWire a b -> GameWire a b
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

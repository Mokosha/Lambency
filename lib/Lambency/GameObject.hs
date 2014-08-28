module Lambency.GameObject (
  mkObject,
  withVelocity,
  pulseSound
) where

--------------------------------------------------------------------------------
import Lambency.Render
import Lambency.Sound
import Lambency.Transform
import Lambency.Types

import Control.Arrow
import Control.Wire
import Control.Monad.Writer

import Linear.Vector
--------------------------------------------------------------------------------

mkObject :: RenderObject -> GameWire a Transform -> GameWire a a
mkObject ro xfw = mkGen $ \dt val -> do
  (xform, nextWire) <- stepWire xfw dt (Right val)
  case xform of
    Right xf -> do
      addRenderAction xf ro
      return (Right val, mkObject ro nextWire)
    Left i -> do
      return $ (Left i, mkObject ro nextWire)

withVelocity :: (Monad m, Monoid s) =>
                Transform -> Wire (Timed Float s) e m a Vec3f ->
                Wire (Timed Float s) e m a Transform
withVelocity initial velWire = velWire >>> (moveXForm initial)
  where moveXForm :: (Monad m, Monoid s) =>
                     Transform -> Wire (Timed Float s) e m Vec3f Transform
        moveXForm xf = mkPure $ \t vel -> let
          newxform = translate (dtime t *^ vel) xf
          in (Right newxform, moveXForm newxform)

pulseSound :: Sound -> GameWire a a
pulseSound sound = mkGenN $ \val ->
  censor (SoundAction sound StartSound :) $
  return (Right val, Control.Wire.id)

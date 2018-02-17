module Lambency.GameObject (
  wireFrom,
  mkObject,
  staticObject,
  withVelocity,
  pulseSound
) where

--------------------------------------------------------------------------------
import Lambency.Render
import Lambency.Sound
import Lambency.Transform
import Lambency.Types

import Control.Arrow
import Control.Wire hiding ((.))

import Linear.Vector
--------------------------------------------------------------------------------

wireFrom :: GameMonad a -> (a -> GameWire b c) -> GameWire b c
wireFrom prg fn = mkGen $ \dt val -> do
  seed <- prg
  stepWire (fn seed) dt (Right val)

doOnce :: GameMonad () -> GameWire a a
doOnce pgm = wireFrom pgm $ const Control.Wire.id

mkObject :: RenderObject -> GameWire a Transform -> GameWire a a
mkObject ro xfw = mkGen $ \dt val -> do
  (xform, nextWire) <- stepWire xfw dt (Right val)
  case xform of
    Right xf -> addRenderAction xf ro >> return (Right val, mkObject ro nextWire)
    Left i -> return (Left i, mkObject ro nextWire)

staticObject :: RenderObject -> Transform -> GameWire a a
staticObject ro = mkObject ro . mkConst . Right

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
pulseSound = doOnce . startSound

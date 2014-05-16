module Graphics.Rendering.Lambency.GameObject (
  mkObject,
  withVelocity,
  pulseSound,
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
                Transform -> Wire (Timed Float s) e m a Vec3 ->
                Wire (Timed Float s) e m a Transform
withVelocity initial velWire = velWire >>> (moveXForm initial)
  where moveXForm :: (Monad m, Monoid s) =>
                     Transform -> Wire (Timed Float s) e m Vec3 Transform
        moveXForm xf = mkPure $ \t vel -> let
          newxform = translate (dtime t *& vel) xf
          in (Right newxform, moveXForm newxform)

pulseSound :: Sound -> GameWire a a
pulseSound sound = mkGenN $ \val ->
  censor (SoundAction sound StartSound :) $
  return (Right val, Control.Wire.id)

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

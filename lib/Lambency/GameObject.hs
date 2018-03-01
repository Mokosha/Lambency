module Lambency.GameObject (
  wireFrom,
  bracketResource, liftWire, withResource, joinResources, withDefault,
  mkPureWire, stepPureWire,
  doOnce, doOnceWithInput,
  quitWire,
  mkObject,
  staticObject,
  withVelocity,
  pulseSound
) where

--------------------------------------------------------------------------------
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Wire

import Data.Maybe

import Lambency.Render
import Lambency.Sound
import Lambency.Transform
import Lambency.Types

import Prelude hiding ((.), id)

import qualified Graphics.UI.GLFW as GLFW
import FRP.Netwire.Input

import Linear.Vector
--------------------------------------------------------------------------------

wireFrom :: GameMonad a -> (a -> GameWire b c) -> GameWire b c
wireFrom prg fn = mkGen $ \dt val -> do
  seed <- prg
  stepWire (fn seed) dt (Right val)

doOnce :: GameMonad () -> GameWire a a
doOnce pgm = wireFrom pgm $ const Control.Wire.id

doOnceWithInput :: (a -> GameMonad ()) -> GameWire a a
doOnceWithInput fn = mkGenN $ \x -> fn x >> return (Right x, mkId)

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

-- | Runs the initial loading program and uses the resource until the generated
-- wire inhibits, at which point it unloads the resource. Once the resource is
-- freed, the resulting wire returns Nothing indefinitely. The resulting wire
-- also takes a signal to terminate from its input.
--
-- TODO: Maybe should restrict this to certain types of resources?
bracketResource :: IO r -> (r -> IO ()) -> ResourceContextWire r a b
                -> PureWire (a, Bool) (Maybe b)
bracketResource load unload (RCW rcw) = PW $ mkGen $ \dt x -> do
  resource <- GameMonad $ liftIO load
  stepWire (go resource rcw) dt (Right x)
    where
      go resource w = mkGen $ \dt (x, quitSignal) -> do
        (result, w') <- runReaderT (stepWire w dt (Right x)) resource
        let shouldQuit (Left _) = True
            shouldQuit _ = quitSignal
        if shouldQuit result
          then do
            GameMonad $ liftIO (unload resource)
            return (Right Nothing, pure Nothing)
          else return (Just <$> result, go resource w')

withResource :: (r -> GameWire a b) -> ResourceContextWire r a b
withResource wireGen = RCW $ mkGen $ \dt x -> ReaderT $ \r -> do
  (result, w') <- stepWire (wireGen r) dt (Right x)
  return (result, getResourceWire $ liftWire w')

liftWire :: GameWire a b -> ResourceContextWire r a b
liftWire = withResource . const

loopWith :: PureWire (a, c) b -> c -> (b -> c -> c) -> PureWire (a, c) b
loopWith m x fn =
  loop $ second (PW $ delay x) >>> (first m >>> (arr fst &&& arr (uncurry fn)))

joinResources :: Monoid b
              => [PureWire (a, Bool) (Maybe b)]
              -> PureWire (a, Bool) (Maybe b)
joinResources res = loopWith (fmap mconcat $ sequenceA res) False ((&&) . isJust)

withDefault :: GameWire a b -> PureWire a b -> PureWire a b
withDefault w (PW m) = PW $ w <|> m

mkPureWire :: (TimeStep -> a -> GameMonad (b, PureWire a b)) -> PureWire a b
mkPureWire f = PW $ mkGen $ \dt x -> do
  (r, PW w') <- f dt x
  return (Right r, w')

stepPureWire :: PureWire a b -> TimeStep -> a -> GameMonad (b, PureWire a b)
stepPureWire (PW w) dt x = do
  (Right r, w') <- stepWire w dt (Right x)
  return (r, PW w')

-- Wire that behaves like the identity wire until the given key
-- is pressed, then inhibits forever.
quitWire :: GLFW.Key -> GameWire a a
quitWire key =
  rSwitch mkId .
  (mkId &&& (now . pure mkEmpty . keyPressed key <|> never))

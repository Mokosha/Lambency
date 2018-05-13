module Lambency.GameObject (
  wireFrom, contWireFrom, liftWire, liftWireRCW,
  bracketResource, withResource, joinResources, withDefault,
  transformedContext, transformedResourceContext,
  clippedContext, clippedResourceContext,
  withSubResource,
  mkContWire, mkContSF, mkContSFN, stepContWire,
  doOnce, doOnceWithInput, everyFrame,
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
import Control.Monad.Writer
import Control.Wire

import Data.Maybe
import Data.Either (isLeft)
import Data.Foldable
import Data.Semigroup ()

import Lambency.Renderer
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

contWireFrom :: GameMonad a -> (a -> ContWire b c) -> ContWire b c
contWireFrom prg fn = mkContWire $ \dt val -> do
  seed <- prg
  stepContWire (fn seed) dt val

doOnce :: GameMonad () -> GameWire a a
doOnce pgm = wireFrom pgm $ const Control.Wire.id

doOnceWithInput :: (a -> GameMonad ()) -> GameWire a a
doOnceWithInput fn = mkGenN $ \x -> fn x >> return (Right x, mkId)

everyFrame :: (a -> GameMonad b) -> ContWire a b
everyFrame fn = CW $ mkGen_ $ \x -> Right <$> (fn x)

mkObject :: RenderObject -> GameWire a Transform -> GameWire a a
mkObject ro xfw = mkGen $ \dt val -> do
  (xform, nextWire) <- stepWire xfw dt (Right val)
  case xform of
    Right xf -> addRenderAction xf ro >> return (Right val, mkObject ro nextWire)
    Left i -> return (Left i, mkObject ro nextWire)

staticObject :: RenderObject -> Transform -> GameWire a a
staticObject ro = mkObject ro . mkConst . Right

-- | `transformedContext a b` runs b as if all actions within it were rendered
-- with the transform produced by a
transformedContext :: GameWire a Transform -> GameWire a b -> GameWire a b
transformedContext xfw w = mkGen $ \dt x -> do
  (xfResult, xfw') <- stepWire xfw dt (Right x)
  case xfResult of
    Right xf ->
      addTransformedRenderAction xf $
      second (transformedContext xfw') <$> stepWire w dt (Right x)
    Left i -> return (Left i, transformedContext xfw' w)

clippedContext :: GameWire a b -> GameWire b c -> GameWire a c
clippedContext cw w = mkGen $ \dt x ->
  addClippedRenderAction (stepWire cw dt (Right x)) $ \(clipResult, cw') ->
  case clipResult of
    Right clip -> second (clippedContext cw') <$> stepWire w dt (Right clip)
    Left i -> return (Left i, clippedContext cw' w)

withVelocity :: (Monad m, Semigroup s, Monoid s) =>
                Transform -> Wire (Timed Float s) e m a Vec3f ->
                Wire (Timed Float s) e m a Transform
withVelocity initial velWire = velWire >>> (moveXForm initial)
  where moveXForm :: (Monad m, Semigroup s, Monoid s) =>
                     Transform -> Wire (Timed Float s) e m Vec3f Transform
        moveXForm xf = mkPure $ \t vel -> let
          newxform = translate (dtime t *^ vel) xf
          in (Right newxform, moveXForm newxform)

pulseSound :: Sound -> GameWire a a
pulseSound = doOnce . startSound

loadResources :: ResourceLoader a -> GameMonad (a, IO ())
loadResources (ResourceLoader loadPrg) = GameMonad $ do
  rr <- renderer <$> ask
  liftIO $ runWriterT (runReaderT loadPrg rr)

-- | Runs the initial loading program and uses the resource until the generated
-- wire inhibits, at which point it unloads the resource. Once the resource is
-- freed, the resulting wire returns Nothing indefinitely. The resulting wire
-- also takes a signal to terminate from its input.
bracketResource :: ResourceLoader r
                -> ResourceContextWire r a b
                -> ContWire (a, Bool) (Maybe b)
bracketResource load (RCW rcw) = CW $ mkGen $ \dt x -> do
  -- TODO: Maybe should restrict this to certain types of resources?
  (resource, unload) <- loadResources load
  stepWire (go unload resource rcw) dt (Right x)
    where
      go unload res w = mkGen $ \dt (x, quitSignal) ->
        let quit = GameMonad $ do
              _ <- liftIO unload
              return (Right Nothing, pure Nothing)
        in if quitSignal then quit else do
          (result, w') <- runReaderT (stepWire w dt (Right x)) res
          if isLeft result then quit else return (Just <$> result, go unload res w')

liftWireRCW :: GameWire a b -> ResourceContextWire r a b
liftWireRCW = RCW . liftWire

liftWire :: (Monad m, MonadTrans t, Monad (t m))
          => Wire s e m a b -> Wire s e (t m) a b
liftWire = mapWire lift

withResource :: (r -> GameWire a b) -> ResourceContextWire r a b
withResource wireGen = RCW $ mkGen $ \dt x -> do
  (r, w) <- second liftWire <$>
                (ReaderT $ \r -> stepWire (wireGen r) dt (Right x))
  return (r, w)

withinContext :: r -> ResourceContextWire r a b -> GameWire a b
withinContext res (RCW w) =
  mkGen $ \dt x ->
  second (withinContext res . RCW) <$> runReaderT (stepWire w dt (Right x)) res

transformedResourceContext :: ResourceContextWire r a Transform
                           -> ResourceContextWire r a b
                           -> ResourceContextWire r a b
transformedResourceContext xf w = RCW $ mkGen $ \dt x ->
  second liftWire <$>
  (ReaderT $ \r ->
    let xf' = withinContext r xf
        w' = withinContext r w
        xfw = transformedContext xf' w'
     in stepWire xfw dt (Right x))

clippedResourceContext :: ResourceContextWire r a b
                       -> ResourceContextWire r b c
                       -> ResourceContextWire r a c
clippedResourceContext cw w = RCW $ mkGen $ \dt x ->
  second liftWire <$>
  (ReaderT $ \r ->
    let cw' = withinContext r cw
        w' = withinContext r w
        xfw = clippedContext cw' w'
     in stepWire xfw dt (Right x))

joinResources :: Monoid b
              => [ContWire (a, Bool) (Maybe b)]
              -> ContWire (a, Bool) (Maybe b)
joinResources = mkWire . fmap msequence . sequenceA
  where
    mkWire (CW w) = CW $ mkGen $ \dt (x, quit) -> do
      (Right result, w') <- stepWire w dt (Right (x, quit))
      if not quit && isNothing result
        then stepWire w' dt (Right (undefined, True))
        else return (Right result, getContinuousWire . mkWire $ CW w')

    msequence :: (MonadPlus m, Monoid b) => [m b] -> m b
    msequence [] = mzero
    msequence (v : vs) = foldr (\x y -> x >>= ((<$> y) . mappend)) v vs

withDefault :: GameWire a b -> ContWire a b -> ContWire a b
withDefault w (CW m) = CW $ w <|> m

withSubResource :: (r' -> r)
                -> ResourceContextWire r a b
                -> ResourceContextWire r' a b
withSubResource f (RCW w) = RCW $ mkGen $ \dt x -> do
  (r, w') <- withReaderT f $ stepWire w dt (Right x)
  return (r, getResourceWire $ withSubResource f (RCW w'))

mkContSF :: (TimeStep -> a -> (b, ContWire a b)) -> ContWire a b
mkContSF f = CW $ mkSF $ \dt x -> let (x', CW w') = f dt x in (x', w')

mkContSFN :: (a -> (b, ContWire a b)) -> ContWire a b
mkContSFN f = CW $ mkSFN $ \x -> let (x', CW w') = f x in (x', w')

mkContWire :: (TimeStep -> a -> GameMonad (b, ContWire a b)) -> ContWire a b
mkContWire f = CW $ mkGen $ \dt x -> do
  (r, CW w') <- f dt x
  return (Right r, w')

stepContWire :: ContWire a b -> TimeStep -> a -> GameMonad (b, ContWire a b)
stepContWire (CW w) dt x = do
  (Right r, w') <- stepWire w dt (Right x)
  return (r, CW w')

-- Wire that behaves like the identity wire until the given key
-- is pressed, then inhibits forever.
quitWire :: GLFW.Key -> GameWire a a
quitWire key =
  rSwitch mkId . (mkId &&& (now . pure mkEmpty . keyPressed key <|> never))

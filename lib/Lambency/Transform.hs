module Lambency.Transform (
  Transform, Invertible(..), Transformable3D(..),
  fromForwardUp, fromCoordinateBasis, identity,
  right, up, forward, localRight, localUp, localForward,
  scale, position,

  rotateWorld, xform2Matrix, transformPoint, invTransformPoint
) where

--------------------------------------------------------------------------------

import Linear.Matrix hiding (identity)
import Linear.Metric
import Linear.Vector
import Linear.V3
import Linear.V4
import Linear.Quaternion hiding (rotate)
import qualified Linear.Quaternion as Quat

import qualified Control.Wire as W
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative
#endif

--------------------------------------------------------------------------------

type Vec3f = V3 Float
type Quatf = Quaternion Float

type Mat3f = M33 Float
type Mat4f = M44 Float

-- A Transform consists of a right vector, an up vector, a forward vector, a
-- position in world space, and a scaling vector.
type CoordinateBasis = (Vec3f, Vec3f, Vec3f)

fromForwardUp :: Vec3f -> Vec3f -> CoordinateBasis
fromForwardUp f u =
  let
    r = u `cross` f
    u' = f `cross` r
  in (r, u', f)

basis2Matrix :: CoordinateBasis -> Mat3f
basis2Matrix (r, u, f) = adjoint $ V3 r u f

-- Basis is orthonormal, so inverse should be a simple transpose.
invertBasis :: CoordinateBasis -> CoordinateBasis
invertBasis (n1, n2, n3) =
  let
    V3 x1 y1 z1 = n1
    V3 x2 y2 z2 = n2
    V3 x3 y3 z3 = n3
  in
  (V3 x1 x2 x3,
   V3 y1 y2 y3,
   V3 z1 z2 z3)

-- !FIXME! Transform here is really just a series of operations
-- and not a full blown transform. Ideally, we'd like a few types:
--   1. TRS that represents a scale, rotation, and translation in
--      that order
--   2. Invertible transform that represents an arbitrary sequence
--      of translations, rotations, and scalings.
--   3. Transform that represents anything we can do in linear algebra
--
-- If we support these, then there is a clear way to go from TRS, which
-- is what almost every object in a 3D scene needs to a Transform, which
-- is what the underlying graphics engine is expecting, while still
-- allowing customized transforms
data Transform = Identity
               | Scale Vec3f Transform
               | OrthoNormal CoordinateBasis Transform
               | Translate Vec3f Transform
                 deriving (Show)

identity :: Transform
identity = Identity

fromCoordinateBasis :: CoordinateBasis -> Transform
fromCoordinateBasis b = OrthoNormal b Identity

-- !FIXME! There are Lens definitions for these
-- too, but they're used as ex ey and ez in Linear.
localRight :: Vec3f
localRight = V3 1 0 0

localUp :: Vec3f
localUp = V3 0 1 0

localForward :: Vec3f
localForward = V3 0 0 1

right :: Transform -> Vec3f
right Identity = localRight
right (Scale _ xf) = right xf
right (OrthoNormal (r, _, _) _) = r
right (Translate _ xf) = right xf

up :: Transform -> Vec3f
up Identity = localUp
up (Scale _ xf) = up xf
up (OrthoNormal (_, u, _) _) = u
up (Translate _ xf) = up xf

forward :: Transform -> Vec3f
forward Identity = localForward
forward (Scale _ xf) = forward xf
forward (OrthoNormal (_, _, f) _) = f
forward (Translate _ xf) = forward xf

scale :: Transform -> Vec3f
scale Identity = V3 1 1 1
scale (Scale s xf) = (*) <$> s <*> (scale xf)
scale (OrthoNormal _ xf) = scale xf
scale (Translate _ xf) = scale xf

position :: Transform -> Vec3f
position Identity = zero
position (Scale _ xf) = position xf
position (OrthoNormal _ xf) = position xf
position (Translate t xf) = t ^+^ (position xf)

updateAxis :: Vec3f -> Vec3f -> Vec3f -> Transform -> Transform
updateAxis nr nu nf Identity = OrthoNormal (nr, nu, nf) Identity
updateAxis nr nu nf (Scale s xf) = OrthoNormal (nr, nu, nf) $ Scale s xf
updateAxis nr nu nf (OrthoNormal _ xf) = OrthoNormal (nr, nu, nf) xf
updateAxis nr nu nf (Translate t xf) = Translate t $ updateAxis nr nu nf xf

renormalize :: Transform -> Transform
renormalize xf = updateAxis (right xf) u' f' xf
  where f' = (right xf) `cross` (up xf)
        u' = f' `cross` (right xf)

rotateWorld :: Quatf -> Transform -> Transform
rotateWorld quat xf = let

  r = right xf
  u = up xf
  f = forward xf

  invWorldMat :: Mat3f
  invWorldMat = V3 r u f

  worldMat :: Mat3f
  worldMat = adjoint invWorldMat

  rotateAxis :: Vec3f -> Vec3f
  rotateAxis = signorm . (worldMat !*) . (Quat.rotate quat) . (invWorldMat !*)

  in
   renormalize $ updateAxis (rotateAxis r) (rotateAxis u) (rotateAxis f) xf

-- Returns a matrix where that transforms a coordinate space such that the
-- new coordinate system's origin is located at the value of 'p' of the old
-- coordinate space, and the three axes that define forward up and right are
-- now the basis in Z, Y, and X respectively. Scale is applied localy in the
-- original coordinate space.
xform2Matrix :: Transform -> Mat4f
xform2Matrix xf =
  let
    extendWith :: a -> V3 a -> V4 a
    extendWith w (V3 x y z) = V4 x y z w

    V3 rx ry rz = right xf
    V3 ux uy uz = up xf
    V3 fx fy fz = forward xf
    V3 sx sy sz = scale xf
  in
   V4
   (extendWith 0.0 $ (sx *^) $ V3 rx ux fx)
   (extendWith 0.0 $ (sy *^) $ V3 ry uy fy)
   (extendWith 0.0 $ (sz *^) $ V3 rz uz fz)
   (extendWith 1.0 $ position xf)

class Invertible a where
  invert :: a -> a

instance Invertible Transform where
  invert Identity = Identity
  invert xform = (foldl (.) id $ modifiers xform) Identity
    where modifiers :: Transform -> [Transform -> Transform]
          modifiers Identity = []
          modifiers (Scale (V3 x y z) xf) = Scale (V3 (1/x) (1/y) (1/z)) : (modifiers xf)
          modifiers (OrthoNormal b xf) = OrthoNormal (invertBasis b) : (modifiers xf)
          modifiers (Translate t xf) = Translate (negated t) : (modifiers xf)

transformPoint :: Transform -> Vec3f -> Vec3f
transformPoint Identity = id
transformPoint (Scale s xf) = \x -> (*) <$> s <*> (transformPoint xf x)
transformPoint (OrthoNormal b xf) = ((basis2Matrix b) !*) . (transformPoint xf)
transformPoint (Translate t xf) = (^+^ t) . (transformPoint xf)

invTransformPoint :: Transform -> Vec3f -> Vec3f
invTransformPoint xf = transformPoint (invert xf)

class Transformable3D a where
  translate :: Vec3f -> a -> a
  rotate :: Quatf -> a -> a
  nonuniformScale :: Vec3f -> a -> a

  uniformScale :: Float -> a -> a
  uniformScale s = nonuniformScale $ V3 s s s

  transform :: Transform -> a -> a
  transform Identity = id
  transform (Scale s xf) = (nonuniformScale s) . (transform xf)
  transform (OrthoNormal (r, u, _) xf) = let
    determineRot :: Vec3f -> Vec3f -> Quatf
    determineRot v1 v2 = axisAngle (v1 `cross` v2) (acos $ v1 `dot` v2)
    firstRot = determineRot  r localRight
    secondRot = determineRot (Quat.rotate firstRot u) localUp
   in
    rotate (firstRot * secondRot) . (transform xf)
  transform (Translate t xf) = (translate t) . (transform xf)

instance Transformable3D Transform where
  translate t Identity = Translate t Identity
  translate t (Scale s xf) = Translate t $ Scale s xf
  translate t (OrthoNormal b xf) = Translate t $ OrthoNormal b xf
  translate t (Translate t' xf) = Translate (t ^+^ t') xf

  nonuniformScale s Identity = Scale s Identity
  nonuniformScale s (Scale s' xf) = Scale ((*) <$> s <*> s') xf
  nonuniformScale s (OrthoNormal b xf) = OrthoNormal b $ nonuniformScale s xf
  nonuniformScale s (Translate t xf) = Translate t $ nonuniformScale s xf

  -- Rotates the coordinate axis of the transform by the given quaternion. This
  -- function performs a local rotation
  rotate quat xf = let
    fn :: Vec3f -> Vec3f
    fn = Quat.rotate quat
    in updateAxis (fn $ right xf) (fn $ up xf) (fn $ forward xf) xf

--instance Transformable3D (V3 Float) where
--  translate = (^+^)
--  rotate = Quat.rotate
--  nonuniformScale = liftA2 (*)

instance (W.Monoid s, Monad m, Transformable3D b) =>
         Transformable3D (W.Wire s e m a b) where
  translate v w = W.mkGen $ \t a -> do
    (result, w') <- W.stepWire w t (Right a)
    case result of
      Left _ -> return $ (result, translate v w')
      Right r -> return $ (Right $ translate v r, translate v w')

  nonuniformScale s w = W.mkGen $ \t a -> do
    (result, w') <- W.stepWire w t (Right a)
    case result of
      Left _ -> return $ (result, translate s w')
      Right r -> return $ (Right $ nonuniformScale s r, nonuniformScale s w')

  rotate r w = W.mkGen $ \t a -> do
    (result, w') <- W.stepWire w t (Right a)
    case result of
      Left _ -> return $ (result, rotate r w')
      Right b -> return $ (Right $ rotate r b, rotate r w')

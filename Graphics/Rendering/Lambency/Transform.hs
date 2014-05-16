module Graphics.Rendering.Lambency.Transform (
  Transform, Invertible(..), Transformable3D(..),
  fromForwardUp, fromCoordinateBasis, identity,
  right, up, forward, right', up', forward',
  localRight, localUp, localForward, localRight', localUp', localForward',
  scale, position,

  rotateWorld, xform2Matrix, transformPoint, invTransformPoint
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import qualified Control.Wire as W

--------------------------------------------------------------------------------

-- A Transform consists of a right vector, an up vector, a forward vector, a
-- position in world space, and a scaling vector.
type CoordinateBasis = (Normal3, Normal3, Normal3)

fromForwardUp :: Normal3 -> Normal3 -> CoordinateBasis
fromForwardUp f u =
  let
    r = u &^ f
    u' = f &^ r
  in (r, u', f)

basis2Matrix :: CoordinateBasis -> Mat3
basis2Matrix (r, u, f) = transpose $ Mat3 (n r) (n u) (n f)
  where n = fromNormal

-- Basis is orthonormal, so inverse should be a simple transpose.
invertBasis :: CoordinateBasis -> CoordinateBasis
invertBasis (n1, n2, n3) =
  let
    Vec3 x1 y1 z1 = fromNormal n1
    Vec3 x2 y2 z2 = fromNormal n2
    Vec3 x3 y3 z3 = fromNormal n3
  in
  (toNormalUnsafe $ Vec3 x1 x2 x3,
   toNormalUnsafe $ Vec3 y1 y2 y3,
   toNormalUnsafe $ Vec3 z1 z2 z3)

data Transform = Identity
               | Scale Vec3 Transform
               | OrthoNormal CoordinateBasis Transform
               | Translate Vec3 Transform
                 deriving (Show)

identity :: Transform
identity = Identity

fromCoordinateBasis :: CoordinateBasis -> Transform
fromCoordinateBasis b = OrthoNormal b Identity

localRight' :: Vec3
localRight' = vec3X

localUp' :: Vec3
localUp' = vec3Y

localForward' :: Vec3
localForward' = vec3Z

localRight :: Normal3
localRight = toNormalUnsafe vec3X

localUp :: Normal3
localUp = toNormalUnsafe vec3Y

localForward :: Normal3
localForward = toNormalUnsafe vec3Z

right :: Transform -> Normal3
right Identity = localRight
right (Scale _ xf) = right xf
right (OrthoNormal (r, _, _) _) = r
right (Translate _ xf) = right xf

up :: Transform -> Normal3
up Identity = localUp
up (Scale _ xf) = up xf
up (OrthoNormal (_, u, _) _) = u
up (Translate _ xf) = up xf

forward :: Transform -> Normal3
forward Identity = localForward
forward (Scale _ xf) = forward xf
forward (OrthoNormal (_, _, f) _) = f
forward (Translate _ xf) = forward xf

right' :: Transform -> Vec3
right' = fromNormal . right

up' :: Transform -> Vec3
up' = fromNormal . up

forward' :: Transform -> Vec3
forward' = fromNormal . forward

scale :: Transform -> Vec3
scale Identity = Vec3 1 1 1
scale (Scale s xf) = s &! (scale xf)
scale (OrthoNormal _ xf) = scale xf
scale (Translate _ xf) = scale xf

position :: Transform -> Vec3
position Identity = zero
position (Scale _ xf) = position xf
position (OrthoNormal _ xf) = position xf
position (Translate t xf) = t &+ (position xf)

updateAxis :: Normal3 -> Normal3 -> Normal3 -> Transform -> Transform
updateAxis nr nu nf Identity = OrthoNormal (nr, nu, nf) Identity
updateAxis nr nu nf (Scale s xf) = OrthoNormal (nr, nu, nf) $ Scale s xf
updateAxis nr nu nf (OrthoNormal _ xf) = OrthoNormal (nr, nu, nf) xf
updateAxis nr nu nf (Translate t xf) = Translate t $ updateAxis nr nu nf xf

renormalize :: Transform -> Transform
renormalize xf = updateAxis (right xf) u' f' xf
  where f' = (right xf) &^ (up xf)
        u' = f' &^ (right xf)

rotateWorld :: UnitQuaternion -> Transform -> Transform
rotateWorld quat xf = let

  r = right xf
  u = up xf
  f = forward xf
  
  invWorldMat :: Mat3
  invWorldMat = Mat3 (fromNormal r) (fromNormal u) (fromNormal f)

  worldMat :: Mat3
  worldMat = transpose invWorldMat

  rotateAxis :: Normal3 -> Normal3
  rotateAxis = mkNormal . (worldMat *.) . (actU quat) . (invWorldMat *.) . fromNormal

  in
   renormalize $ updateAxis (rotateAxis r) (rotateAxis u) (rotateAxis f) xf

-- Returns a matrix where that transforms a coordinate space such that the
-- new coordinate system's origin is located at the value of 'p' of the old
-- coordinate space, and the three axes that define forward up and right are
-- now the basis in Z, Y, and X respectively. Scale is applied localy in the
-- original coordinate space.
xform2Matrix :: Transform -> Mat4
xform2Matrix xf =
  let
    Vec3 rx ry rz = right' xf
    Vec3 ux uy uz = up' xf
    Vec3 fx fy fz = forward' xf
    Vec3 sx sy sz = scale xf
  in
   Mat4
   (extendZero $ (sx *&) $ Vec3 rx ux fx)
   (extendZero $ (sy *&) $ Vec3 ry uy fy)
   (extendZero $ (sz *&) $ Vec3 rz uz fz)
   (extendWith 1.0 $ position xf)

class Invertible a where
  invert :: a -> a

instance Invertible Transform where
  invert Identity = Identity
  invert xform = (foldl (.) id $ modifiers xform) Identity
    where modifiers :: Transform -> [Transform -> Transform]
          modifiers Identity = []
          modifiers (Scale (Vec3 x y z) xf) = Scale (Vec3 (1/x) (1/y) (1/z)) : (modifiers xf)
          modifiers (OrthoNormal b xf) = OrthoNormal (invertBasis b) : (modifiers xf)
          modifiers (Translate t xf) = Translate (neg t) : (modifiers xf)

transformPoint :: Transform -> Vec3 -> Vec3
transformPoint Identity = id
transformPoint (Scale s xf) = (s &!) . (transformPoint xf)
transformPoint (OrthoNormal b xf) = ((basis2Matrix b) *.) . (transformPoint xf)
transformPoint (Translate t xf) = (&+ t) . (transformPoint xf)

invTransformPoint :: Transform -> Vec3 -> Vec3
invTransformPoint xf = transformPoint (invert xf)

class Transformable3D a where
  translate :: Vec3 -> a -> a
  rotate :: UnitQuaternion -> a -> a
  nonuniformScale :: Vec3 -> a -> a

  uniformScale :: Float -> a -> a
  uniformScale s = nonuniformScale $ Vec3 s s s

instance Transformable3D Transform where
  translate t Identity = Translate t Identity
  translate t (Scale s xf) = Translate t $ Scale s xf
  translate t (OrthoNormal b xf) = Translate t $ OrthoNormal b xf
  translate t (Translate t' xf) = Translate (t &+ t') xf

  nonuniformScale s Identity = Scale s Identity
  nonuniformScale s (Scale s' xf) = Scale (s &! s') xf
  nonuniformScale s (OrthoNormal b xf) = OrthoNormal b $ nonuniformScale s xf
  nonuniformScale s (Translate t xf) = Translate t $ nonuniformScale s xf

  -- Rotates the coordinate axis of the transform by the given quaternion. This
  -- function performs a local rotation
  rotate quat xf = let
    fn :: Normal3 -> Normal3
    fn = toNormalUnsafe . (actU quat) . fromNormal
    in updateAxis (fn $ right xf) (fn $ up xf) (fn $ forward xf) xf

instance Transformable3D Vec3 where
  translate = (&+)
  rotate = actU
  nonuniformScale = (&!)

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
-- instance Transformable3D b => Transformable3D (W.Wire s e m a b) where

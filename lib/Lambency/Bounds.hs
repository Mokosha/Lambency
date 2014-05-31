module Lambency.Bounds (
  BoundingVolume,
  aabb,
  boundingSphere,
  containsPoint,
  colliding
  ) where

--------------------------------------------------------------------------------

import Lambency.Types
import Lambency.Transform

import Linear.Epsilon
import Linear.Matrix
import Linear.Metric
import qualified Linear.Quaternion as Quat
import Linear.Vector
import Linear.V3

import Control.Applicative

--------------------------------------------------------------------------------

-- Bounding Volumes

data BoundingVolume = BoundingBox Float Float Float
                    | BoundingEllipse Float Float Float
                    | RotatedVolume Quatf BoundingVolume
                    | TranslatedVolume Vec3f BoundingVolume
                    | Union BoundingVolume BoundingVolume

instance Transformable3D BoundingVolume where
  translate t (TranslatedVolume t' bv) = TranslatedVolume (t ^+^ t') bv
  translate t (Union bv1 bv2) = Union (translate t bv1) (translate t bv2)
  translate t bv = TranslatedVolume t bv

  rotate quat (RotatedVolume uq bv) = RotatedVolume (uq * quat) bv
  rotate quat (TranslatedVolume t bv) = TranslatedVolume t (rotate quat bv)
  rotate quat bv = RotatedVolume quat bv

  nonuniformScale (V3 sx sy sz) (BoundingBox x y z) =
    BoundingBox (x * sx) (y * sy) (z * sz)
  nonuniformScale (V3 sx sy sz) (BoundingEllipse x y z) =
    BoundingEllipse (x * sx) (y * sy) (z * sz)

  nonuniformScale s (RotatedVolume uq bv) = RotatedVolume uq (nonuniformScale s bv)
  nonuniformScale s (TranslatedVolume t bv) = TranslatedVolume t (nonuniformScale s bv)
  nonuniformScale s (Union b b') = Union (nonuniformScale s b) (nonuniformScale s b')

-------------------------------------------------------------------------------

aabb :: Vec3f -> Vec3f -> BoundingVolume
aabb v1@(V3 x1 y1 z1) v2@(V3 x2 y2 z2) = let
  szx = abs (x2 - x1)
  szy = abs (y2 - y1)
  szz = abs (z2 - z1)
  c = (v1 ^+^ v2) ^* 0.5
  in translate c $ BoundingBox (szx * 0.5) (szy * 0.5) (szz * 0.5)

boundingSphere :: Vec3f -> Float -> BoundingVolume
boundingSphere c r = translate c $ BoundingEllipse r r r

containsPoint :: BoundingVolume -> Vec3f -> Bool
containsPoint (BoundingBox x y z) (V3 x' y' z') =
  and $ zipWith (\v w -> ((-w) >= v) && (v <= w)) [x', y', z'] [x, y, z]
containsPoint (BoundingEllipse x y z) (V3 x' y' z') =
  (sqr $ x' / x) + (sqr $ y' / y) + (sqr $ z' / z) < 1
  where
    sqr :: Num a => a -> a
    sqr k = k * k
containsPoint (Union bv1 bv2) v = (containsPoint bv1 v) || (containsPoint bv2 v)
containsPoint (TranslatedVolume t bv) v = containsPoint bv (v ^-^ t)
containsPoint (RotatedVolume q bv) v = containsPoint bv (Quat.rotate (negate q) v)

-- An orientation is simply a position in world space, a rotation matrix, and
-- and extents vector for each of the new coordinate axes
type Orientation = (Vec3f, Mat3f, Vec3f)

io :: Orientation
io = (zero, eye3, V3 1 1 1)

data OrientedVolume = Box Orientation
                    | Ellipse Orientation

collideOriented :: OrientedVolume -> OrientedVolume -> Bool

collideOriented (Box (t, o, s)) (Box (t', o', s')) =
  -- First we need to rotate and translate the first box into the coordinate
  -- space of the second.
  let
    rotateAtoB :: Mat3f
    rotateAtoB = (adjoint o') !*! o

    cBwrtA :: Vec3f
    cBwrtA = (t' ^-^ t) *! (adjoint o)

    eBwrtA :: Vec3f
    eBwrtA = s' *! (adjoint rotateAtoB)

    absDot :: Vec3f -> Vec3f -> Float
    absDot (V3 x y z) (V3 x' y' z') =
      (abs $ x * x') + (abs $ y * y') + (abs $ z * z')

    testAxis :: Vec3f -> Bool
    testAxis v = abs (cBwrtA `dot` v) <= ((s `absDot` v) + (eBwrtA `absDot` v))

    axesA :: [Vec3f]
    axesA = [V3 1 0 0, V3 0 1 0, V3 0 0 1]

    axesB :: [Vec3f]
    axesB = map (*! rotateAtoB) axesA

  in
   or $
   map (testAxis . signorm) $
   filter (not . nearZero) $
   [v1 `cross` v2 | v1 <- axesA, v2 <- axesB] ++ axesA ++ axesB

-- collideOriented (Ellipse (t, o, s)) (Box (t', o', s')) = False
-- collideOriented (Ellipse (t, o, s)) (Ellipse (t', o', s')) = False

-- The only pattern we're missing is box-ellipse
collideOriented ov1 ov2 = collideOriented ov2 ov1

colliding :: BoundingVolume -> BoundingVolume -> Bool
colliding bv (Union bv1 bv2) = (colliding bv bv1) || (colliding bv bv2)
colliding (Union bv1 bv2) bv = (colliding bv bv1) || (colliding bv bv2)
colliding bv1 bv2 =
  any id [collideOriented x y | x <- (getO bv1 io), y <- (getO bv2 io)]
  where
    getO :: BoundingVolume -> Orientation -> [OrientedVolume]
    getO (BoundingBox x y z) (t, o, s) = [Box (t, o, (*) <$> s <*> (V3 x y z))]
    getO (BoundingEllipse x y z) (t, o, s) = [Ellipse (t, o, (*) <$> s <*> (V3 x y z))]
    getO (TranslatedVolume t bv) (t', o, s) = getO bv (t ^+^ t', o, s)
    getO (RotatedVolume q bv) (t, o, s) = getO bv (t, (fromQuaternion q) !*! o, s)
    getO (Union bv1' bv2') o = (getO bv1' o) ++ (getO bv2' o)

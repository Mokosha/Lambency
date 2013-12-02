module Graphics.Rendering.Lambency.Transform (
  Transform(..), identityXForm,
  localRight, localUp, localForward,

  right, up, forward,

  rotate, rotateWorld, translate, uniformScale,
  
  xform2Matrix,
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

--------------------------------------------------------------------------------

-- A Transform consists of a right vector, an up vector, a forward vector, a
-- position in world space, and a scaling vector.
data Transform = XForm Normal3 Normal3 Normal3 Vec3 Vec3

localRight :: Normal3
localRight = toNormalUnsafe vec3X

localUp :: Normal3
localUp = toNormalUnsafe vec3Y

localForward :: Normal3
localForward = toNormalUnsafe vec3Z

identityXForm :: Transform
identityXForm = XForm localRight localUp localForward zero (Vec3 1 1 1)

right :: Transform -> Normal3
right (XForm r _ _ _ _) = r

up :: Transform -> Normal3
up (XForm _ u _ _ _) = u

forward :: Transform -> Normal3
forward (XForm _ _ f _ _) = f

renormalize :: Transform -> Transform
renormalize (XForm r u _ p s) = XForm r u' f' p s
  where f' = r &^ u
        u' = f' &^ r

-- Rotates the coordinate axis of the transform by the given quaternion. This
-- function performs a local rotation
rotate :: UnitQuaternion -> Transform -> Transform
rotate quat (XForm r u f p s) = let
  fn :: Normal3 -> Normal3
  fn = toNormalUnsafe . (actU quat) . fromNormal
  in XForm (fn r) (fn u) (fn f) p s

rotateWorld :: UnitQuaternion -> Transform -> Transform
rotateWorld quat (XForm r u f p s) = let
  invWorldMat :: Mat3
  invWorldMat = Mat3 (fromNormal r) (fromNormal u) (fromNormal f)

  worldMat :: Mat3
  worldMat = transpose invWorldMat

  rotateAxis :: Normal3 -> Normal3
  rotateAxis = mkNormal . (worldMat *.) . (actU quat) . (invWorldMat *.) . fromNormal

  in
   renormalize $ XForm (rotateAxis r) (rotateAxis u) (rotateAxis f) p s
  

uniformScale :: Float -> Transform -> Transform
uniformScale s (XForm r u f p _) = XForm r u f p (Vec3 s s s)

translate :: Vec3 -> Transform -> Transform
translate t (XForm r u f p s) = XForm r u f (p &+ t) s

-- Returns a matrix where that transforms a coordinate space such that the
-- new coordinate system's origin is located at the value of 'p' of the old
-- coordinate space, and the three axes that define forward up and right are
-- now the basis in Z, Y, and X respectively.
xform2Matrix :: Transform -> Mat4
xform2Matrix (XForm r u f p s) =
  let te n sc = extendWith (p &. (fromNormal n)) (sc s *& (fromNormal n))
  in transpose $ Mat4 (te r _1) (te u _2) (te f _3) (Vec4 0 0 0 1)

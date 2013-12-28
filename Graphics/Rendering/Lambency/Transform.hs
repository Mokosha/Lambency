module Graphics.Rendering.Lambency.Transform (
  Transform, fromForwardUp, fromCoordinateBasis, identity,
  right, up, forward, right', up', forward',
  localRight, localUp, localForward, localRight', localUp', localForward',
  scale, position,

  rotate, rotateWorld, translate, uniformScale, nonuniformScale,
  
  xform2Matrix, transformPoint
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

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

-- Rotates the coordinate axis of the transform by the given quaternion. This
-- function performs a local rotation
rotate :: UnitQuaternion -> Transform -> Transform
rotate quat xf = let
  fn :: Normal3 -> Normal3
  fn = toNormalUnsafe . (actU quat) . fromNormal
  in updateAxis (fn $ right xf) (fn $ up xf) (fn $ forward xf) xf

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

nonuniformScale :: Vec3 -> Transform -> Transform
nonuniformScale s Identity = Scale s Identity
nonuniformScale s (Scale s' xf) = Scale (s &! s') xf
nonuniformScale s (OrthoNormal b xf) = OrthoNormal b $ nonuniformScale s xf
nonuniformScale s (Translate t xf) = Translate t $ nonuniformScale s xf

uniformScale :: Float -> Transform -> Transform
uniformScale s = nonuniformScale $ Vec3 s s s

translate :: Vec3 -> Transform -> Transform
translate t Identity = Translate t Identity
translate t (Scale s xf) = Translate t $ Scale s xf
translate t (OrthoNormal b xf) = Translate t $ OrthoNormal b xf
translate t (Translate t' xf) = Translate (t &+ t') xf

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

transformPoint :: Transform -> Vec3 -> Vec3
transformPoint xform pt = let
  Vec4 x y z _ = (extendWith 1.0 pt) .* (xform2Matrix xform)
  in Vec3 x y z

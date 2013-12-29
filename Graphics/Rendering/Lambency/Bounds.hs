module Graphics.Rendering.Lambency.Bounds (
  aabb,
  obb,
  boundingSphere,
  containsPoint,
  contains
  ) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Types
import Graphics.Rendering.Lambency.Transform

import Data.Vect.Float hiding (distance)
--------------------------------------------------------------------------------

aabb :: Vec3 -> Vec3 -> BoundingVolume
aabb v1@(Vec3 x1 y1 z1) v2@(Vec3 x2 y2 z2) = let
  szx = abs (x2 - x1)
  szy = abs (y2 - y1)
  szz = abs (z2 - z1)
  c = (v1 &+ v2) &* 0.5
  in TransformedVolume
     (nonuniformScale (Vec3 (szx * 0.5) (szy * 0.5) (szz * 0.5)) $
      translate c identity)
     UnitBox

obb :: Vec3 -> Vec3 -> Transform -> BoundingVolume
obb mi ma xf = TransformedVolume xf $ aabb mi ma

boundingSphere :: Vec3 -> Float -> BoundingVolume
boundingSphere c r =
  TransformedVolume (uniformScale r $ translate c identity) UnitSphere

containsPoint :: BoundingVolume -> Vec3 -> Bool
containsPoint UnitBox (Vec3 x y z) =
  and $ map (\v -> ((-1) >= v) && (v <= 1)) [x, y, z]
containsPoint UnitSphere v = lensqr v < 1
containsPoint (Union bv1 bv2) v = (containsPoint bv1 v) || (containsPoint bv2 v)
containsPoint (TransformedVolume xf bv) v = containsPoint bv (invTransformPoint xf v)

distance :: BoundingVolume -> Vec3 -> Float
distance bv v@(Vec3 x y z) =
  case bv of
    UnitBox ->
      if containsPoint UnitBox v then
        minimum $ map (((-1) +) . abs) [x, y, z]
      else
        let [x', y', z'] = map (max 0 . (\k -> abs k - 1)) [x, y, z]
        in len $ Vec3 x' y' z'

    UnitSphere -> len v - 1

    (Union bv1 bv2) ->
      if (containsPoint bv1 v) && (containsPoint bv2 v) then
        -- The point is within both volumes, so the distance is the closer
        -- of the two. The closer one will be the one with the less negative
        -- distance...
        max (distance bv1 v) (distance bv2 v)
      else
        -- At least one volume doesn't contain the point, so the distance is
        -- the smallest distance from either...
        min (distance bv1 v) (distance bv2 v)

    -- !FIXME! We need to extract the scale and then inverse transform the
    -- point and determine the distance in each axis and then multiply that
    -- by the scale.
    (TransformedVolume xf bv') -> distance bv' v

-- Returns whether or not the first bounding volume is completely contained
-- within the second.
contains :: BoundingVolume -> BoundingVolume -> Bool
contains UnitBox bv =
  and $ map (containsPoint bv)
  [Vec3 x y z | x <- [(-1), 1], y <- [(-1), 1], z <- [(-1), 1]]
contains UnitSphere bv = (distance bv zero) < (-0.9999)
contains (Union bv1 bv2) bv = (contains bv1 bv) && (contains bv2 bv)
contains (TransformedVolume xf bv') bv =
  contains bv' (TransformedVolume (invert xf) bv)

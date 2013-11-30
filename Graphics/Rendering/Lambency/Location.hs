module Graphics.Rendering.Lambency.Location (

  Location(..),
  getPosition, getOrientation, getScale,
  loc2Matrix
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Dim4
import Data.Vect.Float.Util.Quaternion

--------------------------------------------------------------------------------

-- Location is simply a position in world space, a scaling, and an orientation
data Location = Location Vec3 UnitQuaternion Float

getPosition :: Location -> Vec3
getPosition (Location p _ _) = p

getOrientation :: Location -> UnitQuaternion
getOrientation (Location _ o _) = o

getScale :: Location -> Float
getScale (Location _ _ s) = s

loc2Matrix :: Location -> Mat4
loc2Matrix (Location pos rot scale) =
  let s = Mat4 (scale *& vec4X) (scale *& vec4Y) (scale *& vec4Z) vec4W
      r = (extendWith 1.0) . fromOrtho . leftOrthoU $ rot
      t = Mat4 vec4X vec4Y vec4Z (extendWith 1.0 pos)
  in s .*. r .*. t


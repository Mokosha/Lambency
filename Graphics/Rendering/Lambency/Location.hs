module Graphics.Rendering.Lambency.Location (

  Location,
  getPosition, getOrientation, getScale,
  
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
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


module Graphics.Rendering.Lambency.Object (
  GameObject(..),
  updateObjs,
  interactObjs,
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Renderable

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import Control.Applicative

import Data.Maybe (catMaybes)

--------------------------------------------------------------------------------

type Time = Double

data GameObject a = GameObject {
  position :: Vec3,
  orientation :: UnitQuaternion,
  renderObject :: Maybe RenderObject,
  gameObject :: a,
  update :: Time -> GameObject a -> Maybe (GameObject a),
  collide :: GameObject a -> [GameObject a] -> Maybe (GameObject a)
}

updateObjs :: Time -> [GameObject a] -> [GameObject a]
updateObjs dt objs = catMaybes $ (\obj -> update obj dt obj) <$> objs

interactObjs :: [GameObject a] -> [GameObject a]
interactObjs allobjs = catMaybes $ (\obj -> (collide obj) obj allobjs) <$> allobjs

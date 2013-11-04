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

--------------------------------------------------------------------------------

type Time = Double

data GameObject a = GameObject {
  position :: Vec3,
  orientation :: UnitQuaternion,
  renderObject :: Maybe RenderObject,
  gameObject :: a,
  update :: Time -> a -> Maybe a,
  collide :: a -> [a] -> Maybe a
}

filterMaybe :: (GameObject a, Maybe a) -> [GameObject a]
filterMaybe (_, Nothing) = []
filterMaybe (obj, Just go) = [(\o -> o { gameObject = go }) obj]

updateObjs :: Time -> [GameObject a] -> [GameObject a]
updateObjs dt objs = do
  newMaybeObjs <- (\obj -> (obj, update obj dt $ gameObject obj)) <$> objs
  filterMaybe newMaybeObjs

interactObjs :: [GameObject a] -> [GameObject a]
interactObjs allobjs = do
  newMaybeObjs <- (\obj -> (obj, (collide obj) (gameObject obj) (gameObject <$> allobjs))) <$> allobjs
  filterMaybe newMaybeObjs

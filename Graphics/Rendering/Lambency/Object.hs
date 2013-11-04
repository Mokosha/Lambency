module Graphics.Rendering.Lambency.Object (
  BaseObject(..),
  GameObject(..),
  getRenderObject,
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

data BaseObject = BaseObject {
  position :: Vec3,
  orientation :: UnitQuaternion,
  renderObj :: Maybe RenderObject
}

data GameObject a = GameObject {
  baseObject :: BaseObject,
  gameObject :: a,
  update :: Time -> a -> Maybe a,
  collide :: a -> [a] -> Maybe a
}

getRenderObject :: GameObject a -> Maybe RenderObject
getRenderObject go = renderObj $ baseObject go

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

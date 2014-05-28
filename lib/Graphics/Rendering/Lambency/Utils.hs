module Graphics.Rendering.Lambency.Utils (
  compareZero,
  compareClose,
  destructMat4,
  clamp,
  newRange,
  newRangeC,
) where

--------------------------------------------------------------------------------

import Linear.Epsilon
import Linear.Metric

import Prelude hiding (concat)
import Data.Foldable
--------------------------------------------------------------------------------

compareZero :: (Ord a, Epsilon a, Metric v) => v a -> Bool
compareZero x = nearZero $ (abs $ x `dot` x)

compareClose :: (Ord a, Epsilon a, Metric v) => v a -> v a -> Bool
compareClose x y = nearZero $ max (y `dot` y) (x `dot` x) - (x `dot` y)

destructMat4 :: (Functor f, Foldable f) => f (f a) -> [a]
destructMat4 = concat . (fmap toList)

clamp :: Ord a => a -> a -> a -> a
clamp x a b = if x < a then a else if x > b then b else x

newRange :: Floating a => a -> (a, a) -> (a, a) -> a
newRange x (omin, omax) (nmin, nmax) =
  nmin + (nmax - nmin) * ((x - omin) / (omax - omin))

newRangeC :: (Ord a, Floating a) => a -> (a, a) -> (a, a) -> a
newRangeC x o n@(nmin, nmax) = clamp (newRange x o n) nmin nmax

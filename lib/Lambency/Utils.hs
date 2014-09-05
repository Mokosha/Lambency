module Lambency.Utils (
  compareZero,
  compareClose,
  destructMat4,
  clamp,
  newRange,
  newRangeC,
  --
  CyclicList(..),
  advance,
  cyclicLength,
  cyclicFromList,
  cyclicToList,
  cycleSingleton,
  cycles
) where

--------------------------------------------------------------------------------
import Control.Comonad

import Data.Foldable

import Linear.Epsilon
import Linear.Metric

import Prelude hiding (concat)
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

--------------------------------------------------------------------------------
-- Cyclic lists

data CyclicList a = CyclicList [a] a [a]

instance Functor CyclicList where
  fmap f (CyclicList p c n) = CyclicList (fmap f p) (f c) (fmap f n)

advance :: CyclicList a -> CyclicList a
advance (CyclicList p c []) = let (r:rs) = reverse (c:p) in CyclicList [] r rs
advance (CyclicList p c (n:ns)) = CyclicList (c:p) n ns

cyclicLength :: CyclicList a -> Int
cyclicLength (CyclicList x _ z) = length x + length z + 1

cyclicFromList :: [a] -> CyclicList a
cyclicFromList [] = error "Cannot create empty cyclic list"
cyclicFromList (x:xs) = CyclicList [] x xs

cyclicToList :: CyclicList a -> [a]
cyclicToList (CyclicList p c n) = concat [reverse p, [c], n]

cycleSingleton :: a -> CyclicList a
cycleSingleton x = CyclicList [] x []

cycles :: CyclicList a -> [CyclicList a]
cycles cl = let
  helper 0 _ = []
  helper n cl' = cl' : (helper (n-1) $ advance cl')
  in helper (cyclicLength cl) cl

instance Comonad CyclicList where
  extract (CyclicList _ x _) = x
  duplicate = cyclicFromList . cycles

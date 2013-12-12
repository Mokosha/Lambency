module Graphics.Rendering.Lambency.Utils (
  compareZero,
  compareClose,
  destructMat4,
  negN,
  newRange,
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Dim4

--------------------------------------------------------------------------------

compareZero :: (DotProd v) => v -> Bool
compareZero v = (abs $ v &. v) < 1e-6

compareClose :: (DotProd v) => v -> v -> Bool
compareClose a b = max (b &. b) (a &. a) - (a &. b) < 1e-6

destructMat4 :: Mat4 -> [Float]
destructMat4 mat = let
  Mat4 r1 r2 r3 r4 = mat
  in
   destructVec4 [r1, r2, r3, r4]

negN :: UnitVector v u => u -> u
negN = toNormalUnsafe . neg . fromNormal

newRange :: Floating a => a -> (a, a) -> (a, a) -> a
newRange x (omin, omax) (nmin, nmax) =
  nmin + (nmax - nmin) * ((x - omin) / (omax - omin))

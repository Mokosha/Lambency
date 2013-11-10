module Graphics.Rendering.Lambency.Utils (
  compareZero,
  quatFromVecs,
  destructMat4
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion
import Data.Vect.Float.Util.Dim4

--------------------------------------------------------------------------------

compareZero :: (DotProd v) => v -> Bool
compareZero v = (abs $ v &. v) < 1e-6

checkParallel :: (DotProd v) => v -> v -> Bool
checkParallel v1 v2 = 1 - (abs $ v1 &. v2) < 1e-6 

quatFromVecs :: Normal3 -> Normal3 -> UnitQuaternion
quatFromVecs n1 n2
  | 1 - (abs d) < 1e-6 = handleParallel
  | otherwise = rotU cv $ angle (fromNormal n1) (fromNormal n2)
  where
    d = n1 &. n2
    cv = fromNormal $ n1 &^ n2
    handleParallel :: UnitQuaternion
    handleParallel
      | d < 0 = let n3X = toNormalUnsafe vec3X
                    nv = fromNormal $ n1 &^ n3X
                in
                 if checkParallel n1 n3X then
                   rotU vec3Y pi
                 else
                   rotU nv pi
      | otherwise = unitU     

destructMat4 :: Mat4 -> [Float]
destructMat4 mat = let
  Mat4 r1 r2 r3 r4 = mat
  in
   destructVec4 [r1, r2, r3, r4]

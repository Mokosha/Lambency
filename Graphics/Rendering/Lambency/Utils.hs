module Graphics.Rendering.Lambency.Utils (
  quatFromVecs
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

--------------------------------------------------------------------------------

quatFromVecs :: Normal3 -> Normal3 -> UnitQuaternion
quatFromVecs n1 n2 =
  let cross = n1 &^ n2
      cv = fromNormal cross
      d = n1 &. n2
  in
   if 1 - (abs d) < 0.000001 then
     if n1 &. n2 < 0 then
       let n3X = toNormalUnsafe vec3X
           n = n1 &^ n3X
           nv = fromNormal n
       in
        if 1 - (abs $ n1 &. n3X) < 0.000001 then
          rotU vec3Y pi
        else
          rotU nv pi
      else
       unitU
   else
     rotU cv $ angle (fromNormal n1) (fromNormal n2)

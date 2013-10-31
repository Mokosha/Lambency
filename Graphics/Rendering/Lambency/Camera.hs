module Graphics.Rendering.Lambency.Camera (
  Camera(..),
  OrthoCamera(..),
  simpleOrthoCamera,
  getViewProjMatrix,
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Dim4

--------------------------------------------------------------------------------

instance Eq Vec3 where
  (==) (Vec3 a b c) (Vec3 d e f) = (a == d) && (b == e) && (c == f)

genTransMatrix :: Vec3 -> Ortho4
genTransMatrix p =
  transpose . toOrthoUnsafe $ Mat4 vec4X vec4Y vec4Z (extendWith 1.0 $ neg p)

genRotMatrix :: Normal3 -> Ortho4
genRotMatrix n = let
  nv = fromNormal n
  axis = vec3Z &^ nv
  agl = angle nv vec3Z
  in
   if axis == (Vec3 0 0 0) then
     one
   else
     toOrthoUnsafe . (extendWith 1) $ rotMatrix3 axis agl

class Camera c where
  getPosition :: c -> Vec3
  setPosition :: c -> Vec3 -> c

  getDirection :: c -> Normal3
  setDirection :: c -> Normal3 -> c

  getNearPlane :: c -> Float
  setNearPlane :: c -> Float -> c

  getFarPlane :: c -> Float
  setFarPlane :: c -> Float -> c

  getProjMatrix :: c -> Mat4

getViewProjMatrix :: Camera c => c -> [Float]
getViewProjMatrix c = let
  pm = getProjMatrix c
  pos = getPosition c
  dir = getDirection c
  -- !SPEED! We can probably construct this ourselves
  -- instead of using utility functions to generate it...
  viewMatrix = (genTransMatrix pos) .*. (genRotMatrix dir)
  Mat4 r1 r2 r3 r4 = (.*.) pm $ fromOrtho viewMatrix
  in
   (d [r1]) ++ (d [r2]) ++ (d [r3]) ++ (d [r4])
   where d :: [Vec4] -> [Float]
         d = destructVec4
--     pm .*. (genTransMatrix pos) .*. (genRotMatrix dir)

data OrthoCamera = Ortho {
  position :: Vec3,
  direction :: Normal3,
  left :: Float,
  right :: Float,
  top :: Float,
  bottom :: Float,
  near :: Float,
  far :: Float
}

simpleOrthoCamera :: OrthoCamera
simpleOrthoCamera = Ortho {
  position = Vec3 0 0 0,
  direction = toNormalUnsafe $ Vec3 0 0 1,
  left = -10,
  right = 10,
  top = 10,
  bottom = -10,
  near = 0.1,
  far = 1000.0
}

instance Camera OrthoCamera where
  getPosition = position
  setPosition c pos = Ortho {
    position = pos,
    direction = direction c,
    left = left c,
    right = right c,
    top = top c,
    bottom = bottom c,
    near = near c,
    far = far c
  }
  
  getDirection = direction
  setDirection c dir = Ortho {
    position = position c,
    direction = dir,
    left = left c,
    right = right c,
    top = top c,
    bottom = bottom c,
    near = near c,
    far = far c
  }
  
  getNearPlane = near
  setNearPlane c n = Ortho {
    position = position c,
    direction = direction c,
    left = left c,
    right = right c,
    top = top c,
    bottom = bottom c,
    near = n,
    far = far c
  }

  getFarPlane = far
  setFarPlane c f = Ortho {
    position = position c,
    direction = direction c,
    left = left c,
    right = right c,
    top = top c,
    bottom = bottom c,
    near = near c,
    far = f
  }

  getProjMatrix c = let
    t = top c
    b = bottom c
    l = left c
    r = right c
    n = near c
    f = far c
    in
     Mat4
     (Vec4 (2.0 / (r - l)) 0 0 (-(r+l)/(r-l)))
     (Vec4 0 (2.0 / (t - b)) 0 (-(t+b)/(t-b)))
     (Vec4 0 0 (2.0 / (f - n)) (-(f+n)/(f-n)))
     (Vec4 0 0 0 1)

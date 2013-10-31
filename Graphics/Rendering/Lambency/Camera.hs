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

instance Eq Vec4 where
  (==) (Vec4 a b c g) (Vec4 d e f h) = (a == d) && (b == e) && (c == f) && (g == h)

class Camera c where
  getPosition :: c -> Vec3
  setPosition :: c -> Vec3 -> c

  getDirection :: c -> Normal3
  setDirection :: c -> Normal3 -> c

  getUpDirection :: c -> Normal3
  setUpDirection :: c -> Normal3 -> c

  getNearPlane :: c -> Float
  setNearPlane :: c -> Float -> c

  getFarPlane :: c -> Float
  setFarPlane :: c -> Float -> c

  getProjMatrix :: c -> Mat4

genViewMatrix :: Camera c => c -> Mat4
genViewMatrix c = let
  dir = (getDirection c)
  side = crossprod dir $ getUpDirection c
  up = side &^ dir
  in
   if (f side) == zero then
     one
   else
     transpose $ Mat4 (f side) (f up) (neg $ f dir) (extendWith 1.0 $ neg (getPosition c))
  where f = extendZero . fromNormal

getViewProjMatrix :: Camera c => c -> [Float]
getViewProjMatrix c = let
  pm = getProjMatrix c
  vm = genViewMatrix c
  Mat4 r1 r2 r3 r4 = pm .*. vm
  in
   destructVec4 [r1, r2, r3, r4]

data OrthoCamera = Ortho {
  position :: Vec3,
  direction :: Normal3,
  upDirection :: Normal3,
  left :: Float,
  right :: Float,
  top :: Float,
  bottom :: Float,
  near :: Float,
  far :: Float
}

simpleOrthoCamera :: OrthoCamera
simpleOrthoCamera = Ortho {
  position = zero,
  direction = toNormalUnsafe $ neg vec3Z,
  upDirection = toNormalUnsafe vec3Y,
  left = -10,
  right = 10,
  top = 10,
  bottom = -10,
  near = 0.1,
  far = 1000.0
}

instance Camera OrthoCamera where
  getPosition = position
  setPosition c pos = (\cam -> cam { position = pos }) c
  
  getDirection = direction
  setDirection c dir = (\cam -> cam { direction = dir}) c

  getUpDirection = upDirection
  setUpDirection c dir = (\cam -> cam { upDirection = dir}) c
  
  getNearPlane = near
  setNearPlane c n = (\cam -> cam { near = n }) c

  getFarPlane = far
  setFarPlane c f = (\cam -> cam { far = f }) c

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

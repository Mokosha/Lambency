module Graphics.Rendering.Lambency.Camera (
  Camera(..),
  CameraType(..),
  GameCamera,
  simpleOrthoCamera,
  getViewProjMatrix,
  renderCamera
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Dim4
import Data.Vect.Float.Util.Quaternion

import Graphics.Rendering.Lambency.Object
import Graphics.Rendering.Lambency.Renderable
--------------------------------------------------------------------------------

instance Eq Vec4 where
  (==) (Vec4 a b c g) (Vec4 d e f h) = (a == d) && (b == e) && (c == f) && (g == h)

instance Eq Vec3 where
  (==) (Vec3 a b c) (Vec3 d e f) = (a == d) && (b == e) && (c == f)

instance Eq Normal3 where
  (==) n1 n2 = (fromNormal n1) == (fromNormal n2)

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

data CameraType =
  Ortho {
    upDirection :: Normal3,
    left :: Float,
    right :: Float,
    top :: Float,
    bottom :: Float,
    near :: Float,
    far :: Float
    }

newtype GameCamera = GameCamera (GameObject CameraType)

simpleOrthoCamera :: GameCamera
simpleOrthoCamera = GameCamera GameObject {
  position = Vec3 0 0 0,
  orientation = unitU,
  renderObject = Nothing,
  gameObject = Ortho {
    upDirection = toNormalUnsafe vec3Y,
    left = -10,
    right = 10,
    top = 10,
    bottom = -10,
    near = 0.1,
    far = 1000.0
    },
  update = (\t a -> Just a),
  collide = (\a as -> Just a)
}

instance Camera GameCamera where
  getPosition (GameCamera c) = position c
  setPosition (GameCamera c) pos = GameCamera $ (\cam -> cam {position = pos}) c
  
  getDirection (GameCamera c) = toNormalUnsafe $ actU (orientation c) (neg vec3Z)
  setDirection (GameCamera c) dir = GameCamera $ (\cam -> cam { orientation = dir2quat dir }) c
    where dir2quat :: Normal3 -> UnitQuaternion
          dir2quat n =
            let m = toNormalUnsafe $ neg vec3Z
                cross = n &^ m
                cv = fromNormal cross
            in
             if (fromNormal cross) == zero then
               if _3 (fromNormal n) > 0 then rotU vec3Y pi else unitU
             else
               mkU . fromQ $ normalizeQ . toQ $ Vec4 (_1 cv) (_2 cv) (_3 cv) ((+1) $ n &. m)

  getUpDirection (GameCamera c) = (upDirection . gameObject) c
  setUpDirection (GameCamera c) dir = GameCamera $ (\cam -> cam { gameObject = (\go -> go {upDirection = dir}) $ gameObject c }) c

  getNearPlane (GameCamera c) = (near . gameObject) c
  setNearPlane (GameCamera c) n = GameCamera $ (\cam -> cam { gameObject = (\go -> go {near = n}) $ gameObject c }) c

  getFarPlane (GameCamera c) = (far . gameObject) c
  setFarPlane (GameCamera c) f = GameCamera $ (\cam -> cam { gameObject = (\go -> go {far = f}) $ gameObject c }) c

  getProjMatrix (GameCamera c) = let
    t = (top . gameObject) c
    b = (bottom . gameObject) c
    l = (left . gameObject) c
    r = (right . gameObject) c
    n = (near . gameObject) c
    f = (far . gameObject) c
    in
     Mat4
     (Vec4 (2.0 / (r - l)) 0 0 (-(r+l)/(r-l)))
     (Vec4 0 (2.0 / (t - b)) 0 (-(t+b)/(t-b)))
     (Vec4 0 0 (2.0 / (f - n)) (-(f+n)/(f-n)))
     (Vec4 0 0 0 1)


renderCamera :: Camera c => c -> RenderObject -> IO ()
renderCamera cam ro = do
  (beforeRender . material) ro
  (render ro) ro
  (afterRender . material) ro

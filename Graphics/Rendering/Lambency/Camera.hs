module Graphics.Rendering.Lambency.Camera (
  Camera(..),
  CameraType(..),
  GameCamera(..),
  mkOrthoCamera,
  renderCamera
) where

--------------------------------------------------------------------------------

import Data.Vect.Float
import Data.Vect.Float.Util.Dim4
import Data.Vect.Float.Util.Quaternion

import Graphics.Rendering.Lambency.Object
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Utils

import Data.Array.IO
import Data.Array.Storable

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
--------------------------------------------------------------------------------

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

mkOrthoCamera :: Normal3 -> Float -> Float -> Float -> Float -> Float -> Float -> CameraType
mkOrthoCamera up l r t b n f = Ortho {
  upDirection = up,
  left = l,
  right = r,
  top = t,
  bottom = b,
  near = n,
  far = f
}

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

  getGameObject :: c -> GameObject CameraType

  getProjMatrix :: c -> Mat4

genViewMatrix :: Camera c => c -> Mat4
genViewMatrix c = let
  dir = (getDirection c)
  side = crossprod dir $ getUpDirection c
  up = side &^ dir
  in
   if compareZero side then
     one
   else
     Mat4 (f side) (f up) (neg $ f dir) (extendWith 1.0 $ neg (getPosition c))
  where f = extendZero . fromNormal

getViewProjMatrix :: Camera c => c -> [Float]
getViewProjMatrix c = let
  pm = getProjMatrix c
  vm = genViewMatrix c
  Mat4 r1 r2 r3 r4 = vm .*. pm
  in
   destructVec4 [r1, r2, r3, r4]

newtype GameCamera = GameCamera (GameObject CameraType)

instance Camera GameCamera where
  getPosition (GameCamera c) = position c
  setPosition (GameCamera c) pos = GameCamera $ (\cam -> cam {position = pos}) c
  
  getDirection (GameCamera c) = toNormalUnsafe $ actU (orientation c) (neg vec3Z)
  setDirection (GameCamera c) dir = GameCamera $ (\cam -> cam { orientation = dir2quat dir }) c
    where dir2quat :: Normal3 -> UnitQuaternion
          dir2quat n = quatFromVecs n $ (toNormalUnsafe . neg) vec3Z

  getUpDirection (GameCamera c) = (upDirection . gameObject) c
  setUpDirection (GameCamera c) dir = GameCamera $ (\cam -> cam { gameObject = (\go -> go {upDirection = dir}) $ gameObject c }) c

  getNearPlane (GameCamera c) = (near . gameObject) c
  setNearPlane (GameCamera c) n = GameCamera $ (\cam -> cam { gameObject = (\go -> go {near = n}) $ gameObject c }) c

  getFarPlane (GameCamera c) = (far . gameObject) c
  setFarPlane (GameCamera c) f = GameCamera $ (\cam -> cam { gameObject = (\go -> go {far = f}) $ gameObject c }) c

  getGameObject (GameCamera c) = c

  getProjMatrix (GameCamera c) = let
    t = (top . gameObject) c
    b = (bottom . gameObject) c
    l = (left . gameObject) c
    r = (right . gameObject) c
    n = (near . gameObject) c
    f = (far . gameObject) c
    in
     Mat4
     (Vec4 (2.0 / (r - l)) 0 0 0)
     (Vec4 0 (2.0 / (t - b)) 0 0)
     (Vec4 0 0 ((-2.0) / (f - n)) 0)
     (Vec4 (-(r+l)/(r-l)) (-(t+b)/(t-b)) (-(f+n)/(f-n)) 1)

renderCamera :: Camera c => c -> RenderObject -> IO ()
renderCamera cam ro = do
  (beforeRender . material) ro

  case (shaderProgram . material) ro of
    Nothing -> return ()
    Just prg -> do
      (GL.UniformLocation mvpLoc) <- GL.get $ GL.uniformLocation prg "mvpMatrix"
      if mvpLoc == (-1) then return ()
        else do
        mvpArr <- newListArray (0 :: Int, 15) (map realToFrac $ getViewProjMatrix cam)
        withStorableArray mvpArr (\ptr -> GLRaw.glUniformMatrix4fv mvpLoc 1 0 ptr)

  (render ro) ro
  (afterRender . material) ro

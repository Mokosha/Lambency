module Graphics.Rendering.Lambency.Camera (
  Camera,
  GameCamera(..),
  mkOrthoCamera,
  mkPerspCamera,
  getViewProjMatrix,

  getCamXForm,
  setCamXForm,
  getCamDist,
  setCamDist,
  getCamPos,
  setCamPos,
  getCamDir,
  setCamDir,
  getCamUp,
  setCamUp,
  getCamNear,
  setCamNear,
  getCamFar,
  setCamFar
) where
--------------------------------------------------------------------------------
import Graphics.Rendering.Lambency.Utils
import qualified Graphics.Rendering.Lambency.Transform as XForm

import Data.Vect.Float
--------------------------------------------------------------------------------

data CameraViewDistance = CameraViewDistance {
  near :: Float,
  far :: Float
} deriving (Show, Eq)

data CameraType =
  Ortho {
    left :: Float,
    right :: Float,
    top :: Float,
    bottom :: Float
  }
  | Persp {
    fovY :: Float,
    aspect :: Float
  }
  deriving (Show, Eq)

data Camera = Camera XForm.Transform CameraType CameraViewDistance deriving(Show, Eq)

type Time = Double
data GameCamera = GameCamera Camera (Time -> Camera -> Camera)

mkXForm :: Vec3 -> Normal3 -> Normal3 -> XForm.Transform
mkXForm pos dir up = let
  r = dir &^ up
  u' = r &^ dir
  in XForm.XForm {
    XForm.right = r,
    XForm.up = u',
    XForm.forward = negN dir,
    XForm.position = pos,
    XForm.scale = Vec3 1 1 1
    }

mkOrthoCamera :: Vec3 -> Normal3 -> Normal3 ->
                 Float -> Float -> Float -> Float -> Float -> Float ->
                 Camera
mkOrthoCamera pos dir up l r t b n f = Camera

  (mkXForm pos dir up)

  Ortho {
    left = l,
    right = r,
    top = t,
    bottom = b
  }

  CameraViewDistance {
    near = n,
    far = f
  }

mkPerspCamera :: Vec3 -> Normal3 -> Normal3 ->
                 Float -> Float -> Float -> Float -> Camera
mkPerspCamera pos dir up fovy aspratio n f = Camera

  (mkXForm pos dir up)

  Persp {
    fovY = fovy,
    aspect = aspratio
  }

  CameraViewDistance {
    near = n,
    far = f
  }

-- !FIXME! Change the following functions to val -> Camera -> Camera
getCamXForm :: Camera -> XForm.Transform
getCamXForm (Camera xf _ _) = xf

setCamXForm :: Camera -> XForm.Transform -> Camera
setCamXForm (Camera _ cam dist) xf = Camera xf cam dist

getCamDist :: Camera -> CameraViewDistance
getCamDist (Camera _ _ dist) = dist

setCamDist :: Camera -> CameraViewDistance -> Camera
setCamDist (Camera loc cam _) dist = Camera loc cam dist

getCamPos :: Camera -> Vec3
getCamPos = XForm.position . getCamXForm

setCamPos :: Camera -> Vec3 -> Camera
setCamPos c p = let
  (XForm.XForm _ u nd _ _) = getCamXForm c
  in
   setCamXForm c $ mkXForm p (negN nd) u

getCamDir :: Camera -> Normal3
getCamDir = negN . XForm.forward . getCamXForm

setCamDir :: Camera -> Normal3 -> Camera
setCamDir c d = let
  (XForm.XForm _ u _ p _) = getCamXForm c
  in
   setCamXForm c $ mkXForm p d u

getCamUp :: Camera -> Normal3
getCamUp = XForm.up . getCamXForm

setCamUp :: Camera -> Normal3 -> Camera
setCamUp c u = let
  (XForm.XForm _ _ nd p _) = getCamXForm c
  in
   setCamXForm c $ mkXForm p (negN nd) u

getCamNear :: Camera -> Float
getCamNear = near . getCamDist

setCamNear :: Camera -> Float -> Camera
setCamNear c n = let
  dist = getCamDist c
  in
   setCamDist c $ (\d -> d { near = n }) dist

getCamFar :: Camera -> Float
getCamFar = (far . getCamDist)

setCamFar :: Camera -> Float -> Camera
setCamFar c f = let
  dist = getCamDist c
  in
   setCamDist c $ (\d -> d { far = f }) dist

getViewMatrix :: Camera -> Mat4
getViewMatrix (Camera xf _ _) =
  XForm.xform2Matrix $ (\xf' -> xf' { XForm.position = neg (XForm.position xf) }) xf

getProjMatrix :: Camera -> Mat4
getProjMatrix (Camera _ (Ortho {top = t, bottom = b, left = l, right = r}) dist) = let
  n = near dist
  f = far dist
  in
   Mat4
   (Vec4 (2.0 / (r - l)) 0 0 0)
   (Vec4 0 (2.0 / (t - b)) 0 0)
   (Vec4 0 0 ((-2.0) / (f - n)) 0)
   (Vec4 (-(r+l)/(r-l)) (-(t+b)/(t-b)) (-(f+n)/(f-n)) 1)

getProjMatrix (Camera _ (Persp {fovY = fovy, aspect = a}) dist) = let
  n = near dist
  f = far dist
  t = n * (tan (fovy * 0.5))
  r = t * a
  in
   Mat4
   (Vec4 (n / r) 0 0 0)
   (Vec4 0 (n / t) 0 0)
   (Vec4 0 0 (-(f+n)/(f-n)) (-1))
   (Vec4 0 0 (-(2*f*n)/(f-n)) 0)

getViewProjMatrix :: Camera -> Mat4
getViewProjMatrix c = (getViewMatrix c) .*. (getProjMatrix c)

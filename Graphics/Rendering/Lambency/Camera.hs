module Graphics.Rendering.Lambency.Camera (
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
  setCamFar,

  mkFixedCam,
  mkDebugCam,
) where
--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import Graphics.UI.Lambency.Input

import Graphics.Rendering.Lambency.Utils
import Graphics.Rendering.Lambency.Types
import qualified Graphics.Rendering.Lambency.Transform as XForm

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

import qualified Control.Wire as W
import Control.Monad.RWS.Strict
--------------------------------------------------------------------------------

mkXForm :: Vec3 -> Normal3 -> Normal3 -> XForm.Transform
mkXForm pos dir up = let
  r = dir &^ up
  u' = r &^ dir
  in XForm.translate pos $ XForm.fromCoordinateBasis (r, u', negN dir)

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
  xf = getCamXForm c
  nd = XForm.forward xf
  u = XForm.up xf
  in
   setCamXForm c $ mkXForm p (negN nd) u

getCamDir :: Camera -> Normal3
getCamDir = negN . XForm.forward . getCamXForm

setCamDir :: Camera -> Normal3 -> Camera
setCamDir c d = let
  xf = getCamXForm c
  u = XForm.up xf
  p = XForm.position xf
  in
   setCamXForm c $ mkXForm p d u

getCamUp :: Camera -> Normal3
getCamUp = XForm.up . getCamXForm

setCamUp :: Camera -> Normal3 -> Camera
setCamUp c u = let
  xf = getCamXForm c
  nd = XForm.forward xf
  p = XForm.position xf
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
  let
    pos = neg . XForm.position $ xf
    sca = XForm.scale xf
    r = XForm.right xf
    u = XForm.up xf
    f = XForm.forward xf
    te :: Normal3 -> (Vec3 -> Float) -> Vec4
    te n sc = extendWith (pos &. (fromNormal n)) (sc sca *& (fromNormal n))
  in transpose $ Mat4 (te r _1) (te u _2) (te f _3) (Vec4 0 0 0 1)

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

--

mkFixedCam :: Monad m => Camera -> W.Wire s e m a Camera
mkFixedCam cam = W.mkConst $ Right cam
  
mkDebugCam :: Camera -> W.Wire Timestep e GameMonad a Camera
mkDebugCam (Camera xform camTy camSz) =
  W.mkGen $ \time _ -> let
    W.Timed dt () = time ()
    in do
      ipt <- get
      let newcam :: Camera
          newcam = updCam dt ipt
      put $ resetCursorPos ipt
      return (Right newcam, mkDebugCam newcam)
  where
    updCam :: Float -> Input -> Camera
    updCam dt ipt = Camera finalXForm camTy camSz
     where
      (mx, my) = case (cursor ipt) of
        Just x -> x
        Nothing -> (0, 0)

      tr :: GLFW.Key -> Float -> (XForm.Transform -> Normal3) ->
            (XForm.Transform -> XForm.Transform)
      tr k sc dir = let
        vdir = fromNormal . dir
        s = 3.0 * dt * sc
        in
         withPressedKey ipt k (\x -> XForm.translate (s *& (vdir x)) x)

      movement :: XForm.Transform -> XForm.Transform
      movement = foldl1 (.) [
        tr GLFW.Key'W (-1.0) XForm.forward,
        tr GLFW.Key'S (1.0) XForm.forward,
        tr GLFW.Key'A (-1.0) XForm.right,
        tr GLFW.Key'D (1.0) XForm.right,
        XForm.rotate $ foldl1 (.*.) [
          rotU' (XForm.up xform) (-asin mx),
          rotU' (XForm.right xform) (-asin my)]
        ]

      finalXForm = mkXForm
                   (XForm.position newXForm)
                   (negN $ XForm.forward newXForm)
                   (toNormalUnsafe vec3Y)
        where
          newXForm = movement xform

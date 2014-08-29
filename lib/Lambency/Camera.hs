module Lambency.Camera (
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
  mkViewerCam,
  mkDebugCam,
  mk2DCam,
) where
--------------------------------------------------------------------------------
import qualified Graphics.UI.GLFW as GLFW

import Lambency.Types
import qualified Lambency.Transform as XForm

import qualified Control.Wire as W

import FRP.Netwire.Input

import Linear.Matrix
import Linear.Metric
import qualified Linear.Quaternion as Quat
import Linear.Vector
import Linear.V2
import Linear.V3
import Linear.V4
--------------------------------------------------------------------------------

mkXForm :: Vec3f -> Vec3f -> Vec3f -> XForm.Transform
mkXForm pos dir up = let
  r = signorm $ dir `cross` up
  u' = signorm $ r `cross` dir
  in XForm.translate pos $ XForm.fromCoordinateBasis (r, u', negate dir)

mkOrthoCamera :: Vec3f -> Vec3f -> Vec3f  ->
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

mkPerspCamera :: Vec3f -> Vec3f -> Vec3f ->
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

getCamPos :: Camera -> Vec3f
getCamPos = XForm.position . getCamXForm

setCamPos :: Camera -> Vec3f -> Camera
setCamPos c p = let
  xf = getCamXForm c
  nd = XForm.forward xf
  u = XForm.up xf
  in
   setCamXForm c $ mkXForm p (negate nd) u

getCamDir :: Camera -> Vec3f
getCamDir = negate . XForm.forward . getCamXForm

setCamDir :: Camera -> Vec3f -> Camera
setCamDir c d = let
  xf = getCamXForm c
  u = XForm.up xf
  p = XForm.position xf
  in
   setCamXForm c $ mkXForm p d u

getCamUp :: Camera -> Vec3f
getCamUp = XForm.up . getCamXForm

setCamUp :: Camera -> Vec3f -> Camera
setCamUp c u = let
  xf = getCamXForm c
  nd = XForm.forward xf
  p = XForm.position xf
  in
   setCamXForm c $ mkXForm p (negate nd) u

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

getViewMatrix :: Camera -> Mat4f
getViewMatrix (Camera xf _ _) =
  let
    extendWith :: Float -> Vec3f -> Vec4f
    extendWith w (V3 x y z) = V4 x y z w
    pos = negate . XForm.position $ xf
    (V3 sx sy sz) = XForm.scale xf
    r = XForm.right xf
    u = XForm.up xf
    f = XForm.forward xf
    te :: Vec3f -> Float -> Vec4f
    te n sc = extendWith (pos `dot` n) (sc *^ n)
  in adjoint $ V4 (te r sx) (te u sy) (te f sz) (V4 0 0 0 1)

getProjMatrix :: Camera -> Mat4f
getProjMatrix (Camera _ (Ortho {top = t, bottom = b, left = l, right = r}) dist) = let
  n = near dist
  f = far dist
  in
   V4
   (V4 (2.0 / (r - l)) 0 0 0)
   (V4 0 (2.0 / (t - b)) 0 0)
   (V4 0 0 ((-2.0) / (f - n)) 0)
   (V4 (-(r+l)/(r-l)) (-(t+b)/(t-b)) (-(f+n)/(f-n)) 1)

getProjMatrix (Camera _ (Persp {fovY = fovy, aspect = a}) dist) = let
  n = near dist
  f = far dist
  t = n * (tan (fovy * 0.5))
  r = t * a
  in
   V4
   (V4 (n / r) 0 0 0)
   (V4 0 (n / t) 0 0)
   (V4 0 0 (-(f+n)/(f-n)) (-1))
   (V4 0 0 (-(2*f*n)/(f-n)) 0)

getViewProjMatrix :: Camera -> Mat4f
getViewProjMatrix c = (getViewMatrix c) !*! (getProjMatrix c)

--

mkFixedCam :: Monad m => Camera -> W.Wire s e m a Camera
mkFixedCam cam = W.mkConst $ Right cam

mkViewerCam :: Camera -> GameWire a Camera
mkViewerCam cam@(Camera xform camTy camSz) = let
  finalXForm :: (Float, Float) -> XForm.Transform
  finalXForm (0, 0) = XForm.identity
  finalXForm (mx, my) = mkXForm newPos (signorm $ negate newPos) (XForm.up xform)
    where
      newPos :: Vec3f
      newPos = XForm.transformPoint rotation $ getCamPos cam
        where
          rotation :: XForm.Transform
          rotation = flip XForm.rotateWorld XForm.identity $
                     foldl1 (*) [
                       Quat.axisAngle (XForm.up xform) (-asin mx),
                       Quat.axisAngle (XForm.right xform) (-asin my)]
  in
   ((mouseMickies W.>>> (mousePressed GLFW.MouseButton'1)) W.<|> (W.pure (0, 0))) W.>>>
   (W.mkGenN $ \pos -> do
       let newcam = Camera (finalXForm pos) camTy camSz
       return (Right newcam, mkViewerCam newcam))

mkDebugCam :: Camera -> GameWire a Camera
mkDebugCam initCam = W.loop ((W.second (W.delay initCam W.>>> updCam)) W.>>> feedback)
  where
  feedback :: GameWire (a, b) (b, b)
  feedback = W.mkPure_ $ \(_, x) -> Right (x, x)

  tr :: GLFW.Key -> Float -> (XForm.Transform -> Vec3f) ->
        GameWire XForm.Transform XForm.Transform
  tr key sc dir = (trans W.>>> (keyPressed key)) W.<|> W.mkId
    where
      trans :: GameWire XForm.Transform XForm.Transform
      trans = W.mkSF $ \ts xf -> (XForm.translate (3.0 * (W.dtime ts) * sc *^ (dir xf)) xf, trans)

  updCam :: GameWire Camera Camera
  updCam = (W.mkId W.&&& (W.arr getCamXForm W.>>> xfWire)) W.>>> (W.mkSF_ $ uncurry stepCam)
    where

      xfWire :: GameWire XForm.Transform XForm.Transform
      xfWire =
        (tr GLFW.Key'W (-1.0) XForm.forward) W.>>>
        (tr GLFW.Key'S (1.0) XForm.forward) W.>>>
        (tr GLFW.Key'A (-1.0) XForm.right) W.>>>
        (tr GLFW.Key'D (1.0) XForm.right) W.>>>
        (W.mkId W.&&& mouseMickies) W.>>>
        (W.mkSF_ $ \(xf, (mx, my)) ->
          XForm.rotate
          (foldl1 (*) [
              Quat.axisAngle (XForm.up xf) (-asin mx),
              Quat.axisAngle (XForm.right xf) (-asin my)])
          xf)

      stepCam :: Camera -> XForm.Transform -> Camera
      stepCam cam newXForm = setCamXForm cam finalXForm
        where
          finalXForm = mkXForm
                       (XForm.position newXForm)
                       (negate $ XForm.forward newXForm)
                       (V3 0 1 0)

mk2DCam :: Int -> Int -> GameWire Vec2f Camera
mk2DCam sx sy = let
  toHalfF :: Integral a => a -> Float
  toHalfF x = 0.5 * (fromIntegral x)

  hx :: Float
  hx = toHalfF sx

  hy :: Float
  hy = toHalfF sy

  screenCenter :: V3 Float
  screenCenter = V3 hx hy 1

  trPos :: Vec2f -> Vec3f
  trPos (V2 x y) = (V3 x y 0) ^+^ screenCenter
 in
   W.mkSF_ $ \vec -> mkOrthoCamera
   (trPos vec) (negate XForm.localForward) XForm.localUp (-hx) (hx) (hy) (-hy) 0.01 50.0

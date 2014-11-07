module Main where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))
import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import Linear
--------------------------------------------------------------------------------

--------------------------------------------------
-- Constants

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

bulletSz :: Float
bulletSz = 5.0

shipSz :: Float
shipSz = 10.0

shipSpeed :: Float
shipSpeed = 25.0

bulletSpeed :: Float
bulletSpeed = 100.0

bulletDelay :: Float
bulletDelay = 1.0

--------------------------------------------------
-- Rendering

renderQuad :: L.RenderObject -> V2 Float -> Float -> L.GameMonad ()
renderQuad ro (V2 x y) sz =
  let hsz = sz * 0.5
      xform = L.translate (V3 (x - hsz) (y - hsz) (-1)) $
              L.nonuniformScale (V3 sz sz 1) $
              L.identity
  in L.addRenderAction xform ro

--------------------------------------------------
-- Logic

-- A bullet is just a sprite that doesn't interact with anything...
type Bullet = L.GameWire () ()

-- A Ship is something that takes direction and produces bullets
type Ship = L.GameWire (V2 Float) [Bullet]

inScreen :: V2 Float -> Bool
inScreen (V2 px py) =
  let (V2 sx sy) = fmap fromIntegral (V2 screenWidth screenHeight)
  in
   if (px < 0 || px > sx || py < 0 || py > sy)
   then False
   else True

bulletWire :: V2 Float -> V2 Float -> L.RenderObject -> Bullet
bulletWire pos vel bullet = mkGen $ \dt _ -> do
  let pos' = pos ^+^ (dtime dt * bulletSpeed *^ vel)
  renderQuad bullet pos' bulletSz
  if (not $ inScreen pos')
    then return (Left mempty, bulletWire pos' vel bullet)
    else return (Right (), bulletWire pos' vel bullet)

shipWire :: V2 Float -> L.RenderObject -> L.RenderObject -> Ship
shipWire pos' ship' bullet' = loop $ (second $ delay 0) >>> (shipFeedback pos' ship' bullet')
  where
    shipFeedback :: V2 Float -> L.RenderObject -> L.RenderObject ->
                    L.GameWire (V2 Float, Float) ([Bullet], Float)
    shipFeedback pos ship bullet = 
      mkGen $ \dt (vel, t) -> do
        let newPos = pos ^+^ (dtime dt * shipSpeed *^ vel)
            nextSW = shipFeedback newPos ship bullet
            b = bulletWire newPos vel bullet
        renderQuad ship newPos shipSz
        if (vel /= zero && t > bulletDelay)
          then return $ (Right ([b], 0 :: Float), nextSW) -- Spawn bullet
          else return $ (Right ([], t + (dtime dt)), nextSW)

addVecWire :: L.GameWire () (V2 Float) -> L.GameWire () (V2 Float) -> L.GameWire () (V2 Float)
addVecWire w1 w2 = w1 &&& w2 >>> (arr $ uncurry (^+^))

inputWire :: L.GameWire () (V2 Float)
inputWire =
  ((pure (V2 0 1) >>> keyPressed GLFW.Key'Up) <|> (pure zero)) `addVecWire`
  ((pure (V2 0 (-1)) >>> keyPressed GLFW.Key'Down) <|> (pure zero)) `addVecWire`
  ((pure (V2 1 0) >>> keyPressed GLFW.Key'Right) <|> (pure zero)) `addVecWire`
  ((pure (V2 (-1) 0) >>> keyPressed GLFW.Key'Left) <|> (pure zero))

mkGameWire :: IO (L.GameWire () ())
mkGameWire = do
  white <- L.createSolidTexture (255, 255, 255, 255)
  red <- L.createSolidTexture (255, 0, 0, 255)
  ship <- L.createRenderObject L.quad (L.createTexturedMaterial white)
  bullet <- L.createRenderObject L.quad (L.createTexturedMaterial red)
  let shipW = inputWire >>> (shipWire (V2 240 320) ship bullet)
  return $ runShip shipW []
    where
      runBullet :: L.TimeStep -> Bullet -> L.GameMonad [Bullet]
      runBullet dt bw' = do
        (result, bw) <- stepWire bw' dt $ Right ()
        case result of
          Right _ -> return [bw]
          Left _ -> return []

      runBullets :: L.TimeStep -> [Bullet] -> L.GameMonad [Bullet]
      runBullets _ [] = return []
      runBullets dt (b:bs) = do
        bs' <- runBullets dt bs
        runBullet dt b >>= (\b' -> return (b' ++ bs'))
    
      runShip :: L.GameWire () [Bullet] -> [Bullet] -> L.GameWire () ()
      runShip sw' bullets = mkGen $ \dt _ -> do
        (result, sw) <- stepWire sw' dt (Right ())
        case result of
          Right newb -> do
            bullets' <- runBullets dt (bullets ++ newb)
            return (Right (), runShip sw bullets')
          Left e -> return (Left e, runShip sw bullets)

--------------------------------------------------
-- Init

shooterCam :: L.GameWire () L.Camera
shooterCam = pure zero >>> (L.mk2DCam screenWidth screenHeight)

loadGame :: IO (L.Game ())
loadGame = do
  nolight <- L.createNoLight
  w <- mkGameWire
  return $ L.Game { L.staticLights = [nolight],
                    L.staticGeometry = [],
                    L.mainCamera = shooterCam,
                    L.dynamicLights = [],
                    L.gameLogic = w >>> L.quitWire GLFW.Key'Q}

main :: IO ()
main = L.runWindow screenWidth screenHeight "Space Shooter Demo" () loadGame

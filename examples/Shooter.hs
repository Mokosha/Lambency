module Main where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))
import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import Linear hiding (trace)
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
bulletDelay = 0.2

--------------------------------------------------
-- Rendering

renderQuad :: L.Sprite -> V2 Float -> Float -> L.GameMonad ()
renderQuad s (V2 x y) sz =
  let hsz = sz * 0.5
      pos = V2 (x - hsz) (y - hsz)
      sc = round <$> V2 sz sz
  in L.renderSprite s sc (-1) pos

--------------------------------------------------
-- Logic

-- A bullet is just a sprite that doesn't interact with anything...
type Bullet = L.GameWire () ()

-- A Ship is something that takes direction and produces bullets
type Ship = L.GameWire (V2 Float) [Bullet]

inScreen :: V2 Float -> Bool
inScreen (V2 px py) =
  let (V2 sx sy) = fmap fromIntegral (V2 screenWidth screenHeight)
  in px >= 0 && px <= sx && py >= 0 && py <= sy

bulletWire :: V2 Float -> V2 Float -> L.Sprite -> Bullet
bulletWire pos vel bullet = mkGen $ \dt _ -> do
  let pos' = pos ^+^ (dtime dt * bulletSpeed *^ vel)
  renderQuad bullet pos' bulletSz
  if (not $ inScreen pos')
    then return (Left mempty, bulletWire pos' vel bullet)
    else return (Right (), bulletWire pos' vel bullet)

shipWire :: V2 Float -> L.Sprite -> L.Sprite -> Ship
shipWire pos' ship bullet = loop $ (second $ delay 0) >>> (shipFeedback pos')
  where
    fireWire :: L.GameWire () Bool
    fireWire = (keyDebounced GLFW.Key'Space >>> pure True) <|> pure False

    shipFeedback :: V2 Float -> L.GameWire (V2 Float, Float) ([Bullet], Float)
    shipFeedback pos = 
      mkGen $ \dt (vel, t) -> do
        let newPos = pos ^+^ (dtime dt * shipSpeed *^ vel)
            nextSW = shipFeedback newPos
            b = bulletWire newPos vel bullet
        renderQuad ship newPos shipSz
        (Right fire, _) <- stepWire fireWire dt $ Right ()
        if (vel /= zero && fire && t > bulletDelay)
          then return $ (Right ([b], 0 :: Float), nextSW) -- Spawn bullet
          else return $ (Right ([], t + (dtime dt)), nextSW)

addVecWire :: L.GameWire () (V2 Float) -> L.GameWire () (V2 Float) ->
              L.GameWire () (V2 Float)
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
  ship <- L.loadStaticSpriteWithTexture white
  bullet <- L.loadStaticSpriteWithTexture red
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
        runBullet dt b >>= liftM (++ bs')

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
  w <- mkGameWire
  return $ L.Game { L.mainCamera = shooterCam,
                    L.dynamicLights = [],
                    L.gameLogic = w >>> L.quitWire GLFW.Key'Q}

main :: IO ()
main = L.withWindow screenWidth screenHeight "Space Shooter Demo" $
       L.loadAndRun () loadGame

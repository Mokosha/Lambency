module Main where

--------------------------------------------------------------------------------
import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire.Input
import FRP.Netwire.Move

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import System.FilePath
import Paths_lambency

import Control.Lens
import Linear
--------------------------------------------------------------------------------

type Circle = (V2 Float, Float)
type Rect = (V2 Float, V2 Float)

data Ball = Ball { pos :: V2 Float, vel :: V2 Float }

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

wallHeight :: Int
wallHeight = 10

paddleHeight :: Int
paddleHeight = 40

paddleWidth :: Int
paddleWidth = 10

-- The horizontal offset from the wall
paddleOffset :: Float
paddleOffset = 10

paddleStartY :: Float
paddleStartY = fromIntegral $ screenHeight `div` 2

paddleMinY :: Float
paddleMinY = fromIntegral wallHeight

paddleMaxY :: Float
paddleMaxY = fromIntegral (screenHeight - paddleHeight) - paddleMinY
      
paddleSpeed :: Float
paddleSpeed = 70

scoreOffset :: Float
scoreOffset = 20

paddleLoc :: Bool -> Float -> V2 Float
paddleLoc True y = V2 paddleOffset y
paddleLoc False y = V2 (fromIntegral (screenWidth - paddleWidth) - paddleOffset) y

paddleXForm :: V2 Float -> L.Transform
paddleXForm (V2 x y) =
  L.translate (V3 x y (-1)) $
  L.nonuniformScale (vi2f3 $ V3 paddleWidth paddleHeight 1) $
  L.identity

ballRadius :: Float
ballRadius = 5

ballStartSpeed :: V2 Float
ballStartSpeed = V2 (-65) (-20)

startBall :: Ball
startBall = Ball (0.5 *^ (vi2f2 $ V2 screenWidth screenHeight)) ballStartSpeed

--------------------------------------------------
-- Rendering

renderPaddle :: L.Sprite -> Bool -> Float -> L.GameMonad ()
renderPaddle s b f =
  L.renderSprite s (V2 paddleWidth paddleHeight) (-1) (paddleLoc b f)

renderBall :: L.Sprite -> Ball -> L.GameMonad ()
renderBall s (Ball (V2 x y) _) = L.renderSprite s sc (-1) pp
  where
    pp = V2 (x - ballRadius) (y - ballRadius)
    sc = round <$> ((2 *^) $ V2 ballRadius ballRadius)

renderScore :: L.Font -> Int -> Int -> L.GameMonad ()
renderScore f p1 p2 =
  let scoreLen = L.stringWidth f (show p1)
      c = vi2f2 $ V2 (screenWidth `div` 2) wallHeight
  in do
    L.renderUIString f (show p1) (c + (V2 (-scoreOffset-scoreLen) scoreOffset))
    L.renderUIString f (show p2) (c + (V2 scoreOffset scoreOffset))

dashedMidsection :: L.Sprite -> [(V2 Float, V2 Int, L.Sprite)]
dashedMidsection s = [(tr x, V2 4 20, s) | x <- [0,1..12]]
  where
    tr :: Int -> V2 Float
    tr x = 0.5 *^ (vi2f2 $ V2 screenWidth (80 * x)) ^-^ (V2 1.5 0)

wallScale :: V2 Int
wallScale = V2 screenWidth wallHeight

mkWall :: Bool -> L.Sprite -> (V2 Float, V2 Int, L.Sprite)
mkWall True x = (vi2f2 $ V2 0 (screenHeight - wallHeight), wallScale, x)
mkWall False x = (V2 0 0, wallScale, x)

renderPaddleWire :: L.Sprite -> Bool -> L.GameWire Float Float
renderPaddleWire s playerOne =
  mkGen_ $ \y -> renderPaddle s playerOne y >> return (Right y)

renderBallWire :: L.Sprite -> L.GameWire Ball Ball
renderBallWire s = mkGen_ $ \b -> renderBall s b >> return (Right b)

renderStatic :: [(V2 Float, V2 Int, L.Sprite)] -> L.GameWire a a
renderStatic rs = mkGen_ $ \x -> mapM_ renderIt rs >> return (Right x)
  where
    renderIt (pp, sc, s) = L.renderSprite s sc (-1) pp

--------------------------------------------------
-- Game logic

paddleFeedback :: L.GameWire (a, Float) Float -> L.GameWire (a, Float) (Float, Float)
paddleFeedback handler =
  let againstTop = when ((>= paddleMaxY) . snd) >>>
                   handler >>>
                   (when (< 0) <|> pure 0)

      againstBot = when ((<= paddleMinY) . snd) >>>
                   handler >>>
                   (when (> 0) <|> pure 0)
  in
   (againstTop <|> againstBot <|> handler) >>>
   integral paddleStartY >>>
   (mkId &&& mkId)

paddleWire :: Bool -> L.Sprite ->  L.GameWire (a, Float) Float -> L.GameWire a Float
paddleWire playerOne ro handler =
  loop (second (delay 0) >>> paddleFeedback handler) >>> renderPaddleWire ro playerOne

ballWire :: L.Sprite -> L.GameWire Ball Ball
ballWire ro = integrateBall >>> renderBallWire ro
  where
    integrateBall :: L.GameWire Ball Ball
    integrateBall = mkSF $ \ds (Ball p v) ->
      let dt = realToFrac (dtime ds)
      in (Ball (p ^+^ (dt *^ v)) v, integrateBall)

keyHandler :: L.GameWire (a, Float) Float
keyHandler =
  (pure paddleSpeed . keyPressed GLFW.Key'Up) <|>
  (pure (-paddleSpeed) . keyPressed GLFW.Key'Down) <|>
  pure 0

collidePaddle :: Bool -> L.Sound -> L.GameWire (Ball, Float) Ball
collidePaddle playerOne sound = mkGen_ collide
  where
    collide :: (Ball, Float) -> L.GameMonad (Either String Ball)
    collide (Ball p v, h) = 
      let paddleSz = vi2f2 $ V2 paddleWidth paddleHeight
          paddleRect = (paddleLoc playerOne h, paddleSz)
          v' = (_x %~ negate) v
      in
       if circleIntersectRect (p, ballRadius) paddleRect
       then do
         L.startSound sound
         return . Right $ Ball p v'
       else return . Right $ Ball p v
       
collideWall :: L.GameWire Ball Ball
collideWall = mkSF_ collide
  where
    collide (Ball p v) =
      let by = p ^. _y
          v' = (_y %~ negate) v
          wh = fromIntegral wallHeight + ballRadius
      in
       if by < wh || by > (fromIntegral screenHeight - wh)
       then Ball p v'
       else Ball p v

aiHandler :: L.GameWire (Ball, Float) Float
aiHandler = mkSF_ $ \(Ball p v, h) ->
  let dy = (p ^. _y) - (h + (fromIntegral $ paddleHeight `div` 2))
  in L.clamp ((v ^. _y) + dy) (-paddleSpeed) paddleSpeed

handleScore :: L.Font -> L.GameWire (Int, Ball) (Int, Ball)
handleScore f = scoreWire 0 0
  where
    handleBall :: Ball -> (Ball, Int)
    handleBall b@(Ball p _)
      | bx < 0 = (startBall, 1)
      | bx > (fromIntegral screenWidth) = (startBall, -1)
      | otherwise = (b, 0)
      where
        bx = p ^._x
    
    scoreWire :: Int -> Int -> L.GameWire (Int, Ball) (Int, Ball)
    scoreWire p1 p2 = mkGenN scoreBall
      where
        scoreBall (_, b) =
          let (b', x) = handleBall b
              (p1', p2') = case x of
                  1 -> (p1, p2 + 1)
                  -1 -> (p1 + 1, p2)
                  _ -> (p1, p2)
          in do
            renderScore f p1' p2'
            return (Right (max p1' p2', b'), scoreWire p1' p2')

gameFeedback :: L.Sprite -> L.Sprite -> IO (L.GameWire (Int, Ball) (Int, Ball))
gameFeedback quad circle = do
  fontFilename <- getDataFileName ("examples" </> "kenpixel.ttf")
  sysFont <- L.loadTTFont 36 (V3 1 1 1) fontFilename
  
  sound <- getDataFileName ("examples" </> "pong-bloop.wav") >>= L.loadSound
  return $ (second $
            (collideWith True sound keyHandler) >>>
            (collideWith False sound aiHandler) >>>
            collideWall >>>
            ballWire circle) >>>
    handleScore sysFont
    where
      collideWith :: Bool -> L.Sound ->
                     L.GameWire (Ball, Float) Float ->
                     L.GameWire Ball Ball
      collideWith playerOne s handler =
        (mkId &&& paddleWire playerOne quad handler) >>>
        collidePaddle playerOne s

gameWire :: L.Sprite -> L.Sprite -> IO (L.GameWire Int Int)
gameWire quad circle = do
  feedback <- gameFeedback quad circle
  return $ (loop $ second (delay startBall) >>> feedback) >>> (when (< 10))

pongCam :: L.GameWire () L.Camera
pongCam = pure zero >>> (L.mk2DCam screenWidth screenHeight)

--------------------------------------------------
-- Init

loadGame :: IO (L.Game Int)
loadGame = do
  white <- L.createSolidTexture (255, 255, 255, 255)
  quad <- L.changeSpriteColor (V4 0.4 0.6 0.2 1.0) <$>
          L.loadStaticSpriteWithMask white
  w <- gameWire quad quad
  let staticSprites = [mkWall False quad, mkWall True quad] ++ dashedMidsection quad
      mainWire = w >>> renderStatic staticSprites >>> L.quitWire GLFW.Key'Q
  return $ L.Game { L.mainCamera = pongCam,
                    L.dynamicLights = [],
                    L.gameLogic = mainWire }

main :: IO ()
main = L.withWindow screenWidth screenHeight "Pong Demo" $ L.loadAndRun 0 loadGame

--------------------------------------------------
-- Utils

circleIntersectRect :: Circle -> Rect -> Bool
circleIntersectRect (c, r) (p, sz) =
  let p' = p ^+^ sz
      cx = L.clamp (c ^._x) (p ^._x) (p' ^._x)
      cy = L.clamp (c ^._y) (p ^._y) (p' ^._y)
  in norm (V2 cx cy ^-^ c) < r

vi2f2 :: (Integral a, Floating f) => V2 a -> V2 f
vi2f2 = fmap fromIntegral

vi2f3 :: (Integral a, Floating f) => V3 a -> V3 f
vi2f3 = fmap fromIntegral

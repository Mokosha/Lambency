module Main where

--------------------------------------------------------------------------------
import Control.Monad.Writer (tell)
import Control.Wire hiding ((.))
import FRP.Netwire.Input
import FRP.Netwire.Move

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import Paths_lambency_examples

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

renderPaddle :: L.RenderObject -> Bool -> Float -> L.GameMonad ()
renderPaddle ro b f = L.addRenderAction (paddleXForm $ paddleLoc b f) ro

renderBall :: L.RenderObject -> Ball -> L.GameMonad ()
renderBall ro (Ball (V2 x y) _) = L.addRenderAction xf ro
  where
    xf = L.translate (V3 (x - ballRadius) (y - ballRadius) (-1)) $
         L.nonuniformScale ((2 *^) $ V3 ballRadius ballRadius 1) $
         L.identity

renderScore :: L.Font -> Int -> Int -> L.GameMonad ()
renderScore f p1 p2 =
  let score1 = show p1
      score2 = show p2
      scoreLen = L.stringWidth f score1
      scoreCenter = vi2f2 $ V2 (screenWidth `div` 2) wallHeight
  in
   do
     L.renderUIString f score1 (scoreCenter + (V2 (-scoreOffset-scoreLen) scoreOffset))
     L.renderUIString f score2 (scoreCenter + (V2 scoreOffset scoreOffset))

dashedMidsection :: L.RenderObject -> [(L.Transform, L.RenderObject)]
dashedMidsection ro = [(L.translate (tr x) $ L.nonuniformScale sc L.identity, ro) | x <- [0,1..12]]
  where
    sc :: V3 Float
    sc = V3 3 20 1

    tr :: Int -> V3 Float
    tr x = 0.5 *^ (vi2f3 $ V3 screenWidth (80 * x) 0)

wallXF :: L.Transform
wallXF = L.nonuniformScale (vi2f3 $ V3 screenWidth wallHeight 1) L.identity

mkWall :: Bool -> L.RenderObject -> (L.Transform, L.RenderObject)
mkWall True x = (L.translate (vi2f3 $ V3 0 (screenHeight - wallHeight) (-1)) wallXF, x)
mkWall False x = (wallXF, x)

renderPaddleWire :: L.RenderObject -> Bool -> L.GameWire Float Float
renderPaddleWire ro playerOne = mkGen_ $ \y -> renderPaddle ro playerOne y >> return (Right y)

renderBallWire :: L.RenderObject -> L.GameWire Ball Ball
renderBallWire ro = mkGen_ $ \b -> renderBall ro b >> return (Right b)

--------------------------------------------------
-- Game logic

paddleFeedback :: L.GameWire (a, Float) Float -> L.GameWire (a, Float) (Float, Float)
paddleFeedback handler = handler >>> integral paddleStartY >>> (mkId &&& mkId)

paddleWire :: Bool -> L.RenderObject ->  L.GameWire (a, Float) Float -> L.GameWire a Float
paddleWire playerOne ro handler =
  loop (second (delay 0) >>> paddleFeedback handler) >>> renderPaddleWire ro playerOne

ballWire :: L.RenderObject -> L.GameWire Ball Ball
ballWire ro = integrateBall >>> renderBallWire ro
  where
    integrateBall :: L.GameWire Ball Ball
    integrateBall = mkSF $ \ds (Ball p v) ->
      let dt = realToFrac (dtime ds)
      in (Ball (p ^+^ (dt *^ v)) v, integrateBall)

keyHandler :: L.GameWire (a, Float) Float
keyHandler = 
  (when ((< paddleMaxY) . snd) >>> keyPressed GLFW.Key'Up >>> pure paddleSpeed) <|>
  (when ((> paddleMinY) . snd) >>> keyPressed GLFW.Key'Down >>> pure (-paddleSpeed)) <|>
  pure 0

collidePaddle :: Bool -> L.Sound -> L.GameWire (Ball, Float) Ball
collidePaddle playerOne sound = mkGen_ collide
  where
    collide (Ball p v, h) = 
      let paddleSz = vi2f2 $ V2 paddleWidth paddleHeight
          paddleRect = (paddleLoc playerOne h, paddleSz)
          v' = (_x %~ negate) v
      in
       if circleIntersectRect (p, ballRadius) paddleRect
       then do
         tell $ [L.SoundAction sound L.StartSound]
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
aiHandler = mkSF_ trackBall
  where
    trackBall (Ball p _, h) =
      let diffY = (p ^._y) - h
      in L.clamp diffY (-paddleSpeed) paddleSpeed

handleScore :: L.Font -> L.GameWire (Int, Ball) (Int, Ball)
handleScore f = scoreWire 0 0
  where
    handleBall :: Ball -> (Ball, Int)
    handleBall b@(Ball p _) =
      let bx = p ^._x
      in
       if bx < 0
       then (startBall, 1)
       else if bx > (fromIntegral screenWidth)
            then (startBall, -1)
            else (b, 0)
    
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

gameFeedback :: L.RenderObject -> L.RenderObject -> IO (L.GameWire (Int, Ball) (Int, Ball))
gameFeedback quad circle = do
  sysFont <- getDataFileName ("kenpixel.ttf") >>= L.loadTTFont 36 (V3 1 1 1)
  sound <- getDataFileName ("pong-bloop.wav") >>= L.loadSound
  return $ (second $
            (collideWith True sound keyHandler) >>>
            (collideWith False sound aiHandler) >>>
            collideWall >>>
            ballWire circle) >>>
    handleScore sysFont
    where
      collideWith :: Bool -> L.Sound -> L.GameWire (Ball, Float) Float -> L.GameWire Ball Ball
      collideWith playerOne s handler = (mkId &&& paddleWire playerOne quad handler) >>>
                                        collidePaddle playerOne s

gameWire :: L.RenderObject -> L.RenderObject -> IO (L.GameWire Int Int)
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
  quad <- L.createRenderObject L.quad (L.createTexturedMaterial white)
  nolight <- L.createNoLight
  w <- gameWire quad quad
  return $ L.Game { L.staticLights = [nolight],
                    L.staticGeometry = [mkWall False quad, mkWall True quad] ++ dashedMidsection quad,
                    L.mainCamera = pongCam,
                    L.dynamicLights = [],
                    L.gameLogic = w >>> L.quitWire GLFW.Key'Q}

main :: IO ()
main = L.runWindow screenWidth screenHeight "Pong Demo" 0 loadGame

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

{-# LANGUAGE Arrows #-}
module Main (main) where

--------------------------------------------------------------------------------
import Prelude hiding (id, (.))

import Control.Applicative  ((<|>))
import Control.Monad.Reader (ask)

import Data.Word  (Word16)
import Data.Maybe (maybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMap

import qualified Graphics.UI.GLFW as GLFW

-- In this example, we qualify everything from Lambency in order to highlight
-- which functions are specific to this library. In general, you shouldn't need
-- to do this once you get more acquainted with the types.
import qualified Lambency as L

import Linear

import Control.Wire

import FRP.Netwire.Move
import FRP.Netwire.Input

import System.Environment
---------------------------------------------------------------------------------

kWindowWidth :: Int
kWindowWidth = 200

kWindowHeight :: Int
kWindowHeight = 200

kWindowTitle :: String
kWindowTitle = "Moving square"

kMoveSpeed :: Float
kMoveSpeed = 35  -- pixels per second

kSquareColor :: V4 Float
kSquareColor = V4 1.0 0.0 0.0 1.0

kSquareSize :: Int
kSquareSize = 30  -- in pixels

networkedSquare :: L.NetworkedWire Int () ()
networkedSquare = arr (\_ -> ())
                . (player 0 &&& player 1)
                . L.networkedCopies
                . L.withinNetwork squareOffset
  where
    player :: Int -> L.NetworkedWire Int (IntMap (Maybe (Float, Float))) ()
    player n = L.withinNetwork $
               renderSquare . playerWithOffset (0, 0)
      where
        playerWithOffset off = L.mkContSFN $ \m ->
          case IMap.lookup n m of
            Just (Just off') -> (off', playerWithOffset off')
            _ -> (off, playerWithOffset off)

-- The movement is modeled by an offset, represented with a Float for each
-- axis. This offset will be changing over time, so we use a wire.
squareOffset :: L.ContWire () (Float, Float)
squareOffset = flip L.withDefault (pure (0.0, 0.0))
               $   mkOffset GLFW.Key'Left GLFW.Key'Right
               &&& mkOffset GLFW.Key'Down GLFW.Key'Up
  where
    mkOffset k1 k2 = integral 0
                   . (    keyPressed k1 . pure (-kMoveSpeed)
                      <|> keyPressed k2 . pure kMoveSpeed
                      <|> pure 0.0
                     )

-- Actual rendering code to send a square to be rendered with the given
-- offset
renderSquare :: L.ContWire (Float, Float) ()
renderSquare = L.contWireFrom (L.simpleSprite <$> ask)
               $ \s' -> L.everyFrame
               $ \(x, y) -> do
                 let s = L.changeSpriteColor kSquareColor s'
                 L.renderSprite s (pure kSquareSize) (-1.0) (V2 x y)
                 return ()

-- The moveSquare wire takes as input a bool for when it should quit. quitWire
-- is the wire that produces that bool. The default value is 'False', meaning
-- "don't quit". From netwire-input, 'keyPressed' will inhibit unless the given
-- key is pressed. If it is inhibiting, then no value is being produced, and
-- hence, we will use the default value (False in this case). If it is not
-- inhibiting, then it acts like the identity wire and produces True. The
-- combination of these wires will produce 'False' unless Q is pressed, in which
-- case it will produce 'True'.
quitWire :: L.ContWire a Bool
quitWire = (keyPressed GLFW.Key'Q . pure True) `L.withDefault` pure False

-- Our camera is a 2D camera. It is specified to be the size of the window, so
-- the coordinate space of our game world will initially be the same as the
-- coordinate space of our window. mk2DCam has type 'GameWire Vec2f Camera'
-- which means that we can change the position of our camera by passing in a
-- new position of the bottom left-hand corner into our wire. Since we only
-- want a simple static 2D camera in this example, we keep our camera
-- stationary by composing with the wire 'pure (V2 0 0)'
stationaryCamera :: L.ContWire () L.Camera
stationaryCamera = L.mk2DCam kWindowWidth kWindowHeight . pure (V2 0 0)

-- Since we are not using any dynamic lighting (lights that can change at
-- runtime), we pass the empty list to our game.
dynamicLights :: [L.ContWire () L.Light]
dynamicLights = []

clientWire :: Word16 -> L.ContWire (Bool, ()) (Maybe ())
clientWire port =
  L.runClientWire (127, 0, 0, 1) port 2 (pure ()) (const $ pure Nothing)
                  (\_ -> networkedSquare)

gameWire :: Word16 -> L.ContWire () (Maybe ())
gameWire port = clientWire port . (quitWire &&& id)

game :: Word16 -> L.Game ()
game port = L.Game stationaryCamera dynamicLights (gameWire port)

parsePort :: IO (Maybe Word16)
parsePort = parsePortFromArgs <$> getArgs
  where
    parsePortFromArgs :: [String] -> Maybe Word16
    parsePortFromArgs [] = Nothing
    parsePortFromArgs [_] = Nothing
    parsePortFromArgs ("--port":x:_) = Just (read x)
    parsePortFromArgs (_:xs) = parsePortFromArgs xs

main :: IO ()
main = do
  args <- getArgs
  if "--server" `elem` args
    then do
      putStrLn "Running as server..."
      L.runServer 2 () 0 networkedSquare
    else do
      putStrLn "Running as client..."
      maybe 21815 id <$> parsePort
        >>= L.runOpenGL kWindowWidth kWindowHeight kWindowTitle () . game

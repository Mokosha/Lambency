{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network
  ( NetworkedWire
  , withinNetwork

  , runClientWire
  , networkedCopies
  , runServer
  ) where

--------------------------------------------------------------------------------
import Control.Wire hiding (when)

import Data.Binary hiding (get, put)

import Lambency.Camera
import Lambency.GameLoop
import Lambency.Network.Client
import Lambency.Network.Copies
import Lambency.Network.Server
import Lambency.Network.Types
import Lambency.Network.Utils
import Lambency.Renderer
import Lambency.Types

import Linear

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

runServer :: Binary s => Int -> a -> s -> NetworkedWire s a a -> IO ()
runServer numPlayers initVal initGameState (NCW w) = do
  (config, unloadSprite) <- mkLoopConfig nullRenderer Nothing
  let game = Game (mk2DCam 0 0 . pure (V2 0.0 0.0)) []
           $ CW (runServerWire numPlayers initGameState w)
      st = mkLoopState initVal game
  runGameLoop st config
  unloadSprite

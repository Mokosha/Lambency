{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lambency.Network.Types where

--------------------------------------------------------------------------------
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.State.Strict

import qualified Data.ByteString      as BS
import Data.IntMap (IntMap)
import Data.Profunctor
import Data.Word

import FRP.Netwire

import Lambency.Types
import Lambency.Network.Packet

import Network.Socket hiding (sendTo, recvFrom)
--------------------------------------------------------------------------------

data ConnectionFailure
  = ConnectionFailure'Timeout
  | ConnectionFailure'Refused
    deriving (Eq, Show, Ord, Enum, Bounded, Read)

-- | Incoming packets are modeled as:
--   For each player connected to the server:
--     For each wireID in the current simulation:
--       Store a priority queue of wire packets based on their sequence number
-- type IncomingPackets = TArray Int (IntMap (Heap (Word64, BS.ByteString)))
type IncomingPackets = TArray Int (IntMap [(Word64, BS.ByteString)])

type ClientIDVar = TVar (Maybe (Either ConnectionFailure Int))

data NetworkState s
  = ClientNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets

      -- Client specific
    , clientGameState :: TVar (Maybe s)
    , localClientID :: ClientIDVar
    , serverAddr :: SockAddr
    , packetsOutClient :: [WirePacket]
    }
  | ServerNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets

      -- Server specific
    , serverGameState :: TVar s
    , connectedClients :: TArray Int (Maybe SockAddr)
    , packetsOutServer :: IntMap [WirePacket]
    } deriving (Eq)

type NetworkContext b = StateT (NetworkState b) GameMonad
type RawNetworkedWire s a b = Wire TimeStep String (NetworkContext s) a b
newtype NetworkedWire s a b = NCW { getNetworkedWire :: RawNetworkedWire s a b }
                          deriving ( Functor
                                   , Applicative
                                   , Floating
                                   , Fractional
                                   , Monoid
                                   , Num
                                   , Choice
                                   , Profunctor
                                   , Strong
                                   , Category
                                   , Arrow
                                   , ArrowChoice
                                   , ArrowLoop
                                   )

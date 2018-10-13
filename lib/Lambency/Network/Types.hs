{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lambency.Network.Types where

--------------------------------------------------------------------------------
import Control.Arrow
import Control.Concurrent.STM
import Control.Monad.State.Strict

import Data.Binary
import Data.Bits
import qualified Data.ByteString      as BS
import Data.IntMap (IntMap)
import Data.Profunctor

import GHC.Generics

import FRP.Netwire

import Lambency.Types

import Network.Socket hiding (sendTo, recvFrom)
--------------------------------------------------------------------------------

data ConnectionFailure
  = ConnectionFailure'Timeout
  | ConnectionFailure'Refused
    deriving (Eq, Show, Ord, Enum, Bounded, Read)

data WirePacket =
  WirePacket
  { wpPayload :: BS.ByteString
  , wpNetworkID :: Int
  , wpRequired :: Bool
  , wpPreviousRequired :: SequenceNumber
  } deriving (Generic, Eq, Ord, Show)

instance Binary WirePacket

data ReceivedWirePacket =
  ReceivedWirePacket
  { rwpPayload :: BS.ByteString
  , rwpRequired :: Bool
  , rwpPreviousRequired :: SequenceNumber
  , rwpSequenceNumber :: SequenceNumber
  } deriving (Generic, Eq, Ord, Show)

data Packet
  = Packet'ConnectionRequest
  | Packet'ConnectionAccepted Int
  | Packet'ConnectionDenied
  | Packet'ConnectionDisconnect Int
    -- TODO: Maybe ShortByteString is better?
  | Packet'Payload SequenceNumber Int [WirePacket]
  | Packet'GameState SequenceNumber Int Int BS.ByteString
    deriving (Generic, Eq, Ord, Show)

instance Binary Packet

-- | Incoming packets are modeled as:
--   For each player connected to the server:
--     For each wireID in the current simulation:
--       Store a priority queue of wire packets based on their sequence number
-- type IncomingPackets = TArray Int (IntMap (Heap (SequenceNumber, BS.ByteString)))
type IncomingPackets = TArray Int (IntMap [ReceivedWirePacket])

-- | A sequence number is a monotonically increasing value that can be thought
-- of as a timestamp for when a packet is sent across the wire.
newtype SequenceNumber = SequenceNumber { rawSeqNo :: Word64 }
                         deriving ( Eq, Ord, Show, Read, Bounded, Enum, Generic
                                  , Num, Real, Integral, Binary)

-- | An AckMask is a bitfield that represents the history of packets received.
-- By itself it lacks context (history starting from where?) so it is only
-- useful when paired with a SequenceNumber
newtype AckMask = AckMask { rawAckMask :: Word64 }
                deriving ( Eq, Ord, Show, Bounded, Enum, Generic, Bits, Binary )

-- | The AckState is the sequence number and ack mask pair that determine the
-- most recent set of received packets.
newtype AckState = AckState (SequenceNumber, AckMask)
                 deriving (Eq, Ord, Show, Generic, Binary)

-- | For each player:
--     For each wire:
--       Keep track of the packets received and the most recently received
--       sequence number
type PacketsReceived = TArray Int (IntMap AckState)

-- Until client is connected, this is Nothing. Then is either the client's ID on
-- the server or the reason why we couldn't connect.
type ClientIDVar = TVar (Maybe (Either ConnectionFailure Int))

data NetworkState s
  = ClientNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets
    , packetsReceived :: PacketsReceived

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
    , packetsReceived :: PacketsReceived

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

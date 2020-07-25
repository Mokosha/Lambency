{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Network.Socket
--------------------------------------------------------------------------------

data ConnectionFailure
  = ConnectionFailure'Timeout
  | ConnectionFailure'Refused
    deriving (Eq, Show, Ord, Enum, Bounded, Read)

data WirePacket =
  WirePacket
  { wpPayload :: BS.ByteString
  , wpPlayerID :: Int
  , wpNetworkID :: Int
  , wpLocalSequenceNumber :: SequenceNumber
  , wpSequenceNumber :: SequenceNumber
  , wpDestinationQueues :: [Int]
  } deriving (Generic, Eq, Ord, Show)

instance Binary WirePacket

data Packet
  = Packet'ConnectionRequest
  | Packet'ConnectionAccepted Int
  | Packet'ConnectionDenied
  | Packet'ConnectionDisconnect Int
    -- TODO: Maybe ShortByteString is better?
  | Packet'Payload [WirePacket]
  | Packet'GameState SequenceNumber Int Int BS.ByteString
    deriving (Generic, Eq, Ord, Show)

instance Binary Packet

-- | Incoming packets are modeled as:
--   For each player connected to the server:
--     For each wireID in the current simulation:
--       Store a priority queue of wire packets based on their sequence number
-- type IncomingPackets = TArray Int (IntMap (Heap WirePacket))
type IncomingPackets = TArray Int (IntMap [WirePacket])

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

-- | For each queue:
--     Keep track of the packets received, and the player to which they
--     correspond. To do client-side prediction, we have to recognize when we
--     have placed packets in a queue out of order. This will be whenever we see
--     misordering in a local queue with respect to local and foreign sequence
--     numbers.
-- type PacketsReceived = IntMap (Heap WirePacket)
type PacketsReceived = IntMap [WirePacket]

-- Until client is connected, this is Nothing. Then is either the client's ID on
-- the server or the reason why we couldn't connect.
type ClientIDVar = TVar (Maybe (Either ConnectionFailure Int))

data NetworkState s
  = ClientNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets
    , packetQueues :: PacketsReceived

      -- Client specific
    , serverPacketsAcked :: TVar AckState
    , clientGameState :: TVar (Maybe s)
    , localClientID :: ClientIDVar
    , serverAddr :: SockAddr
    , packetsOutClient :: [WirePacket]
    }
  | ServerNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets
    , packetQueues :: PacketsReceived

      -- Server specific
    , clientPacketsAcked :: TArray Int AckState
    , serverGameState :: TVar s
    , connectedClients :: TArray Int (Maybe SockAddr)
    , packetsOutServer :: IntMap [WirePacket]
    } deriving (Eq)

data NetworkException = NoNetworkException
                      | QueueOrderingException Int SequenceNumber
                      | MultipleNetworkExceptions [NetworkException]
                        deriving (Eq, Ord, Show, Generic)

instance Semigroup NetworkException where
  NoNetworkException <> x = x
  x <> NoNetworkException = x
  (MultipleNetworkExceptions xs) <> (MultipleNetworkExceptions ys) =
    MultipleNetworkExceptions $ xs <> ys
  (MultipleNetworkExceptions xs) <> x = MultipleNetworkExceptions (x : xs)
  x <> (MultipleNetworkExceptions xs) = MultipleNetworkExceptions (x : xs)
  x <> y = MultipleNetworkExceptions [x, y]

instance Monoid NetworkException where
  mempty = NoNetworkException
  mappend = (<>)

type NetworkContext b = StateT (NetworkState b) GameMonad
type RawNetworkedWire s a b =
  Wire TimeStep NetworkException (NetworkContext s) a b
newtype NetworkedWire s a b = NCW { getNetworkedWire :: RawNetworkedWire s a b }
                          deriving ( Functor
                                   , Applicative
                                   , Floating
                                   , Fractional
                                   , Num
                                   , Choice
                                   , Profunctor
                                   , Strong
                                   , Category
                                   , Arrow
                                   , ArrowChoice
                                   , ArrowLoop
                                   )
deriving instance Semigroup b => Semigroup (NetworkedWire s a b)
deriving instance Monoid b => Monoid (NetworkedWire s a b)

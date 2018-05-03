module Lambency.Network
  ( connectToFullInformationServer
  , runFullInformationServer
  )where

--------------------------------------------------------------------------------
import Control.Monad.Trans (liftIO)
import Control.Wire

import Data.Binary
import Data.Map (Map)
import qualified Data.Map as Map

import Lambency.Types
import Lambency.GameObject

import Lambency.Network.EventServer

import Prelude hiding ((.), id)

import Network.Socket
--------------------------------------------------------------------------------

createUDPSocket :: Word16 -> IO Socket
createUDPSocket port = do
  sock <- socket AF_INET Datagram 0
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind sock $ SockAddrInet (fromIntegral port) localhost
  return sock

{--
data NetworkState {
  clients :: Map Int (Maybe SockAddr),
  packetQueues :: Map Int [ByteString],

networkedCopies :: Int -> (NetworkedWire a a, Map Int (NetworkedWire () a))

-- | An authority is a wire that's sync'd to the server. It will predict locally
-- what the correct result is, but will always confirm the result with the
-- server and not drop any packets.
authority :: GameWire a b -> NetworkedWire a b

withinNetwork :: GameWire a b -> NetworkedWire a b

runNetworkedWire :: (Word8, Word8, Word8, Word8) -> NetworkedWire a a -> GameWire a a
--}
-- | Opens a connection to a server where each client has full knowledge of the
-- results of the other clients. This is useful for things like
-- (non-deterministic) chat rooms, or board games such as chess.
connectToFullInformationServer
  :: (Binary a, Binary b)
  => (Word8, Word8, Word8, Word8)
  -- ^ What server do we connect to?
  -> GameWire a b
  -- ^ Each Player's connection
  -> Int
  -- ^ Number of players
  -> ContWire a (Maybe b, Map Int (Maybe b))
  -- ^ Results
connectToFullInformationServer addr w numPlayers =
  let serverAddr = SockAddrInet 211518 (tupleToHostAddress addr)
      mkWire sock = eventClientWire numPlayers sock serverAddr w
      mkGameSocket = GameMonad $ liftIO $ createUDPSocket 18152
  in wireFrom mkGameSocket mkWire `withDefault` pure (Nothing, Map.empty)

-- | 'runFullInformationServer n w' creates a server with 'n' copies of the wire
-- 'w'. The incoming connections each provide the values needed for 'a' and the
-- produced values 'b' are sent to each client.
runFullInformationServer :: (Binary a, Binary b)
                         => Int -> GameWire a b -> IO ()
runFullInformationServer numPlayers w = withSocketsDo $ do
  sock <- createUDPSocket 21518
  runEventServer sock numPlayers w  

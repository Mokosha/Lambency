{-# LANGUAGE TupleSections #-}
module Lambency.Network.Utils where

--------------------------------------------------------------------------------
import Control.Monad.State.Strict
import Control.Wire
import Data.Word

import Lambency.Network.Types
import Lambency.Types

import Network.Socket hiding (sendTo, recvFrom)

import Prelude hiding ((.), id)

import System.Console.Concurrent
--------------------------------------------------------------------------------
receiveWirePacket :: SequenceNumber -> WirePacket -> ReceivedWirePacket
receiveWirePacket seqNo wp =
  ReceivedWirePacket
  { rwpPayload = wpPayload wp
  , rwpRequired = wpRequired wp
  , rwpPreviousRequired = wpPreviousRequired wp
  , rwpSequenceNumber = seqNo
  }

createUDPSocket :: Word16 -> IO Socket
createUDPSocket port = do
  sock <- socket AF_INET Datagram 0
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind sock $ SockAddrInet (fromIntegral port) localhost
  return sock

networkIO :: IO a -> NetworkContext s a
networkIO = lift . GameMonad . liftIO

netStrLn :: String -> IO ()
netStrLn s = outputConcurrent $ s ++ ['\n']

networkWireFrom :: NetworkContext s a
                -> (a -> NetworkedWire s b c)
                -> NetworkedWire s b c
networkWireFrom prg fn = NCW $ mkGen $ \dt val -> do
  seed <- prg
  stepWire (getNetworkedWire $ fn seed) dt (Right val)

withinNetwork :: ContWire a b -> NetworkedWire s a b
withinNetwork = NCW . mapWire (\m -> StateT $ \x -> (,x) <$> m) . getContinuousWire

withNetworkState :: NetworkedWire s a b -> NetworkState s -> GameWire a b
withNetworkState (NCW w) st = mkGen $ \dt x -> do
  ((res, w'), st') <- runStateT (stepWire w dt (Right x)) st
  return (res, withNetworkState (NCW w') st')


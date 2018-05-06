{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network
  ( runClientWire
  , connectToFullInformationServer
  , runFullInformationServer
  )where

--------------------------------------------------------------------------------
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict
import Control.Wire

import Data.Binary hiding (get)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map             as Map
import Data.Profunctor

import GHC.Generics

import Lambency.Types
import Lambency.GameObject

import Lambency.Network.EventServer

import Prelude hiding ((.), id)

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString
--------------------------------------------------------------------------------

createUDPSocket :: Word16 -> IO Socket
createUDPSocket port = do
  sock <- socket AF_INET Datagram 0
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind sock $ SockAddrInet (fromIntegral port) localhost
  return sock

data NetworkState =
  ClientNetworkState
  { localSocket :: Socket
  , localClientID :: Int
  , serverAddr :: SockAddr
  , packetsOut :: [BS.ByteString]
  , packetsIn :: Map Int [BS.ByteString]
  } deriving (Eq)

type NetworkContext = StateT NetworkState GameMonad
type RawNetworkedWire a b = Wire TimeStep String NetworkContext a b
newtype NetworkedWire a b = NCW { getNetworkedWire :: RawNetworkedWire a b }
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

data Packet a
  = Packet'ConnectionRequest
  | Packet'ConnectionAccepted Int
  | Packet'ConnectionDenied
  | Packet'ConnectionDisconnect Int
  | Packet'Payload Int !a
    deriving (Generic, Eq, Ord, Show)

instance Binary b => Binary (Packet b)

kMaxPayloadSize :: Integral a => a
kMaxPayloadSize = 1024

encPkt :: (Binary a) => Packet a -> BS.ByteString
encPkt = BSL.toStrict . encode

sendConnRequest :: Socket -> SockAddr -> IO Int
sendConnRequest sock addr =
  let pkt :: Packet ()
      pkt = Packet'ConnectionRequest
  in sendTo sock (encPkt pkt) addr

sendAccepted :: Int -> Socket -> SockAddr -> IO Int
sendAccepted clientID sock addr =
  let pkt :: Packet ()
      pkt = Packet'ConnectionAccepted clientID
  in sendTo sock (encPkt pkt) addr

sendDisconnected :: Int -> Socket -> SockAddr -> IO Int
sendDisconnected clientID sock addr =
  let pkt :: Packet ()
      pkt = Packet'ConnectionDisconnect clientID
  in sendTo sock (encPkt pkt) addr

sendConnDenied :: Socket -> SockAddr -> IO Int
sendConnDenied sock addr =
  let pkt :: Packet ()
      pkt = Packet'ConnectionDenied
  in sendTo sock (encPkt pkt) addr

sendPayload :: (Binary a) => a -> Int -> Socket -> SockAddr -> IO Int
sendPayload x clientID sock addr =
  sendTo sock (encPkt $ Packet'Payload clientID x) addr

decodePkt :: (Binary a) => BSL.ByteString -> Maybe (Packet a)
decodePkt bytes = case decodeOrFail bytes of
  Right (_, _, x) -> Just x
  _ -> Nothing

networkWireFrom :: NetworkContext a -> (a -> NetworkedWire b c) -> NetworkedWire b c
networkWireFrom prg fn = NCW $ mkGen $ \dt val -> do
  seed <- prg
  stepWire (getNetworkedWire $ fn seed) dt (Right val)

networkedCopies :: Binary a => Int -> (NetworkedWire a (a, Map Int (Maybe a)))
networkedCopies numPlayers = NCW (client &&& peers)
  where
    client :: RawNetworkedWire a a
    client = undefined

    peers :: RawNetworkedWire b (Map Int (Maybe a))
    peers = undefined

-- | An authority is a wire that's sync'd to the server. It will predict locally
-- what the correct result is, but will always confirm the result with the
-- server and not drop any packets.
authority :: GameWire a b -> NetworkedWire a b
authority w = undefined

withinNetwork :: GameWire a b -> NetworkedWire a b
withinNetwork = NCW . mapWire (\m -> StateT $ \x -> (,x) <$> m)

withNetworkState :: NetworkedWire a b -> NetworkState -> GameWire a b
withNetworkState (NCW w) s = mkGen $ \dt x -> do
  ((res, w'), s') <- runStateT (stepWire w dt (Right x)) s
  return (res, withNetworkState (NCW w) s')

connectedClient :: Int -> NetworkedWire a a -> NetworkedWire a a
connectedClient clientID (NCW _w) = networkWireFrom addClient (\_ -> clientW _w)
  where
    addClient = modify $ \s -> s { localClientID = clientID }

    clientW :: RawNetworkedWire a a -> NetworkedWire a a
    clientW = undefined
    -- clientW w = NCW $ mkGen $ \dt x -> do
      -- Send out all of the outgoing packets as a single packet

data ConnectionFailure
  = ConnectionFailure'Timeout
  | ConnectionFailure'Refused

data ConnectionState
  = ConnectionState'Failure ConnectionFailure
  | ConnectionState'Connecting
  | ConnectionState'Connected Int

kTimeout :: Float
kTimeout = 5.0

runClientWire :: forall a
               . (Word8, Word8, Word8, Word8)
              -- ^ Address to connect to
              -> GameWire a a
              -- ^ GameWire to run while connecting
              -> (ConnectionFailure -> GameWire a a)
              -- ^ GameWire to switch to if the connection failed
              -> NetworkedWire a a
              -- ^ Wire to run once connected
              -> GameWire a a
runClientWire addr whileConnecting onFailure (NCW client) =
  wireFrom mkNetworkState $ withNetworkState
    $ (NCW $ switch $ second mkResult . connectServer wcn . (id &&& timeF))
  where
    mkNetworkState = do
      sock <- GameMonad $ liftIO $ createUDPSocket 21518
      return $ ClientNetworkState
               { localSocket = sock
               , localClientID = (-1)
               , serverAddr = SockAddrInet 21518 $ tupleToHostAddress addr
               , packetsIn = Map.empty
               , packetsOut = []
               }

    wcn :: RawNetworkedWire a a
    wcn = getNetworkedWire $ withinNetwork whileConnecting

    mkResult :: RawNetworkedWire ConnectionState (Event (RawNetworkedWire a a))
    mkResult = fmap (getNetworkedWire . toResultW) <$> became connectionResult

    toResultW :: ConnectionState -> NetworkedWire a a
    toResultW ConnectionState'Connecting = error "Still connecting..."
    toResultW (ConnectionState'Failure f) = withinNetwork $ onFailure f
    toResultW (ConnectionState'Connected cid) = connectedClient cid (NCW client)

    connectionResult :: ConnectionState -> Bool
    connectionResult ConnectionState'Connecting = False
    connectionResult _ = True

    networkIO :: IO b -> NetworkContext b
    networkIO = lift . GameMonad . liftIO

    connectServer :: RawNetworkedWire a a
                  -> RawNetworkedWire (a, Float) (a, ConnectionState)
    connectServer w = mkGen $ \dt (x, t) -> do
      (result, w') <- stepWire w dt (Right x)
      let returnConn c = return ((,c) <$> result, connectServer w')
      if t > kTimeout
        then returnConn (ConnectionState'Failure ConnectionFailure'Timeout)
        else do
          -- Send connecting packet
          sock <- localSocket <$> get
          sa <- serverAddr <$> get
          _ <- networkIO $ sendConnRequest sock sa

          -- Receive incoming packet and see what's up
          (bytes, pktAddr) <- networkIO $ recvFrom sock kMaxPayloadSize
          if (pktAddr /= sa || BS.length bytes == 0)
            then returnConn ConnectionState'Connecting
            else case (decodePkt (BSL.fromStrict bytes)) :: Maybe (Packet ()) of
              Just (Packet'ConnectionAccepted clientID) ->
                returnConn $ ConnectionState'Connected clientID
              Just (Packet'ConnectionDenied) ->
                returnConn $ ConnectionState'Failure ConnectionFailure'Refused

              -- Otherwise just ignore the packet
              _ -> returnConn ConnectionState'Connecting

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
  let serverAddr = SockAddrInet 21518 (tupleToHostAddress addr)
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

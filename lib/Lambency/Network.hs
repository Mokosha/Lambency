{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network
  ( runClientWire
  , networkedCopies
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict
import Control.Wire

import Data.Binary hiding (get, put)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict   as Map
import Data.List (sortBy)
import Data.Profunctor

import GHC.Generics

import Lambency.Types
import Lambency.GameObject

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

data WirePacket =
  WirePacket
  { wpPayload :: BS.ByteString
  , wpNetworkID :: Int
  } deriving (Generic, Eq, Ord, Show)

instance Binary WirePacket

data NetworkState =
  ClientNetworkState
  { localSocket :: Socket
  , localClientID :: Int
  , nextWireID :: Int
  , serverAddr :: SockAddr
  , packetsOut :: [WirePacket]
  , packetsIn :: IntMap [(Word64, WirePacket)]
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

networkIO :: IO a -> NetworkContext a
networkIO = lift . GameMonad . liftIO

data Packet
  = Packet'ConnectionRequest
  | Packet'ConnectionAccepted Int
  | Packet'ConnectionDenied
  | Packet'ConnectionDisconnect Int
    -- TODO: Maybe ShortByteString is better?
  | Packet'Payload Int Word64 [WirePacket]
    deriving (Generic, Eq, Ord, Show)

instance Binary Packet

kMaxPayloadSize :: Integral a => a
kMaxPayloadSize = 1024

encPkt :: Packet -> BS.ByteString
encPkt = BSL.toStrict . encode

sendConnRequest :: Socket -> SockAddr -> IO Int
sendConnRequest sock = sendTo sock (encPkt Packet'ConnectionRequest)

sendAccepted :: Int -> Socket -> SockAddr -> IO Int
sendAccepted clientID sock =
  sendTo sock (encPkt $ Packet'ConnectionAccepted clientID)

sendDisconnected :: Int -> Socket -> SockAddr -> IO Int
sendDisconnected clientID sock =
  sendTo sock (encPkt $ Packet'ConnectionDisconnect clientID)

sendConnDenied :: Socket -> SockAddr -> IO Int
sendConnDenied sock = sendTo sock (encPkt Packet'ConnectionDenied)

sendPayload :: Word64 -> Int -> [WirePacket] -> Socket -> SockAddr -> IO Int
sendPayload pktNo cid dat sock =
  sendTo sock (encPkt $ Packet'Payload cid pktNo dat)

decodePkt :: BSL.ByteString -> Maybe Packet
decodePkt bytes = case decodeOrFail bytes of
  Right (_, _, x) -> Just x
  _ -> Nothing

networkWireFrom :: NetworkContext a -> (a -> NetworkedWire b c) -> NetworkedWire b c
networkWireFrom prg fn = NCW $ mkGen $ \dt val -> do
  seed <- prg
  stepWire (getNetworkedWire $ fn seed) dt (Right val)

networkedCopies :: forall a
                 . Binary a
                => (NetworkedWire a (a, IntMap (Maybe a)))
networkedCopies = networkWireFrom registerWire $ \(clientID, wireID) ->
  NCW (client clientID wireID &&& peers clientID wireID Map.empty)
  where
    registerWire :: NetworkContext (Int, Int)
    registerWire = do
      st <- get
      let wireID = nextWireID st
      put $ st { nextWireID = wireID + 1 }
      return (localClientID st, wireID)

    -- Just send packets and hope other side receives them...
    client :: Int -> Int -> RawNetworkedWire a a
    client _ wireID = mkGen_ $ \x -> do
      -- TODO: Use clientID to actually make sure that we're receiving packets
      -- in order as we expect
      let dat = BSL.toStrict $ encode x
      modify $ \s -> s { packetsOut = WirePacket dat wireID : (packetsOut s) }
      return $ Right x

    peers :: Int -> Int -> IntMap Int -> RawNetworkedWire b (IntMap (Maybe a))
    peers clientID wireID connectedPlayers = mkGenN $ \_ -> do
      -- Find all packets that correspond to this wireID but not this clientID
      playerData <- Map.filterWithKey (\k _ -> k /= clientID) . packetsIn <$> get
      let wireData =
            Map.map (filter (\(_, WirePacket _ wid) -> wid == wireID)) playerData
          newPlayers = Map.difference wireData connectedPlayers
          numConnectedPlayers = Map.size connectedPlayers
          newConnectedPlayers = Map.union connectedPlayers
                              $ Map.fromList
                              $ zip (Map.keys newPlayers) [numConnectedPlayers..]

          mkResult :: (Int, [(Word64, WirePacket)]) -> (Int, Maybe a)
          mkResult (player, []) = (newConnectedPlayers Map.! player, Nothing)
          mkResult (player, (_, WirePacket payload _):_) =
            let pid = newConnectedPlayers Map.! player
            in (pid, Just $ decode (BSL.fromStrict payload))

          result = Map.fromList $ map mkResult (Map.toList wireData)

      return $ (Right result, peers clientID wireID newConnectedPlayers)

{--
-- | An authority is a wire that's sync'd to the server. It will predict locally
-- what the correct result is, but will always confirm the result with the
-- server and not drop any packets.
authority :: GameWire a b -> NetworkedWire a b
authority w = undefined
--}

withinNetwork :: GameWire a b -> NetworkedWire a b
withinNetwork = NCW . mapWire (\m -> StateT $ \x -> (,x) <$> m)

withNetworkState :: NetworkedWire a b -> NetworkState -> GameWire a b
withNetworkState (NCW w) s = mkGen $ \dt x -> do
  ((res, w'), s') <- runStateT (stepWire w dt (Right x)) s
  return (res, withNetworkState (NCW w') s')

connectedClient :: Int -> NetworkedWire a a -> NetworkedWire a a
connectedClient clientID (NCW _w) =
  networkWireFrom addClient (\_ -> NCW $ clientW 0 _w)
  where
    addClient = modify $ \s -> s { localClientID = clientID }

    clientW :: Word64 -> RawNetworkedWire a a -> RawNetworkedWire a a
    clientW seqNo w = mkGen $ \dt x -> do
      -- Grab all of the existing data from the server and place it in the
      -- corresponding list
      sock <- localSocket <$> get
      sa <- serverAddr <$> get
      (bytes, pktAddr) <- networkIO $ recvFrom sock kMaxPayloadSize
      c <- if (pktAddr /= sa || BS.length bytes == 0) then return True else do
        case decodePkt (BSL.fromStrict bytes) of
          Just (Packet'Payload cid seqNo' bs) -> do
            modify $ \s ->
              s { packetsIn =
                     Map.map (sortBy (compare `on` fst)) $
                     Map.insertWith (++) cid ((seqNo',) <$>  bs) (packetsIn s) }
            return True
          Just (Packet'ConnectionDisconnect cid)
            | cid == clientID -> return False
            | otherwise -> do
              modify $ \s -> s { packetsIn = Map.delete cid (packetsIn s) }
              return True

          -- Ignore all other packets
          _ -> return True

      -- Run the wire
      (res, w') <- stepWire w dt (Right x)

      -- If we've been disconnected, switch to the empty wire
      if not c then return (res, mkEmpty) else do
        -- If we're still connected, Send out all of the outgoing data as a
        -- single packet
        outPackets <- packetsOut <$> get
        _ <- networkIO $ sendPayload seqNo clientID outPackets sock sa

        -- Reset all of the outgoing packets
        modify $ \s -> s { packetsOut = [] }

        return (res, clientW (seqNo + 1) w')

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
               , nextWireID = 0
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
            else case (decodePkt (BSL.fromStrict bytes)) of
              Just (Packet'ConnectionAccepted clientID) ->
                returnConn $ ConnectionState'Connected clientID
              Just (Packet'ConnectionDenied) ->
                returnConn $ ConnectionState'Failure ConnectionFailure'Refused

              -- Otherwise just ignore the packet
              _ -> returnConn ConnectionState'Connecting

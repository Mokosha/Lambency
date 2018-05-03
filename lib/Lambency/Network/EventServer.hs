{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network.EventServer (
  runEventServer,
  eventClientWire
) where
--------------------------------------------------------------------------------
import Control.Wire
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import qualified Control.Wire as W

import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString      as BS
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Generics (Generic)

import Lambency.Camera
import Lambency.GameSession
import Lambency.GameLoop
import Lambency.Renderer
import Lambency.Types

import Linear

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

data ConnectionPacket
  = ConnectionPacket'Request
  | ConnectionPacket'Accepted Int
  | ConnectionPacket'Denied
  | ConnectionPacket'Disconnect Int
    deriving (Generic, Eq, Ord, Show)

instance Binary ConnectionPacket

data DataPacket a b
  = DataPacket'Connection ConnectionPacket
  | DataPacket'ClientPayload !a
  | DataPacket'ServerPayload Int !b
    deriving (Generic, Eq, Ord, Show)

instance (Binary a, Binary b) => Binary (DataPacket a b)

type ConnectedClient a b = (Int, GameWire a b)

data ConnectedClients a b = ConnectedClients
  { numSlots :: Int
  , gameWire :: GameWire a b
  , clientSlots :: Map SockAddr (ConnectedClient a b)
  }

kMaxPayloadSize :: Integral a => a
kMaxPayloadSize = 1024

encPktForClient :: (Binary a, Binary b)
                => ConnectedClient a b -> DataPacket a b -> BS.ByteString
encPktForClient _ = BSL.toStrict . encode

encPktForWire :: (Binary a, Binary b)
              => GameWire a b -> DataPacket a b -> BS.ByteString
encPktForWire _ = BSL.toStrict . encode

encConnForClient :: (Binary a, Binary b)
                 => ConnectedClient a b -> ConnectionPacket -> BS.ByteString
encConnForClient c = encPktForClient c . DataPacket'Connection

sendAccepted :: (Binary a, Binary b)
             => ConnectedClient a b -> Socket -> SockAddr
             -> IO Int
sendAccepted  c@(clientID, _) sock addr =
  sendTo sock (encConnForClient c $ ConnectionPacket'Accepted clientID) addr

sendDisconnected :: (Binary a, Binary b)
                 => ConnectedClient a b -> Socket -> SockAddr -> IO Int
sendDisconnected c@(clientID, _) sock addr =
  sendTo sock (encConnForClient c $ ConnectionPacket'Disconnect clientID) addr

sendConnDenied :: (Binary a, Binary b)
               => ConnectedClient a b -> Socket -> SockAddr -> IO Int
sendConnDenied c sock addr =
  sendTo sock (encConnForClient c $ ConnectionPacket'Denied) addr

sendServerPayload :: (Binary a, Binary b)
                  => ConnectedClient a b -> b -> Socket -> SockAddr -> IO Int
sendServerPayload c@(clientID, _) x sock addr =
  sendTo sock (encPktForClient c $ DataPacket'ServerPayload clientID x) addr

sendClientPayload :: (Binary a, Binary b)
                  => GameWire a b
                  -> a
                  -> Socket
                  -> SockAddr
                  -> IO Int
sendClientPayload w x sock addr =
  sendTo sock (encPktForWire w $ DataPacket'ClientPayload x) addr

sendConnectionRequest :: (Binary a, Binary b)
                      => GameWire a b -> Socket -> SockAddr -> IO Int
sendConnectionRequest w sock addr =
  flip (sendTo sock) addr
      $ encPktForWire w $ DataPacket'Connection ConnectionPacket'Request

-- TODO: Think about using type applications for this.
decodePacketForWire :: (Binary a, Binary b)
                    => GameWire a b -> BSL.ByteString -> Maybe (DataPacket a b)
decodePacketForWire _ bytes =
  case decodeOrFail bytes of
    Right (_, _, x) -> Just x
    _ -> Nothing
            
decodePacketForClient :: (Binary a, Binary b)
                      => ConnectedClient a b -> BSL.ByteString
                      -> Maybe (DataPacket a b)
decodePacketForClient _ bytes =                      
  case decodeOrFail bytes of
    Right (_, _, x) -> Just x
    _ -> Nothing

handleClient :: (Binary a, Binary b)
             => Socket
             -> SockAddr
             -> ConnectedClient a b
             -> ConnectedClients a b
             -> BSL.ByteString
             -> TimeStep
             -> GameMonad (ConnectedClients a b)
handleClient sock addr client clients bytes ts =
  case decodePacketForClient client bytes of
    Just (DataPacket'Connection ConnectionPacket'Request) -> do
      _ <- GameMonad $ liftIO $ sendAccepted client sock addr
      return clients
    Just (DataPacket'ClientPayload x) ->
      let (_, w) = client
       in do
        -- Run the wire
        (res, w') <- W.stepWire w ts (Right x)
        case res of
          Left _ -> do
            -- Wire inhibited -- disconnect!
            _ <- GameMonad $ liftIO $ sendDisconnected client sock addr
            return $ clients {
              clientSlots = Map.delete addr (clientSlots clients) }

          Right y -> do
            -- Send return value to all connected clients!
            forM_ (Map.keys $ clientSlots clients) $
              GameMonad . liftIO . sendServerPayload client y sock
            let newClient = (fst client, w')
            return $ clients {
              clientSlots = Map.insert addr newClient (clientSlots clients) }

    -- Discard all other packets
    _ -> return clients

findNewSlot :: Map SockAddr (ConnectedClient a b) -> Int
findNewSlot = go 0 . sortBy (compare `on` fst) . Map.elems
  where
    go x [] = x
    go x ((y, _):rest)
      | x == y = go (x+1) rest
      | otherwise = x

handleNewConnection :: (Binary a, Binary b)
                    => Socket -> SockAddr -> ConnectedClients a b -> BSL.ByteString
                    -> GameMonad (ConnectedClients a b)
handleNewConnection sock addr clients bytes =
  case decodeOrFail bytes of
    Right (_, _, ConnectionPacket'Request) ->
      let clientConns = clientSlots clients
          newClientID = findNewSlot clientConns
          newClient = (newClientID, gameWire clients)
          newClients = Map.insert addr newClient clientConns
      in if newClientID == numSlots clients
         then do
           _ <- GameMonad $ liftIO $ sendConnDenied newClient sock addr
           return clients
         else do
           _ <- GameMonad $ liftIO $ sendAccepted newClient sock addr
           return $ clients { clientSlots = newClients }

    -- Any other packet than a connection request is ignored.
    _ -> return clients

handlePacket :: (Binary a, Binary b)
             => Socket
             -> ConnectedClients a b
             -> BSL.ByteString
             -> SockAddr
             -> TimeStep
             -> GameMonad (ConnectedClients a b)
handlePacket sock clients bytes addr ts = do
  case Map.lookup addr (clientSlots clients) of
    Just client -> handleClient sock addr client clients bytes ts
    Nothing -> handleNewConnection sock addr clients bytes

eventServerWire :: (Binary a, Binary b)
                => Socket -> ConnectedClients a b -> ContWire () (Maybe ())
eventServerWire sock clients = CW $ W.mkGen $ \dt _ -> do
  (bytes, addr) <- GameMonad $ liftIO $ recvFrom sock kMaxPayloadSize
  newClients <- handlePacket sock clients (BSL.fromStrict bytes) addr dt
  let (CW w') = eventServerWire sock newClients
  return (Right (Just ()), w')

runEventServer :: (Binary a, Binary b)
               => Socket -> Int -> GameWire a b -> IO ()
runEventServer sock numPlayers w = do
  (config, unloadSprite) <- mkLoopConfig nullRenderer Nothing
  let game = Game (mk2DCam 0 0 . pure (V2 0.0 0.0)) [] 
           $ eventServerWire sock (ConnectedClients numPlayers w Map.empty)
      st = mkLoopState () game
  runGameLoop st config
  unloadSprite

clientWireWithID :: (Binary a, Binary b)
                 => Socket -> SockAddr -> Int -> GameWire a b
                 -> GameWire a (Maybe b, Map Int (Maybe b))
clientWireWithID sock addr clientID w = W.mkGenN $ \x -> do
  _ <- GameMonad $ liftIO $ sendClientPayload w x sock addr
  let recvAll pkts = do
        bytes <- BSL.fromStrict . fst <$> recvFrom sock kMaxPayloadSize
        if BSL.length bytes == 0
          then return pkts
          else case decodePacketForWire w bytes of
            Just (DataPacket'ServerPayload respID y) ->
              recvAll $ (respID, Just y) : pkts
            Just (DataPacket'Connection (ConnectionPacket'Disconnect cid)) ->
              recvAll $ (cid, Nothing) : pkts

            -- Ignore other types of packets
            _ -> recvAll pkts
  pktMap <- Map.fromList <$> (GameMonad $ liftIO $ recvAll [])
  case Map.lookup clientID pktMap of
    Just Nothing -> return (Left mempty, clientWireWithID sock addr clientID w)
    Just x' -> return (Right (x', Map.delete clientID pktMap),
                      clientWireWithID sock addr clientID w)
    Nothing -> return (Right (Nothing, Map.delete clientID pktMap),
                       clientWireWithID sock addr clientID w)

eventClientWire :: (Binary a, Binary b)
                => Int -> Socket -> SockAddr -> GameWire a b
                -> GameWire a (Maybe b, Map Int (Maybe b))
eventClientWire numPlayers sock serverAddr =
  let emptyPlayerList =
        (Nothing, Map.fromList [ (x, Nothing) | x <- [1..(numPlayers - 1)]])
      waitForResponse w = W.mkGenN $ \_ -> do
        bytes <- GameMonad $ liftIO $ do
          _ <- sendConnectionRequest w sock serverAddr
          BSL.fromStrict . fst <$> recvFrom sock kMaxPayloadSize
        case decodeOrFail bytes of
          Right (_, _, ConnectionPacket'Accepted clientID) ->
            return (Right emptyPlayerList,
                    clientWireWithID sock serverAddr clientID w)

          -- If we didn't get a connection accepted response, then ignore all of
          -- the other packets and just try again
          _ -> return (Right emptyPlayerList, waitForResponse w)
  in waitForResponse

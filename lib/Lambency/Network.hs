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
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict
import Control.Wire

import Data.Binary hiding (get, put)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict   as IMap
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict      as Map
import Data.Maybe (isJust)
import Data.Profunctor

import GHC.Generics

import Lambency.Camera
import Lambency.GameLoop
import Lambency.GameObject
import Lambency.GameSession
import Lambency.Renderer
import Lambency.Types

import Linear

import Prelude hiding ((.), id)

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString
--------------------------------------------------------------------------------

createUDPSocket :: Word16 -> IO Socket
createUDPSocket port = do
  sock <- socket AF_INET Datagram 0
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind sock $ SockAddrInet (fromIntegral port) localhost
  setNonBlockIfNeeded (fdSocket sock)
  return sock

data WirePacket =
  WirePacket
  { wpPayload :: BS.ByteString
  , wpNetworkID :: Int
  } deriving (Generic, Eq, Ord, Show)

instance Binary WirePacket

data NetworkState
  = ClientNetworkState
    { localSocket :: Socket
    , localClientID :: Int
    , nextWireID :: Int
    , serverAddr :: SockAddr
    , packetsOutClient :: [WirePacket]
    , packetsIn :: IntMap [(Word64, WirePacket)]
    }
  | ServerNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , connectedClients :: Map SockAddr Int
    , packetsOutServer :: IntMap [WirePacket]
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

filterForWireID :: Int -> IntMap [(a, WirePacket)] -> IntMap [(a, WirePacket)]
filterForWireID w = IMap.map (filter ((== w) . wpNetworkID . snd))

networkedCopiesClient :: forall a
                       . Binary a
                      => (Int, Int)
                      -> NetworkedWire a (IntMap (Maybe a))
networkedCopiesClient (clientID, wireID) =
  NCW (arr (uncurry $ IMap.insert 0) . (client &&& peers IMap.empty))
  where
    -- Just send packets and hope other side receives them...
    client :: RawNetworkedWire a (Maybe a)
    client = mkGen_ $ \x -> do
      -- TODO: Use clientID to actually make sure that we're receiving packets
      -- in order as we expect
      let dat = BSL.toStrict $ encode x
      modify $ \s -> s { packetsOutClient =
                            WirePacket dat wireID : (packetsOutClient s) }
      return $ Right (Just x)

    peers :: IntMap Int -> RawNetworkedWire b (IntMap (Maybe a))
    peers connectedPlayers = mkGenN $ \_ -> do
      -- Find all packets that correspond to this wireID but not this clientID
      playerData <- IMap.filterWithKey (\k _ -> k /= clientID) . packetsIn <$> get
      let wireData = filterForWireID wireID playerData
          newPlayers = IMap.difference wireData connectedPlayers
          -- Slot zero is reserved for local player
          numConnectedPlayers = 1 + IMap.size connectedPlayers
          newConnectedPlayers = IMap.union connectedPlayers
                              $ IMap.fromList
                              $ zip (IMap.keys newPlayers) [numConnectedPlayers..]

          mkResult :: (Int, [(Word64, WirePacket)]) -> (Int, Maybe a)
          mkResult (player, []) = (newConnectedPlayers IMap.! player, Nothing)
          mkResult (player, (_, WirePacket payload _):_) =
            let pid = newConnectedPlayers IMap.! player
            in (pid, Just $ decode (BSL.fromStrict payload))

          result = IMap.fromList $ map mkResult (IMap.toList wireData)

      return $ (Right result, peers newConnectedPlayers)

networkedCopiesServer :: forall a
                       . Binary a => Int -> NetworkedWire a (IntMap (Maybe a))
networkedCopiesServer = NCW . peers
  where
    -- TODO: No guarantee that players appear to connect in the same order!
    -- Packets sent from server might arrive OOO on the clients, meaning that
    -- they will generate different mappings for each player.
    peers :: Int -> RawNetworkedWire a (IntMap (Maybe a))
    peers wireID = mkGen_ $ \_ -> do
      -- Get all packets with this wire ID
      pkts <- filterForWireID wireID .  packetsIn <$> get
      players <- connectedClients <$> get
      results <- forM (Map.elems players) $ \pid ->
        case (sortBy (compare `on` fst)) <$> (IMap.lookup pid pkts) of
          Nothing -> return (pid, Nothing)
          Just [] -> return (pid, Nothing)
          Just ((_, wp):_) -> do
            modify $ \s -> s { packetsOutServer =
                                  IMap.adjust (wp:) pid (packetsOutServer s) }
            return (pid, Just $ decode (BSL.fromStrict $ wpPayload wp))

      -- We need to reset the packets from the input state here
      forM_ (fst <$> filter (isJust . snd) results) $ \pid ->
        modify $ \s -> s { packetsIn = IMap.adjust tail pid (packetsIn s) }
      return . Right . IMap.fromList $ results

networkedCopies :: Binary a => NetworkedWire a (IntMap (Maybe a))
networkedCopies = networkWireFrom registerWire $ \r -> case r of
  Left ids -> networkedCopiesClient ids
  Right wid -> networkedCopiesServer wid
  where
    registerWire :: NetworkContext (Either (Int, Int) Int)
    registerWire = do
      st <- get
      let wireID = nextWireID st
      put $ st { nextWireID = wireID + 1 }
      return $ case st of
        ClientNetworkState{} -> Left (localClientID st, wireID)
        _                    -> Right wireID

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
    addClient = do
      networkIO $ putStrLn "Connected to server!"
      modify $ \s -> s { localClientID = clientID }

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
                     IMap.map (sortBy (compare `on` fst)) $
                     IMap.insertWith (++) cid ((seqNo',) <$>  bs) (packetsIn s) }
            return True
          Just (Packet'ConnectionDisconnect cid)
            | cid == clientID -> return False
            | otherwise -> do
              modify $ \s -> s { packetsIn = IMap.delete cid (packetsIn s) }
              return True

          -- Ignore all other packets
          _ -> return True

      -- Run the wire
      (res, w') <- stepWire w dt (Right x)

      -- If we've been disconnected, switch to the empty wire
      if not c then return (res, mkEmpty) else do
        -- If we're still connected, Send out all of the outgoing data as a
        -- single packet
        outPackets <- packetsOutClient <$> get
        _ <- networkIO $ sendPayload seqNo clientID outPackets sock sa

        -- Reset all of the outgoing packets
        modify $ \s -> s { packetsOutClient = [] }

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

runServerWire :: Int -> RawNetworkedWire a a -> GameWire a (Maybe a)
runServerWire numPlayers initW =
  wireFrom mkNetworkState $ withNetworkState (NCW $ runW 0 initW)
  where
    mkNetworkState = do
      sock <- GameMonad $ liftIO $ do
        putStrLn "Creating server on port 18152."
        createUDPSocket 18152
      return $ ServerNetworkState
               { localSocket = sock
               , nextWireID = 0
               , connectedClients = Map.empty
               , packetsIn = IMap.empty
               , packetsOutServer = IMap.empty
               }

    handlePlayerData :: BS.ByteString -> Int -> SockAddr
                     -> NetworkContext ()
    handlePlayerData bytes pid addr =
      case (decodePkt (BSL.fromStrict bytes)) of
        -- Handle connection requests
        Just Packet'ConnectionRequest -> do
          sock <- localSocket <$> get
          _ <- networkIO $ sendAccepted pid sock addr
          return ()

        -- Handle incoming data
        Just (Packet'Payload cid seqNo pkt) ->
          if pid /= cid then return () else modify $ \s ->
          s { packetsIn = IMap.insertWith (++) pid ((seqNo,) <$> pkt) (packetsIn s) }

        -- Ignore everything else
        _ -> return ()

    handlePotentialNewPlayer :: BS.ByteString -> SockAddr -> NetworkContext ()
    handlePotentialNewPlayer bytes addr =
      case (decodePkt (BSL.fromStrict bytes)) of
        -- Only need to handle connection requests
        Just Packet'ConnectionRequest -> do
          st <- get
          let nextPlayerID = Map.size (connectedClients st)
          if nextPlayerID >= numPlayers
            then networkIO (sendConnDenied (localSocket st) addr) >> return ()
            else do
              networkIO $ putStrLn $
                "Received connection request -- assigning slot " ++ show nextPlayerID
              put $ st { connectedClients =
                            Map.insert addr nextPlayerID (connectedClients st) }
              _ <- networkIO $ sendAccepted nextPlayerID (localSocket st) addr
              return ()

        -- Otherwise just ignore the packet
        _ -> return ()

    runW :: Word64 -> RawNetworkedWire a a -> RawNetworkedWire a (Maybe a)
    runW seqNo w = mkGen $ \dt x -> do
      -- Receive data and handle it.
      -- TODO: If we haven't received data from a connected player for more
      -- than five seconds we should consider them disconnected.
      st <- get
      -- TODO: We only dequeue from one source per frame -- may receive many
      -- more packets than that.
      (bytes, pktAddr) <- networkIO $ recvFrom (localSocket st) kMaxPayloadSize
      case (Map.lookup pktAddr $ connectedClients st) of
        Just pid -> handlePlayerData bytes pid pktAddr
        Nothing -> handlePotentialNewPlayer bytes pktAddr

      (res, w') <- stepWire w dt (Right x)

      -- Send data produced by stepWire
      packetsOut <- packetsOutServer <$> get
      clients <- connectedClients <$> get
      forM_ (Map.toList clients) $ \(addr, pid) ->
        case IMap.lookup pid packetsOut of
          Nothing -> return ()
          Just pkts -> do
            _ <- networkIO $ sendPayload seqNo pid pkts (localSocket st) addr
            return ()

      -- TODO: We terminate when everyone disconnects -- need to identify that
      -- case?
      return $ (Just <$> res, runW (seqNo + 1) w')

runServer :: Int -> a -> NetworkedWire a a -> IO ()
runServer numPlayers initVal (NCW w) = do
  (config, unloadSprite) <- mkLoopConfig nullRenderer Nothing
  let game = Game (mk2DCam 0 0 . pure (V2 0.0 0.0)) []
           $ CW (runServerWire numPlayers w)
      st = mkLoopState initVal game
  runGameLoop st config
  unloadSprite

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
      sock <- GameMonad $ liftIO $ do
        putStrLn $
          "Connecting to server at address " ++ show addr ++ " on port 18152"
        createUDPSocket 21518
      return $ ClientNetworkState
               { localSocket = sock
               , localClientID = (-1)
               , nextWireID = 0
               , serverAddr = SockAddrInet 18152 $ tupleToHostAddress addr
               , packetsIn = IMap.empty
               , packetsOutClient = []
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
        then do
          networkIO $ putStrLn "Timed out connecting to server!"
          returnConn (ConnectionState'Failure ConnectionFailure'Timeout)
        else do
          -- Send connecting packet
          networkIO $ putStrLn "Sending connection request"
          sock <- localSocket <$> get
          sa <- serverAddr <$> get
          _ <- networkIO $ sendConnRequest sock sa
          networkIO $ putStrLn "Connection request sent"

          -- Receive incoming packet and see what's up
          networkIO $ putStrLn "Checking received packets"
          (bytes, pktAddr) <- networkIO $ recvFrom sock kMaxPayloadSize
          networkIO $ putStrLn $
            "Received packet with " ++ show (BS.length bytes) ++ " bytes"
          if (pktAddr /= sa || BS.length bytes == 0)
            then returnConn ConnectionState'Connecting
            else case (decodePkt (BSL.fromStrict bytes)) of
              Just (Packet'ConnectionAccepted clientID) -> do
                networkIO $ putStrLn $
                  "Received connection accept! ClientID: " ++ show clientID
                returnConn $ ConnectionState'Connected clientID
              Just (Packet'ConnectionDenied) -> do
                networkIO $ putStrLn "Received connection denied!"
                returnConn $ ConnectionState'Failure ConnectionFailure'Refused

              -- Otherwise just ignore the packet
              _ -> returnConn ConnectionState'Connecting

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
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Wire

import Data.Array.MArray
import Data.Binary hiding (get, put)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict   as IMap
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Profunctor
import Data.Time

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

import System.Console.Concurrent
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

data NetworkState
  = ClientNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets

      -- Client specific
    , localClientID :: ClientIDVar
    , serverAddr :: SockAddr
    , packetsOutClient :: [WirePacket]
    }
  | ServerNetworkState
    { localSocket :: Socket
    , nextWireID :: Int
    , packetsIn :: IncomingPackets

      -- Server specific
    , connectedClients :: TArray Int (Maybe SockAddr)
    , packetsOutServer :: IntMap [WirePacket]
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

netStrLn :: String -> IO ()
netStrLn s = outputConcurrent $ s ++ ['\n']

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

networkedCopiesPeers :: forall a b
                      . (Show a, Binary a)
                     => Int -> RawNetworkedWire b (IntMap (Maybe a))
networkedCopiesPeers wireID = mkGen_ $ \_ -> do
  -- Find all packets for connected players that correspond to this wireID
  -- and dequeue the first packet in the list
  -- TODO: We need to create some sort of synchronization here in order to make
  -- sure that the time domains of the various simulations match up. Otherwise
  -- we're asking for trouble.
  packetsInArray <- packetsIn <$> get
  playerData <- networkIO $ atomically $ do
    (playerMin, playerMax) <- getBounds packetsInArray
    forM [playerMin..playerMax] $ \pid -> do
      pdat <- readArray packetsInArray pid
      case IMap.lookup wireID pdat of
        Nothing -> return (pid, Nothing)
        Just [] -> return (pid, Nothing)
        Just (x:xs) -> do
          writeArray packetsInArray pid $ IMap.insert wireID xs pdat
          return (pid, Just x)

  let mkResult :: (Word64, BS.ByteString) -> a
      mkResult = decode . BSL.fromStrict . snd

      result = IMap.fromList $ map (second $ fmap mkResult) playerData

  return $ Right result

networkedCopiesClient :: forall a
                       . (Show a, Binary a)
                      => Int
                      -> NetworkedWire a (IntMap (Maybe a))
networkedCopiesClient wireID =
  NCW (swapFirstPlayer . (client &&& networkedCopiesPeers wireID))
  where
    swapFirstPlayer :: RawNetworkedWire (Maybe a, IntMap (Maybe a))
                                        (IntMap (Maybe a))
    swapFirstPlayer = mkGen_ $ \(x, m) -> do
      clientID <- localClientID <$> get >>= networkIO . atomically . readTVar
      case clientID of
        Nothing -> error "Client still connecting??"
        Just (Left _) -> error "Client never connected??"
        Just (Right cid) -> do
          let swapKeys k
                | k == cid = 0
                | k == 0 = cid
                | otherwise = k
          return . Right $ IMap.mapKeys swapKeys $ IMap.insert cid x m

    -- Just send packets and hope other side receives them...
    client :: RawNetworkedWire a (Maybe a)
    client = mkGen_ $ \x -> do
      -- TODO: Use clientID to actually make sure that we're receiving packets
      -- in order as we expect
      let dat = BSL.toStrict $ encode x
      modify $ \s -> s { packetsOutClient =
                            WirePacket dat wireID : (packetsOutClient s) }
      return $ Right (Just x)

networkedCopiesServer :: forall a
                       . (Show a, Binary a)
                      => Int -> NetworkedWire a (IntMap (Maybe a))
networkedCopiesServer wireID = NCW $ sendAllPackets . networkedCopiesPeers wireID
  where
    sendAllPackets :: RawNetworkedWire (IntMap (Maybe a)) (IntMap (Maybe a))
    sendAllPackets = mkGen_ $ \m -> do
      forM_ (IMap.toList m) $ \(pid, x) ->
        case x of
          Nothing -> return ()
          Just dat -> 
            let pkt = WirePacket (BSL.toStrict $ encode dat) wireID
            in do
              modify' $ \s -> s {
                packetsOutServer =
                   IMap.insertWith (++) pid [pkt] (packetsOutServer s) }
      return (Right m)

networkedCopies :: (Binary a, Show a) => NetworkedWire a (IntMap (Maybe a))
networkedCopies = networkWireFrom registerWire $ \r -> case r of
  Left wid -> networkedCopiesClient wid
  Right wid -> networkedCopiesServer wid
  where
    registerWire :: NetworkContext (Either Int Int)
    registerWire = do
      st <- get
      let wireID = nextWireID st
      put $ st { nextWireID = wireID + 1 }
      return $ case st of
        ClientNetworkState{} -> Left wireID
        _                    -> Right wireID

{--
-- | An authority is a wire that's sync'd to the server. It will predict locally
-- what the correct result is, but will always confirm the result with the
-- server and not drop any packets.
authority :: GameWire a b -> NetworkedWire a b
authority w = undefined
--}

withinNetwork :: ContWire a b -> NetworkedWire a b
withinNetwork = NCW . mapWire (\m -> StateT $ \x -> (,x) <$> m) . getContinuousWire

withNetworkState :: NetworkedWire a b -> NetworkState -> GameWire a b
withNetworkState (NCW w) s = mkGen $ \dt x -> do
  ((res, w'), s') <- runStateT (stepWire w dt (Right x)) s
  return (res, withNetworkState (NCW w') s')

--------------------------------------------------------------------------------
-- Client

data ConnectionState
  = ConnectionState'Failure ConnectionFailure
  | ConnectionState'Connecting
  | ConnectionState'Connected

-- Measured in seconds
kTimeout :: Integer
kTimeout = 5

clientReceiveLoop :: ReaderT NetworkState IO ()
clientReceiveLoop = startConnThread >>= (withConcurrentOutput . connectClient)
  where
    sendConnPkts :: UTCTime -> NetworkState -> IO ()
    sendConnPkts startTime st = do
      -- Did we timeout?
      t <- getCurrentTime
      if diffUTCTime t startTime > fromInteger kTimeout
        then atomically $
             writeTVar (localClientID st) (Just $ Left ConnectionFailure'Timeout)
        else do
          -- Send connecting packet
          _ <- sendConnRequest (localSocket st) (serverAddr st)
          threadDelay 33333  -- approx 30 pkts/s
          sendConnPkts startTime st

    startConnThread :: ReaderT NetworkState IO ThreadId
    startConnThread = do
      st <- ask
      liftIO $ do
        startTime <- getCurrentTime
        forkIO $ sendConnPkts startTime st

    connectClient :: ThreadId -> ReaderT NetworkState IO ()
    connectClient connectingThread = do
      st <- ask

      -- Receive incoming packet and see what's up
      liftIO $ do
        (bytes, pktAddr) <- recvFrom (localSocket st) kMaxPayloadSize
        if (pktAddr /= serverAddr st)
          then runReaderT (connectClient connectingThread) st
          else case (decodePkt (BSL.fromStrict bytes)) of
            Just (Packet'ConnectionAccepted clientID) -> do
              netStrLn $ "Received connection accept! ClientID: " ++ show clientID
              killThread connectingThread
              atomically $ writeTVar (localClientID st) (Just $ Right clientID)
              runReaderT clientConnectedLoop st
            Just Packet'ConnectionDenied -> do
              netStrLn "Received connection denied!"
              killThread connectingThread
              atomically $ writeTVar (localClientID st) $
                Just $ Left ConnectionFailure'Refused

            -- Otherwise just ignore the packet
            _ -> runReaderT (connectClient connectingThread) st

    -- Grab all of the existing data from the server and place it in the
    -- corresponding list
    clientConnectedLoop :: ReaderT NetworkState IO ()
    clientConnectedLoop = do
      st <- ask
      clientID <- liftIO $ atomically $ readTVar (localClientID st)
      (bytes, pktAddr) <- liftIO $ recvFrom (localSocket st) kMaxPayloadSize
      c <- if (pktAddr /= serverAddr st)
           then return True
           else do
             case decodePkt (BSL.fromStrict bytes) of
               Just (Packet'Payload cid seqNo wps) -> do
                 liftIO $ atomically $ forM_ wps $ \wp -> do
                   pkts <- readArray (packetsIn st) cid
                   let newPkt = (seqNo, wpPayload wp)
                   writeArray (packetsIn st) cid $
                     IMap.insertWith (++) (wpNetworkID wp) [newPkt] pkts
                 return True
               Just (Packet'ConnectionDisconnect cid)
                 | Just (Right cid) == clientID -> return False
                 | otherwise -> do
                   liftIO $ atomically $ do
                     writeArray (packetsIn st) cid IMap.empty
                   return True

               -- Ignore all other packets
               _ -> return True

      if not c
        then liftIO $ atomically $ writeTVar (localClientID st) Nothing
        else clientConnectedLoop

connectedClient :: ThreadId -> NetworkedWire a a -> NetworkedWire a a
connectedClient tid (NCW _w) = NCW $ clientW 0 _w
  where
    clientW :: Word64 -> RawNetworkedWire a a -> RawNetworkedWire a a
    clientW seqNo w = mkGen $ \dt x -> do
      -- Run the wire
      (res, w') <- stepWire w dt (Right x)

      -- If we've been disconnected, switch to the empty wire
      st <- get
      clientID <- networkIO $ atomically $ readTVar (localClientID st)
      case clientID of
        -- Disconnected?
        Nothing -> do
          networkIO $ killThread tid
          return (res, mkEmpty)

        -- Connection failure of some sort?
        Just (Left _) -> do
          networkIO $ killThread tid
          return (res, mkEmpty)

        -- If we're still connected, Send out all of the outgoing data as a
        -- single packet
        Just (Right cid) -> do
          outPackets <- packetsOutClient <$> get
          let sock = localSocket st
              sa = serverAddr st
          _ <- networkIO $ sendPayload seqNo cid outPackets sock sa

          -- Reset all of the outgoing packets
          modify $ \s -> s { packetsOutClient = [] }

          return (res, clientW (seqNo + 1) w')

runClientWire :: forall a
               . (Word8, Word8, Word8, Word8)
              -- ^ Address to connect to
              -> Word16
              -- ^ Port to connect to
              -> Int
              -- ^ Num Players
              -> ContWire a a
              -- ^ GameWire to run while connecting
              -> (ConnectionFailure -> ContWire a a)
              -- ^ GameWire to switch to if the connection failed
              -> NetworkedWire a a
              -- ^ Wire to run once connected
              -> ContWire a a
runClientWire addr port numPlayers whileConnecting onFailure (NCW client) =
  CW $ wireFrom mkNetworkState $ \(st, tid) ->
    flip withNetworkState st
    $ (NCW $ switch $ second (mkResult tid) . connectServer wcn)
  where
    mkNetworkState = GameMonad . liftIO $ do
      sock <- do
        netStrLn $ concat ["Connecting via port "
                          , show port
                          , " to server at address "
                          , show addr
                          , " on port 18152"
                          ]

        createUDPSocket port
      cidVar <- newTVarIO Nothing
      pktsInVar <- atomically $ newArray (0, numPlayers - 1) IMap.empty

      let st = ClientNetworkState
               { localSocket = sock
               , nextWireID = 0
               , packetsIn = pktsInVar
               , localClientID = cidVar
               , serverAddr = SockAddrInet 18152 $ tupleToHostAddress addr
               , packetsOutClient = []
               }

      -- Start the connecting loop
      tid <- forkIO $ runReaderT clientReceiveLoop st
      return (st, tid)

    wcn :: RawNetworkedWire a a
    wcn = getNetworkedWire $ withinNetwork whileConnecting

    mkResult :: ThreadId
             -> RawNetworkedWire ConnectionState (Event (RawNetworkedWire a a))
    mkResult tid = fmap (getNetworkedWire . toResultW tid) <$> became connectionResult

    toResultW :: ThreadId -> ConnectionState -> NetworkedWire a a
    toResultW _ ConnectionState'Connecting = error "Still connecting..."
    toResultW _ (ConnectionState'Failure f) = withinNetwork $ onFailure f
    toResultW tid ConnectionState'Connected = connectedClient tid (NCW client)

    connectionResult :: ConnectionState -> Bool
    connectionResult ConnectionState'Connecting = False
    connectionResult _ = True

    connectServer :: RawNetworkedWire a a
                  -> RawNetworkedWire a (a, ConnectionState)
    connectServer w = mkGen $ \dt x -> do
      (result, w') <- stepWire w dt (Right x)
      let returnConn c = return ((,c) <$> result, connectServer w')
      st <- get
      cid <- networkIO $ atomically $ readTVar (localClientID st)
      case cid of
        Nothing -> returnConn ConnectionState'Connecting
        Just (Left f) -> returnConn $ ConnectionState'Failure f
        Just (Right _) -> returnConn ConnectionState'Connected

--------------------------------------------------------------------------------
-- Server

serverReceiveLoop :: ReaderT NetworkState IO ()
serverReceiveLoop = do
  -- Receive data and handle it.
  -- TODO: If we haven't received data from a connected player for more
  -- than five seconds we should consider them disconnected.

  -- TODO: We only dequeue from one source per frame -- may receive many
  -- more packets than that.
  st <- ask
  (bytes, pktAddr) <- liftIO $ recvFrom (localSocket st) kMaxPayloadSize

  -- Have we connected to this address?
  -- TODO: This doesn't work in general, multiple clients may be behind NAT
  mpid <- liftIO $ atomically $ do
    let clients = connectedClients st
    (playerMin, playerMax) <- getBounds clients
    fmap catMaybes $ forM [playerMin..playerMax] $ \pid -> do
      addr <- readArray clients pid
      return $ do
        -- Monad is: Maybe
        x <- addr
        guard $ x == pktAddr
        return pid

  case mpid of
    [] -> handlePotentialNewPlayer bytes pktAddr
    [pid] -> handlePlayerData bytes pid pktAddr
    _ -> error "Multiple of same addr connected!"
  serverReceiveLoop

  where
    handlePlayerData :: BS.ByteString -> Int -> SockAddr
                     -> ReaderT NetworkState IO ()
    handlePlayerData bytes pid addr =
      case (decodePkt (BSL.fromStrict bytes)) of
        -- Handle connection requests
        Just Packet'ConnectionRequest -> do
          sock <- localSocket <$> ask
          _ <- liftIO $ sendAccepted pid sock addr
          return ()

        -- Handle incoming data
        Just (Packet'Payload cid seqNo pkts) -> do
          pktsIn <- packetsIn <$> ask
          liftIO $ atomically $ do
            (pMin, pMax) <- getBounds pktsIn
            let within a b x = x >= a && x <= b
            if not (within pMin pMax cid)
              then return ()  -- Ignore out of bounds
              else do
                wireData <- readArray pktsIn cid
                let getPkt :: WirePacket -> (Int, [(Word64, BS.ByteString)])
                    getPkt wp = (wpNetworkID wp, [(seqNo, wpPayload wp)])

                    newData = IMap.fromList $ map getPkt pkts
                writeArray pktsIn cid $ IMap.map (sortBy (compare `on` fst))
                                      $ IMap.unionWith (++) wireData newData

        -- Ignore everything else
        _ -> return ()

    findOpenSlot :: ReaderT NetworkState IO (Maybe Int)
    findOpenSlot = do
      st <- ask
      liftIO $ atomically $ do
        let clients = connectedClients st
        (playerMin, playerMax) <- getBounds clients
        slots <- fmap catMaybes $ forM [playerMin..playerMax] $ \pid -> do
          addr <- readArray clients pid
          case addr of
            Nothing -> return $ Just pid
            Just _ -> return Nothing

        case slots of
          [] -> return Nothing
          (s:_) -> return (Just s)

    handlePotentialNewPlayer :: BS.ByteString -> SockAddr
                             -> ReaderT NetworkState IO ()
    handlePotentialNewPlayer bytes addr =
      case (decodePkt (BSL.fromStrict bytes)) of
        -- Only need to handle connection requests
        Just Packet'ConnectionRequest -> do
          st <- ask
          openSlot <- findOpenSlot
          liftIO $ case openSlot of
            Just x -> do
              netStrLn $ "Received connection request -- assigning slot " ++ show x
              atomically $ writeArray (connectedClients st) x (Just addr)
              _ <- sendAccepted x (localSocket st) addr
              return ()
            Nothing -> sendConnDenied (localSocket st) addr >> return ()

        -- Otherwise just ignore the packet
        _ -> return ()

runServerWire :: Int -> RawNetworkedWire a a -> GameWire a (Maybe a)
runServerWire numPlayers initW =
  wireFrom mkNetworkState $ \(tid, st) ->
    withNetworkState (NCW $ runW 0 tid initW) st
  where
    mkNetworkState = GameMonad . liftIO $ do
      netStrLn "Creating server on port 18152."
      sock <- createUDPSocket 18152

      pktsInArr <- atomically $ newArray (0, numPlayers - 1) IMap.empty
      clients <- atomically $ newArray (0, numPlayers - 1) Nothing

      let st = ServerNetworkState
               { localSocket = sock
               , nextWireID = 0
               , packetsIn = pktsInArr
               , connectedClients = clients
               , packetsOutServer = IMap.empty
               }

      tid <- forkIO $ runReaderT serverReceiveLoop st
      return (tid, st)

    runW :: Word64 -> ThreadId -> RawNetworkedWire a a -> RawNetworkedWire a (Maybe a)
    runW seqNo tid w = mkGen $ \dt x -> do
      (res, w') <- stepWire w dt (Right x)

      -- Send data produced by stepWire
      st <- get
      clients <- networkIO $ atomically $ do
        let clients = connectedClients st
        (playerMin, playerMax) <- getBounds clients
        map fromJust . filter isJust <$>
          forM [playerMin..playerMax] (readArray clients)

      forM_ clients $ \addr ->
        forM_ (IMap.toList $ packetsOutServer st) $ \(pid, pkts) -> do
          networkIO (sendPayload seqNo pid pkts (localSocket st) addr)
            >> return ()
      modify' $ \s -> s { packetsOutServer = IMap.empty }

      -- TODO: We terminate when everyone disconnects -- need to identify that
      -- case?
      case res of
        Left e -> do
          networkIO $ killThread tid
          return (Left e, runW (seqNo + 1) tid w')
        Right _ -> return $ (Just <$> res, runW (seqNo + 1) tid w')

runServer :: Int -> a -> NetworkedWire a a -> IO ()
runServer numPlayers initVal (NCW w) = withConcurrentOutput $ do
  (config, unloadSprite) <- mkLoopConfig nullRenderer Nothing
  let game = Game (mk2DCam 0 0 . pure (V2 0.0 0.0)) []
           $ CW (runServerWire numPlayers w)
      st = mkLoopState initVal game
  runGameLoop st config
  unloadSprite

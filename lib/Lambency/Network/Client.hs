{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Lambency.Network.Client (runClientWire) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Wire hiding (when)

import Data.Array.MArray
import Data.Binary hiding (get, put)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString      as BS
import Data.Function (on)
import qualified Data.IntMap as IMap
import Data.List (sortBy, nubBy)
import Data.Time

import Lambency.Types
import Lambency.Network.Packet
import Lambency.Network.Types
import Lambency.Network.Utils
import Lambency.GameObject

import Network.Socket
import Network.Socket.ByteString

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

data ConnectionState s
  = ConnectionState'Failure ConnectionFailure
  | ConnectionState'Connecting
  | ConnectionState'Connected s

-- Measured in seconds
kTimeout :: Integer
kTimeout = 5

-- | Initiates a connection to the server, waits for an initial state to be
-- received, and then uses that initial state to start the network context wire.
clientReceiveLoop :: forall s . Binary s => ReaderT (NetworkState s) IO ()
clientReceiveLoop = startConnThread >>= connectClient
  where
    sendConnPkts :: UTCTime -> NetworkState s -> IO ()
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

    startConnThread :: ReaderT (NetworkState s) IO ThreadId
    startConnThread = do
      st <- ask
      liftIO $ do
        startTime <- getCurrentTime
        forkIO $ sendConnPkts startTime st

    receiveInitialState :: [(SequenceNumber, Int, Int, BS.ByteString)]
                        -> ReaderT (NetworkState s) IO ()
    receiveInitialState pktsSoFar = do
      st <- ask
      (bytes, pktAddr) <- liftIO $ recvFrom (localSocket st) kMaxPayloadSize
      let connectWithState gsBytes = do
            let gs = decode $ BSL.fromStrict gsBytes
            liftIO $ atomically $ writeTVar (clientGameState st) (Just gs)
            clientConnectedLoop
      if (pktAddr /= serverAddr st)
        then receiveInitialState pktsSoFar
        else case decodePkt bytes of
          Just (Packet'GameState seqNo pktIdx numPkts p)
            | numPkts == 1 -> connectWithState p
            | otherwise -> do
              let newpkt = (seqNo, pktIdx, numPkts, p)
                  getPktIdx (_, i, _, _) = i
               in case pktsSoFar of
                [] -> receiveInitialState [newpkt]
                (curSeqNo, _, curNumPkts, _):_
                  | seqNo > curSeqNo -> receiveInitialState [newpkt]
                  | seqNo < curSeqNo -> receiveInitialState pktsSoFar
                  | curNumPkts /= numPkts -> error "Same seq no, different num pkts??"
                  | otherwise ->
                    let pktList = sortBy (compare `on` getPktIdx)
                                $ nubBy ((==) `on` getPktIdx) (newpkt : pktsSoFar)
                        encGS = BS.concat [payload | (_, _, _, payload) <- pktList]
                    in if length pktList == numPkts
                       then connectWithState encGS
                       else receiveInitialState pktList

          -- Ignore everything else
          _ -> receiveInitialState pktsSoFar

    connectClient :: ThreadId -> ReaderT (NetworkState s) IO ()
    connectClient connectingThread = do
      st <- ask

      -- Receive incoming packet and see what's up
      liftIO $ do
        (bytes, pktAddr) <- recvFrom (localSocket st) kMaxPayloadSize
        if (pktAddr /= serverAddr st)
          then runReaderT (connectClient connectingThread) st
          else case decodePkt bytes of
            Just (Packet'ConnectionAccepted clientID) -> do
              netStrLn $ "Received connection accept! ClientID: " ++ show clientID
              killThread connectingThread
              atomically $ writeTVar (localClientID st) (Just $ Right clientID)
              runReaderT (receiveInitialState []) st
            Just Packet'ConnectionDenied -> do
              netStrLn "Received connection denied!"
              killThread connectingThread
              atomically $ writeTVar (localClientID st) $
                Just $ Left ConnectionFailure'Refused

            -- Otherwise just ignore the packet
            _ -> runReaderT (connectClient connectingThread) st

    -- Grab all of the existing data from the server and place it in the
    -- corresponding list
    clientConnectedLoop :: ReaderT (NetworkState s) IO ()
    clientConnectedLoop = do
      st <- ask
      (clientID, bytes, pktAddr) <- liftIO $ do
        cid <- atomically $ readTVar (localClientID st)
        (b, addr) <- recvFrom (localSocket st) kMaxPayloadSize
        return (cid, b, addr)
      c <- if (pktAddr /= serverAddr st)
           then return True
           else do
             case decodePkt bytes of
               Just (Packet'Payload wps) -> do
                 liftIO $ atomically $ forM_ wps $ \wp -> do
                   pkts <- readArray (packetsIn st) (wpPlayerID wp)
                   writeArray (packetsIn st) (wpPlayerID wp) $
                     IMap.insertWith (++) (wpNetworkID wp) [wp] pkts
                 return True
               Just (Packet'ConnectionDisconnect cid)
                 | Just (Right cid) == clientID -> return False
                 | otherwise -> liftIO . atomically $ do
                     writeArray (packetsIn st) cid IMap.empty
                     return True

               -- Ignore all other packets
               _ -> return True

      if c
        then clientConnectedLoop
        else liftIO $ atomically $ writeTVar (localClientID st) Nothing

connectedClient :: ThreadId -> NetworkedWire s a a
                -> NetworkedWire s (Bool, a) (Maybe a)
connectedClient tid (NCW _w) = NCW $ clientW 0 _w
  where
    clientW :: SequenceNumber -> RawNetworkedWire s a a
            -> RawNetworkedWire s (Bool, a) (Maybe a)
    clientW seqNo w = mkGen $ \dt (disc, x) -> do
      -- Run the wire
      (res, w') <- stepWire w dt (Right x)

      -- If we've been disconnected, switch to the empty wire
      st <- get
      clientID <- networkIO $ atomically $ readTVar (localClientID st)
      case clientID of
        -- Disconnected?
        Nothing -> do
          networkIO $ killThread tid
          return (Right Nothing, pure Nothing)

        -- Connection failure of some sort?
        Just (Left _) -> do
          networkIO $ killThread tid
          return (Right Nothing, pure Nothing)

        -- If we're still connected, Send out all of the outgoing data as a
        -- single packet
        Just (Right cid) -> do
          if disc
            then do
              -- Client chose to disconnect?
              networkIO $ do
                _ <- sendDisconnected cid (localSocket st) (serverAddr st)
                killThread tid
              return (Right Nothing, pure Nothing)

            else do
              -- We're still connected and we don't want to disconnect -- send
              -- all the queued packets!
              outPackets <- packetsOutClient <$> get
              let sock = localSocket st
                  sa = serverAddr st

                  fixPackets wp = wp { wpPlayerID = cid
                                     , wpSequenceNumber = seqNo
                                     }
              _ <- networkIO $ sendPayload (fixPackets <$> outPackets) sock sa

              -- Reset all of the outgoing packets
              modify $ \s -> s { packetsOutClient = [] }

              return (Just <$> res, clientW (seqNo + 1) w')

runClientWire :: forall a s
               . Binary s
              => (Word8, Word8, Word8, Word8)
              -- ^ Address to connect to
              -> Word16
              -- ^ Port to connect to
              -> Int
              -- ^ Num Players
              -> ContWire a a
              -- ^ Wire to run while connecting
              -> (ConnectionFailure -> ContWire (Bool, a) (Maybe a))
              -- ^ Wire to switch to if the connection failed
              -> (s -> NetworkedWire s a a)
              -- ^ Wire to run with initial state once connected
              -> ContWire (Bool, a) (Maybe a)
runClientWire addr port numPlayers whileConnecting onFailure mkClient =
  CW $ wireFrom mkNetworkState $ \(st, tid) ->
    flip withNetworkState st
    $ (NCW $ switch $ second (mkResult tid) . connectServer wcn)
  where
    mkNetworkState :: GameMonad (NetworkState s, ThreadId)
    mkNetworkState = GameMonad . liftIO $ do
      let serverPort :: PortNumber  -- TODO: Should be configurable from CLI
          serverPort = 18152
      netStrLn $ concat ["Connecting via port ", show port
                        , " to server at address "
                        , show addr, ":", show serverPort
                        ]
      sock <- createUDPSocket port
      cidVar <- newTVarIO Nothing
      pktsInVar <- atomically $ newArray (0, numPlayers - 1) IMap.empty
      pktsAcked <- newTVarIO $ AckState (0, AckMask 0)
      gstvar <- newTVarIO Nothing

      let st = ClientNetworkState
               { localSocket = sock
               , nextWireID = 0
               , packetsIn = pktsInVar
               , packetQueues = IMap.empty
               , serverPacketsAcked = pktsAcked
               , clientGameState = gstvar
               , localClientID = cidVar
               , serverAddr = SockAddrInet serverPort $ tupleToHostAddress addr
               , packetsOutClient = []
               }

      -- Start the connecting loop
      tid <- forkIO $ runReaderT clientReceiveLoop st
      return (st, tid)

    wcn :: RawNetworkedWire s a a
    wcn = getNetworkedWire $ withinNetwork whileConnecting

    mkResult tid =
      fmap (getNetworkedWire . toResultW tid) <$> became connectionResult

    toResultW :: ThreadId -> ConnectionState s -> NetworkedWire s (Bool, a) (Maybe a)
    toResultW _ ConnectionState'Connecting = error "Still connecting..."
    toResultW _ (ConnectionState'Failure f) = withinNetwork $ onFailure f
    toResultW tid (ConnectionState'Connected st) = connectedClient tid $ mkClient st

    connectionResult :: ConnectionState s -> Bool
    connectionResult ConnectionState'Connecting = False
    connectionResult _ = True

    connectServer :: RawNetworkedWire s a a
                  -> RawNetworkedWire s (Bool, a) (Maybe a, ConnectionState s)
    connectServer w = mkGen $ \dt (disc, x) -> do
      (result, w') <- stepWire w dt (Right x)
      let returnConn c =
            if disc
            then return (Right (Nothing, c), connectServer w')
            else return ((,c) . Just <$> result, connectServer w')
      st <- get
      cid <- networkIO $ atomically $ readTVar (localClientID st)
      case cid of
        Nothing -> returnConn ConnectionState'Connecting
        Just (Left f) -> returnConn $ ConnectionState'Failure f
        Just (Right pid) -> do
          gst <- networkIO $ atomically $ readTVar (clientGameState st)
          case gst of
            Nothing -> do
              let sock = localSocket st
                  sa = serverAddr st
              when disc $ networkIO (sendDisconnected pid sock sa) >> return ()
              returnConn ConnectionState'Connecting
            Just g -> returnConn $ ConnectionState'Connected g

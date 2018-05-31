{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network.Server (runServerWire) where

--------------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Wire hiding (when)

import Data.Array.MArray
import Data.Binary hiding (get, put)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import qualified Data.IntMap.Strict   as IMap
import Data.List (sortBy)
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.Time

import Lambency.GameObject
import Lambency.Network.Packet
import Lambency.Network.Types
import Lambency.Network.Utils
import Lambency.Types

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

serverReceiveLoop :: ReaderT (NetworkState s) IO ()
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
    disconnectPlayer :: Int -> ReaderT (NetworkState s) IO ()
    disconnectPlayer pid = do
      liftIO $ netStrLn $ "Disconnecting player: " ++ show pid
      -- Send a disconnect notification to all other connected clients
      clients <- connectedClients <$> ask
      addrs <- fmap (catMaybes . map snd . filter ((/= pid) . fst)) $
               liftIO $ atomically $ getAssocs clients
      sock <- localSocket <$> ask
      liftIO $ do
        forM_ addrs $ sendDisconnected pid sock

        -- Remove client from connected clients
        atomically $ writeArray clients pid Nothing

    handlePlayerData :: BS.ByteString -> Int -> SockAddr
                     -> ReaderT (NetworkState s) IO ()
    handlePlayerData bytes pid addr =
      case decodePkt bytes of
        -- Handle connection requests
        Just Packet'ConnectionRequest -> do
          sock <- localSocket <$> ask
          _ <- liftIO $ sendAccepted pid sock addr
          return ()

        -- Handle disconnects
        Just (Packet'ConnectionDisconnect cid) ->
          if pid == cid then disconnectPlayer cid else return ()

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
                let getPkt :: WirePacket -> (Int, [(SequenceNumber, BS.ByteString)])
                    getPkt wp = (wpNetworkID wp, [(seqNo, wpPayload wp)])

                    newData = IMap.fromList $ map getPkt pkts
                writeArray pktsIn cid $ IMap.map (sortBy (compare `on` fst))
                                      $ IMap.unionWith (++) wireData newData

        -- Ignore everything else
        _ -> return ()

    findOpenSlot :: ReaderT (NetworkState s) IO (Maybe Int)
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
                             -> ReaderT (NetworkState s) IO ()
    handlePotentialNewPlayer bytes addr =
      case decodePkt bytes of
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

runServerWire :: forall a s . Binary s
              => Int -> s -> RawNetworkedWire s a a
              -> GameWire a (Maybe a)
runServerWire numPlayers initGS initW =
  wireFrom mkNetworkState $ \(tid, st, t) ->
    withNetworkState (NCW $ runW t 0 tid initW) st
  where
    mkNetworkState = GameMonad . liftIO $ do
      netStrLn "Creating server on port 18152."
      sock <- createUDPSocket 18152

      pktsInArr <- atomically $ newArray (0, numPlayers - 1) IMap.empty
      pktsRecvdArr <- atomically $ newArray (0, numPlayers - 1) IMap.empty
      clients <- atomically $ newArray (0, numPlayers - 1) Nothing
      gstVar <- newTVarIO initGS

      let st = ServerNetworkState
               { localSocket = sock
               , nextWireID = 0
               , packetsIn = pktsInArr
               , packetsReceived = pktsRecvdArr
               , serverGameState = gstVar
               , connectedClients = clients
               , packetsOutServer = IMap.empty
               }

      tid <- forkIO $ runReaderT serverReceiveLoop st
      t <- getCurrentTime
      return (tid, st, t)

    runW :: UTCTime -> SequenceNumber -> ThreadId -> RawNetworkedWire s a a
         -> RawNetworkedWire s a (Maybe a)
    runW lastStateSent seqNo tid w = mkGen $ \dt x -> do
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

      -- Send the state every 2 seconds
      curT <- networkIO getCurrentTime
      let sendState = diffUTCTime curT lastStateSent > 2
      when sendState $ networkIO $ do
        gs <- BSL.toStrict . encode <$> atomically (readTVar $ serverGameState st)
        forM_ clients $ sendGameState seqNo gs (localSocket st)

      let nextW
            | sendState = runW curT (seqNo + 1) tid w'
            | otherwise = runW lastStateSent (seqNo + 1) tid w'

      -- TODO: We terminate when everyone disconnects -- need to identify that
      -- case?
      case res of
        Left e -> do
          networkIO $ killThread tid
          return (Left e, nextW)
        Right _ -> return $ (Just <$> res, nextW)

{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network.Copies (networkedCopies) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Wire hiding (when)

import Data.Array.MArray
import Data.Binary hiding (get, put)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString      as BS
import qualified Data.IntMap as IMap
import Data.IntMap (IntMap)

import Lambency.Network.Packet
import Lambency.Network.Types
import Lambency.Network.Utils

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

networkedCopiesPeers :: forall a b s
                      . Binary a
                     => Int -> RawNetworkedWire s b (IntMap (Maybe a))
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

networkedCopiesClient :: forall a s
                       . Binary a
                      => Int
                      -> NetworkedWire s a (IntMap (Maybe a))
networkedCopiesClient wireID =
  NCW (swapFirstPlayer . (client &&& networkedCopiesPeers wireID))
  where
    swapFirstPlayer :: RawNetworkedWire s (Maybe a, IntMap (Maybe a))
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
    client :: RawNetworkedWire s a (Maybe a)
    client = mkGen_ $ \x -> do
      -- TODO: Use clientID to actually make sure that we're receiving packets
      -- in order as we expect
      let dat = BSL.toStrict $ encode x
      modify' $ \s -> s {
        packetsOutClient = WirePacket dat wireID : (packetsOutClient s) }
      return $ Right (Just x)

networkedCopiesServer :: forall a s
                       . Binary a
                      => Int -> NetworkedWire s a (IntMap (Maybe a))
networkedCopiesServer wireID = NCW $ sendAllPackets . networkedCopiesPeers wireID
  where
    sendAllPackets :: RawNetworkedWire s (IntMap (Maybe a)) (IntMap (Maybe a))
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

networkedCopies :: Binary a => NetworkedWire s a (IntMap (Maybe a))
networkedCopies = networkWireFrom registerWire $ \r -> case r of
  Left wid -> networkedCopiesClient wid
  Right wid -> networkedCopiesServer wid
  where
    registerWire :: NetworkContext s (Either Int Int)
    registerWire = do
      st <- get
      let wireID = nextWireID st
      put $ st { nextWireID = wireID + 1 }
      return $ case st of
        ClientNetworkState{} -> Left wireID
        _                    -> Right wireID

{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Network.Copies (networkedCopies) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Wire hiding (when)

import Data.Array.MArray
import Data.Binary hiding (get, put)
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import qualified Data.IntMap as IMap
import Data.IntMap (IntMap)
import Data.List (sortBy)

import Lambency.Network.Packet
import Lambency.Network.Types
import Lambency.Network.Utils

import Prelude hiding ((.), id)
--------------------------------------------------------------------------------

acked :: AckState -> SequenceNumber -> Bool
acked (AckState (SequenceNumber seqNo, AckMask mask)) (SequenceNumber pkt)
  | pkt == seqNo = True
  | pkt > seqNo = False
  | seqNo - pkt > (maybe 64 fromIntegral $ bitSizeMaybe seqNo) = True
  | otherwise = testBit mask (fromIntegral $ seqNo - pkt - 1)

updateAckState :: AckState -> SequenceNumber -> AckState
updateAckState st@(AckState (SequenceNumber seqNo, AckMask mask))
               (SequenceNumber pkt)
  | pkt > seqNo = let s = fromIntegral $ pkt - seqNo
                      newMask = (mask `shift` s) `setBit` (s - 1)
                   in AckState (SequenceNumber pkt, AckMask newMask)
  | pkt == seqNo = st
  | otherwise = let s = fromIntegral $ seqNo - pkt
                    newMask = mask `setBit` (s - 1)
                 in AckState (SequenceNumber seqNo, AckMask newMask)

mostRecent :: AckState -> SequenceNumber -> Bool
mostRecent (AckState (s, _)) s' = s' > s

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
  packetsReceivedArray <- packetsReceived <$> get
  playerData <- networkIO $ atomically $ do
    (playerMin, playerMax) <- getBounds packetsInArray
    forM [playerMin..playerMax] $ \playerID -> do
      playerPackets <- readArray packetsInArray playerID
      case IMap.lookup wireID playerPackets of
        Nothing -> return (playerID, Nothing)
        Just wirePackets -> do
          -- TODO: Inspect packetsReceived to see what the latest status
          -- is for this wire, and update it accordingly (if the packet is
          -- required or not). We should only consume the packet if it is
          -- ready for consumption.
          ackStates <- readArray packetsReceivedArray playerID

          let sortedPackets = sortBy (compare `on` rwpSequenceNumber) wirePackets

              ackStateAlterFn seqNo Nothing = Just $ AckState (seqNo, AckMask 0)
              ackStateAlterFn seqNo (Just st) = Just $ updateAckState st seqNo

              ackPacket :: SequenceNumber -> STM ()
              ackPacket seqNo =
                writeArray packetsReceivedArray playerID $
                IMap.alter (ackStateAlterFn seqNo) wireID ackStates
          
              -- Returns Nothing if the packet has already been received and
              -- should be dropped. Otherwise, returns true if the packet is
              -- ready to be consumed, and marks the associated AckState as such
              acceptPacket rwp = let seqNo = rwpSequenceNumber rwp in
                case IMap.lookup wireID ackStates of
                  Nothing -> ackPacket seqNo >> return (Just True)
                  Just st
                    -- If we haven't received the previous required packet, then
                    -- we don't accept this packet yet.
                    | not (st `acked` (rwpPreviousRequired rwp)) ->
                        return (Just False)
                    -- If we already accepted this packet, then we don't accept
                    -- it again, and it should be dropped.
                    | st `acked` seqNo -> return Nothing
                    -- Don't accept this packet if it's not the most recent
                    -- packet, but at least mark it as seen.
                    | not (st `mostRecent` seqNo) ->
                        ackPacket seqNo >> return Nothing
                    | otherwise -> ackPacket seqNo >> return (Just True)

              -- If we've accepted a packet, then we want to drop it from the
              -- list of queue'd packets, and return it as the next packet
              -- accepted. If we haven't accepted it, then we need to decide
              -- what to do with it:
              --   * If it's required, keep it in the queue and try again.
              --   * If it's not required and it's the only packet, keep it in
              --     the queue and try again next time.
              --   * If it's not required and we have other packets pending,
              --     drop the packet from the queue.
              consumePackets :: [ReceivedWirePacket] -> STM (Int, Maybe a)
              consumePackets [] = return (playerID, Nothing)
              consumePackets (rwp:rwps) = do
                let dropPacket = writeArray packetsInArray playerID $
                                 IMap.insert wireID rwps playerPackets

                    wirePkt = decode $ BSL.fromStrict (rwpPayload rwp)
      
                accepted <- acceptPacket rwp
                case accepted of
                  Nothing -> dropPacket >> return (playerID, Nothing)
                  Just True -> dropPacket >> return (playerID, Just wirePkt)
                  Just False -> case rwps of
                    [] -> return (playerID, Nothing)
                    _ | rwpRequired rwp -> return (playerID, Nothing)
                      | otherwise -> dropPacket >> consumePackets rwps

          consumePackets sortedPackets

  return . Right $ IMap.fromList playerData

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
          wp = WirePacket dat wireID False 0
      modify' $ \s -> s { packetsOutClient = wp : packetsOutClient s }
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
            let pkt = WirePacket (BSL.toStrict $ encode dat) wireID False 0
            in modify' $ \s -> s {
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

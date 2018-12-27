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

addPacketToQueues :: WirePacket -> NetworkContext s ()
addPacketToQueues wp = modify $ \st -> st
  { packetQueues =
       foldr ($) (packetQueues st) (IMap.update addWP <$> wpDestinationQueues wp)
  }
  where
    addWP = Just . sortBy (compare `on` wpSequenceNumber) . (wp:)

-- The result type for whether or not we accept a packet with respect to a
-- packet queue. Is Nothing if the packet has already been received and
-- should be dropped. Otherwise, is the list of queues for which the packet
-- is ready to be consumed.
canAccept :: AckState -> SequenceNumber -> Maybe Bool
canAccept ackState seqNo
  -- If we already accepted this packet, then we don't accept
  -- it again, and it should be dropped.
  | ackState `acked` seqNo = Nothing
  -- Don't accept this packet if it's not the most recent packet.
  | not (ackState `mostRecent` seqNo) = Just False
  | otherwise = Just True

-- The return type for a wire packet handler has two parts:
--   1. For the given player, did we have a wire packet ready to use here?
--   2. A list of finalizers to apply once we've collected the updates for
--      each player.
type WirePacketHandlerResult s a = ([a], NetworkContext s ())
type WirePacketHandler s a
   = Int  -- player ID
  -> [WirePacket]
  -> STM (WirePacketHandlerResult s a)

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray array ix f = readArray array ix >>= writeArray array ix . f

getServerPacketHandler :: forall s a . Binary a
                       => NetworkContext s (WirePacketHandler s a)
getServerPacketHandler = do
  packetsAcked <- clientPacketsAcked <$> get
  return $ \playerID ->
    let -- If we've accepted a packet, then we want to drop it from the
        -- list of queue'd packets, and return it as the next packet
        -- accepted. If we haven't accepted it, then we need to decide
        -- what to do with it:
        --   * If it's required, keep it in the queue and try again.
        --   * If it's not required and it's the only packet, keep it in
        --     the queue and try again next time.
        --   * If it's not required and we have other packets pending,
        --     drop the packet from the queue.
        consumePackets :: [WirePacket] -> STM ([a], NetworkContext s ())
        consumePackets = liftM (first reverse) . foldM addPkt ([], return ())

        addPkt :: ([a], NetworkContext s ()) -> WirePacket
               -> STM ([a], NetworkContext s ())
        addPkt (pkts, finalizers) pkt = do
          let wirePkt = decode $ BSL.fromStrict (wpPayload pkt)
          ackedPlayer <- readArray packetsAcked playerID
          case canAccept ackedPlayer (wpLocalSequenceNumber pkt) of
            Just True ->
              let finalizer = do
                    addPacketToQueues pkt
                    networkIO $ atomically $
                      modifyArray packetsAcked playerID $
                      flip updateAckState (wpLocalSequenceNumber pkt)
              in return (wirePkt : pkts, finalizers >> finalizer)
            _ -> return (pkts, finalizers)

      in consumePackets . sortBy (compare `on` wpLocalSequenceNumber)

getClientPacketHandler :: forall s a . Binary a
                       => NetworkContext s (WirePacketHandler s a)
getClientPacketHandler = do
  packetsAcked <- serverPacketsAcked <$> get

  return $ \_ ->
    let -- If we've accepted a packet, then we want to drop it from the
        -- list of queue'd packets, and return it as the next packet
        -- accepted. If we haven't accepted it, then we need to decide
        -- what to do with it:
        --   * If it's required, keep it in the queue and try again.
        --   * If it's not required and it's the only packet, keep it in
        --     the queue and try again next time.
        --   * If it's not required and we have other packets pending,
        --     drop the packet from the queue.
        consumePackets :: [WirePacket] -> STM ([a], NetworkContext s ())
        consumePackets = liftM (first reverse) . foldM addPkt ([], return ())

        addPkt :: ([a], NetworkContext s ()) -> WirePacket
               -> STM ([a], NetworkContext s ())
        addPkt (pkts, finalizers) pkt = do
          let wirePkt = decode $ BSL.fromStrict (wpPayload pkt)
          ackedPlayer <- readTVar packetsAcked
          case canAccept ackedPlayer (wpSequenceNumber pkt) of
            Just True ->
              let finalizer = do
                    addPacketToQueues pkt
                    networkIO $ atomically $
                      writeTVar packetsAcked $
                      updateAckState ackedPlayer (wpSequenceNumber pkt)
              in return (wirePkt : pkts, finalizers >> finalizer)
            _ -> return (pkts, finalizers)

      in consumePackets . sortBy (compare `on` wpSequenceNumber)

networkedCopiesPeers :: forall a b s
                      . Binary a
                     => WirePacketHandler s a
                     -> Int
                     -> RawNetworkedWire s b (IntMap [a])
networkedCopiesPeers handler wireID = mkGen_ $ \_ -> do
  -- Find all packets for connected players that correspond to this wireID
  -- and dequeue the first packet in the list
  -- TODO: We need to create some sort of synchronization here in order to make
  -- sure that the time domains of the various simulations match up. Otherwise
  -- we're asking for trouble.
  packetsInArray <- packetsIn <$> get
  recvd <- networkIO $ atomically $ do
    (playerMin, playerMax) <- getBounds packetsInArray
    forM [playerMin..playerMax] $ \playerID -> do
      playerPackets <- readArray packetsInArray playerID
      case IMap.lookup wireID playerPackets of
        Nothing -> return ((playerID, []), return ())
        Just wirePackets -> do
          (result, finalizer) <- handler playerID wirePackets
          return ((playerID, result), finalizer)

  -- Update packet queues with received packets.
  sequence_ $ map snd recvd
  return . Right . IMap.fromList $ map fst recvd

networkedCopiesClient :: forall a s
                       . Binary a
                      => Int
                      -> NetworkedWire s a (IntMap [a])
networkedCopiesClient wireID =
  networkWireFrom getClientPacketHandler $ \h ->
  NCW (swapFirstPlayer . (client &&& networkedCopiesPeers h wireID))
  where
    getClientID = do
      clientID <- localClientID <$> get >>= networkIO . atomically . readTVar
      case clientID of
        Nothing -> error "Client still connecting??"
        Just (Left _) -> error "Client never connected??"
        Just (Right cid) -> return cid

    swapFirstPlayer :: RawNetworkedWire s ([a], IntMap [a]) (IntMap [a])
    swapFirstPlayer = mkGen_ $ \(x, m) -> do
      cid <- getClientID
      let swapKeys k
            | k == cid = 0
            | k == 0 = cid
            | otherwise = k
      return . Right $ IMap.mapKeys swapKeys $ IMap.insert cid x m

    -- Just send packets and hope other side receives them...
    client :: RawNetworkedWire s a [a]
    client = mkGen_ $ \x -> do
      -- TODO: Use clientID to actually make sure that we're receiving packets
      -- in order as we expect
      cid <- getClientID
      let dat = BSL.toStrict $ encode x
          wp = WirePacket dat cid wireID 0 0 []
      modify' $ \s -> s { packetsOutClient = wp : packetsOutClient s }
      return $ Right [x]

networkedCopiesServer :: forall a s
                       . Binary a
                      => Int -> NetworkedWire s a (IntMap [a])
networkedCopiesServer wireID = networkWireFrom getServerPacketHandler $ \h ->
  NCW $ sendAllPackets . networkedCopiesPeers h wireID
  where
    sendAllPackets :: RawNetworkedWire s (IntMap [a]) (IntMap [a])
    sendAllPackets = mkGen_ $ \m -> do
      forM_ (IMap.toList m) $ \(pid, dats) ->
        forM_ dats $ \dat -> modify' $ \s -> s {
          packetsOutServer =
             let pkt = WirePacket (BSL.toStrict $ encode dat) pid wireID 0 0 []
             in IMap.insertWith (++) pid [pkt] (packetsOutServer s)
          }
      return (Right m)

networkedCopies :: Binary a => NetworkedWire s a (IntMap [a])
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

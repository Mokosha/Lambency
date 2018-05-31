{-# LANGUAGE DeriveGeneric #-}
module Lambency.Network.Packet (
    WirePacket(..)
  , Packet(..)
  , kMaxPayloadSize
  , sendConnRequest
  , sendAccepted
  , sendDisconnected
  , sendConnDenied
  , sendPayload
  , sendGameState
  , decodePkt
) where

--------------------------------------------------------------------------------
import Control.Monad (forM)

import Data.Binary hiding (get, put)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString      as BS

import Lambency.Network.Types

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString
--------------------------------------------------------------------------------

kMaxPayloadSize :: Integral a => a
kMaxPayloadSize = 1024

encPkt :: Packet -> BS.ByteString
encPkt pkt
  | (BS.length bs) > kMaxPayloadSize = error "Packet too big!"
  | otherwise = bs
  where bs = BSL.toStrict (encode pkt)

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

sendPayload :: SequenceNumber -> Int -> [WirePacket] -> Socket -> SockAddr -> IO Int
sendPayload pktNo cid dat sock =
  sendTo sock (encPkt $ Packet'Payload cid pktNo dat)

sendGameState :: SequenceNumber -> BS.ByteString -> Socket -> SockAddr -> IO Int
sendGameState pktNo st sock addr =
  let mkPkts bs
        | BS.length bs == 0 = []
        | otherwise =
          let (xs, ys) = BS.splitAt (kMaxPayloadSize - 32) bs
          in xs : mkPkts ys

      pkts = mkPkts st
      numPkts = length pkts
  in case pkts of
    [] -> sendTo sock (encPkt $ Packet'GameState pktNo 0 1 BS.empty) addr
    _ -> do
      szs <- forM (zip pkts [0..]) $ \(pkt, i) -> do
        sendTo sock (encPkt $ Packet'GameState pktNo i numPkts pkt) addr
      return $ sum szs

decodePkt :: BS.ByteString -> Maybe Packet
decodePkt bytes = case decodeOrFail (BSL.fromStrict bytes) of
  Right (_, _, x) -> Just x
  _ -> Nothing

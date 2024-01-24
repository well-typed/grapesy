{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.StreamingType (tests) where

import Codec.Serialise qualified as Cbor
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Default
import Data.IORef
import Data.List
import Data.Proxy
import Data.Void
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Common.StreamType qualified as StreamType
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType qualified as Server

import Test.Driver.ClientServer

tests :: TestTree
tests =
    testGroup "Test.Sanity.StreamingType" [
        testCaseInfo "calculator" $
          test_calculator_cbor def
      ]

-- | Calculator gRPC client\/server example.
--
-- Uses CBOR as the data format, and uses all stream types.
data CalcRpc (serv :: Symbol) (meth :: Symbol)

-- | Non-streaming sum of a list of numbers
type SumQuickRpc = CalcRpc "calculator" "sumQuick"

-- | Input is the list of numbers to sum, output is the sum. Serialization
-- format is CBOR.
instance IsRPC SumQuickRpc where
  type Input  SumQuickRpc = [Int]
  type Output SumQuickRpc = Int
  serializationFormat _ = "cbor"
  serviceName         _ = "calculator"
  methodName          _ = "sumQuick"
  messageType         _ = "cbor"
  serializeInput      _ = Cbor.serialise @(Input SumQuickRpc)
  serializeOutput     _ = Cbor.serialise @(Output SumQuickRpc)
  deserializeInput    _ = first show . Cbor.deserialiseOrFail @(Input SumQuickRpc)
  deserializeOutput   _ = first show . Cbor.deserialiseOrFail @(Output SumQuickRpc)
instance
  StreamType.SupportsStreamingType SumQuickRpc 'StreamType.NonStreaming

-- | Server-streaming sum of an inclusive range
--
-- The server streams intermediate results as it sums the list of numbers
type SumFromToRpc = CalcRpc "calculator" "sumFromTo"

-- | Input is a pair of 'Int's specifying the inclusive range to sum, output is
-- 'Int's representing the intermediate sums calculated by the server.
-- Serialization format is CBOR.
instance IsRPC SumFromToRpc where
  type Input  SumFromToRpc = (Int, Int)
  type Output SumFromToRpc = Int
  serializationFormat _ = "cbor"
  serviceName         _ = "calculator"
  methodName          _ = "sumFromTo"
  messageType         _ = "cbor"
  serializeInput      _ = Cbor.serialise @(Input SumFromToRpc)
  serializeOutput     _ = Cbor.serialise @(Output SumFromToRpc)
  deserializeInput    _ = first show . Cbor.deserialiseOrFail @(Input SumFromToRpc)
  deserializeOutput   _ = first show . Cbor.deserialiseOrFail @(Output SumFromToRpc)
instance
  StreamType.SupportsStreamingType SumFromToRpc 'StreamType.ServerStreaming

-- | Client-streaming sum
--
-- The client streams 'Int's while the server accumulates the sum, then reports
-- at the end.
type SumListenRpc = CalcRpc "calculator" "sumListen"

-- | Input is 'Int', output is 'Int', serialization format is CBOR.
instance IsRPC SumListenRpc where
  type Input  SumListenRpc = Int
  type Output SumListenRpc = Int
  serializationFormat _ = "cbor"
  serviceName         _ = "calculator"
  methodName          _ = "sumListen"
  messageType         _ = "cbor"
  serializeInput      _ = Cbor.serialise @(Input SumListenRpc)
  serializeOutput     _ = Cbor.serialise @(Output SumListenRpc)
  deserializeInput    _ = first show . Cbor.deserialiseOrFail @(Input SumListenRpc)
  deserializeOutput   _ = first show . Cbor.deserialiseOrFail @(Output SumListenRpc)
instance
  StreamType.SupportsStreamingType SumListenRpc 'StreamType.ClientStreaming

-- | Bi-directional streaming sum
--
-- The client streams integers while the server accumulates a sum and reports
-- the current sum for every integer it receives.
type SumChatRpc = CalcRpc "calculator" "sumChat"

-- | Input is 'Int', output is 'Int', serialization format is CBOR.
instance IsRPC SumChatRpc where
  type Input  SumChatRpc = Int
  type Output SumChatRpc = Int
  serializationFormat _ = "cbor"
  serviceName         _ = "calculator"
  methodName          _ = "sumChat"
  messageType         _ = "cbor"
  serializeInput      _ = Cbor.serialise @(Input SumChatRpc)
  serializeOutput     _ = Cbor.serialise @(Output SumChatRpc)
  deserializeInput    _ = first show . Cbor.deserialiseOrFail @(Input SumChatRpc)
  deserializeOutput   _ = first show . Cbor.deserialiseOrFail @(Output SumChatRpc)
instance
  StreamType.SupportsStreamingType SumChatRpc 'StreamType.BiDiStreaming

test_calculator_cbor :: ClientServerConfig -> IO String
test_calculator_cbor config = do
    testClientServer assessCustomException $ def {
        config
      , client = \withConn -> withConn $ \conn -> do
          nonStreamingSumCheck conn
          serverStreamingSumCheck conn
          clientStreamingSumCheck conn
          biDiStreamingSumCheck conn
      , server = [
            nonStreamingSumHandler
          , serverStreamingSumHandler
          , clientStreamingSumHandler
          , biDiStreamingSumHandler
          ]
      }
  where
    -- Starting number for the sums (inclusive)
    start :: Int
    start = 0

    -- Ending number for the sums (inclusive)
    end :: Int
    end = 100

    nums :: [Int]
    nums = [start .. end]

    -- Intermediate results of summing nums ([0,1,3,6,10,...])
    intermediateSums :: [Int]
    intermediateSums =
      snd $ mapAccumL (\acc n -> let s = acc + n in (s,s)) 0 nums

    -- Ask for the sum of nums in nonStreaming fashion and check
    -- that it's accurate
    nonStreamingSumCheck :: Client.Connection -> IO ()
    nonStreamingSumCheck conn = do
      resp <- StreamType.nonStreaming (Client.rpc @SumQuickRpc conn) nums
      assertEqual "" (sum nums) resp

    -- Return the sum of a list of numbers
    nonStreamingSumHandler :: Server.RpcHandler IO
    nonStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @SumQuickRpc) $
        StreamType.mkNonStreaming $ \(ns :: [Int]) ->
          return (sum ns)

    -- Ask for the sum of nums with the server streaming intermediate
    -- results. Save the intermediate results, then make sure they are accurate.
    serverStreamingSumCheck :: Client.Connection -> IO ()
    serverStreamingSumCheck conn = do
      receivedSumFromTo <- newIORef @[Int] []
      StreamType.serverStreaming (Client.rpc @SumFromToRpc conn) (start, end) $ \n -> do
        atomicModifyIORef' receivedSumFromTo $ \ns -> (n:ns, ())
      resp <- readIORef receivedSumFromTo
      assertEqual "" intermediateSums (reverse resp)

    -- Stream the intermediate sums while summing a whole range of numbers
    serverStreamingSumHandler :: Server.RpcHandler IO
    serverStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @SumFromToRpc) $
        StreamType.mkServerStreaming $ \((from, to) :: (Int, Int)) send ->
          foldM_
            (\acc n -> let next = acc + n in send next >> return next)
            0
            [from .. to]

    -- Stream a list of numbers to the server and expect the sum of the numbers
    -- back
    clientStreamingSumCheck :: Client.Connection -> IO ()
    clientStreamingSumCheck conn = do
      sendingNums <- newIORef @[Int] nums
      resp <- StreamType.clientStreaming (Client.rpc @SumListenRpc conn) $ do
        atomicModifyIORef' sendingNums $
          \case
            []     -> ([], NoMoreElems NoMetadata)
            [x]    -> ([], FinalElem x NoMetadata)
            (x:xs) -> (xs, StreamElem x)
      assertEqual "" (sum nums) resp

    -- Receive a stream of numbers, return the sum
    clientStreamingSumHandler :: Server.RpcHandler IO
    clientStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @SumListenRpc) $
        StreamType.mkClientStreaming $ \recv -> do
          sum <$> StreamElem.collect recv

    -- Stream numbers, get back intermediate sums, make sure the intermediate
    -- sums we get back are accurate
    biDiStreamingSumCheck :: Client.Connection -> IO ()
    biDiStreamingSumCheck conn = do
      sendingNums <- newIORef @[Int] nums
      recvingNums <- newIORef @[Int] []
      StreamType.biDiStreaming (Client.rpc @SumChatRpc conn)
        ( do
            atomicModifyIORef' sendingNums $
              \case
                []     -> ([], NoMoreElems NoMetadata)
                [x]    -> ([], FinalElem x NoMetadata)
                (x:xs) -> (xs, StreamElem x)
        )
        ( \acc ->
            atomicModifyIORef' recvingNums ((,()) . (acc:))
        )
      recvdNums <- readIORef recvingNums
      assertEqual "" intermediateSums (reverse recvdNums)

    -- Receive numbers and stream the intermediate sums back
    biDiStreamingSumHandler :: Server.RpcHandler IO
    biDiStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @SumChatRpc) $
        StreamType.mkBiDiStreaming $ \recv send ->
          let
            go acc =
              recv >>= \case
                NoMoreElems _ -> return ()
                FinalElem n _ -> send (acc + n) >> go (acc + n)
                StreamElem n  -> send (acc + n) >> go (acc + n)
          in
            go 0

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

assessCustomException :: SomeException -> CustomException Void
assessCustomException = const CustomExceptionUnexpected

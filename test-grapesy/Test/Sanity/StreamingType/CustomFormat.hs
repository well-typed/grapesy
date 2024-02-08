{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.StreamingType.CustomFormat (tests) where

import Codec.Serialise qualified as Cbor
import Control.Monad
import Data.Bifunctor
import Data.IORef
import Data.Kind
import Data.List
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO (rpc)
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Common.StreamType (SupportsStreamingType, StreamingType(..))
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.StreamType qualified as Server

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  API definition
-------------------------------------------------------------------------------}

-- | Calculator gRPC client\/server example
--
-- This is used lifted to the type-level to describe the available RPC calls.
--
-- Uses CBOR as the data format, and uses all stream types.
data Calculator = Calc Function

data Function =
    -- | Non-streaming sum of a list of numbers
    SumQuick

    -- | Server-streaming sum of an inclusive range
    --
    -- The server streams intermediate results as it sums the list of numbers
  | SumFromTo

    -- | Client-streaming sum
    --
    -- The client streams 'Int's while the server accumulates the sum, then reports
    -- at the end.
  | SumListen

    -- | Bi-directional streaming sum
    --
    -- The client streams integers while the server accumulates a sum and reports
    -- the current sum for every integer it receives.
  | SumChat

class ( Typeable fun
      , Show (CalcInput  fun)
      , Show (CalcOutput fun)
      , Cbor.Serialise (CalcInput  fun)
      , Cbor.Serialise (CalcOutput fun)
      ) => CalculatorFunction (fun :: Function) where
  type family CalcInput  (fun :: Function) :: Type
  type family CalcOutput (fun :: Function) :: Type

  calculatorMethod :: Proxy fun -> Text

instance SupportsStreamingType (Calc SumQuick)  NonStreaming
instance CalculatorFunction SumQuick where
  type CalcInput  SumQuick = [Int]
  type CalcOutput SumQuick = Int
  calculatorMethod _ = "sumQuick"

instance SupportsStreamingType (Calc SumFromTo) ServerStreaming
instance CalculatorFunction SumFromTo where
  type CalcInput  SumFromTo = (Int, Int)
  type CalcOutput SumFromTo = Int
  calculatorMethod _ = "sumFromTo"

instance SupportsStreamingType (Calc SumListen) ClientStreaming
instance CalculatorFunction SumListen where
  type CalcInput  SumListen = Int
  type CalcOutput SumListen = Int
  calculatorMethod _ = "sumListen"

instance SupportsStreamingType (Calc SumChat) BiDiStreaming
instance CalculatorFunction SumChat where
  type CalcInput  SumChat = Int
  type CalcOutput SumChat = Int
  calculatorMethod _ = "sumChat"

instance CalculatorFunction fun => IsRPC (Calc fun) where
  type Input  (Calc fun) = CalcInput  fun
  type Output (Calc fun) = CalcOutput fun

  serializationFormat _ = "cbor"
  messageType         _ = "cbor"
  serviceName         _ = "calculator"
  methodName          _ = calculatorMethod (Proxy @fun)
  serializeInput      _ = Cbor.serialise @(CalcInput  fun)
  serializeOutput     _ = Cbor.serialise @(CalcOutput fun)
  deserializeInput    _ = first show . Cbor.deserialiseOrFail @(CalcInput  fun)
  deserializeOutput   _ = first show . Cbor.deserialiseOrFail @(CalcOutput fun)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
    testGroup "Test.Sanity.StreamingType.CustomFormat" [
        testCaseInfo "calculator" $
          test_calculator_cbor def
      ]

test_calculator_cbor :: ClientServerConfig -> IO String
test_calculator_cbor config = do
    testClientServer noCustomExceptions $ def {
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
      resp <- Client.nonStreaming conn (rpc @(Calc SumQuick)) nums
      assertEqual "" (sum nums) resp

    -- Return the sum of a list of numbers
    nonStreamingSumHandler :: Server.RpcHandler IO
    nonStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @(Calc SumQuick)) $
        Server.mkNonStreaming $ \(ns :: [Int]) ->
          return (sum ns)

    -- Ask for the sum of nums with the server streaming intermediate
    -- results. Save the intermediate results, then make sure they are accurate.
    serverStreamingSumCheck :: Client.Connection -> IO ()
    serverStreamingSumCheck conn = do
      receivedSumFromTo <- newIORef @[Int] []
      Client.serverStreaming conn (rpc @(Calc SumFromTo)) (start, end) $ \n -> do
        atomicModifyIORef' receivedSumFromTo $ \ns -> (n:ns, ())
      resp <- readIORef receivedSumFromTo
      assertEqual "" intermediateSums (reverse resp)

    -- Stream the intermediate sums while summing a whole range of numbers
    serverStreamingSumHandler :: Server.RpcHandler IO
    serverStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @(Calc SumFromTo)) $
        Server.mkServerStreaming $ \((from, to) :: (Int, Int)) send ->
          foldM_
            (\acc n -> let next = acc + n in send next >> return next)
            0
            [from .. to]

    -- Stream a list of numbers to the server and expect the sum of the numbers
    -- back
    clientStreamingSumCheck :: Client.Connection -> IO ()
    clientStreamingSumCheck conn = do
      sendingNums <- newIORef @[Int] nums
      resp <- Client.clientStreaming conn (rpc @(Calc SumListen)) $ do
        atomicModifyIORef' sendingNums $
          \case
            []     -> ([], NoMoreElems NoMetadata)
            [x]    -> ([], FinalElem x NoMetadata)
            (x:xs) -> (xs, StreamElem x)
      assertEqual "" (sum nums) resp

    -- Receive a stream of numbers, return the sum
    clientStreamingSumHandler :: Server.RpcHandler IO
    clientStreamingSumHandler =
      Server.streamingRpcHandler (Proxy @(Calc SumListen)) $
        Server.mkClientStreaming $ \recv -> do
          sum <$> StreamElem.collect recv

    -- Stream numbers, get back intermediate sums, make sure the intermediate
    -- sums we get back are accurate
    biDiStreamingSumCheck :: Client.Connection -> IO ()
    biDiStreamingSumCheck conn = do
      sendingNums <- newIORef @[Int] nums
      recvingNums <- newIORef @[Int] []
      Client.biDiStreaming conn (rpc @(Calc SumChat))
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
      Server.streamingRpcHandler (Proxy @(Calc SumChat)) $
        Server.mkBiDiStreaming $ \recv send ->
          let
            go acc =
              recv >>= \case
                NoMoreElems _ -> return ()
                FinalElem n _ -> send (acc + n) >> go (acc + n)
                StreamElem n  -> send (acc + n) >> go (acc + n)
          in
            go 0

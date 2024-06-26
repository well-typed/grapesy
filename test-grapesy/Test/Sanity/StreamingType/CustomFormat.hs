{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.StreamingType.CustomFormat (tests) where

import Codec.Serialise qualified as Cbor
import Control.Concurrent.Async (concurrently)
import Data.Bifunctor
import Data.ByteString qualified as Strict (ByteString)
import Data.Kind
import Data.List
import Data.Proxy
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client (rpc)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.NextElem qualified as NextElem
import Network.GRPC.Common.StreamType
import Network.GRPC.Server.StreamType (ServerHandler')
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

  calculatorMethod :: Proxy fun -> Strict.ByteString

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

type instance Input  (Calc fun) = CalcInput  fun
type instance Output (Calc fun) = CalcOutput fun

type instance RequestMetadata          (Calc fun) = NoMetadata
type instance ResponseInitialMetadata  (Calc fun) = NoMetadata
type instance ResponseTrailingMetadata (Calc fun) = NoMetadata

instance CalculatorFunction fun => IsRPC (Calc fun) where
  rpcContentType _ = defaultRpcContentType "cbor"
  rpcServiceName _ = "calculator"
  rpcMethodName  _ = calculatorMethod (Proxy @fun)
  rpcMessageType _ = Nothing

instance CalculatorFunction fun => SupportsClientRpc (Calc fun) where
  rpcSerializeInput    _ = Cbor.serialise @(CalcInput  fun)
  rpcDeserializeOutput _ = first show . Cbor.deserialiseOrFail @(CalcOutput fun)

instance CalculatorFunction fun => SupportsServerRpc (Calc fun) where
  rpcDeserializeInput _ = first show . Cbor.deserialiseOrFail @(CalcInput  fun)
  rpcSerializeOutput  _ = Cbor.serialise @(CalcOutput fun)

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

tests :: TestTree
tests =
    testGroup "Test.Sanity.StreamingType.CustomFormat" [
        testCase "calculator" test_calculator_cbor
      ]

test_calculator_cbor :: IO ()
test_calculator_cbor = do
    testClientServer $ ClientServerTest {
        config = def
      , client = simpleTestClient $ \conn -> do
          nonStreamingSumCheck conn
          serverStreamingSumCheck conn
          clientStreamingSumCheck conn
          biDiStreamingSumCheck conn
      , server = [
            Server.fromMethod nonStreamingSumHandler
          , Server.fromMethod serverStreamingSumHandler
          , Server.fromMethod clientStreamingSumHandler
          , Server.fromMethod biDiStreamingSumHandler
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
    nonStreamingSumHandler :: ServerHandler' NonStreaming IO (Calc SumQuick)
    nonStreamingSumHandler = Server.mkNonStreaming $ return . sum

    -- Ask for the sum of nums with the server streaming intermediate
    -- results. Save the intermediate results, then make sure they are accurate.
    serverStreamingSumCheck :: Client.Connection -> IO ()
    serverStreamingSumCheck conn = do
        resp <- Client.serverStreaming conn (rpc @(Calc SumFromTo)) (start, end) $ \recv ->
                  NextElem.collect recv
        assertEqual "" intermediateSums resp

    -- Stream the intermediate sums while summing a whole range of numbers
    serverStreamingSumHandler :: ServerHandler' ServerStreaming IO (Calc SumFromTo)
    serverStreamingSumHandler = Server.mkServerStreaming $ \(from, to) send ->
        NextElem.mapM_ send $ drop 1 (scanl (+) 0 [from .. to])

    -- Stream a list of numbers to the server and expect the sum of the numbers
    -- back
    clientStreamingSumCheck :: Client.Connection -> IO ()
    clientStreamingSumCheck conn = do
        resp <- Client.clientStreaming_ conn (rpc @(Calc SumListen)) $ \send ->
                  NextElem.mapM_ send nums
        assertEqual "" (sum nums) resp

    -- Receive a stream of numbers, return the sum
    clientStreamingSumHandler :: ServerHandler' ClientStreaming IO (Calc SumListen)
    clientStreamingSumHandler = Server.mkClientStreaming $ \recv ->
        sum <$> NextElem.collect recv

    -- Stream numbers, get back intermediate sums, make sure the intermediate
    -- sums we get back are accurate
    biDiStreamingSumCheck :: Client.Connection -> IO ()
    biDiStreamingSumCheck conn = do
        ((), recvdNums) <- Client.biDiStreaming conn (rpc @(Calc SumChat)) $ \send recv ->
          concurrently
            (NextElem.mapM_ send nums)
            (NextElem.collect recv)
        assertEqual "" intermediateSums recvdNums

    -- Receive numbers and stream the intermediate sums back
    biDiStreamingSumHandler :: ServerHandler' BiDiStreaming IO (Calc SumChat)
    biDiStreamingSumHandler = Server.mkBiDiStreaming $ \recv send ->
        let
          go acc =
            recv >>= \case
              NoNextElem -> send NoNextElem
              NextElem n -> send (NextElem (acc + n)) >> go (acc + n)
        in
          go 0

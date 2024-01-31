-- | Test functionality required by the gRPC interop tests
module Test.Sanity.Interop (tests) where

import Control.Exception
import Data.Default
import Data.Proxy
import Data.Void
import Test.Tasty
import Test.Tasty.HUnit
import Data.ProtoLens

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Common.StreamType qualified as StreamType
import Network.GRPC.Server.StreamType qualified as Server
import Network.GRPC.Spec

import Proto.Src.Proto.Grpc.Testing.Empty
import Proto.Src.Proto.Grpc.Testing.Test

import Test.Driver.ClientServer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Sanity.Interop" [
      testCaseInfo "emptyUnary" test_emptyUnary
    ]

{-------------------------------------------------------------------------------
  Individual tests
-------------------------------------------------------------------------------}

type EmptyCall = Protobuf TestService "emptyCall"

-- | Test that the empty message has an empty encoding
--
-- This test fails if we unconditionally compress (the /compressed/ form of the
-- empty message is larger than the uncompressed form, as compression introduces
-- minor overhead).
test_emptyUnary :: IO String
test_emptyUnary =
    testClientServer assessCustomException def {
        client = \withConn -> withConn $ \conn ->
          Client.withRPC conn def (Proxy @EmptyCall) $ \call -> do
            Client.sendFinalInput call (defMessage :: Empty)
            streamElem <- Client.recvOutputWithEnvelope call
            case StreamElem.value streamElem of
              Nothing                      -> fail "Expected answer"
              Just (envelope, _x :: Empty) -> verifyEnvelope envelope
      , server = [
            Server.streamingRpcHandler (Proxy @EmptyCall) $
              StreamType.mkNonStreaming $ \(_ ::Empty) ->
                return (defMessage :: Empty)
          ]
      }
  where
    -- We don't /expect/ the empty message to be compressed, due to the overhead
    -- mentioned above. However, /if/ it is compressed, perhaps using a custom
    -- zero-overhead compression algorithm, it's size should be zero.
    verifyEnvelope :: InboundEnvelope -> IO ()
    verifyEnvelope envelope = do
        assertEqual "uncompressed size" (inboundUncompressedSize envelope) 0
        case inboundCompressedSize envelope of
          Nothing   -> return ()
          Just size -> assertEqual "compressed size" size 0

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

assessCustomException :: SomeException -> CustomException Void
assessCustomException = const CustomExceptionUnexpected

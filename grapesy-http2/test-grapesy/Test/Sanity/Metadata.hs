{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Sanity.Metadata (tests) where

import Control.Monad
import Data.Binary (Binary)
import Data.ByteString qualified as BSS
import Data.ByteString qualified as Strict (ByteString)
import Data.String
import GHC.Generics (Generic)
import Test.Driver.ClientServer
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.Binary qualified as Client.Binary
import Network.GRPC.Common
import Network.GRPC.Common.Binary
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Binary qualified as Server.Binary
import Network.GRPC.Spec.Serialization qualified as Spec

tests :: TestTree
tests = testGroup "Test.Sanity.Metadata" [
      testCase "summarizeAndEcho" $ test_summarizeAndEcho 10
    ]

{-------------------------------------------------------------------------------
  Trailers
-------------------------------------------------------------------------------}

-- | Sanity check: test that the server can receive client metadata and can
-- echo it as trailing metadata. We also verify that the server can /announce/
-- that trailing metadata before it sends it.
test_summarizeAndEcho :: Int -> Assertion
test_summarizeAndEcho n = testClientServer $ ClientServerTest {
      config = def{serverPort = Right 50051}
    , server = [Server.someRpcHandler execInstr]
    , client = simpleTestClient $ \conn -> do
          Client.withRPC conn callParams (Proxy @ExecInstr) $ \call -> do
            Client.Binary.sendFinalInput call SummarizeAndEcho

            -- Check that trailers announced
            initResponse <- Client.recvInitialResponse call
            case initResponse of
              Left trailersOnly ->
                assertFailure $ "Unexpected trailers-only " ++ show trailersOnly
              Right x ->
                case Client.responseTrailerNames x of
                  Left err ->
                    assertFailure $ show err
                  Right Nothing ->
                    assertFailure "Trailer not present"
                  Right (Just names) ->
                    forM_ metadata $ \md ->
                      assertBool ("Missing " ++ show md) $ flip elem names $
                        Spec.buildHeaderName (customMetadataName md)

            -- Check summary and trailing metadata
            (summary, trailers) <- Client.Binary.recvFinalOutput call
            assertEqual "" (summarize metadata) $ summary
            assertEqual "" metadata             $ trailers

    }
  where
    metadata :: [CustomMetadata]
    metadata = [
          CustomMetadata
            (fromString $ "md-" ++ printf "%02d" i) -- for sorting purposes
            (fromString $ show i)
        | i <- [1 .. n]
        ]

    callParams :: Client.CallParams ExecInstr
    callParams = def{Client.callRequestMetadata = metadata}

{-------------------------------------------------------------------------------
  Server handler
-------------------------------------------------------------------------------}

type ExecInstr = RawRpc "TestMetadata" "ExecInstr"

type instance RequestMetadata          ExecInstr = [CustomMetadata]
type instance ResponseInitialMetadata  ExecInstr = [CustomMetadata]
type instance ResponseTrailingMetadata ExecInstr = [CustomMetadata]

data Instruction =
    -- | Summary the request metadata, and echo it as trailing metadata
    SummarizeAndEcho
  deriving stock (Generic)
  deriving anyclass (Binary)

execInstr :: Server.RpcHandler IO ExecInstr
execInstr = Server.mkRpcHandlerNoDefMetadata $ \call -> do
    requestMetadata <- Server.getRequestMetadata call
    instr <- Server.Binary.recvFinalInput call
    case instr of
      SummarizeAndEcho -> do
        -- We need to explicitly set the trailers, because they vary from one
        -- request to the next (that is, they aren't static)
        Server.setResponseInitialMetadataAndTrailers call [] . Just $
          map customMetadataName requestMetadata
        Server.Binary.sendFinalOutput @Summary call (
            summarize requestMetadata
          , requestMetadata
          )

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

type Summary = [(Strict.ByteString, Int)]

summarize :: [CustomMetadata] -> Summary
summarize = map aux
  where
    aux :: CustomMetadata -> (Strict.ByteString, Int)
    aux md = (
          getHeaderName $ customMetadataName  md
        , BSS.length    $ customMetadataValue md
        )

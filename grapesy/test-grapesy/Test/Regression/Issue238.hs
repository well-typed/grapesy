{-# LANGUAGE OverloadedStrings #-}

-- | Tests for <https://github.com/well-typed/grapesy/issues/238>
module Test.Regression.Issue238 (tests) where

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as Text
import Test.Tasty
import Test.Tasty.HUnit

import Network.GRPC.Client (rpc)
import Network.GRPC.Client qualified as Client
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Protobuf qualified as Server
import Network.GRPC.Server.Run qualified as Server
import Network.GRPC.Server.StreamType qualified as Server

import Proto.API.RouteGuide
import Proto.API.Trivial

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Issue238" [
      testGroup "Trivial" [
          testCase "nonStreaming1" test_trivial_nonStreaming1
        , testCase "nonStreaming2" test_trivial_nonStreaming2
        ]
    , testGroup "LowLevel" [
          testCase "nonStreaming1" test_lowLevel_nonStreaming1
        , testCase "nonStreaming2" test_lowLevel_nonStreaming2
        ]
    , testGroup "RouteGuide" [
          testCase "nonStreaming1" test_routeGuide_nonStreaming1
        , testCase "nonStreaming2" test_routeGuide_nonStreaming2
        , testCase "nonStreaming3" test_routeGuide_nonStreaming3
        ]
    ]

{-------------------------------------------------------------------------------
  Without Protobuf
-------------------------------------------------------------------------------}

-- | Undefined handler body
test_trivial_nonStreaming1 :: Assertion
test_trivial_nonStreaming1 =
    testWith handlers client
  where
    handlers :: [Server.SomeRpcHandler IO]
    handlers = [Server.someRpcHandler $ Server.mkRpcHandler @Trivial undefined]

    client :: Client.Connection -> IO ByteString
    client conn = Client.nonStreaming conn (rpc @Trivial) mempty

-- | Like 'test_trivial_nonStreaming1', but without the call to @mkRpcHandler@
--
-- This matters, because 'mkRpcHandler' sets the initial metadata.
test_trivial_nonStreaming2 :: Assertion
test_trivial_nonStreaming2 =
    testWith handlers client
  where
    handlers :: [Server.SomeRpcHandler IO]
    handlers = [Server.someRpcHandler @Trivial undefined]

    client :: Client.Connection -> IO ByteString
    client conn = Client.nonStreaming conn (rpc @Trivial) mempty

{-------------------------------------------------------------------------------
  Low-level API

  The ticket specifically says "when client uses high-level API". In this
  section we therefore compare the behaviour of the @test_trivial@ tests (which
  use the high-level API) to the corresponding behaviour with the low-level API.
-------------------------------------------------------------------------------}

-- | Direct equivalent of 'test_trivial_nonStreaming2'
test_lowLevel_nonStreaming1 :: Assertion
test_lowLevel_nonStreaming1 =
    testWith handlers client
  where
    handlers :: [Server.SomeRpcHandler IO]
    handlers = [Server.someRpcHandler @Trivial undefined]

    client :: Client.Connection -> IO ByteString
    client conn =
        Client.withRPC conn def (Proxy @Trivial) $ \call -> do
          Client.sendFinalInput call mempty
          fst <$> Client.recvFinalOutput call

-- | Like 'test_lowLevel_nonStreaming1, but without sending the input
test_lowLevel_nonStreaming2 :: Assertion
test_lowLevel_nonStreaming2 =
    testWith handlers client
  where
    handlers :: [Server.SomeRpcHandler IO]
    handlers = [Server.someRpcHandler @Trivial undefined]

    client :: Client.Connection -> IO ByteString
    client conn =
        Client.withRPC conn def (Proxy @Trivial) $ \call -> do
          fst <$> Client.recvFinalOutput call

{-------------------------------------------------------------------------------
  With Protobuf

  Crucially, this uses 'fromMethods', which is not unlikely to have undefineds
  in it during development.

  The @test_routeGuide_nonStreaming<N>@ tests all use @undefined@ somewhere in
  the declaration of the methods, but differ on how much is defined.
-------------------------------------------------------------------------------}

-- | Completely @undefined@ 'Methods'
--
-- This is different from 'test_routeGuide_nonStreaming2', where the /skeleton/
-- is defined but the individual handlers are not: here we cannot even construct
-- the list without triggering an exception, forcing us to be very careful with
-- exception handling during lookup.
test_routeGuide_nonStreaming1 :: Assertion
test_routeGuide_nonStreaming1 =
    testWith (Server.fromMethods methods) client
  where
    methods :: Server.Methods IO (Server.ProtobufMethodsOf RouteGuide)
    methods = undefined

    client :: Client.Connection -> IO (Proto Feature)
    client conn = Client.nonStreaming conn (rpc @GetFeature) defMessage

test_routeGuide_nonStreaming2 :: Assertion
test_routeGuide_nonStreaming2 =
    testWith (Server.fromMethods methods) client
  where
    methods :: Server.Methods IO (Server.ProtobufMethodsOf RouteGuide)
    methods =
          Server.Method undefined
        $ Server.Method undefined
        $ Server.Method undefined
        $ Server.Method undefined
        $ Server.NoMoreMethods

    client :: Client.Connection -> IO (Proto Feature)
    client conn = Client.nonStreaming conn (rpc @GetFeature) defMessage

test_routeGuide_nonStreaming3 :: Assertion
test_routeGuide_nonStreaming3 =
    testWith (Server.fromMethods methods) client
  where
    methods :: Server.Methods IO (Server.ProtobufMethodsOf RouteGuide)
    methods =
          Server.Method (Server.mkNonStreaming undefined)
        $ Server.Method undefined
        $ Server.Method undefined
        $ Server.Method undefined
        $ Server.NoMoreMethods

    client :: Client.Connection -> IO (Proto Feature)
    client conn = Client.nonStreaming conn (rpc @GetFeature) defMessage

{-------------------------------------------------------------------------------
  Auxiliary: test setup

  We don't use the test clients/server infrastructure here, since this issue
  is about exception handling, and the test harnass does quite a bit of
  exception processing.
-------------------------------------------------------------------------------}

testWith ::
     [Server.SomeRpcHandler IO]
  -> (Client.Connection -> IO a)
  -> Assertion
testWith handlers client = do
    server <- Server.mkGrpcServer serverParams handlers
    Server.forkServer def serverConfig server $ \runningServer -> do
      serverPort <- Server.getServerPort runningServer

      let serverAddr = Client.ServerInsecure $ Client.Address {
           addressHost      = "127.0.0.1"
         , addressPort      = serverPort
         , addressAuthority = Nothing
         }
      Client.withConnection def serverAddr $ \conn ->
        checkClientReceivesUndefined $ client conn
  where
    serverConfig :: Server.ServerConfig
    serverConfig = Server.ServerConfig {
          serverInsecure = Just $ Server.InsecureConfig Nothing 0
        , serverSecure   = Nothing
        }

    serverParams :: Server.ServerParams
    serverParams = def {
          Server.serverTopLevel = \handler unmask req resp -> do
            _result :: Either SomeException () <- try $ handler unmask req resp
            -- Ignore any exceptions
            return ()
        }

-- | Verify that the client  is notified of the undefined handler
checkClientReceivesUndefined ::
     HasCallStack
  => IO a -> Assertion
checkClientReceivesUndefined k = do
    result <- try k
    case result of
      Right _ ->
        assertFailure "Unexpected successful response"
      Left err ->
        case grpcErrorMessage err of
          Just msg ->
            assertBool (show (Text.unpack msg) ++ " contains \"undefined\"") $
              "undefined" `Text.isInfixOf` msg
          Nothing ->
            assertFailure "Missing error message"
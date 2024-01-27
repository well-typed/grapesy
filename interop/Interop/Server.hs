module Interop.Server (server) where

import Control.Monad.Catch (generalBracket, ExitCase(..))
import Data.Default
import Data.ProtoLens.Labels ()

import Network.GRPC.Common
import Network.GRPC.Common.StreamType
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Paths_grapesy

import Proto.Ping
import Proto.Src.Proto.Grpc.Testing.Test

import Interop.Cmdline
import Interop.Server.Handlers.EmptyCall
import Interop.Server.Handlers.Ping
import Interop.Server.Handlers.UnaryCall

{-------------------------------------------------------------------------------
  Handlers

  The expected server functionality is described at
  <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#server>

  The relevant @.proto@ definitions are

  * @grpc-repo/src/proto/grpc/testing/test.proto@ (main service definition)
  * @grpc-repo/src/proto/grpc/testing/messages.proto@ (most message types)
  * @grpc-repo/src/proto/grpc/testing/empty.proto@ (@Empty@ message)
-------------------------------------------------------------------------------}

methodsPingService :: Methods IO (ProtobufMethodsOf PingService)
methodsPingService =
      Method (mkNonStreaming handlePing)
    $ NoMoreMethods

methodsTestService :: Methods IO (ProtobufMethodsOf TestService)
methodsTestService =
      UnsupportedMethod -- cacheableUnaryCall
    $ Method (mkNonStreaming handleEmptyCall)
    $ UnsupportedMethod -- fullDuplexCall
    $ UnsupportedMethod -- halfDuplexCall
    $ UnsupportedMethod -- streamingInputCall
    $ UnsupportedMethod -- streamingOutputCall
    $ Method (mkNonStreaming handleUnaryCall) -- unaryCall
    $ UnsupportedMethod -- unimplementedCall
    $ NoMoreMethods

services :: Services IO (ProtobufServices '[PingService, TestService])
services =
      Service methodsPingService
    $ Service methodsTestService
    $ NoMoreServices

{-------------------------------------------------------------------------------
  Main server definition
-------------------------------------------------------------------------------}

server :: Cmdline -> IO ()
server cmdline = showStartStop $ do
    serverConfig <-
      if cmdUseTLS cmdline then do
        pubCert <- getDataFileName "interop.pem"
        privKey <- getDataFileName "interop.key"

        return ServerConfig {
            serverInsecure = Nothing
          , serverSecure   = Just SecureConfig {
                secureHost       = "0.0.0.0"
              , securePort       = cmdPort cmdline
              , securePubCert    = pubCert
              , secureChainCerts = []
              , securePrivKey    = privKey
              , secureSslKeyLog  = SslKeyLogFromEnv
              }
          }
      else
        return ServerConfig {
            serverSecure   = Nothing
          , serverInsecure = Just InsecureConfig {
                insecureHost = Nothing
              , insecurePort = cmdPort cmdline
              }
          }

    runServerWithHandlers
      serverConfig
      def
      (fromServices services)

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

showStartStop :: forall a. IO a -> IO a
showStartStop act = fst <$>
    generalBracket
      start
      (\() -> stop)
      (\() -> act)
  where
    start :: IO ()
    start = putStrLn "grapesy interop server started"

    stop :: ExitCase a -> IO ()
    stop (ExitCaseSuccess _)   = putStrLn $ "server terminated normally"
    stop (ExitCaseException e) = putStrLn $ "server exception: " ++ show e
    stop  ExitCaseAbort        = error "impossible in IO"


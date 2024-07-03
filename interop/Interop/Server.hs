module Interop.Server (
      withInteropServer
    , runInteropServer
    ) where

import Control.Exception (SomeException)
import Control.Monad.Catch (generalBracket, ExitCase(..))

import Network.GRPC.Common
import Network.GRPC.Internal.XIO qualified as XIO
import Network.GRPC.Server
import Network.GRPC.Server.Protobuf
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType

import Interop.API
import Interop.Cmdline

import Interop.Server.PingService.Ping                qualified as Ping
import Interop.Server.TestService.EmptyCall           qualified as EmptyCall
import Interop.Server.TestService.FullDuplexCall      qualified as FullDuplexCall
import Interop.Server.TestService.StreamingInputCall  qualified as StreamingInputCall
import Interop.Server.TestService.StreamingOutputCall qualified as StreamingOutputCall
import Interop.Server.TestService.UnaryCall           qualified as UnaryCall

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
      Method (mkNonStreaming Ping.handle)
    $ NoMoreMethods

methodsTestService :: Methods IO (ProtobufMethodsOf TestService)
methodsTestService =
      UnsupportedMethod -- cacheableUnaryCall
    $ Method    (mkNonStreaming EmptyCall.handle)
    $ RawMethod (mkRpcHandler FullDuplexCall.handle)
    $ UnsupportedMethod -- halfDuplexCall
    $ RawMethod (mkRpcHandler StreamingInputCall.handle)
    $ RawMethod (mkRpcHandler StreamingOutputCall.handle)
    $ RawMethod (mkRpcHandler UnaryCall.handle)
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

withInteropServer :: Cmdline -> (RunningServer -> IO a) -> IO a
withInteropServer cmdline k = do
    server <- mkGrpcServer serverParams $ fromServices services
    forkServer serverParams serverConfig server k
  where
    serverConfig :: ServerConfig
    serverConfig
      | cmdUseTLS cmdline
      = ServerConfig {
            serverInsecure = Nothing
          , serverSecure   = Just SecureConfig {
                secureHost       = "0.0.0.0"
              , securePort       = cmdPort cmdline
              , securePubCert    = cmdPubCert cmdline
              , secureChainCerts = []
              , securePrivKey    = cmdPrivKey cmdline
              , secureSslKeyLog  = cmdSslKeyLog cmdline
              }
          }

     | otherwise
     = ServerConfig {
            serverSecure   = Nothing
          , serverInsecure = Just InsecureConfig {
                insecureHost = Nothing
              , insecurePort = cmdPort cmdline
              }
          }

    serverParams :: ServerParams
    serverParams = def {
          serverTopLevel      = swallowExceptions
        , serverVerifyHeaders = True
        }

    -- Don't show handler exceptions on stderr
    swallowExceptions ::
         RequestHandler SomeException ()
      -> RequestHandler XIO.NeverThrows   ()
    swallowExceptions h req resp = h req resp `XIO.catchError` \_ -> return ()

runInteropServer :: Cmdline -> IO ()
runInteropServer cmdline = withInteropServer cmdline $ \server ->
    showStartStop $
      waitServer server

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


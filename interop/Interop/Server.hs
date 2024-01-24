{-# LANGUAGE OverloadedLabels  #-}

module Interop.Server (server) where

import Control.Lens ((.~), (^.))
import Control.Monad.Catch (generalBracket, ExitCase(..))
import Data.Default
import Data.Function ((&))
import Data.ProtoLens
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
import Interop.TestCases

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

handlePing :: PingMessage -> IO PongMessage
handlePing ping = return $ defMessage & #id .~ (ping ^. #id)

handlers :: Methods IO (ProtobufMethodsOf TestService)
handlers =
      Method handleCacheableUnaryCall
    $ UnsupportedMethod -- emptyCall
    $ UnsupportedMethod -- fullDuplexCall
    $ UnsupportedMethod -- halfDuplexCall
    $ UnsupportedMethod -- streamingInputCall
    $ UnsupportedMethod -- streamingOutputCall
    $ UnsupportedMethod -- unaryCall
    $ UnsupportedMethod -- unimplementedCall
    $ NoMoreMethods

services :: Services IO (ProtobufServices '[PingService, TestService])
services =
      Service (Method (mkNonStreaming handlePing) NoMoreMethods)
    $ Service handlers
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


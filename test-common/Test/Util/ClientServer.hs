{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Util.ClientServer (
    -- * Configuraiton
    ClientServerConfig(..)
    -- ** TLS
  , TlsSetup(..)
  , TlsOk(..)
  , TlsFail(..)
    -- ** Evaluation
  , ExpectedException(..)
  , UnexpectedException(..)
  , CustomException(..)
  , isExpectedException
    -- * Run
  , runTestClientServer
    -- ** Lower-level functionality
  , ServerHandlerLock -- opaque
  , newServerHandlerLock
  , runTestClient
  , runTestServer
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Tracer
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Network.HTTP2.Server qualified as HTTP2
import Network.TLS
import Text.Show.Pretty

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression (CompressionNegotationFailed)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Internal
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server

import Debug.Concurrent

import Test.Util.PrettyVal

import Paths_grapesy

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data ClientServerConfig = ClientServerConfig {
      -- | Compression algorithms supported by the client
      clientCompr :: Compr.Negotation

      -- | Compression algorithms supported the server
    , serverCompr :: Compr.Negotation

      -- | TLS setup (if using)
    , useTLS :: Maybe TlsSetup
    }

instance Default ClientServerConfig where
  def = ClientServerConfig {
      clientCompr = Compr.none
    , serverCompr = Compr.none
    , useTLS      = Nothing
    }

{-------------------------------------------------------------------------------
  We collect log messages from both the client and the server
-------------------------------------------------------------------------------}

data LogMsg =
    ServerLogMsg Server.ServerDebugMsg
  | ClientLogMsg Client.ClientDebugMsg
  deriving stock (Show, GHC.Generic)
  deriving anyclass (PrettyVal)

collectLogMsgs :: (MonadIO m) => MVar [a] -> Tracer m a
collectLogMsgs v = arrow $ emit $ \x -> liftIO $
    modifyMVar_ v $ \xs -> do
      -- print x
      return (x:xs)

{-------------------------------------------------------------------------------
  Configuration: TLS
-------------------------------------------------------------------------------}

-- | TLS setup
data TlsSetup = TlsOk TlsOk | TlsFail TlsFail

-- | TLS setup that we expect to work
data TlsOk =
    -- | Configure the client so that the server's cert is a known root
    --
    -- This means that client can validate the server's cert, even though it is
    -- self signed.
    TlsOkCertAsRoot

    -- | Configure the client to not validate the server's cert at all
  | TlsOkSkipValidation

-- | TLS setup that should result in an error
data TlsFail =
    -- | Don't take any special provisions in the client
    --
    -- This means that TLS validation will fail, since the server's cert is
    -- self signed.
    TlsFailValidation

    -- | Client uses the wrong address (127.0.0.1 instead of localhost)
    --
    -- This too will result in a TLS failure (hostname mismatch). To ensure we
    -- get the TLS failure we expect, we /do/ add the server's certificate to
    -- the client's trusted certs.
  | TlsFailHostname

    -- | The server is not configured for TLS
  | TlsFailUnsupported

{-------------------------------------------------------------------------------
  Config evaluation (do we expect an error?)
-------------------------------------------------------------------------------}

data ExpectedException e =
    ExpectedExceptionTls TLSException
  | ExpectedExceptionGrpc GrpcException
  | ExpectedExceptionCompressionNegotationFailed CompressionNegotationFailed
  | ExpectedExceptionDouble (DoubleException (ExpectedException e))
  | ExpectedExceptionCustom e
  deriving stock (Show, GHC.Generic)
  deriving anyclass (PrettyVal)

data UnexpectedException = UnexpectedException {
      -- | The top-level exception (including any wrapper exceptions)
      unexpectedExceptionTopLevel :: SomeException

      -- | The (possibly nested) exception that was actually unexpected
    , unexpectedExceptionNested :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

data CustomException e =
    CustomExceptionExpected e
  | CustomExceptionUnexpected
  | CustomExceptionNested SomeException

-- | Check if the given configuration gives rise to expected exceptions
--
-- Returns 'Right' the expected exception if any (with any wrapper exceptions
-- removed), 'Left' some (possibly nested) exception if the exception was
-- unexpected.
isExpectedException :: forall e.
     ClientServerConfig
  -> (SomeException -> CustomException e)
  -- ^ Assess custom exceptiosn
  --
  -- Can either return e nested exception, or an evaluation whether the
  -- exception is expected or not.
  -> SomeException
  -> Either UnexpectedException (ExpectedException e)
isExpectedException cfg assessCustomException topLevel =
    go topLevel
  where
    go :: SomeException -> Either UnexpectedException (ExpectedException e)
    go err
      --
      -- Expected exceptions
      --

      | Just (tls :: TLSException) <- fromException err
      = case (useTLS cfg, tls) of
#if MIN_VERSION_tls(1,9,0)
          (   Just (TlsFail TlsFailValidation)
            , HandshakeFailed (Error_Protocol _msg UnknownCa)
            ) ->
            Right $ ExpectedExceptionTls tls
          (   Just (TlsFail TlsFailHostname)
            , HandshakeFailed (Error_Protocol _msg CertificateUnknown)
            ) ->
             Right $ ExpectedExceptionTls tls
#else
          (   Just (TlsFail TlsFailValidation)
            , HandshakeFailed (Error_Protocol (_msg, _bool, UnknownCa))
            ) ->
            Right $ ExpectedExceptionTls tls
          (   Just (TlsFail TlsFailHostname)
            , HandshakeFailed (Error_Protocol (_msg, _bool, CertificateUnknown))
            ) ->
             Right $ ExpectedExceptionTls tls
#endif
          (   Just (TlsFail TlsFailUnsupported)
            , HandshakeFailed (Error_Packet_Parsing _)
            ) ->
             Right $ ExpectedExceptionTls tls
          _otherwise ->
            Left $ UnexpectedException topLevel err

      | Just (grpc :: GrpcException) <- fromException err
      , compressionNegotationFailure
      , GrpcUnknown <- grpcError grpc
      , Just msg <- grpcErrorMessage grpc
      , "CompressionNegotationFailed" `Text.isInfixOf` msg
      = Right $ ExpectedExceptionGrpc grpc

      | Just (compr :: CompressionNegotationFailed) <- fromException err
      , compressionNegotationFailure
      = Right $ ExpectedExceptionCompressionNegotationFailed compr

      --
      -- Wrappers
      --

      | Just err' <- maybeNestedException err
      = go err'

      | Just (DoubleException { doubleExceptionClient
                              , doubleExceptionServer
                              }) <- fromException err
      = case (go doubleExceptionClient, go doubleExceptionServer) of
          (Left unexpected, _) -> Left unexpected
          (_, Left unexpected) -> Left unexpected
          (Right expected, Right expected') ->
            Right $ ExpectedExceptionDouble DoubleException {
                doubleExceptionClient = expected
              , doubleExceptionServer = expected'
              , doubleExceptionAnnotation = ()
              }

      --
      -- Custom exceptions
      --

      | otherwise
      = case assessCustomException err of
          CustomExceptionExpected err' ->
            Right $ ExpectedExceptionCustom err'
          CustomExceptionUnexpected ->
            Left $ UnexpectedException topLevel err
          CustomExceptionNested err' ->
            go err'

    compressionNegotationFailure :: Bool
    compressionNegotationFailure =
        Set.disjoint (Map.keysSet (Compr.supported (clientCompr cfg)))
                     (Map.keysSet (Compr.supported (serverCompr cfg)))


{-------------------------------------------------------------------------------
  Server handler lock

  We don't want to terminate the test when some of the server handlers are
  still running; this will result in those handlers being killed with a
  'KilledByHttp2ThreadManager' exception, confusing the test results.
-------------------------------------------------------------------------------}

newtype ServerHandlerLock = ServerHandlerLock (TVar Int)

newServerHandlerLock :: IO ServerHandlerLock
newServerHandlerLock = ServerHandlerLock <$> newTVarIO 0

waitForHandlerTermination :: ServerHandlerLock -> STM ()
waitForHandlerTermination (ServerHandlerLock lock) = do
    activeHandlers <- readTVar lock
    unless (activeHandlers == 0) $ retry

serverHandlerLockHook :: ServerHandlerLock -> HTTP2.Server -> HTTP2.Server
serverHandlerLockHook (ServerHandlerLock lock) server request aux respond =
    bracket_ register unregister $
      server request aux respond
  where
    register, unregister :: IO ()
    register   = atomically $ modifyTVar lock (\x -> x + 1)
    unregister = atomically $ modifyTVar lock (\x -> x - 1)

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runTestServer ::
     ClientServerConfig
  -> Tracer IO SomeException
  -> Tracer IO Server.ServerDebugMsg
  -> ServerHandlerLock
  -> [Server.RpcHandler IO]
  -> IO ()
runTestServer cfg serverExceptions serverTracer handlerLock serverHandlers = do
    pubCert <- getDataFileName "grpc-demo.cert"
    privKey <- getDataFileName "grpc-demo.priv"

    let serverSetup :: Server.ServerSetup
        serverSetup = def {
              Server.serverHandlerHook = serverHandlerLockHook handlerLock
            }

        serverConfig :: Server.ServerConfig
        serverConfig =
            case useTLS cfg of
              Nothing -> Server.ServerConfig {
                  serverSetup    = serverSetup
                , serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Nothing
                    , insecurePort = "50051"
                    }
                , serverSecure   = Nothing
                }
              Just (TlsFail TlsFailUnsupported) -> Server.ServerConfig {
                  serverSetup    = serverSetup
                , serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Nothing
                    , insecurePort = "50052"
                    }
                , serverSecure   = Nothing
                }
              Just _tlsSetup -> Server.ServerConfig {
                  serverSetup    = serverSetup
                , serverInsecure = Nothing
                , serverSecure   = Just $ Server.SecureConfig {
                      secureHost       = "localhost"
                    , securePort       = 50052
                    , securePubCert    = pubCert
                    , secureChainCerts = []
                    , securePrivKey    = privKey
                    , secureSslKeyLog  = SslKeyLogNone
                    }
                }


        serverParams :: Server.ServerParams
        serverParams = Server.ServerParams {
              serverCompression     = serverCompr cfg
            , serverExceptionTracer = serverExceptions
            , serverDebugTracer     = serverTracer
            }

    Server.withServer serverParams serverHandlers $
      Server.runServer serverConfig

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

runTestClient :: forall a.
     ClientServerConfig
  -> Tracer IO Client.ClientDebugMsg
  -> ((forall b. (Client.Connection -> IO b) -> IO b) -> IO a)
  -> IO a
runTestClient cfg clientTracer clientRun = do
    pubCert <- getDataFileName "grpc-demo.cert"

    let clientParams :: Client.ConnParams
        clientParams = Client.ConnParams {
              connDebugTracer     = clientTracer
            , connCompression     = clientCompr cfg
            , connDefaultTimeout  = Nothing

              -- We need a single reconnect, to enable wait-for-ready.
              -- This avoids a race condition between the server starting first
              -- and the client starting first.
            , connReconnectPolicy =
                  Client.ReconnectAfter (0.1, 0.2)
                $ Client.DontReconnect
            }

        clientServer :: Client.Server
        clientServer =
            case useTLS cfg of
              Just tlsSetup ->
                Client.ServerSecure
                  ( case tlsSetup of
                      TlsOk TlsOkCertAsRoot ->
                        correctClientSetup
                      TlsOk TlsOkSkipValidation ->
                        Client.NoServerValidation
                      TlsFail TlsFailValidation ->
                        Client.ValidateServer mempty
                      TlsFail TlsFailHostname ->
                        correctClientSetup
                      TlsFail TlsFailUnsupported ->
                        correctClientSetup
                  )
                  -- We enable key logging in the client and disable it in the
                  -- server. This avoids the client and server trying to write
                  -- to the same file.
                  SslKeyLogFromEnv
                  clientAuthority

              Nothing ->
                Client.ServerInsecure
                  clientAuthority
          where
            correctClientSetup :: Client.ServerValidation
            correctClientSetup =
                Client.ValidateServer $
                  Client.certStoreFromPath pubCert

        clientAuthority :: Client.Authority
        clientAuthority =
            case useTLS cfg of
              Just tlsSetup -> Client.Authority {
                  authorityHost = case tlsSetup of
                                    TlsFail TlsFailHostname -> "127.0.0.1"
                                    _otherwise              -> "localhost"
                , authorityPort = 50052
                }

              Nothing -> Client.Authority {
                  authorityHost = "localhost"
                , authorityPort = 50051
                }

    clientRun $ Client.withConnection clientParams clientServer

{-------------------------------------------------------------------------------
  Main entry point: run server and client together
-------------------------------------------------------------------------------}

runTestClientServer :: forall a.
     ClientServerConfig
  -> ((forall b. (Client.Connection -> IO b) -> IO b) -> IO a)
  -> [Server.RpcHandler IO]
  -> IO a
runTestClientServer cfg clientRun serverHandlers = do
    logMsgVar <- newMVar []
    let logTracer :: Tracer IO LogMsg
        logTracer = collectLogMsgs logMsgVar

    -- Normally, when a handler throws an exception, that request is simply
    -- aborted, but the server does not shut down. However, for the sake of
    -- testing, if a handler throws an unexpected exception, the test should
    -- fail. We therefore monitor for these exceptions.
    serverHandlerExceptions <- newEmptyTMVarIO
    let serverExceptions :: Tracer IO SomeException
        serverExceptions = arrow $ emit $ \err ->
            atomically $ void $ tryPutTMVar serverHandlerExceptions err

    -- Start server
    serverHandlerLock <- newServerHandlerLock
    server <- async $ do
      runTestServer
        cfg
        serverExceptions
        (contramap ServerLogMsg logTracer)
        serverHandlerLock
        serverHandlers

    -- Start client
    --
    -- We run this in its own thread, so we can catch its exceptions separately
    -- from the one from the server (see below)
    client <- async $
      runTestClient
        cfg
        (contramap ClientLogMsg logTracer)
        clientRun

    -- The server never shuts down under normal circumstances; so we wait for
    -- the client to terminate, then wait for any potential still-running
    -- server handlers to terminate, monitoring for exceptions, and then shut
    -- down the server.
    clientRes <- waitCatch client
    serverRes <- atomically $
                     (Left <$> readTMVar serverHandlerExceptions)
                   `orElse`
                     (Right <$> waitForHandlerTermination serverHandlerLock)
    cancel server

    logMsgs <- reverse <$> readMVar logMsgVar

    -- We are careful to report exceptions from the client and the server
    -- independently: an exception in one can cause an exception in the other,
    -- but if would then show only one of those two exceptions, it might hide
    -- the reason reason for the failure.
    case (serverRes, clientRes) of
      (Right (), Right a) ->
        return a
      (Left serverErr, Right _) ->
        throwIO $ ServerException {
            serverException     = serverErr
          , serverExceptionLogs = logMsgs
          }
      (Right (), Left clientErr) ->
        throwIO $ ClientException {
            clientException     = clientErr
          , clientExceptionLogs = logMsgs
          }
      (Left serverErr, Left clientErr) ->
        throwIO $ DoubleException {
            doubleExceptionServer     = serverErr
          , doubleExceptionClient     = clientErr
          , doubleExceptionAnnotation = logMsgs
          }

data ServerException = ServerException {
      serverException     :: SomeException
    , serverExceptionLogs :: [LogMsg]
    }
  deriving stock (GHC.Generic)
  deriving anyclass (PrettyVal)
  deriving Show via ShowAsPretty ServerException
  deriving Exception via ExceptionWrapper ServerException

instance HasNestedException ServerException where
  getNestedException = serverException

data ClientException = ClientException {
      clientException     :: SomeException
    , clientExceptionLogs :: [LogMsg]
    }
  deriving stock (GHC.Generic)
  deriving anyclass (PrettyVal)
  deriving Show via ShowAsPretty ClientException
  deriving Exception via ExceptionWrapper ClientException

instance HasNestedException ClientException where
  getNestedException = clientException

data DoubleException e = forall a. (Show a, PrettyVal a) => DoubleException {
      doubleExceptionClient     :: e
    , doubleExceptionServer     :: e
    , doubleExceptionAnnotation :: a
    }
  deriving anyclass (Exception)
  deriving Show via ShowAsPretty (DoubleException e)

instance PrettyVal e => PrettyVal (DoubleException e) where
  prettyVal DoubleException{ doubleExceptionClient
                           , doubleExceptionServer
                           , doubleExceptionAnnotation
                           } =
     Rec "DoubleException" [
         ("doubleExceptionClient", prettyVal doubleExceptionClient)
       , ("doubleExceptionServer", prettyVal doubleExceptionServer)
       , ("doubleExceptionAnnotation", prettyVal doubleExceptionAnnotation)
       ]




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
  , isExpectedException
    -- * Run
  , runTestClientServer
    -- ** Lower-level functionality
  , ServerHandlerLock -- opaque
  , newServerHandlerLock
  , runTestClient
  , runTestServer
  ) where

import Control.Exception (throwIO)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Tracer
import Data.ByteString qualified as Strict (ByteString)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Network.TLS

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression (CompressionNegotationFailed)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Internal
import Network.GRPC.Internal.XIO (NeverThrows)
import Network.GRPC.Internal.XIO qualified as XIO
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server

import Debug.Concurrent

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

      -- | Override content-type used by the client
    , clientContentType :: Maybe Strict.ByteString

      -- | Override content-type used by the server
    , serverContentType :: Maybe Strict.ByteString
    }

instance Default ClientServerConfig where
  def = ClientServerConfig {
      clientCompr       = def
    , serverCompr       = def
    , useTLS            = Nothing
    , clientContentType = Nothing
    , serverContentType = Nothing
    }

{-------------------------------------------------------------------------------
  We collect log messages from both the client and the server
-------------------------------------------------------------------------------}

data LogMsg =
    ServerLogMsg Server.ServerDebugMsg
  | ClientLogMsg Client.ClientDebugMsg
  deriving stock (Show, GHC.Generic)

collectLogMsgs :: (MonadIO m) => MVar [a] -> Tracer m a
collectLogMsgs v = arrow $ emit $ \x -> liftIO $
    modifyMVar_ v $ \xs -> return (x:xs)

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
  | ExpectedExceptionPeer PeerException
  | ExpectedExceptionDouble (DoubleException (ExpectedException e))
  | ExpectedExceptionCustom e
  deriving stock (Show, GHC.Generic)

data UnexpectedException = UnexpectedException {
      -- | The top-level exception (including any wrapper exceptions)
      unexpectedExceptionTopLevel :: SomeException

      -- | The (possibly nested) exception that was actually unexpected
    , unexpectedExceptionNested :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Check if the given configuration gives rise to expected exceptions
--
-- Returns 'Right' the expected exception if any (with any wrapper exceptions
-- removed), 'Left' some (possibly nested) exception if the exception was
-- unexpected.
--
-- NOTE: We /never/ expect the 'KilledByHttp2ThreadManager' exception from
-- @http2@, as we protect the handler from this; see 'runHandler' for details
-- (TL;DR: /if/ the handler tries to communicate after an
-- 'KilledByHttp2ThreadManager' exception, it will find that the channel has
-- been closed).
isExpectedException :: forall e.
     ClientServerConfig
  -> (SomeException -> Maybe e)
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

      -- Expected exception: TLS

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

      -- Expected exception: compression

      -- .. client side
      | Just (grpc :: GrpcException) <- fromException err
      , compressionNegotationFailure
      , GrpcUnknown <- grpcError grpc
      , Just msg <- grpcErrorMessage grpc
      , "CompressionNegotationFailed" `Text.isInfixOf` msg
      = Right $ ExpectedExceptionGrpc grpc

      -- .. server side
      | Just (Server.CallSetupFailed err') <- fromException err
      , Just (compr :: CompressionNegotationFailed) <- fromException err'
      , compressionNegotationFailure
      = Right $ ExpectedExceptionCompressionNegotationFailed compr

      -- Expected exception: invalid content-type sent by client

      -- .. client side
      | Just (grpc :: GrpcException) <- fromException err
      , invalidClientContentType
      , GrpcUnknown <- grpcError grpc
      , Just msg <- grpcErrorMessage grpc
      , "Content-Type" `Text.isInfixOf` msg
      = Right $ ExpectedExceptionGrpc grpc

      -- .. server side
      | Just (Server.CallSetupFailed err') <- fromException err
      , Just err''@(PeerSentInvalidHeaders msg) <- fromException err'
      , invalidClientContentType
      , "Content-Type" `List.isInfixOf` msg
      = Right $ ExpectedExceptionPeer err''

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
          Just err' -> Right $ ExpectedExceptionCustom err'
          Nothing   -> Left $ UnexpectedException topLevel err

    compressionNegotationFailure :: Bool
    compressionNegotationFailure =
        Set.disjoint (Map.keysSet (Compr.supported (clientCompr cfg)))
                     (Map.keysSet (Compr.supported (serverCompr cfg)))

    -- TODO: By rights /any/ format (not just "gibberish") should be ok
    invalidClientContentType :: Bool
    invalidClientContentType = not $
        case clientContentType cfg of
          Nothing                           -> True
          Just "application/grpc"           -> True
          Just "application/grpc+gibberish" -> True
          _otherwise                        -> False

{-------------------------------------------------------------------------------
  Server handler lock

  We don't want to terminate the test when some of the server handlers are
  still running; this will result in those handlers being killed with a
  'KilledByHttp2ThreadManager' exception, confusing the test results.

  Moreover, when a handler throws an exception, that request is simply aborted,
  but the server does not shut down. However, for the sake of testing, if a
  handler throws an unexpected exception, the test should fail. We therefore
  monitor for these exceptions.
-------------------------------------------------------------------------------}

data HandlerState =
    HandlerActive
  | HandlerDone
  | HandlerError SomeException
  deriving stock (Show)

newtype ServerHandlerLock = ServerHandlerLock (TVar (Map ThreadId HandlerState))

newServerHandlerLock :: IO ServerHandlerLock
newServerHandlerLock = ServerHandlerLock <$> newTVarIO Map.empty

waitForHandlerTermination :: ServerHandlerLock -> STM (Either SomeException ())
waitForHandlerTermination (ServerHandlerLock lock) = do
    go [] =<< Map.toList <$> readTVar lock
  where
    -- We only want to retry if no handlers errored out
    go ::
         [ThreadId]                  -- Active
      -> [(ThreadId, HandlerState)]  -- Still to consider
      -> STM (Either SomeException ())
    go active []           = if null active
                               then return $ Right ()
                               else retry
    go active ((h, st):hs) = case st of
                               HandlerActive  -> go (h:active) hs
                               HandlerDone    -> go    active  hs
                               HandlerError e -> return $ Left e

topLevelWithHandlerLock ::
     ServerHandlerLock
  -> Server.RequestHandler SomeException ()
  -> Server.RequestHandler NeverThrows   ()
topLevelWithHandlerLock (ServerHandlerLock lock) handler req respond = do
    tid <- XIO.unsafeNeverThrowsIO $
      myThreadId
    XIO.unsafeNeverThrowsIO $
      atomically $ modifyTVar lock $ Map.insert tid HandlerActive
    res <- XIO.tryError $
      handler req respond
    XIO.unsafeNeverThrowsIO $
      atomically $
        modifyTVar lock $ Map.insert tid $
          case res of
            Left err -> HandlerError err
            Right () -> HandlerDone

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runTestServer ::
     ClientServerConfig
  -> Tracer IO Server.ServerDebugMsg
  -> ServerHandlerLock
  -> [Server.RpcHandler IO]
  -> IO ()
runTestServer cfg serverTracer handlerLock serverHandlers = do
    pubCert <- getDataFileName "grpc-demo.pem"
    privKey <- getDataFileName "grpc-demo.key"

    let serverConfig :: Server.ServerConfig
        serverConfig =
            case useTLS cfg of
              Nothing -> Server.ServerConfig {
                  serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Nothing
                    , insecurePort = 50051
                    }
                , serverSecure   = Nothing
                }
              Just (TlsFail TlsFailUnsupported) -> Server.ServerConfig {
                  serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Nothing
                    , insecurePort = 50052
                    }
                , serverSecure   = Nothing
                }
              Just _tlsSetup -> Server.ServerConfig {
                  serverInsecure = Nothing
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
              serverCompression = serverCompr cfg
            , serverDebugTracer = serverTracer
            , serverTopLevel    = topLevelWithHandlerLock handlerLock
            , serverContentType = serverContentType cfg
            }

    Server.runServerWithHandlers serverConfig serverParams serverHandlers

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

runTestClient :: forall a.
     ClientServerConfig
  -> Tracer IO Client.ClientDebugMsg
  -> ((forall b. (Client.Connection -> IO b) -> IO b) -> IO a)
  -> IO a
runTestClient cfg clientTracer clientRun = do
    pubCert <- getDataFileName "grpc-demo.pem"

    let clientParams :: Client.ConnParams
        clientParams = Client.ConnParams {
              connDebugTracer    = clientTracer
            , connCompression    = clientCompr cfg
            , connDefaultTimeout = Nothing
            , connContentType    = clientContentType cfg

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

        clientAuthority :: Client.Address
        clientAuthority =
            case useTLS cfg of
              Just tlsSetup -> Client.Address {
                  addressHost      = case tlsSetup of
                                       TlsFail TlsFailHostname -> "127.0.0.1"
                                       _otherwise              -> "localhost"
                , addressPort      = 50052
                , addressAuthority = Nothing
                }

              Nothing -> Client.Address {
                  addressHost      = "localhost"
                , addressPort      = 50051
                , addressAuthority = Nothing
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

    -- Start server
    serverHandlerLock <- newServerHandlerLock
    server <- async $ do
      runTestServer
        cfg
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
    serverRes <- atomically $ waitForHandlerTermination serverHandlerLock
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
  deriving stock (Show, GHC.Generic)
  deriving Exception via ExceptionWrapper ServerException

instance HasNestedException ServerException where
  getNestedException = serverException

data ClientException = ClientException {
      clientException     :: SomeException
    , clientExceptionLogs :: [LogMsg]
    }
  deriving stock (Show, GHC.Generic)
  deriving Exception via ExceptionWrapper ClientException

instance HasNestedException ClientException where
  getNestedException = clientException

data DoubleException e = forall a. Show a => DoubleException {
      doubleExceptionClient     :: e
    , doubleExceptionServer     :: e
    , doubleExceptionAnnotation :: a
    }
  deriving anyclass (Exception)

deriving stock instance Show e => Show (DoubleException e)


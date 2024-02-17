{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Util.ClientServer (
    -- * Configuraiton
    ClientServerConfig(..)
  , ContentTypeOverride(..)
    -- ** TLS
  , TlsSetup(..)
  , TlsOk(..)
  , TlsFail(..)
    -- ** Evaluation
  , ExpectedException(..)
  , UnexpectedException(..)
  , DeliberateException(..)
  , isExpectedException
    -- * Run
  , ClientServerTest(..)
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Network.HTTP.Types qualified as HTTP
import Network.TLS

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
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

data ContentTypeOverride =
    -- | Use the default content-type
    NoOverride

    -- | Override with a valid alternative content-type
    --
    -- It is the responsibility of the test to make sure that this content-type
    -- is in fact valid.
  | ValidOverride Strict.ByteString

    -- | Override with an invalid content-type
  | InvalidOverride Strict.ByteString

data ClientServerConfig = ClientServerConfig {
      -- | Compression algorithms supported by the client
      clientCompr :: Compr.Negotation

      -- | Initial compression algorithm used by the client (if any)
    , clientInitCompr :: Maybe Compr.Compression

      -- | Compression algorithms supported the server
    , serverCompr :: Compr.Negotation

      -- | TLS setup (if using)
    , useTLS :: Maybe TlsSetup

      -- | Override content-type used by the client
    , clientContentType :: ContentTypeOverride

      -- | Override content-type used by the server
    , serverContentType :: ContentTypeOverride

      -- | Should we expect any clients to terminate early?
      --
      -- \"Termination\" here could be either normal termination (no exception,
      -- but not properly closing the RPC call either) or abnormal termination
      -- (throwing an exception).
    , expectEarlyClientTermination :: Bool

      -- | Should we expect any server handlers to terminate early?
      --
      -- Same comments for \"termination\" apply.
    , expectEarlyServerTermination :: Bool
    }

instance Default ClientServerConfig where
  def = ClientServerConfig {
      clientCompr       = def
    , clientInitCompr   = Nothing
    , serverCompr       = def
    , useTLS            = Nothing
    , clientContentType = NoOverride
    , serverContentType = NoOverride
    , expectEarlyClientTermination = False
    , expectEarlyServerTermination = False
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
  Expected exceptions

  Ideally 'isExpectedServerException' and 'isExpectedClientException' should
  have identical structure, illustrating that the exceptions are consistent
  between the server API and the client API. This ideal isn't /quite/ reached;
  for example, if a server handler throws an exception, the client is informed,
  but the reverse is not true (the client simply disconnects).

  TODO: Early server termination does not result in an exception server-side.
  This is inconsistent.
-------------------------------------------------------------------------------}

isExpectedServerException :: ClientServerConfig -> SomeException -> Bool
isExpectedServerException cfg e
  --
  -- Deliberate exceptions
  --

  | Just (DeliberateException _) <- fromException e'
  = True

  --
  -- Early client termination
  --

  | Just Server.ClientDisconnected{} <- fromException e'
  , expectEarlyClientTermination cfg
  = True

  --
  -- Call setup failure
  --

  | Just Server.CallSetupInvalidRequestHeaders{} <- fromException e'
  , InvalidOverride _ <- clientContentType cfg
  = True

  --
  -- Compression negotation
  --

  | Just Server.CallSetupUnsupportedCompression{} <- fromException e'
  , compressionNegotationFailure cfg
  = True

  --
  -- Fall-through
  --

  | otherwise
  = False
  where
    e' = innerNestedException e

isExpectedClientException :: ClientServerConfig -> SomeException -> Bool
isExpectedClientException cfg e
  --
  -- Deliberate exceptions
  --

  -- Client threw deliberate exception
  | Just (DeliberateException _) <- fromException e'
  = True

  -- Server threw deliberat exception
  | Just grpcException <- fromException e'
  , Just msg <- grpcErrorMessage grpcException
  , "DeliberateException" `Text.isInfixOf` msg
  = True

  --
  -- Early client termination
  --

  | Just ChannelDiscarded{} <- fromException e'
  , expectEarlyClientTermination cfg
  = True

  --
  -- Early server termination
  --

  | Just grpcException <- fromException e'
  , Just msg <- grpcErrorMessage grpcException
  , "HandlerTerminated" `Text.isInfixOf` msg
  , expectEarlyServerTermination cfg
  = True

  --
  -- Call setup failure
  --
  -- TODO: Perhaps we should verify the contents of the body.
  --

  | Just (Client.CallSetupUnexpectedStatus status _body) <- fromException e'
  , InvalidOverride _ <- clientContentType cfg
  , status == HTTP.badRequest400
  = True

  --
  -- Compression negotation
  --

  -- Client choose unsupported compression
  | Just (Client.CallSetupUnexpectedStatus status _body) <- fromException e'
  , compressionNegotationFailure cfg
  , status == HTTP.badRequest400
  = True

  -- Server chose unsupported compression
  | Just Client.CallSetupUnsupportedCompression{} <- fromException e'
  , compressionNegotationFailure cfg
  = True

  --
  -- Fall-through
  --

  | otherwise
  = False
  where
    e' = innerNestedException e

compressionNegotationFailure :: ClientServerConfig -> Bool
compressionNegotationFailure cfg = or [
      Set.disjoint clientSupported serverSupported
    , case clientInitCompr cfg of
        Nothing    -> False
        Just compr -> Compr.compressionId compr `Set.notMember` serverSupported
    ]
  where
    clientSupported, serverSupported :: Set Compr.CompressionId
    clientSupported = Map.keysSet (Compr.supported (clientCompr cfg))
    serverSupported = Map.keysSet (Compr.supported (serverCompr cfg))

{-------------------------------------------------------------------------------
  Config evaluation (do we expect an error?)
-------------------------------------------------------------------------------}

data ExpectedException =
    ExpectedExceptionTls TLSException
  | ExpectedExceptionGrpc GrpcException
  | ExpectedExceptionPeer PeerException
  | ExpectedExceptionDouble (DoubleException ExpectedException)
  | ExpectedExceptionDeliberate DeliberateException
  deriving stock (Show, GHC.Generic)

data UnexpectedException = UnexpectedException {
      -- | The top-level exception (including any wrapper exceptions)
      unexpectedExceptionTopLevel :: SomeException

      -- | The (possibly nested) exception that was actually unexpected
    , unexpectedExceptionNested :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Exception thrown by client or handler to test exception handling
data DeliberateException = forall e. Exception e => DeliberateException e
  deriving anyclass (Exception)

deriving stock instance Show DeliberateException

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
isExpectedException ::
     ClientServerConfig
  -> SomeException
  -> Either UnexpectedException ExpectedException
isExpectedException cfg topLevel =
    go topLevel
  where
    go :: SomeException -> Either UnexpectedException ExpectedException
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
      -- Fall-through
      --

      | otherwise
      = Left $ UnexpectedException topLevel err

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
     ClientServerConfig
  -> ServerHandlerLock
  -> Server.RequestHandler SomeException ()
  -> Server.RequestHandler NeverThrows   ()
topLevelWithHandlerLock cfg (ServerHandlerLock lock) handler req respond = do
    tid <- XIO.unsafeTrustMe $ myThreadId

    let markActive, markDone :: XIO.XIO' NeverThrows ()
        markActive = XIO.unsafeTrustMe $
            atomically $ modifyTVar lock $ Map.insert tid HandlerActive
        markDone = XIO.unsafeTrustMe $
            atomically $ modifyTVar lock $ Map.insert tid $ HandlerDone

        markError :: SomeException -> XIO.XIO' NeverThrows ()
        markError err = XIO.unsafeTrustMe $
            atomically $ modifyTVar lock $ Map.insert tid $ HandlerError err

    markActive
    res <- XIO.tryError $ handler req respond
    case res of
      Right () ->
        markDone
      Left e -> do
        if isExpectedServerException cfg e then
          markDone
        else do
          XIO.swallowIO $ putStrLn $ "Server uhoh: " ++ show (innerNestedException e)
          markError e

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
            , serverTopLevel    = topLevelWithHandlerLock cfg handlerLock
            , serverContentType =
                case serverContentType cfg of
                  NoOverride            -> Nothing
                  ValidOverride   ctype -> Just ctype
                  InvalidOverride ctype -> Just ctype
            }

    Server.runServerWithHandlers serverConfig serverParams serverHandlers

{-------------------------------------------------------------------------------
  Client
-------------------------------------------------------------------------------}

runTestClient :: forall a.
     ClientServerConfig
  -> Tracer IO Client.ClientDebugMsg
  -> (((Client.Connection -> IO ()) -> IO ()) -> IO a)
  -> IO a
runTestClient cfg clientTracer clientRun = do
    pubCert <- getDataFileName "grpc-demo.pem"

    let clientParams :: Client.ConnParams
        clientParams = Client.ConnParams {
              connDebugTracer     = clientTracer
            , connCompression     = clientCompr cfg
            , connInitCompression = clientInitCompr cfg
            , connDefaultTimeout  = Nothing

              -- Content-type
            , connContentType =
                case clientContentType cfg of
                  NoOverride            -> Nothing
                  ValidOverride   ctype -> Just ctype
                  InvalidOverride ctype -> Just ctype

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

        withConn :: (Client.Connection -> IO ()) -> IO ()
        withConn k = do
            mb :: Either SomeException b <-
              try $ Client.withConnection clientParams clientServer k
            case mb of
              Right b -> return b
              Left e ->
                if isExpectedClientException cfg e
                  then return ()
                  else do
                    putStrLn $ "Client uhoh: " ++ show (innerNestedException e)
                    throwIO e

    clientRun withConn

{-------------------------------------------------------------------------------
  Main entry point: run server and client together
-------------------------------------------------------------------------------}

data ClientServerTest a = ClientServerTest {
      config :: ClientServerConfig
    , client :: ((Client.Connection -> IO ()) -> IO ()) -> IO a
    , server :: [Server.RpcHandler IO]
    }

runTestClientServer :: forall a. ClientServerTest a -> IO a
runTestClientServer (ClientServerTest cfg clientRun serverHandlers) = do
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


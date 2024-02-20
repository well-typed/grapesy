{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Driver.ClientServer (
    ClientServerTest(..)
  , testClientServer
  , propClientServer
    -- * Configuration
  , ClientServerConfig(..)
  , ContentTypeOverride(..)
  , TlsSetup(..)
  , TlsFail(..)
  , TlsOk(..)
    -- * Constructing clients
  , TestClient
  , simpleTestClient
    -- * Exception handling
  , DeliberateException(..)
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Tracer
import Data.ByteString qualified as Strict (ByteString)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server qualified as HTTP2.Server
import Network.TLS
import Test.QuickCheck.Monadic qualified as QuickCheck
import Test.Tasty.QuickCheck qualified as QuickCheck

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Internal.XIO (NeverThrows)
import Network.GRPC.Internal.XIO qualified as XIO
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server

import Paths_grapesy

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Run client server test, and check for expected failures
testClientServer :: ClientServerTest -> IO ()
testClientServer test =
    runTestClientServer test

-- | Turn client server test into property
propClientServer :: IO ClientServerTest -> QuickCheck.Property
propClientServer mkTest =
    QuickCheck.monadicIO $ liftIO $ runTestClientServer =<< mkTest

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

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
  between the server API and the client API. This ideal isn't /quite/ reached:

  * If a server handler throws an exception, the client is informed,
    but the reverse is not true (the client simply disconnects).
  * TLS exceptions are thrown to the client, but since no handler is ever run,
    we don't see these exceptions server-side.

  TODO: Early server termination does not result in an exception server-side.
  This is inconsistent.
-------------------------------------------------------------------------------}

-- | Exception thrown by client or handler to test exception handling
data DeliberateException = forall e. Exception e => DeliberateException e
  deriving anyclass (Exception)

deriving stock instance Show DeliberateException

isExpectedServerException :: ClientServerConfig -> SomeException -> Bool
isExpectedServerException cfg e
  --
  -- Deliberate exceptions
  --

  | Just (DeliberateException _) <- fromException e
  = True

  --
  -- Early client termination
  --

  | Just Server.ClientDisconnected{} <- fromException e
  , expectEarlyClientTermination cfg
  = True

  --
  -- Call setup failure
  --

  | Just Server.CallSetupInvalidRequestHeaders{} <- fromException e
  , InvalidOverride _ <- clientContentType cfg
  = True

  --
  -- Compression negotation
  --

  | Just Server.CallSetupUnsupportedCompression{} <- fromException e
  , compressionNegotationFailure cfg
  = True

  --
  -- Fall-through
  --

  | otherwise
  = False

isExpectedClientException :: ClientServerConfig -> SomeException -> Bool
isExpectedClientException cfg e
  --
  -- Deliberate exceptions
  --

  -- Client threw deliberate exception
  | Just (DeliberateException _) <- fromException e
  = True

  -- Server threw deliberat exception
  | Just grpcException <- fromException e
  , Just msg <- grpcErrorMessage grpcException
  , "DeliberateException" `Text.isInfixOf` msg
  = True

  --
  -- Early client termination
  --

  | Just ChannelDiscarded{} <- fromException e
  , expectEarlyClientTermination cfg
  = True

  --
  -- Early server termination
  --

  | Just grpcException <- fromException e
  , Just msg <- grpcErrorMessage grpcException
  , "HandlerTerminated" `Text.isInfixOf` msg
  , expectEarlyServerTermination cfg
  = True

  --
  -- Call setup failure
  --
  -- TODO: Perhaps we should verify the contents of the body.
  --

  | Just (Client.CallSetupUnexpectedStatus status _body) <- fromException e
  , InvalidOverride _ <- clientContentType cfg
  , status == HTTP.badRequest400
  = True

  --
  -- Compression negotation
  --

  -- Client choose unsupported compression
  | Just (Client.CallSetupUnexpectedStatus status _body) <- fromException e
  , compressionNegotationFailure cfg
  , status == HTTP.badRequest400
  = True

  -- Server chose unsupported compression
  | Just Client.CallSetupUnsupportedCompression{} <- fromException e
  , compressionNegotationFailure cfg
  = True

  --
  -- TLS problems
  --

  | Just tls <- fromException e
  , isExpectedTLSException cfg tls
  = True

  --
  -- Fall-through
  --

  | otherwise
  = False

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

isExpectedTLSException :: ClientServerConfig -> TLSException -> Bool
isExpectedTLSException cfg tls =
    case (useTLS cfg, tls) of
#if MIN_VERSION_tls(1,9,0)
      (   Just (TlsFail TlsFailValidation)
        , HandshakeFailed (Error_Protocol _msg UnknownCa)
        ) -> True
      (   Just (TlsFail TlsFailHostname)
        , HandshakeFailed (Error_Protocol _msg CertificateUnknown)
        ) -> True
#else
      (   Just (TlsFail TlsFailValidation)
        , HandshakeFailed (Error_Protocol (_msg, _bool, UnknownCa))
        ) -> True
      (   Just (TlsFail TlsFailHostname)
        , HandshakeFailed (Error_Protocol (_msg, _bool, CertificateUnknown))
        ) -> True
#endif
      (   Just (TlsFail TlsFailUnsupported)
        , HandshakeFailed (Error_Packet_Parsing _)
        ) -> True
      _otherwise ->
        False

{-------------------------------------------------------------------------------
  Test failures
-------------------------------------------------------------------------------}

-- | Test failure
--
-- When a test fails, we want to report the /first/ test failure; anything else
-- might result in difficult to debug test cases, because that first test
-- failure (first exception) might have all kinds of hard-to-predict
-- consequences. This is somewhat tricky to achieve in a concurrent test
-- setting; for example, if a server handler throws an exception, this exception
-- will be raised in the client also, but we want a guarantee that we see the
-- /handler/ exception, not the client one. We therefore wrap every client test
-- and every handler in an exception wrapper which, after verifying that the
-- exception was not expected, will write the exception (i.e., the test failure)
-- to a test-wide 'FirstTestFailure'.
--
-- We then run the client in a separate thread, and wait for it to finish, /or/
-- for a test failure to be reported. Doing these two checks independently means
-- that if the client deadlocks because of some test failure somewhere else, we
-- don't wait but instead report the test failure. In the client we throw
-- 'TestFailure' if we do see a test failure; this helps in tests where we run
-- multiple clients, as it signals that there is no point waiting for the other
-- tests to terminate.
--
-- In a similar fashion we then wait for all handlers to terminate also or,
-- again, for some test failure to be reported. (In this case the exception is
-- not rethrown, because handlers should never throw at all.)
--
-- Finally, we check 'FirstTestFailure', report failure if it's set, or test
-- success otherwise.
--
-- Note: if we have multiple independent clients, running independent tests,
-- then we have multiple concurrent test failures, there /is/ no clear notion
-- of a \"first\" test failure. However, in this case which exception we report
-- as \"the\" test failure is not very important; by definition, in this case
-- the one exception cannot be the /cause/ for the other exception (if it was,
-- then one must happen /before/ the other).
newtype FirstTestFailure = FirstTestFailure (TMVar SomeException)

data TestFailure = TestFailure
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Mark test failure
--
-- Does nothing if an earlier test failure has already been marked.
markTestFailure :: FirstTestFailure -> SomeException  -> IO ()
markTestFailure (FirstTestFailure firstTestFailure) err =
    void $ atomically $ tryPutTMVar firstTestFailure err

{-------------------------------------------------------------------------------
  Server handler lock
-------------------------------------------------------------------------------}

-- |  Server handler lock
--
-- Handlers are initiated by calls from clients, but may outlive the connection
-- to the client. Therefore, after we wait for the clients to terminate, we
-- should also wait for all handlers to terminate.
--
-- See 'FirstTestFailure' for discussion of handler exceptions.
newtype ServerHandlerLock = ServerHandlerLock (TVar Int)

newServerHandlerLock :: IO ServerHandlerLock
newServerHandlerLock = ServerHandlerLock <$> newTVarIO 0

waitForHandlerTermination :: ServerHandlerLock -> STM ()
waitForHandlerTermination (ServerHandlerLock lock) = do
    activeHandlers <- readTVar lock
    when (activeHandlers > 0) retry

topLevelWithHandlerLock ::
     ClientServerConfig
  -> FirstTestFailure
  -> ServerHandlerLock
  -> Server.RequestHandler SomeException ()
  -> Server.RequestHandler NeverThrows   ()
topLevelWithHandlerLock cfg firstTestFailure (ServerHandlerLock lock) handler =
    handler'
  where
    handler' ::
         HTTP2.Server.Request
      -> (HTTP2.Server.Response -> IO ())
      -> XIO.XIO' NeverThrows ()
    handler' req respond = do
        markActive
        result <- XIO.tryError $ handler req respond
        XIO.unsafeTrustMe $
          case result of
            Right () ->
              return ()
            Left err | isExpectedServerException cfg err ->
              return ()
            Left err ->
              markTestFailure firstTestFailure err
        markDone

    markActive, markDone :: XIO.XIO' NeverThrows ()
    markActive = XIO.unsafeTrustMe $ atomically $ modifyTVar lock (\n -> n + 1)
    markDone   = XIO.unsafeTrustMe $ atomically $ modifyTVar lock (\n -> n - 1)

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

runTestServer ::
     ClientServerConfig
  -> Tracer IO Server.ServerDebugMsg
  -> FirstTestFailure
  -> ServerHandlerLock
  -> [Server.RpcHandler IO]
  -> IO ()
runTestServer cfg serverTracer firstTestFailure handlerLock serverHandlers = do
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
              serverCompression =
                serverCompr cfg
            , serverDebugTracer =
                serverTracer
            , serverTopLevel =
                topLevelWithHandlerLock cfg firstTestFailure handlerLock
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

type TestClient =
          Client.ConnParams
          -- ^ Test-appropriate connection parameters
       -> Client.Server
          -- ^ Test server to connect to
       -> (IO () -> IO ())
          -- ^ Delimit test scope
          --
          -- Any test failures will be limited to this scope. Important when
          -- running multiple tests.
       -> IO ()

simpleTestClient :: (Client.Connection -> IO ()) -> TestClient
simpleTestClient test params testServer delimitTestScope =
    Client.withConnection params testServer $ \conn ->
      delimitTestScope $ test conn

runTestClient ::
     ClientServerConfig
  -> Tracer IO Client.ClientDebugMsg
  -> FirstTestFailure
  -> TestClient
  -> IO ()
runTestClient cfg clientTracer firstTestFailure clientRun = do
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

        delimitTestScope :: IO () -> IO ()
        delimitTestScope test = do
            result :: Either SomeException () <- try test
            case result of
              Right () ->
                return ()
              Left err | isExpectedClientException cfg err ->
                return ()
              Left err -> do
                markTestFailure firstTestFailure err
                throwIO TestFailure

    clientRun clientParams clientServer delimitTestScope

{-------------------------------------------------------------------------------
  Main entry point: run server and client together
-------------------------------------------------------------------------------}

data ClientServerTest = ClientServerTest {
      config :: ClientServerConfig
    , client :: TestClient
    , server :: [Server.RpcHandler IO]
    }

runTestClientServer :: ClientServerTest -> IO ()
runTestClientServer (ClientServerTest cfg clientRun serverHandlers) = do
    -- Setup client and server
    logMsgVar         <- newMVar []
    firstTestFailure  <- newEmptyTMVarIO
    serverHandlerLock <- newServerHandlerLock

    let server :: IO ()
        server =
          runTestServer
            cfg
            (contramap ServerLogMsg $ collectLogMsgs logMsgVar)
            (FirstTestFailure firstTestFailure)
            serverHandlerLock
            serverHandlers

    let client :: IO ()
        client =
          runTestClient
            cfg
            (contramap ClientLogMsg $ collectLogMsgs logMsgVar)
            (FirstTestFailure firstTestFailure)
            clientRun

    -- Run the test
    testResult :: Maybe SomeException <-
      -- Start the server
      withAsync server $ \_serverThread ->
        -- Start the client
        withAsync client $ \clientThread -> do
          -- Wait for clients to terminate (or test failure)
          atomically $
              (void $ waitCatchSTM clientThread)
            `orElse`
              (void $ readTMVar firstTestFailure)

          -- Wait for handlers to terminate (or test failure)
          -- (Note that the server /itself/ normally never terminates)
          atomically $
              (waitForHandlerTermination serverHandlerLock)
            `orElse`
              (void $ readTMVar firstTestFailure)

          -- Get the test result, and leave the scope of both @withAsync@ calls
          -- (thereby killing the client and server if they are still running)
          atomically $ tryReadTMVar firstTestFailure

    -- Provide additional information in case of a test failure
    case testResult of
      Nothing      -> return () -- Test passed
      Just failure -> do
        logMsgs <- reverse <$> readMVar logMsgVar
        throwIO $ ClientServerFailure logMsgs failure

data ClientServerFailure = ClientServerFailure {
      clientServerLogs    :: [LogMsg]
    , clientServerFailure :: SomeException
    }
  deriving stock (Show)
  deriving anyclass (Exception)


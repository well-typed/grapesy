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
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Network.HTTP2.Server qualified as HTTP2.Server
import Network.Socket (PortNumber)
import Network.TLS
import Test.QuickCheck.Monadic qualified as QuickCheck
import Test.Tasty.QuickCheck qualified as QuickCheck

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server qualified as Server
import Network.GRPC.Server.Run qualified as Server
import Test.Util.Exception

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
      -- | Port number used by the server
      --
      -- The client will query the server for its port; this makes it possible
      -- to use @0@ for 'serverPort', so that the server picks a random
      -- available port (this is the default).
      serverPort :: PortNumber

      -- | Compression algorithms supported by the client
    , clientCompr :: Compr.Negotation

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
  | ValidOverride Server.ContentType

    -- | Override with an invalid (possibly missing) content-type
  | InvalidOverride (Maybe Server.ContentType)

instance Default ClientServerConfig where
  def = ClientServerConfig {
        serverPort        = 0
      , clientCompr       = def
      , clientInitCompr   = Nothing
      , serverCompr       = def
      , useTLS            = Nothing
      , clientContentType = NoOverride
      , serverContentType = NoOverride
      , expectEarlyClientTermination = False
      , expectEarlyServerTermination = False
      }

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
-------------------------------------------------------------------------------}

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
  -- Early server termination
  --

  | Just Server.HandlerTerminated{} <- fromException e
  , expectEarlyServerTermination cfg
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

  -- Server threw deliberate exception
  | Just grpcException <- fromException e
  , Just msg <- grpcErrorMessage grpcException
  , "DeliberateException" `Text.isInfixOf` msg
  = True

  --
  -- Early client termination
  --

  | Just grpcException <- fromException e
  , expectEarlyClientTermination cfg
  , grpcError grpcException == GrpcCancelled
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

  | Just grpcException <- fromException e
  , GrpcUnknown <- grpcError grpcException
  , Just msg <- grpcErrorMessage grpcException
  , "415" `Text.isInfixOf` msg
  , InvalidOverride _ <- clientContentType cfg
  = True

  --
  -- Compression negotation
  --

  -- Client choose unsupported compression
  --
  -- We respond with 400 Bad Request, which gets turned into GrpcInternal
  -- by 'classifyServerResponse'.
  | Just grpcException <- fromException e
  , GrpcInternal <- grpcError grpcException
  , compressionNegotationFailure cfg
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
      (Just (TlsFail TlsFailValidation)  , HandshakeFailed _) -> True
      (Just (TlsFail TlsFailUnsupported) , HandshakeFailed _) -> True
      _otherwise -> False

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
  -> Server.RequestHandler ()
  -> Server.RequestHandler ()
topLevelWithHandlerLock cfg
                        firstTestFailure
                        (ServerHandlerLock lock)
                        handler
                        unmask =
    handler'
  where
    handler' ::
         HTTP2.Server.Request
      -> (HTTP2.Server.Response -> IO ())
      -> IO ()
    handler' req respond = do
        markActive
        result <- try $ handler unmask req respond
        case result of
          Right () ->
            return ()
          Left err | isExpectedServerException cfg err ->
            return ()
          Left err ->
            markTestFailure firstTestFailure err
        markDone

    markActive, markDone :: IO ()
    markActive = atomically $ modifyTVar lock (\n -> n + 1)
    markDone   = atomically $ modifyTVar lock (\n -> n - 1)

{-------------------------------------------------------------------------------
  Server
-------------------------------------------------------------------------------}

withTestServer ::
     ClientServerConfig
  -> FirstTestFailure
  -> ServerHandlerLock
  -> [Server.SomeRpcHandler IO]
  -> (Server.RunningServer -> IO a)
  -> IO a
withTestServer cfg firstTestFailure handlerLock serverHandlers k = do
    pubCert <- getDataFileName "grpc-demo.pem"
    privKey <- getDataFileName "grpc-demo.key"

    let serverConfig :: Server.ServerConfig
        serverConfig =
            case useTLS cfg of
              Nothing -> Server.ServerConfig {
                  serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Just "127.0.0.1"
                    , insecurePort = serverPort cfg
                    }
                , serverSecure   = Nothing
                }
              Just (TlsFail TlsFailUnsupported) -> Server.ServerConfig {
                  serverInsecure = Just Server.InsecureConfig {
                      insecureHost = Just "127.0.0.1"
                    , insecurePort = serverPort cfg
                    }
                , serverSecure   = Nothing
                }
              Just _tlsSetup -> Server.ServerConfig {
                  serverInsecure = Nothing
                , serverSecure   = Just $ Server.SecureConfig {
                      secureHost       = "127.0.0.1"
                    , securePort       = serverPort cfg
                    , securePubCert    = pubCert
                    , secureChainCerts = []
                    , securePrivKey    = privKey
                    , secureSslKeyLog  = SslKeyLogNone
                    }
                }

        serverParams :: Server.ServerParams
        serverParams = def {
              Server.serverCompression =
                serverCompr cfg
            , Server.serverTopLevel =
                topLevelWithHandlerLock cfg firstTestFailure handlerLock
            , Server.serverContentType =
                case serverContentType cfg of
                  NoOverride            -> Just Server.ContentTypeDefault
                  ValidOverride   ctype -> Just ctype
                  InvalidOverride ctype -> ctype
            , Server.serverVerifyHeaders =
                -- We want to check that we can spot invalid headers
                -- (and that we don't generate any in the client)
                True
            }

    server <- Server.mkGrpcServer serverParams serverHandlers
    Server.forkServer def serverConfig server k

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
  -> FirstTestFailure
  -> PortNumber
  -> TestClient
  -> IO ()
runTestClient cfg firstTestFailure port clientRun = do
    pubCert <- getDataFileName "grpc-demo.pem"

    let clientParams :: Client.ConnParams
        clientParams = Client.ConnParams {
              connCompression           = clientCompr cfg
            , connInitCompression       = clientInitCompr cfg
            , connDefaultTimeout        = Nothing
            , connVerifyHeaders         = True
            , connHTTP2Settings         = defaultHTTP2Settings

              -- Content-type
            , connContentType =
                case clientContentType cfg of
                  NoOverride            -> Just Server.ContentTypeDefault
                  ValidOverride   ctype -> Just ctype
                  InvalidOverride ctype -> ctype

              -- We need a single reconnect, to enable wait-for-ready.
              -- This avoids a race condition between the server starting first
              -- and the client starting first.
            , connReconnectPolicy =
                  Client.ReconnectAfter def $ do
                    threadDelay 100_000
                    return Client.DontReconnect
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
              Just _tlsSetup -> Client.Address {
                  addressHost      = "127.0.0.1"
                , addressPort      = port
                , addressAuthority = Nothing
                }

              Nothing -> Client.Address {
                  addressHost      = "127.0.0.1"
                , addressPort      = port
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
    , server :: [Server.SomeRpcHandler IO]
    }

runTestClientServer :: ClientServerTest -> IO ()
runTestClientServer (ClientServerTest cfg clientRun handlers) = do
    -- Setup client and server
    firstTestFailure  <- FirstTestFailure <$> newEmptyTMVarIO
    serverHandlerLock <- newServerHandlerLock

    let server :: (Server.RunningServer -> IO a) -> IO a
        server = withTestServer cfg firstTestFailure serverHandlerLock handlers

    let client :: PortNumber -> IO ()
        client port = runTestClient cfg firstTestFailure port clientRun

    -- Run the test
    server $ \runningServer -> do
      port <- Server.getServerPort runningServer

      withAsync (client port) $ \clientThread -> do
        let failure = waitForFailure runningServer clientThread firstTestFailure

        -- Wait for client to terminate (or test failure)
        -- (the 'orElse' is only relevant if a /handler/ throws an exception)
        atomically $
            (void $ waitCatchSTM clientThread)
          `orElse`
            (void failure)

        -- Wait for handlers to terminate (or test failure)
        -- (Note that the server /itself/ normally never terminates)
        atomically $
            (waitForHandlerTermination serverHandlerLock)
          `orElse`
            (void failure)

        atomically $ do
            (failure >>= throwSTM)
          `orElse`
            return ()

-- | Wait for test failure (retries/blocks if tests have not yet failed)
--
-- /If/ a first test failure has been reported, we prefer to report it. It is
-- however possible that either the server or the client threw an exception
-- /without/ the 'FirstTestFailure' being populated; for example, this can
-- happen if the server fails to start at all, or if there is a bug in the test
-- framework itself.
waitForFailure ::
     Server.RunningServer  -- ^ Server
  -> Async ()              -- ^ Client
  -> FirstTestFailure      -- ^ First test failure
  -> STM SomeException
waitForFailure server client (FirstTestFailure firstTestFailure) =
      (readTMVar firstTestFailure)
    `orElse`
      (Server.waitServerSTM server >>= serverAux)
    `orElse`
      (waitCatchSTM client >>= clientAux)
  where
    serverAux ::
         ( Either SomeException ()
         , Either SomeException ()
         )
      -> STM SomeException
    serverAux (Left e, _) = return e
    serverAux (_, Left e) = return e
    serverAux _otherwise  = throwSTM $ UnexpectedServerTermination

    clientAux :: Either SomeException () -> STM SomeException
    clientAux (Left e)   = return e
    clientAux _otherwise = retry

-- | We don't expect the server to shutdown until we kill it
data UnexpectedServerTermination = UnexpectedServerTermination
  deriving stock (Show)
  deriving anyclass (Exception)

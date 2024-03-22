module Interop.Cmdline (
    getCmdline
  , defaultCmdline
    -- * Definition
  , Cmdline(..)
  , cmdPort
  , Mode(..)
  , TestCase(..)
  ) where

import Data.Foldable (asum)
import Network.Socket (PortNumber, HostName)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

import Network.GRPC.Common

import Paths_grapesy

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {

      --
      -- Command line arguments used by the gRPC test suite
      --

      cmdMode         :: Mode
    , cmdPortOverride :: Maybe PortNumber
    , cmdUseTLS       :: Bool
    , cmdTestCase     :: Maybe TestCase
    , cmdHost         :: HostName

      -- | @:authority@/SNI hostname override
    , cmdServerHostOverride :: Maybe HostName

      -- | Use test certificate as root CA?
    , cmdUseTestCA :: Bool

      --
      -- Additional command line arguments
      --

    , cmdRootCA    :: FilePath
    , cmdPubCert   :: FilePath
    , cmdPrivKey   :: FilePath
    , cmdSslKeyLog :: SslKeyLog

    , cmdTimeoutTest    :: Int
    , cmdTimeoutConnect :: Int

    , cmdSkipTest              :: [TestCase]
    , cmdSkipCompression       :: Bool
    , cmdSkipClientCompression :: Bool
    }
  deriving (Show)

cmdPort :: Cmdline -> PortNumber
cmdPort Cmdline{cmdPortOverride, cmdMode, cmdUseTLS} =
    case (cmdPortOverride, cmdMode, cmdUseTLS) of
      (Just port, _, _) -> port
      (_, SelfTest, _)  -> 0
      (_, _, False)     -> defaultInsecurePort
      (_, _, True)      -> defaultSecurePort

data Mode =
    Server   -- ^ Interop server (against reference client)
  | Client   -- ^ Interop client (against reference server)
  | Ping     -- ^ Ping the grapesy server (for debugging connectivity)
  | SelfTest -- ^ Run interop tests against itself
  deriving (Show)

-- | Interop test cases
--
-- The test cases are described at
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md>.
--
-- Currently unsupported:
--
-- * @cacheable_unary@
-- * @compute_engine_creds@
-- * @jwt_token_creds@
-- * @oauth2_auth_token@
-- * @per_rpc_creds@
-- * @google_default_credentials@
-- * @compute_engine_channel_credentials@
-- * @rpc_soak@
-- * @channel_soak@
--
-- None of the reference clients we have tested with support these.
--
-- Also unsupported:
--
-- * @orca_per_rpc@
-- * @orca_oob@
--
-- These /are/ supported by some reference clients, but use features we do not
-- currently provide.
data TestCase =
    EmptyUnary
  | LargeUnary
  | ClientCompressedUnary
  | ServerCompressedUnary
  | ClientStreaming
  | ClientCompressedStreaming
  | ServerStreaming
  | ServerCompressedStreaming
  | PingPong
  | EmptyStream
  | CustomMetadata
  | StatusCodeAndMessage
  | SpecialStatusMessage
  | UnimplementedMethod
  | UnimplementedService
  | CancelAfterBegin
  | CancelAfterFirstResponse
  | TimeoutOnSleepingServer
  deriving (Eq, Enum, Bounded)

instance Show TestCase where
  show EmptyUnary                = "empty_unary"
  show LargeUnary                = "large_unary"
  show ClientCompressedUnary     = "client_compressed_unary"
  show ServerCompressedUnary     = "server_compressed_unary"
  show ClientStreaming           = "client_streaming"
  show ClientCompressedStreaming = "client_compressed_streaming"
  show ServerStreaming           = "server_streaming"
  show ServerCompressedStreaming = "server_compressed_streaming"
  show PingPong                  = "ping_pong"
  show EmptyStream               = "empty_stream"
  show CustomMetadata            = "custom_metadata"
  show StatusCodeAndMessage      = "status_code_and_message"
  show SpecialStatusMessage      = "special_status_message"
  show UnimplementedMethod       = "unimplemented_method"
  show UnimplementedService      = "unimplemented_service"
  show CancelAfterBegin          = "cancel_after_begin"
  show CancelAfterFirstResponse  = "cancel_after_first_response"
  show TimeoutOnSleepingServer   = "timeout_on_sleeping_server"

{-------------------------------------------------------------------------------
  Get command line args
-------------------------------------------------------------------------------}

defaultCmdline :: IO Cmdline
defaultCmdline = do
    rootCA  <- getDataFileName "interop-ca.pem"
    pubCert <- getDataFileName "interop.pem"
    privKey <- getDataFileName "interop.key"

    return Cmdline {
        cmdMode                  = error "cmdMode: no default"
      , cmdPortOverride          = Nothing
      , cmdUseTLS                = True
      , cmdTestCase              = Nothing
      , cmdHost                  = "127.0.0.1"
      , cmdServerHostOverride    = Just "foo.test.google.fr"
      , cmdUseTestCA             = True
      , cmdRootCA                = rootCA
      , cmdPubCert               = pubCert
      , cmdPrivKey               = privKey
      , cmdSslKeyLog             = SslKeyLogNone
      , cmdTimeoutTest           = 5
      , cmdTimeoutConnect        = 5
      , cmdSkipTest              = []
      , cmdSkipCompression       = False
      , cmdSkipClientCompression = False
      }

getCmdline :: IO Cmdline
getCmdline = do
    defaults <- defaultCmdline

    let parser :: Opt.Parser Cmdline
        parser = parseCmdline defaults

    let opts :: Opt.ParserInfo Cmdline
        opts =
            Opt.info (parser <**> Opt.helper) $ mconcat [
                Opt.fullDesc
              , Opt.progDesc "Server and client for official gRPC interop tests"
              ]

    Opt.execParser opts

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseCmdline :: Cmdline -> Opt.Parser Cmdline
parseCmdline defaults =
    Cmdline
      <$> parseMode

      --
      -- gRPC test suite command line arguments
      --

      <*> (Opt.optional $ asum [
              Opt.option Opt.auto $ mconcat [
                  Opt.long "server_port"
                , Opt.help "Alternative spelling for --port"
                ]
            , Opt.option Opt.auto $ mconcat [
                  Opt.long "port"
                , Opt.help "Override default port. If not specified, will use 50051 if TLS is disabled, 50052 is TLS enabled, and 0 for self-tests (i.e., pick an arbitrary available port number)."
                ]
            ])
      <*> (Opt.option readBool $ mconcat [
              Opt.long "use_tls"
            , Opt.help "Enable TLS"
            , Opt.value (cmdUseTLS defaults)
            , Opt.showDefault
            ])
      <*> (Opt.optional $ Opt.option readTestCase $ mconcat [
              Opt.long "test_case"
            , Opt.help "Test case (ignored by the server; if not specified in the client, run all tests)"
            ])
      <*> (Opt.option Opt.str $ mconcat [
              Opt.long "server_host"
            , Opt.help "Address to bind to (when running as server) or to connect to (as client)"
            , Opt.value (cmdHost defaults)
            , Opt.showDefault
            ])
      <*> (Opt.option readOptionalString $ mconcat [
              Opt.long "server_host_override"
            , Opt.help ":authority/SNI override (set to empty to disable)"
            , Opt.value (cmdServerHostOverride defaults)
            , Opt.showDefault
            ])
      <*> (Opt.option readBool $ mconcat [
              Opt.long "use_test_ca"
            , Opt.help "Use test certificate as root CA"
            , Opt.value (cmdUseTestCA defaults)
            , Opt.showDefault
            ])

      --
      -- Additional command line arguments
      --

      <*> (Opt.strOption $ mconcat [
              Opt.long "root_ca"
            , Opt.value (cmdRootCA defaults)
            , Opt.showDefault
            , Opt.help "Root certificate authority"
            ])
      <*> (Opt.strOption $ mconcat [
              Opt.long "pub_cert"
            , Opt.value (cmdPubCert defaults)
            , Opt.showDefault
            , Opt.help "Server certificate"
            ])
      <*> (Opt.strOption $ mconcat [
              Opt.long "priv_key"
             ,Opt.value (cmdPrivKey defaults)
            , Opt.showDefault
            , Opt.help "Server private key"
            ])
      <*> parseSslkeyLog

      <*> (Opt.option Opt.auto $ mconcat [
               Opt.long "test_timeout"
             , Opt.metavar "SEC"
             , Opt.showDefault
             , Opt.value (cmdTimeoutTest defaults)
             , Opt.help "Test timeout"
             ])
      <*> (Opt.option Opt.auto $ mconcat [
               Opt.long "connect_timeout"
             , Opt.metavar "SEC"
             , Opt.showDefault
             , Opt.value (cmdTimeoutConnect defaults)
             , Opt.help "Timeout for trying to connect to the server"
             ])

      <*> (Opt.many $ Opt.option readTestCase $ mconcat [
              Opt.long "skip_test"
            , Opt.help "Skip test case (all --skip-xyz arguments are ignored by the server)"
            ])
      <*> (Opt.switch $ mconcat [
              Opt.long "skip_compression"
            , Opt.help "Skip compression tests"
            ])
      <*> (Opt.switch $ mconcat [
              Opt.long "skip_client_compression"
            , Opt.help "Skip client compression tests"
            ])

parseSslkeyLog :: Opt.Parser SslKeyLog
parseSslkeyLog = asum [
      Opt.flag' SslKeyLogFromEnv $ mconcat [
          Opt.long "key_log_from_env"
        , Opt.help "Set SSL key logging based on SSLKEYLOGFILE (default is no logging)"
        ]
    , fmap SslKeyLogPath $ Opt.strOption $ mconcat [
          Opt.long "key_log_path"
        , Opt.help "Set the SSL key logging filepath"
        ]
    , pure SslKeyLogNone
    ]

parseMode :: Opt.Parser Mode
parseMode = asum [
      Opt.flag' Server $ mconcat [
          Opt.long "server"
        , Opt.help "Run in server mode"
        ]
    , Opt.flag' Client $ mconcat [
          Opt.long "client"
        , Opt.help "Run in client mode"
        ]
    , Opt.flag' Ping $ mconcat [
          Opt.long "ping"
        , Opt.help "Ping the server (to verify that we can reach it)"
        ]
    , Opt.flag' SelfTest $ mconcat [
          Opt.long "self-test"
        , Opt.help "Run grapesy interop tests against itself (this is the default)"
        ]
    , pure SelfTest
    ]

readBool :: Opt.ReadM Bool
readBool = Opt.str >>= aux
  where
    aux :: String -> Opt.ReadM Bool
    aux "true"  = return True
    aux "false" = return False
    aux x       = fail $ "Could not parse bool " ++ show x

readOptionalString :: Opt.ReadM (Maybe String)
readOptionalString = Opt.str >>= aux
  where
    aux :: String -> Opt.ReadM (Maybe String)
    aux ""  = return Nothing
    aux str = return $ Just str

readTestCase :: Opt.ReadM TestCase
readTestCase = Opt.str >>= aux
  where
    aux :: String -> Opt.ReadM TestCase
    aux "cancel_after_begin"          = return CancelAfterBegin
    aux "cancel_after_first_response" = return CancelAfterFirstResponse
    aux "client_compressed_streaming" = return ClientCompressedStreaming
    aux "client_compressed_unary"     = return ClientCompressedUnary
    aux "client_streaming"            = return ClientStreaming
    aux "custom_metadata"             = return CustomMetadata
    aux "empty_stream"                = return EmptyStream
    aux "empty_unary"                 = return EmptyUnary
    aux "large_unary"                 = return LargeUnary
    aux "ping_pong"                   = return PingPong
    aux "server_compressed_streaming" = return ServerCompressedStreaming
    aux "server_compressed_unary"     = return ServerCompressedUnary
    aux "server_streaming"            = return ServerStreaming
    aux "special_status_message"      = return SpecialStatusMessage
    aux "status_code_and_message"     = return StatusCodeAndMessage
    aux "timeout_on_sleeping_server"  = return TimeoutOnSleepingServer
    aux "unimplemented_method"        = return UnimplementedMethod
    aux "unimplemented_service"       = return UnimplementedService
    aux x                             = fail $ "Unknown test case " ++ show x

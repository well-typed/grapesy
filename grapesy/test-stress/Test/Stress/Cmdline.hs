{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Stress.Cmdline
  ( -- * Types
    Cmdline(..)
  , Role(..)
  , GlobalOpts(..)

    -- ** Client-specific
  , Connect(..)
  , Exec(..)
  , Call(..)

    -- ** Server-specific
  , Security(..)
  , TlsOpts(..)

    -- ** Auxiliary
  , NotPretty(..)

    -- * Parser
  , getCmdline
  ) where

import Control.Applicative ((<|>))
import Data.Foldable (asum, toList)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.Socket (HostName, PortNumber)
import Options.Applicative qualified as Opt
import Text.Show.Pretty (PrettyVal(..), parseValue)

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Server.Run

import Paths_grapesy (getDataFileName)

{-------------------------------------------------------------------------------
  Definitions
-------------------------------------------------------------------------------}

-- | Command specification
data Cmdline = Cmdline {
      cmdRole       :: Role
    , cmdGlobalOpts :: GlobalOpts
    }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

-- | Should we run the client, servers, or both?
data Role =
      -- | Run the clients
      Client {
          -- | Connect over TLS?
          clientSecurity   :: Maybe (NotPretty Client.ServerValidation)
        , clientServerPort :: PortNumber
        , clientConnects   :: [Connect]

          -- | Insist on this compression scheme for all messages
        , clientCompression :: Maybe (NotPretty Compression)
        }

      -- | Run the server
    | Server {
          serverConfig :: ServerConfig
        }

      -- | Run the automatic stress test suite
    | Driver {
          driverWorkingDir :: Maybe FilePath
        , driverDuration   :: Int
        , driverGenCharts  :: Bool
        }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

-- | Connections to execute
data Connect = Connect {
      -- | Execute the connections concurrently or sequentially?
      connectExec :: Exec

      -- | Number of connections
    , connectNum :: Int

      -- | Number of times to repeat the calls on the connection
    , callNum :: Int

      -- | Calls to make on the connections
    , connectCalls :: [Call]
    }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

-- | Concurrent or sequential execution
data Exec = Concurrent | Sequential
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

-- | Types of RPCs
data Call =
      -- | A single message back and forth
      NonStreaming

      -- | Client sends @N@ messages to the server
    | ClientStreaming Int

      -- | Server sends @N@ messages to the client
    | ServerStreaming Int

      -- | Client and server send @N@ messages to each other
    | BiDiStreaming Int
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

data Security =
      Insecure
    | Secure
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

data TlsOpts = TlsOpts {
      tlsPubCert    :: FilePath
    , tlsChainCerts :: [FilePath]
    , tlsPrivKey    :: FilePath
    }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

mkConfig :: Maybe TlsOpts -> HostName -> PortNumber -> ServerConfig
mkConfig mtls host port =
    case mtls of
      Just TlsOpts{..} ->
        ServerConfig Nothing $ Just SecureConfig {
            secureHost       = host
          , securePort       = port
          , securePubCert    = tlsPubCert
          , secureChainCerts = tlsChainCerts
          , securePrivKey    = tlsPrivKey
          , secureSslKeyLog  = SslKeyLogNone
          }
      Nothing ->
        (`ServerConfig` Nothing) $ Just InsecureConfig {
            insecureHost = Just host
          , insecurePort = port
          }

data GlobalOpts = GlobalOpts {
      optsTracing :: Bool
    }
  deriving stock (Show, Generic)
  deriving anyclass (PrettyVal)

-------------------------------------------------------------------------------
-- Top-level parsers
-------------------------------------------------------------------------------

getCmdline :: IO Cmdline
getCmdline = do
    defaultPub  <- getDataFileName "grpc-demo.pem"
    defaultPriv <- getDataFileName "grpc-demo.key"

    let info  :: Opt.ParserInfo Cmdline
        info = Opt.info
                 (          parseCmdline defaultPub defaultPriv
                   Opt.<**> Opt.helper
                 )
                 Opt.fullDesc

    Opt.execParser info

parseCmdline ::
     FilePath
  -> FilePath
  -> Opt.Parser Cmdline
parseCmdline defaultPub defaultPriv =
    Cmdline
      <$> (parseRole defaultPub defaultPriv <|> pure (Driver Nothing 60 False))
      <*> parseGlobalOpts

parseRole :: FilePath -> FilePath -> Opt.Parser Role
parseRole defaultPub defaultPriv = Opt.subparser $ mconcat [
      sub "client" "Run the client" $
        parseClientRole defaultPub
    , sub "server" "Run the server" $
        parseServerRole defaultPub defaultPriv
    , sub "driver" "Run the stress test driver" $
        parseDriverRole
    ]

-------------------------------------------------------------------------------
-- Client option parsers
-------------------------------------------------------------------------------

parseClientRole :: FilePath -> Opt.Parser Role
parseClientRole defaultPub =
    Client
      <$> (fmap WrapNotPretty <$> parseClientSecurity defaultPub)
      <*> parseClientPort
      <*> parseClientConnects
      <*> Opt.optional (WrapNotPretty <$> parseCompression)

parseClientSecurity :: FilePath -> Opt.Parser (Maybe Client.ServerValidation)
parseClientSecurity defaultPub =
    Opt.optional (
        Opt.flag' () (mconcat [
            Opt.long "secure"
          , Opt.help "Connect over TLS"
          ])
        *> parseServerValidation
      )
  where
    parseServerValidation :: Opt.Parser Client.ServerValidation
    parseServerValidation =
        aux
          <$> (Opt.switch $ mconcat [
                  Opt.long "no-server-validation"
                , Opt.help "Skip server (certificate) validation"
                ])
          <*> (Opt.switch $ mconcat [
                  Opt.long "cert-store-from-system"
                , Opt.help "Enable the system certificate store"
                ])
          <*> (Opt.option Opt.str $ mconcat [
                  Opt.long "cert-store-from-path"
                , Opt.help "Load certificate store from file or directory (set to empty to disable)"
                , Opt.metavar "PATH"
                , Opt.value defaultPub
                , Opt.showDefault
                ])
      where
        aux :: Bool -> Bool -> FilePath -> Client.ServerValidation
        aux noServerValidation certStoreFromSystem certStoreFromPath =
            if noServerValidation then
              Client.NoServerValidation
            else
              Client.ValidateServer $ mconcat . concat $ [
                  [ Client.certStoreFromSystem
                  | certStoreFromSystem
                  ]

                , [ Client.certStoreFromPath certStoreFromPath
                  | not (null certStoreFromPath)
                  ]
                ]

parseClientPort :: Opt.Parser PortNumber
parseClientPort =
    Opt.option Opt.auto (mconcat [
        Opt.long "port"
      , Opt.help "Connect to the server at PORT"
      , Opt.metavar "PORT"
      ])

parseClientConnects :: Opt.Parser [Connect]
parseClientConnects =
    Opt.some $ Connect
      <$> Opt.flag Sequential Concurrent (mconcat [
              Opt.long "concurrent"
            , Opt.help "Open connections concurrently"
            ])
      <*> Opt.option Opt.auto (mconcat [
              Opt.long "num-connections"
            , Opt.help "Open N connections"
            , Opt.metavar "N"
            , Opt.value 1
            , Opt.showDefault
            ])
      <*> Opt.option Opt.auto (mconcat [
              Opt.long "num-calls"
            , Opt.help "Repeat the calls N times on each connection"
            , Opt.metavar "N"
            , Opt.value 1
            , Opt.showDefault
            ])
      <*> Opt.some parseCall

parseCall :: Opt.Parser Call
parseCall =
        nonStreaming
    <|> clientStreaming
    <|> serverStreaming
    <|> bidiStreaming
  where
    nonStreaming :: Opt.Parser Call
    nonStreaming =
        Opt.flag' NonStreaming $ mconcat [
            Opt.long "non-streaming"
          , Opt.help "Make a single non-streaming call"
          ]

    clientStreaming :: Opt.Parser Call
    clientStreaming =
        ClientStreaming
          <$> Opt.option Opt.auto (mconcat [
                  Opt.long "client-streaming"
                , Opt.help "Stream N messages from the client"
                , Opt.metavar "N"
                ])

    serverStreaming :: Opt.Parser Call
    serverStreaming =
        ServerStreaming
          <$> Opt.option Opt.auto (mconcat [
                  Opt.long "server-streaming"
                , Opt.help "Stream N messages from the server"
                , Opt.metavar "N"
                ])

    bidiStreaming :: Opt.Parser Call
    bidiStreaming =
        BiDiStreaming
          <$> Opt.option Opt.auto (mconcat [
                  Opt.long "bidi-streaming"
                , Opt.help "Stream N messages from both the client and server"
                , Opt.metavar "N"
                ])

parseCompression :: Opt.Parser Compression
parseCompression = asum $ map go (toList Compr.allSupportedCompression)
  where
    go :: Compression -> Opt.Parser Compression
    go compr = Opt.flag' compr $ mconcat [
          Opt.long comprId
        , Opt.help $ "Insist on " ++ comprId ++ " compression "
        ]
      where
        comprId :: String
        comprId = show (Compr.compressionId compr)

-------------------------------------------------------------------------------
-- Server option parsers
-------------------------------------------------------------------------------

parseServerRole :: FilePath -> FilePath -> Opt.Parser Role
parseServerRole defaultPub defaultPriv =
    aux
      <$> parseServerPort
      <*> parseServerSecurity defaultPub defaultPriv
  where
    aux :: PortNumber -> Maybe TlsOpts -> Role
    aux port mtls = Server $ mkConfig mtls "127.0.0.1" port

parseServerPort :: Opt.Parser PortNumber
parseServerPort =
    Opt.option Opt.auto (mconcat [
        Opt.long "port"
      , Opt.help "Bind the server to port PORT"
      , Opt.metavar "PORT"
      ])

parseServerSecurity :: FilePath -> FilePath -> Opt.Parser (Maybe TlsOpts)
parseServerSecurity defaultPub defaultPriv =
    Opt.optional $
      Opt.flag' () (mconcat [
          Opt.long "secure"
        , Opt.help "Enable TLS on the servers"
        ])
      *> parseTlsOpts defaultPub defaultPriv

parseTlsOpts :: FilePath -> FilePath -> Opt.Parser TlsOpts
parseTlsOpts defaultPub defaultPriv =
    TlsOpts
      <$> Opt.strOption (mconcat [
              Opt.long "tls-pub"
            , Opt.help "TLS public certificate (X.509 format)"
            , Opt.metavar "CERT_FILE"
            , Opt.showDefault
            , Opt.value defaultPub
            ])
      <*> Opt.many (Opt.strOption $ mconcat [
              Opt.long "tls-cert"
            , Opt.metavar "CERT_FILE"
            , Opt.help "TLS chain certificate (X.509 format)"
            ])
      <*> Opt.strOption (mconcat [
              Opt.long "tls-priv"
            , Opt.metavar "KEY_FILE"
            , Opt.help "TLS private key"
            , Opt.showDefault
            , Opt.value defaultPriv
            ])

-------------------------------------------------------------------------------
-- Driver option parsers
-------------------------------------------------------------------------------

parseDriverRole :: Opt.Parser Role
parseDriverRole =
    Driver
      <$> Opt.optional (Opt.strOption (mconcat [
              Opt.long "working-dir"
            , Opt.help "Write result files to this directory, must already exist"
            , Opt.metavar "DIR"
            ]))
      <*> Opt.option Opt.auto (mconcat [
              Opt.long "duration"
            , Opt.help "Run the stress tests for this many seconds"
            , Opt.metavar "SECONDS"
            , Opt.value 60
            , Opt.showDefault
            ])
      <*> Opt.switch (mconcat [
              Opt.long "gen-charts"
            , Opt.help "Generate heap profile charts for stable components"
            ])

-------------------------------------------------------------------------------
-- Internal auxiliary
-------------------------------------------------------------------------------

parseGlobalOpts :: Opt.Parser GlobalOpts
parseGlobalOpts =
    GlobalOpts
      <$> Opt.switch (mconcat [
              Opt.long "verbose"
            , Opt.short 'v'
            , Opt.help "Trace test execution to stdout"
            ])

sub :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
sub cmd desc parser =
    Opt.command cmd $
      Opt.info (parser Opt.<**> Opt.helper) (Opt.progDesc desc)

-------------------------------------------------------------------------------
-- Auxiliary: pretty-val
-------------------------------------------------------------------------------

newtype NotPretty a = WrapNotPretty { unwrapNotPretty :: a }
  deriving newtype (Show)

instance Show a => PrettyVal (NotPretty a) where
  prettyVal (WrapNotPretty x) =
      fromMaybe
        (error $ "prettyVal: could not parse " ++ show x)
        (parseValue $ show x)

instance PrettyVal PortNumber where
  prettyVal = prettyVal . (fromIntegral :: PortNumber -> Integer)

deriving anyclass instance PrettyVal ServerConfig
deriving anyclass instance PrettyVal InsecureConfig
deriving anyclass instance PrettyVal SecureConfig
deriving anyclass instance PrettyVal SslKeyLog

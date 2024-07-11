module KVStore.Cmdline (
    Cmdline(..)
  , Mode(..)
  , getCmdline
  ) where

import Data.Foldable (asum)
import Options.Applicative (Parser, (<**>))
import Options.Applicative qualified as Opt

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdMode              :: Mode
    , cmdDuration          :: Int
    , cmdSimulateWork      :: Bool
    , cmdJSON              :: Bool
    , cmdSecure            :: Bool
    , cmdDisableTcpNoDelay :: Bool
    , cmdPingRateLimit     :: Maybe Int
    }

data Mode =
    Benchmark
  | Server
  | Client

{-------------------------------------------------------------------------------
  Get command line arguments
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline =
    Opt.execParser opts
  where
    opts = Opt.info (parseCmdline <**> Opt.helper) $ mconcat [
        Opt.fullDesc
      , Opt.progDesc "Server and client for official gRPC interop tests"
      ]

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseMode
      <*> (Opt.option Opt.auto $ mconcat [
              Opt.long "duration"
            , Opt.showDefault
            , Opt.value 60
            , Opt.metavar "SEC"
            ])
      <*> (Opt.flag True False $ mconcat [
              Opt.long "no-work-simulation"
            , Opt.help "Disable work simulation (just run as quickly as possible)"
            ])
      <*> (Opt.switch $ mconcat [
              Opt.long "json"
            , Opt.help "Use JSON instead of Protobuf"
            ])
      <*> (Opt.switch $ mconcat [
              Opt.long "secure"
            , Opt.help "Enable TLS"
            ])
      <*> (Opt.switch $ mconcat [
              Opt.long "disable-tcp-nodelay"
            , Opt.help "Disable the TCP_NODELAY option"
            ])
      <*> (Opt.optional $
            Opt.option Opt.auto $ mconcat [
              Opt.long "ping-rate-limit"
            , Opt.metavar "PINGs/sec"
            , Opt.help "Allow at most this many pings per second from the peer"
            ]
          )

parseMode :: Parser Mode
parseMode = asum [
      Opt.flag' Server $ mconcat [
          Opt.long "server"
        , Opt.help "Run in server mode"
        ]
    , Opt.flag' Client $ mconcat [
          Opt.long "client"
        , Opt.help "Run in client mode"
        ]
    , Opt.flag' Benchmark $ mconcat [
          Opt.long "benchmark"
        , Opt.help "Run the server against itself, and count RPCs (this is the default)"
        ]
    , pure Benchmark
    ]



module Interop.Cmdline (
    getCmdline
    -- * Definition
  , Cmdline(..)
  , Mode(..)
  ) where

import Data.Foldable (asum)
import Network.Socket (PortNumber, HostName)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {

      --
      -- Command line arguments used by the gRPC test suite
      --

      cmdMode   :: Mode
    , cmdPort   :: PortNumber
    , cmdUseTLS :: Bool

      --
      -- Additional command line arguments
      --

    , cmdHost :: HostName
    }
  deriving (Show)

data Mode = Server | Client | Ping
  deriving (Show)

{-------------------------------------------------------------------------------
  Get command line args
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = Opt.execParser opts
  where
    opts :: Opt.ParserInfo Cmdline
    opts =
        Opt.info (parseCmdline <**> Opt.helper) $ mconcat [
            Opt.fullDesc
          , Opt.progDesc "Server and client for official gRPC interop tests"
          ]

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseCmdline :: Opt.Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseMode
      <*> (Opt.option Opt.auto $ mconcat [
              Opt.long "port"
            , Opt.help "Port number"
            , Opt.value 50052
            , Opt.showDefault
            ])
      <*> (Opt.option readBool $ mconcat [
              Opt.long "use_tls"
            , Opt.help "Enable TLS"
            , Opt.value True
            , Opt.showDefault
            ])
      <*> (Opt.option Opt.str $ mconcat [
              Opt.long "host"
            , Opt.help "Hostname"
            , Opt.value "127.0.0.1"
            , Opt.showDefault
            ])

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
    ]

readBool :: Opt.ReadM Bool
readBool = Opt.str >>= aux
  where
    aux :: String -> Opt.ReadM Bool
    aux "true"  = return True
    aux "false" = return False
    aux x       = fail $ "Could not parse bool " ++ show x


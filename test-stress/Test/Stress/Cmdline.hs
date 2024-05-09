module Test.Stress.Cmdline (
    Cmdline(..)
  , Role(..)
  , Test(..)
  , getCmdline
  ) where

import Options.Applicative qualified as Opt

import Network.GRPC.Common.StreamType

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdRole :: Role
    }

data Role =
      Client
    | Server

data Test =
      ManyConnections Word
    | ManyCalls Word
    | ManyMessages Word StreamingType
  deriving Show

{-------------------------------------------------------------------------------
  Parser top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = Opt.execParser $
    Opt.info (parseCmdline Opt.<**> Opt.helper) Opt.fullDesc

parseCmdline :: Opt.Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseRole

parseRole :: Opt.Parser Role
parseRole = Opt.subparser $ mconcat [
      sub "client" "Client" (pure Client)
    , sub "server" "Server" (pure Server)
    ]

sub :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
sub cmd desc parser =
    Opt.command cmd $
      Opt.info (parser Opt.<**> Opt.helper) (Opt.progDesc desc)
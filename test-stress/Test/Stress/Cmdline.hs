module Test.Stress.Cmdline (
    Cmdline(..)
  , Role(..)
  , Test(..)
  , getCmdline
  ) where

import Options.Applicative qualified as Opt

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdRole :: Role
    }

data Role =
    Client Test
  | Server

data Test =
    ManyShortLived

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
      sub "client" "Client" (Client <$> parseTest)
    , sub "server" "Server" (pure Server)
    ]

parseTest :: Opt.Parser Test
parseTest = Opt.subparser $ mconcat [
      sub "manyshortlived" "Many short-lived RPC calls" (pure ManyShortLived)
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sub :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
sub cmd desc parser =
    Opt.command cmd $
      Opt.info (parser Opt.<**> Opt.helper) (Opt.progDesc desc)

module Test.Stress.Cmdline (
    Cmdline(..)
  , Role(..)
  , Test(..)
  , getCmdline
  ) where

import Data.Foldable (asum)
import Options.Applicative qualified as Opt

import Network.GRPC.Common.StreamType

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
    ManyConnections Word
  | ManyCalls Word
  | ManyMessages Word StreamingType

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
      sub
        "many-connections"
        "Many connections with single non-streaming RPC call" $
          ManyConnections
            <$> parseWord "NUM_CONNECTIONS" "number of connections to open"
    , sub
        "many-calls"
        "Many non-streaming RPC calls on a single connection" $
          ManyCalls
            <$> parseWord "NUM_CALLS" "number of calls to make"
    , sub
        "many-messages"
        "Single call on a single connection, many messages exchanged" $
          ManyMessages
            <$> parseWord "NUM_MESSAGES" "number of messages"
            <*> parseStreamingType
    ]

parseWord :: String -> String -> Opt.Parser Word
parseWord meta help =
    Opt.argument Opt.auto $
      Opt.metavar meta <> Opt.help help

parseStreamingType :: Opt.Parser StreamingType
parseStreamingType = asum [
      Opt.flag' ClientStreaming $ mconcat [
          Opt.long "stream-client"
        , Opt.help "client-side streaming"
        ]
    , Opt.flag' ServerStreaming $ mconcat [
          Opt.long "stream-server"
        , Opt.help "server-side streaming"
        ]
    , Opt.flag' BiDiStreaming $ mconcat [
          Opt.long "stream-bidi"
        , Opt.help "Bidirectional streaming"
        ]

    -- 'NoStreaming' is tested by the many-connections and many-calls cases
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sub :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
sub cmd desc parser =
    Opt.command cmd $
      Opt.info (parser Opt.<**> Opt.helper) (Opt.progDesc desc)

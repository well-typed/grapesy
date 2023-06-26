{-# LANGUAGE ViewPatterns #-}
module Demo.Server.Cmdline (
    Cmdline(..)
  , getCmdline
  ) where

import Data.Foldable (asum)
import Options.Applicative qualified as Opt
import Network.Socket (ServiceName)

import Network.GRPC.Server.Run

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdInsecure :: Maybe InsecureConfig
    , cmdSecure   :: Maybe SecureConfig
    , cmdDebug    :: Bool
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = Opt.execParser $
    Opt.info (parseCmdline Opt.<**> Opt.helper) Opt.fullDesc

parseCmdline :: Opt.Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseInsecure
      <*> parseSecure
      <*> (Opt.switch $ mconcat [
               Opt.long "debug"
             , Opt.help "Enable debug output"
             ])

parseInsecure :: Opt.Parser (Maybe InsecureConfig)
parseInsecure = asum [
      Opt.flag' Nothing $ mconcat [
          Opt.long "disable-insecure"
        , Opt.help "Disable insecure server (without TLS)"
        ]
    , cfg
        <$> Opt.option Opt.str (mconcat [
                Opt.long "port-insecure"
              , Opt.help "Port number for the insecure server (without TLS)"
              ])
    ]
  where
    cfg :: ServiceName -> Maybe InsecureConfig
    cfg port = Just InsecureConfig {
          insecureHost = Nothing
        , insecurePort = port
        }

parseSecure :: Opt.Parser (Maybe SecureConfig)
parseSecure = asum [
      Opt.flag' Nothing $ mconcat [
          Opt.long "disable-secure"
        , Opt.help "Disable secure server (over TLS)"
        ]
    , cfg
        <$> Opt.option Opt.str (mconcat [
                Opt.long "port-secure"
              , Opt.help "Port number for the insecure server (over TLS)"
              ])
        <*> (Opt.option Opt.str $ mconcat [
                Opt.long "tls-pub"
              , Opt.help "TLS public certificate (X.509 format)"
              ])
        <*> Opt.many (Opt.option Opt.str $ mconcat [
                Opt.long "tls-cert"
              , Opt.help "TLS chain certificate (X.509 format)"
              ])
        <*> (Opt.option Opt.str $ mconcat [
                Opt.long "tls-priv"
              , Opt.help "TLS private key"
              ])
    ]
  where
    cfg ::
         ServiceName
      -> FilePath
      -> [FilePath]
      -> FilePath
      -> Maybe SecureConfig
    cfg port pub chain priv = Just SecureConfig {
          secureHost       = Nothing
        , securePort       = port
        , securePubCert    = pub
        , secureChainCerts = chain
        , securePrivKey    = priv
        }

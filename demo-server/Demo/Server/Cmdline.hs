{-# LANGUAGE ViewPatterns #-}
module Demo.Server.Cmdline (
    Cmdline(..)
  , getCmdline
  ) where

import Data.Foldable (asum)
import Options.Applicative qualified as Opt
import Network.Socket (PortNumber, HostName)

import Network.GRPC.Common
import Network.GRPC.Server.Run

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdInsecure             :: Maybe InsecureConfig
    , cmdSecure               :: Maybe SecureConfig
    , cmdDebug                :: Bool
    , cmdTrailersOnlyShortcut :: Bool
    , cmdDisableCompression   :: Bool
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
      <*> (Opt.switch $ mconcat [
               Opt.long "trailers-only-shortcut"
             , Opt.help "Use Trailers-Only even in non-error cases"
             ])
      <*> (Opt.switch $ mconcat [
               Opt.long "disable-compression"
             , Opt.help "Disable support for compression"
             ])

parseInsecure :: Opt.Parser (Maybe InsecureConfig)
parseInsecure = asum [
      Opt.flag' Nothing $ mconcat [
          Opt.long "disable-insecure"
        , Opt.help "Disable insecure server (without TLS)"
        ]
    , cfg
        <$> Opt.option Opt.auto (mconcat [
                Opt.long "port-insecure"
              , Opt.help "Port number for the insecure server (without TLS)"
              ])
        <*> Opt.optional (Opt.option Opt.str (mconcat [
                Opt.long "host-insecure"
              , Opt.help "Host name to bind the insecure server to"
              ]))
    ]
  where
    cfg :: PortNumber -> Maybe HostName -> Maybe InsecureConfig
    cfg port host = Just InsecureConfig {
          insecureHost = host
        , insecurePort = port
        }

parseSecure :: Opt.Parser (Maybe SecureConfig)
parseSecure = asum [
      Opt.flag' Nothing $ mconcat [
          Opt.long "disable-secure"
        , Opt.help "Disable secure server (over TLS)"
        ]
    , cfg
        <$> Opt.option Opt.auto (mconcat [
                Opt.long "port-secure"
              , Opt.help "Port number for the insecure server (over TLS)"
              ])
        <*> (Opt.option Opt.str $ mconcat [
                Opt.long "host-secure"
              , Opt.help "Host name to bind the secure server to"
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
         PortNumber
      -> HostName
      -> FilePath
      -> [FilePath]
      -> FilePath
      -> Maybe SecureConfig
    cfg port host pub chain priv = Just SecureConfig {
          secureHost       = host
        , securePort       = port
        , securePubCert    = pub
        , secureChainCerts = chain
        , securePrivKey    = priv
        , secureSslKeyLog  = SslKeyLogNone
        }

{-# LANGUAGE ViewPatterns #-}
module Demo.Server.Cmdline (
    Cmdline(..)
  , getCmdline
  ) where

import Data.Foldable (asum)
import Network.Socket (PortNumber, HostName)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

import Network.GRPC.Common
import Network.GRPC.Server.Run

import Paths_grapesy

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdInsecure             :: Maybe InsecureConfig
    , cmdSecure               :: Maybe SecureConfig
    , cmdTrailersOnlyShortcut :: Bool
    , cmdDisableCompression   :: Bool
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = do
    defaultPub  <- getDataFileName "grpc-demo.pem"
    defaultPriv <- getDataFileName "grpc-demo.key"

    let info :: Opt.ParserInfo Cmdline
        info = Opt.info
                 (      parseCmdline defaultPub defaultPriv
                   <**> Opt.helper
                 )
                 Opt.fullDesc

    Opt.execParser info

parseCmdline :: FilePath -> FilePath -> Opt.Parser Cmdline
parseCmdline defaultPub defaultPriv =
    Cmdline
      <$> parseInsecure
      <*> parseSecure defaultPub defaultPriv
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

parseSecure :: FilePath -> FilePath -> Opt.Parser (Maybe SecureConfig)
parseSecure defaultPub defaultPriv = asum [
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
              , Opt.value defaultPub
              , Opt.showDefault
              ])
        <*> Opt.many (Opt.option Opt.str $ mconcat [
                Opt.long "tls-cert"
              , Opt.help "TLS chain certificate (X.509 format)"
              ])
        <*> (Opt.option Opt.str $ mconcat [
                Opt.long "tls-priv"
              , Opt.help "TLS private key"
              , Opt.value defaultPriv
              , Opt.showDefault
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

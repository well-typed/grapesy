{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Command line options
--
-- Intended for unqualified import.
module Demo.Client.Cmdline (
    -- * Definition
    Cmdline(..)
  , API(..)
  , SomeMethod(..)
  , SMethod(..)
    -- * Parser
  , getCmdline
  ) where

import Prelude

import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.Foldable (asum)
import Data.Int
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Socket (HostName, PortNumber)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Protobuf

import Paths_grapesy

import Demo.Client.Util.DelayOr (DelayOr(..))

import Proto.API.Helloworld
import Proto.API.Ping
import Proto.API.RouteGuide

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdServer      :: Client.Server
    , cmdTimeout     :: Maybe Word
    , cmdAPI         :: API
    , cmdCompression :: Maybe Compression
    , cmdMethods     :: [DelayOr SomeMethod]
    }
  deriving (Show)

-- | Which API to use?
data API =
    StreamTypeIO
  | StreamTypeConduit
  | StreamTypeMonadStack
  | Core
  | CoreNoFinal
  deriving (Show)

data SomeMethod where
  SomeMethod :: SMethod rpc -> SomeMethod

deriving stock instance Show SomeMethod

-- | Select method
data SMethod :: Type -> Type where
  SSayHello            :: Proto HelloRequest             -> SMethod SayHello
  SSayHelloStreamReply :: Proto HelloRequest             -> SMethod SayHelloStreamReply
  SSayHelloBidiStream  :: [DelayOr (Proto HelloRequest)] -> SMethod SayHelloBidiStream

  SGetFeature   :: Proto Point                 -> SMethod GetFeature
  SListFeatures :: Proto Rectangle             -> SMethod ListFeatures
  SRecordRoute  :: [DelayOr (Proto Point)]     -> SMethod RecordRoute
  SRouteChat    :: [DelayOr (Proto RouteNote)] -> SMethod RouteChat

  SPing :: Lazy.ByteString -> SMethod Ping

deriving stock instance Show (SMethod rpc)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = do
    defaultPub <- getDataFileName "grpc-demo.pem"

    let info :: Opt.ParserInfo Cmdline
        info = Opt.info (parseCmdline defaultPub <**> Opt.helper) Opt.fullDesc

    Opt.execParser info

parseCmdline :: FilePath -> Opt.Parser Cmdline
parseCmdline defaultPub =
    Cmdline
      <$> parseServer defaultPub
      <*> (Opt.optional $ Opt.option Opt.auto $ mconcat [
               Opt.long "timeout"
             , Opt.metavar "SECONDS"
             ])
      <*> parseAPI
      <*> Opt.optional parseCompression
      <*> Opt.many (parseDelayOr parseSomeMethod)

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

parseServer :: FilePath -> Opt.Parser Client.Server
parseServer defaultPub =
   mkServer
      <$> (Opt.option Opt.str $ mconcat [
              Opt.long "host"
            , Opt.showDefault
            , Opt.value "127.0.0.1"
            ])
      <*> (Opt.optional $ Opt.option Opt.auto $ mconcat [
              Opt.long "port"
            ])
      <*> (Opt.optional $
                 Opt.flag' () (mconcat [
                     Opt.long "secure"
                   , Opt.help "Connect over TLS"
                   ])
              *> parseServerValidation defaultPub)
      <*> (Opt.optional $ Opt.option Opt.str $ mconcat [
              Opt.long "authority"
            , Opt.help "Override the HTTP2 :authority pseudo-header"
            ])
  where
    mkServer ::
         HostName                        -- Host
      -> Maybe PortNumber                -- Port
      -> Maybe Client.ServerValidation   -- Secure?
      -> Maybe String
      -> Client.Server
    mkServer host mPort Nothing mAuth =
        Client.ServerInsecure $
          Client.Address host (fromMaybe defaultInsecurePort mPort) mAuth
    mkServer host mPort (Just validation) mAuth =
        Client.ServerSecure validation def $
          Client.Address host (fromMaybe defaultSecurePort mPort) mAuth

parseServerValidation :: FilePath -> Opt.Parser Client.ServerValidation
parseServerValidation defaultPub =
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

parseCompression :: Opt.Parser Compression
parseCompression = asum [
      Opt.flag' Compr.gzip $ mconcat [
          Opt.long "gzip"
        , Opt.help "Use GZip compression for all messages"
        ]
    , Opt.flag' Compr.deflate $ mconcat [
          Opt.long "deflate"
        , Opt.help "Use deflate compression for all messages"
        ]
#ifdef SNAPPY
    , Opt.flag' Compr.snappy $ mconcat [
          Opt.long "snappy"
        , Opt.help "Use snappy compression for all messages"
        ]
#endif
    ]

parseAPI :: Opt.Parser API
parseAPI = asum [
      Opt.flag' StreamTypeIO $ mconcat [
          Opt.long "streamtype-io"
        , Opt.help "Use the StreamType.IO API (if applicable)"
        ]
    , Opt.flag' StreamTypeConduit $ mconcat [
          Opt.long "streamtype-conduit"
        , Opt.help "Use the StreamType.Conduit API (if applicable)"
        ]
    , Opt.flag' StreamTypeMonadStack $ mconcat [
          Opt.long "streamtype-monadstack"
        , Opt.help "Use the StreamType API with a bespoke monad stack (if applicable)"
        ]
    , Opt.flag' Core $ mconcat [
          Opt.long "core"
        , Opt.help "Use the core API"
        ]
    , Opt.flag' CoreNoFinal $ mconcat [
          Opt.long "core-dont-mark-final"
        , Opt.help "Use the core API; don't mark the last message as final"
        ]
    , pure StreamTypeIO
    ]

{-------------------------------------------------------------------------------
  Select method
-------------------------------------------------------------------------------}

parseSomeMethod :: Opt.Parser SomeMethod
parseSomeMethod = Opt.subparser $ mconcat [
      sub "sayHello" "helloworld.Greeter.SayHello" $
        SomeMethod . SSayHello <$>
          parseHelloRequest
    , sub "sayHelloStreamReply" "helloworld.Greeter.SayHelloStreamReply" $
        SomeMethod . SSayHelloStreamReply <$>
          parseHelloRequest
    , sub "sayHelloBidiStream" "helloworld.Greeter.SayHelloBidiStream" $
        SomeMethod . SSayHelloBidiStream <$>
          Opt.many (parseDelayOr parseHelloRequest)
    , sub "getFeature" "routeguide.RouteGuide.GetFeature" $
        SomeMethod . SGetFeature <$>
          parsePoint ""
    , sub "listFeatures" "routeguide.RouteGuide.ListFeatures" $
        SomeMethod . SListFeatures <$>
          parseRectangle
    , sub "recordRoute" "routeguide.RouteGuide.RecordRoute" $
        SomeMethod . SRecordRoute <$>
          Opt.many (parseDelayOr $ parsePoint "")
    , sub "routeChat" "routeguide.RouteGuide.RouteChat" $
        SomeMethod . SRouteChat <$>
          Opt.many (parseDelayOr $ parseRouteNote)
    , sub "ping" "Ping.ping" $
        SomeMethod . SPing <$>
          Opt.argument Opt.str (Opt.metavar "MSG")
    ]

{-------------------------------------------------------------------------------
  Method arguments
-------------------------------------------------------------------------------}

parseDelayOr :: Opt.Parser a -> Opt.Parser (DelayOr a)
parseDelayOr p = asum [
      Exec <$> p
    , Delay <$> (Opt.option Opt.auto $ mconcat [
          Opt.long "delay"
        , Opt.help "Delay by specified length in seconds"
        , Opt.metavar "DOUBLE"
        ])
    ]

parseHelloRequest :: Opt.Parser (Proto HelloRequest)
parseHelloRequest =
    mkHelloRequest <$> Opt.option Opt.str (mconcat [
        Opt.long "name"
      , Opt.metavar "NAME"
      ])
  where
    mkHelloRequest :: Text -> Proto HelloRequest
    mkHelloRequest name = (defMessage & #name .~ name)

parseLatitude :: String -> Opt.Parser Int32
parseLatitude prefix =
    Opt.option Opt.auto $ mconcat [
        Opt.long $ prefix ++ "latitude"
      ]

parseLongitude :: String -> Opt.Parser Int32
parseLongitude prefix =
    Opt.option Opt.auto $ mconcat [
        Opt.long $ prefix ++ "longitude"
      ]

parsePoint :: String -> Opt.Parser (Proto Point)
parsePoint prefix =
    mkPoint
      <$> parseLatitude  prefix
      <*> parseLongitude prefix
  where
    mkPoint :: Int32 -> Int32 -> Proto Point
    mkPoint latitude longitude =
        defMessage
          & #latitude  .~ latitude
          & #longitude .~ longitude

parseRectangle :: Opt.Parser (Proto Rectangle)
parseRectangle =
    mkRectangle
      <$> parsePoint "lo-"
      <*> parsePoint "hi-"
  where
    mkRectangle :: Proto Point -> Proto Point -> Proto Rectangle
    mkRectangle lo hi =
        defMessage
          & #lo .~ lo
          & #hi .~ hi

parseRouteNote :: Opt.Parser (Proto RouteNote)
parseRouteNote =
    mkRouteNote
      <$> parsePoint ""
      <*> Opt.argument Opt.str (Opt.metavar "MSG")
  where
    mkRouteNote :: Proto Point -> Text -> Proto RouteNote
    mkRouteNote location message =
        defMessage
          & #location .~ location
          & #message  .~ message

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sub :: String -> String -> Opt.Parser a -> Opt.Mod Opt.CommandFields a
sub cmd desc parser =
    Opt.command cmd $
      Opt.info (parser Opt.<**> Opt.helper) (Opt.progDesc desc)

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
  , SService(..)
  , SMethod(..)
    -- * Parser
  , getCmdline
  ) where

import Prelude

import Data.Foldable (asum)
import Data.Int
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Network.Socket (HostName, PortNumber)
import Options.Applicative ((<**>))
import Options.Applicative qualified as Opt

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Common.Protobuf

import Paths_grapesy

import Proto.Helloworld
import Proto.RouteGuide

import Demo.Client.Util.DelayOr

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
    ProtobufIO
  | ProtobufPipes
  | ProtobufCanCallRPC
  | Core
  | CoreNoFinal
  deriving (Show)

data SomeMethod where
  SomeMethod :: SService s -> SMethod s m -> SomeMethod

deriving stock instance Show SomeMethod

-- | Select service
data SService :: Type -> Type where
  SGreeter    :: SService Greeter
  SRouteGuide :: SService RouteGuide

deriving stock instance Show (SService s)

-- | Select method
data SMethod :: Type -> Symbol -> Type where
  SSayHello            :: HelloRequest   -> SMethod Greeter "sayHello"
  SSayHelloStreamReply :: HelloRequest   -> SMethod Greeter "sayHelloStreamReply"
  SSayHelloBidiStream  :: [HelloRequest] -> SMethod Greeter "sayHelloBidiStream"

  SGetFeature   :: Point               -> SMethod RouteGuide "getFeature"
  SListFeatures :: Rectangle           -> SMethod RouteGuide "listFeatures"
  SRecordRoute  :: [DelayOr Point]     -> SMethod RouteGuide "recordRoute"
  SRouteChat    :: [DelayOr RouteNote] -> SMethod RouteGuide "routeChat"

deriving stock instance Show (SMethod s m)

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
            , Opt.value "localhost"
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
      Opt.flag' ProtobufIO $ mconcat [
          Opt.long "protobuf-IO"
        , Opt.help "Use the Protobuf IO API (if applicable)"
        ]
    , Opt.flag' ProtobufPipes $ mconcat [
          Opt.long "protobuf-Pipe"
        , Opt.help "Use the Protobuf Pipe API (if applicable)"
        ]
    , Opt.flag' ProtobufCanCallRPC $ mconcat [
          Opt.long "protobuf-CanCallRPC"
        , Opt.help "Use the Protobuf CanCallRPC API (if applicable)"
        ]
    , Opt.flag' Core $ mconcat [
          Opt.long "core"
        , Opt.help "Use the core API"
        ]
    , Opt.flag' CoreNoFinal $ mconcat [
          Opt.long "core-dont-mark-final"
        , Opt.help "Use the core API; don't mark the last message as final"
        ]
    , pure ProtobufIO
    ]

{-------------------------------------------------------------------------------
  Select method
-------------------------------------------------------------------------------}

parseSomeMethod :: Opt.Parser SomeMethod
parseSomeMethod = Opt.subparser $ mconcat [
      sub "sayHello" "helloworld.Greeter.SayHello" $
        SomeMethod SGreeter . SSayHello <$>
          parseHelloRequest
    , sub "sayHelloStreamReply" "helloworld.Greeter.SayHelloStreamReply" $
        SomeMethod SGreeter . SSayHelloStreamReply <$>
          parseHelloRequest
    , sub "sayHelloBidiStream" "helloworld.Greeter.SayHelloBidiStream" $
        SomeMethod SGreeter . SSayHelloBidiStream <$>
          Opt.many parseHelloRequest
    , sub "getFeature" "routeguide.RouteGuide.GetFeature" $
        SomeMethod SRouteGuide . SGetFeature <$>
          parsePoint ""
    , sub "listFeatures" "routeguide.RouteGuide.ListFeatures" $
        SomeMethod SRouteGuide . SListFeatures <$>
          parseRectangle
    , sub "recordRoute" "routeguide.RouteGuide.RecordRoute" $
        SomeMethod SRouteGuide . SRecordRoute <$>
          Opt.many (parseDelayOr $ parsePoint "")
    , sub "routeChat" "routeguide.RouteGuide.RouteChat" $
        SomeMethod SRouteGuide . SRouteChat <$>
          Opt.many (parseDelayOr $ parseRouteNote)
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

parseHelloRequest :: Opt.Parser HelloRequest
parseHelloRequest =
    mkHelloRequest <$> Opt.option Opt.str (mconcat [
        Opt.long "name"
      , Opt.metavar "NAME"
      ])
  where
    mkHelloRequest :: Text -> HelloRequest
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

parsePoint :: String -> Opt.Parser Point
parsePoint prefix =
    mkPoint
      <$> parseLatitude  prefix
      <*> parseLongitude prefix
  where
    mkPoint :: Int32 -> Int32 -> Point
    mkPoint latitude longitude =
        defMessage
          & #latitude  .~ latitude
          & #longitude .~ longitude

parseRectangle :: Opt.Parser Rectangle
parseRectangle =
    mkRectangle
      <$> parsePoint "lo-"
      <*> parsePoint "hi-"
  where
    mkRectangle lo hi =
        defMessage
          & #lo .~ lo
          & #hi .~ hi

parseRouteNote :: Opt.Parser RouteNote
parseRouteNote =
    mkRouteNote
      <$> parsePoint ""
      <*> Opt.argument Opt.str (Opt.metavar "MSG")
  where
    mkRouteNote :: Point -> Text -> RouteNote
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

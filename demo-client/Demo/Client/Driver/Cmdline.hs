{-# LANGUAGE OverloadedLabels #-}

-- | Command line options
--
-- Intended for unqualified import.
module Demo.Client.Driver.Cmdline (
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

import Control.Lens ((.~))
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Int
import Data.Kind
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Options.Applicative qualified as Opt

import Network.GRPC.Client (Authority(..))
import Network.GRPC.Common.Compression (Compression)
import Network.GRPC.Common.Compression qualified as Compr

import Proto.Helloworld
import Proto.RouteGuide

import Demo.Client.Driver.DelayOr

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdAuthority   :: Authority
    , cmdDebug       :: Bool
    , cmdTimeout     :: Maybe Word
    , cmdAPI         :: API
    , cmdCompression :: Maybe Compression
    , cmdMethod      :: SomeMethod
    }
  deriving (Show)

-- | Which API to use?
data API =
    Protobuf
  | ProtobufPipes
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
  SSayHello            :: [HelloRequest] -> SMethod Greeter "sayHello"
  SSayHelloStreamReply :: HelloRequest   -> SMethod Greeter "sayHelloStreamReply"

  SGetFeature   :: Point               -> SMethod RouteGuide "getFeature"
  SListFeatures :: Rectangle           -> SMethod RouteGuide "listFeatures"
  SRecordRoute  :: [DelayOr Point]     -> SMethod RouteGuide "recordRoute"
  SRouteChat    :: [DelayOr RouteNote] -> SMethod RouteGuide "routeChat"

deriving stock instance Show (SMethod s m)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

getCmdline :: IO Cmdline
getCmdline = Opt.execParser $
    Opt.info (parseCmdline Opt.<**> Opt.helper) Opt.fullDesc

parseCmdline :: Opt.Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseAuthority
      <*> (Opt.switch $ mconcat [
               Opt.long "debug"
             , Opt.help "Enable debug output"
             ])
      <*> (Opt.optional $ Opt.option Opt.auto $ mconcat [
               Opt.long "timeout"
             , Opt.metavar "SECONDS"
             ])
      <*> parseAPI
      <*> Opt.optional parseCompression
      <*> parseSomeMethod

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

parseAuthority :: Opt.Parser Authority
parseAuthority =
    Authority
      <$> (Opt.option Opt.str $ mconcat [
              Opt.long "host"
            , Opt.showDefault
            , Opt.value "127.0.0.1"
            ])
      <*> (Opt.option Opt.auto $ mconcat [
              Opt.long "port"
            , Opt.showDefault
            , Opt.value 50051
            ])

parseCompression :: Opt.Parser Compression
parseCompression = asum [
      Opt.flag' Compr.gzip $ mconcat [
          Opt.long "gzip"
        , Opt.help "Use GZip compression for all messages"
        ]
    ]

parseAPI :: Opt.Parser API
parseAPI = asum [
      Opt.flag' Protobuf $ mconcat [
          Opt.long "protobuf"
        , Opt.help "Use the Protobuf API (if applicable)"
        ]
    , Opt.flag' ProtobufPipes $ mconcat [
          Opt.long "protobuf-pipes"
        , Opt.help "Use the Protobuf pipes API (if applicable)"
        ]
    , Opt.flag' Core $ mconcat [
          Opt.long "core"
        , Opt.help "Use the core API"
        ]
    , Opt.flag' CoreNoFinal $ mconcat [
          Opt.long "core-dont-mark-final"
        , Opt.help "Use the core API; don't mark the last message as final"
        ]
    , pure Protobuf
    ]

{-------------------------------------------------------------------------------
  Select method
-------------------------------------------------------------------------------}

parseSomeMethod :: Opt.Parser SomeMethod
parseSomeMethod = Opt.subparser $ mconcat [
      sub "sayHello" "helloworld.Greeter.SayHello" $
        SomeMethod SGreeter . SSayHello <$>
          Opt.many parseHelloRequest
    , sub "sayHelloStreamReply" "helloworld.Greeter.SayHelloStreamReply" $
        SomeMethod SGreeter . SSayHelloStreamReply <$>
          parseHelloRequest
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
          Opt.many (parseDelayOr parseRouteNote)
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

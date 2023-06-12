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

import Control.Lens ((.~))
import Data.Function ((&))
import Data.Int
import Data.Kind
import Data.ProtoLens
import Data.ProtoLens.Labels ()
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Options.Applicative

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
getCmdline = execParser $ info (parseCmdline <**> helper) fullDesc

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> parseAuthority
      <*> (switch $ mconcat [
               long "debug"
             , help "Enable debug output"
             ])
      <*> (optional $ option auto $ mconcat [
               long "timeout"
             , metavar "SECONDS"
             ])
      <*> parseAPI
      <*> optional parseCompression
      <*> parseSomeMethod

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

parseAuthority :: Parser Authority
parseAuthority =
    Authority
      <$> (option str $ mconcat [
              long "host"
            , showDefault
            , value "127.0.0.1"
            ])
      <*> (option auto $ mconcat [
              long "port"
            , showDefault
            , value 50051
            ])

parseCompression :: Parser Compression
parseCompression = asum [
      flag' Compr.gzip $ mconcat [
          long "gzip"
        , help "Use GZip compression for all messages"
        ]
    ]

parseAPI :: Parser API
parseAPI = asum [
      flag' Protobuf $ mconcat [
          long "protobuf"
        , help "Use the Protobuf API (if applicable)"
        ]
    , flag' ProtobufPipes $ mconcat [
          long "protobuf-pipes"
        , help "Use the Protobuf pipes API (if applicable)"
        ]
    , flag' Core $ mconcat [
          long "core"
        , help "Use the core API"
        ]
    , flag' CoreNoFinal $ mconcat [
          long "core-dont-mark-final"
        , help "Use the core API; don't mark the last message as final"
        ]
    , pure Protobuf
    ]

{-------------------------------------------------------------------------------
  Select method
-------------------------------------------------------------------------------}

parseSomeMethod :: Parser SomeMethod
parseSomeMethod = subparser $ mconcat [
      sub "sayHello" "helloworld.Greeter.SayHello" $
        SomeMethod SGreeter . SSayHello <$>
          many parseHelloRequest
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
          many (parseDelayOr $ parsePoint "")
    , sub "routeChat" "routeguide.RouteGuide.RouteChat" $
        SomeMethod SRouteGuide . SRouteChat <$>
          many (parseDelayOr parseRouteNote)
    ]

{-------------------------------------------------------------------------------
  Method arguments
-------------------------------------------------------------------------------}

parseDelayOr :: Parser a -> Parser (DelayOr a)
parseDelayOr p = asum [
      Exec <$> p
    , Delay <$> (option auto $ mconcat [
          long "delay"
        , help "Delay by specified length in seconds"
        , metavar "DOUBLE"
        ])
    ]

parseHelloRequest :: Parser HelloRequest
parseHelloRequest =
    mkHelloRequest <$> option str (mconcat [
        long "name"
      , metavar "NAME"
      ])
  where
    mkHelloRequest :: Text -> HelloRequest
    mkHelloRequest name = (defMessage & #name .~ name)

parseLatitude :: String -> Parser Int32
parseLatitude prefix =
    option auto $ mconcat [
        long $ prefix ++ "latitude"
      ]

parseLongitude :: String -> Parser Int32
parseLongitude prefix =
    option auto $ mconcat [
        long $ prefix ++ "longitude"
      ]

parsePoint :: String -> Parser Point
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

parseRectangle :: Parser Rectangle
parseRectangle =
    mkRectangle
      <$> parsePoint "lo-"
      <*> parsePoint "hi-"
  where
    mkRectangle lo hi =
        defMessage
          & #lo .~ lo
          & #hi .~ hi

parseRouteNote :: Parser RouteNote
parseRouteNote =
    mkRouteNote
      <$> parsePoint ""
      <*> argument str (metavar "MSG")
  where
    mkRouteNote :: Point -> Text -> RouteNote
    mkRouteNote location message =
        defMessage
          & #location .~ location
          & #message  .~ message

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

sub :: String -> String -> Parser a -> Mod CommandFields a
sub cmd desc parser =
    command cmd $
      info (parser <**> helper) (progDesc desc)

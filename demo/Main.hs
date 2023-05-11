-- | Demo client
--
-- See @docs/demo.md@ for documentation.
module Main (main) where

import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Default
import Data.List.NonEmpty qualified as NE

import Network.GRPC.Client

import Demo.Driver.Cmdline
import Demo.Driver.DelayOr
import Demo.Driver.Logging

import Demo.API.Core.NoFinal.Greeter      qualified as NoFinal.Greeter
import Demo.API.Protobuf.Greeter          qualified as PBuf.Greeter
import Demo.API.Protobuf.Pipes.RouteGuide qualified as Pipes.RouteGuide
import Demo.API.Protobuf.RouteGuide       qualified as PBuf.RouteGuide

{-------------------------------------------------------------------------------
  Application entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmd <- getCmdline

    let unsupportedMode :: IO a
        unsupportedMode = throwIO $ userError $ concat [
              "Mode "
            , show (cmdAPI cmd)
            , " not supported for "
            , show (cmdMethod cmd)
            ]

    withConnection (connParams cmd) $ \conn ->
      case cmdMethod cmd of
        SomeMethod SGreeter (SSayHello names) ->
          forM_ names $ \name ->
            case cmdAPI cmd of
              Protobuf ->
                PBuf.Greeter.sayHello conn (callParams cmd) name
              CoreNoFinal ->
                NoFinal.Greeter.sayHello conn (callParams cmd) name
              _otherwise ->
                unsupportedMode
        SomeMethod SGreeter (SSayHelloStreamReply name) ->
          case cmdAPI cmd of
            Protobuf ->
              PBuf.Greeter.sayHelloStreamReply conn (callParams cmd) name
            _otherwise ->
              unsupportedMode
        SomeMethod SRouteGuide (SGetFeature p) ->
          case cmdAPI cmd of
            Protobuf ->
              PBuf.RouteGuide.getFeature conn (callParams cmd) p
            _otherwise ->
              unsupportedMode
        SomeMethod SRouteGuide (SListFeatures r) ->
          case cmdAPI cmd of
            ProtobufPipes ->
              Pipes.RouteGuide.listFeatures conn (callParams cmd) r
            Protobuf ->
              PBuf.RouteGuide.listFeatures conn (callParams cmd) r
            _otherwise ->
              unsupportedMode
        SomeMethod SRouteGuide (SRecordRoute ps) ->
          case cmdAPI cmd of
            ProtobufPipes ->
              Pipes.RouteGuide.recordRoute conn (callParams cmd) $ yieldAll ps
            Protobuf ->
              PBuf.RouteGuide.recordRoute conn (callParams cmd) =<< execAll ps
            _otherwise ->
              unsupportedMode
        SomeMethod SRouteGuide (SRouteChat notes) ->
          case cmdAPI cmd of
            ProtobufPipes ->
              Pipes.RouteGuide.routeChat conn (callParams cmd) $ yieldAll notes
            Protobuf ->
              PBuf.RouteGuide.routeChat conn (callParams cmd) =<< execAll notes
            _otherwise ->
              unsupportedMode

{-------------------------------------------------------------------------------
  Interpret command line
-------------------------------------------------------------------------------}

connParams :: Cmdline -> ConnParams
connParams cmd = defaults {
      connTracer =
        if cmdDebug cmd
          then contramap show threadSafeTracer
          else connTracer defaults
    , connCompression =
        case cmdCompression cmd of
          Just alg -> NE.singleton alg
          Nothing  -> connCompression defaults
    }
  where
    defaults :: ConnParams
    defaults = defaultConnParams $ cmdAuthority cmd

callParams :: Cmdline -> PerCallParams
callParams cmd = def{
      callTimeout = Timeout Second . TimeoutValue <$> cmdTimeout cmd
    }


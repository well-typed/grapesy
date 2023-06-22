-- | Demo client
--
-- See @docs/demo.md@ for documentation.
module Main (main) where

import Control.Exception
import Control.Monad
import Control.Tracer
import Data.Default
import System.Mem (performMajorGC)

import Network.GRPC.Client
import Network.GRPC.Common.Compression qualified as Compr

import Demo.Client.Cmdline
import Demo.Client.Util.DelayOr
import Demo.Common.Logging

import Demo.Client.API.Core.Greeter              qualified as Core.Greeter
import Demo.Client.API.Core.NoFinal.Greeter      qualified as NoFinal.Greeter
import Demo.Client.API.Core.RouteGuide           qualified as Core.RouteGuide
import Demo.Client.API.Protobuf.Greeter          qualified as PBuf.Greeter
import Demo.Client.API.Protobuf.Pipes.RouteGuide qualified as Pipes.RouteGuide
import Demo.Client.API.Protobuf.RouteGuide       qualified as PBuf.RouteGuide

{-------------------------------------------------------------------------------
  Application entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmd <- getCmdline
    withConnection (connParams cmd) (cmdServer cmd) $ \conn ->
      dispatch cmd conn (cmdMethod cmd)
    performMajorGC

dispatch :: Cmdline -> Connection -> SomeMethod -> IO ()
dispatch cmd conn = \case
    SomeMethod SGreeter (SSayHello names) ->
      forM_ names $ \name -> do
        case cmdAPI cmd of
          Protobuf ->
            PBuf.Greeter.sayHello conn name
          CoreNoFinal ->
            NoFinal.Greeter.sayHello conn name
          _otherwise ->
            unsupportedMode
        performMajorGC
    SomeMethod SGreeter (SSayHelloStreamReply name) ->
      case cmdAPI cmd of
        Core ->
          Core.Greeter.sayHelloStreamReply conn name
        Protobuf ->
          PBuf.Greeter.sayHelloStreamReply conn name
        _otherwise ->
          unsupportedMode
    SomeMethod SRouteGuide (SGetFeature p) ->
      case cmdAPI cmd of
        Protobuf ->
          PBuf.RouteGuide.getFeature conn p
        _otherwise ->
          unsupportedMode
    SomeMethod SRouteGuide (SListFeatures r) ->
      case cmdAPI cmd of
        ProtobufPipes ->
          Pipes.RouteGuide.listFeatures conn r
        Protobuf ->
          PBuf.RouteGuide.listFeatures conn r
        Core ->
          Core.RouteGuide.listFeatures conn r
        _otherwise ->
          unsupportedMode
    SomeMethod SRouteGuide (SRecordRoute ps) ->
      case cmdAPI cmd of
        ProtobufPipes ->
          Pipes.RouteGuide.recordRoute conn $ yieldAll ps
        Protobuf ->
          PBuf.RouteGuide.recordRoute conn =<< execAll ps
        _otherwise ->
          unsupportedMode
    SomeMethod SRouteGuide (SRouteChat notes) ->
      case cmdAPI cmd of
        ProtobufPipes ->
          Pipes.RouteGuide.routeChat conn $ yieldAll notes
        Protobuf ->
          PBuf.RouteGuide.routeChat conn =<< execAll notes
        _otherwise ->
          unsupportedMode
  where
    unsupportedMode :: IO a
    unsupportedMode = throwIO $ userError $ concat [
          "Mode "
        , show (cmdAPI cmd)
        , " not supported for "
        , show (cmdMethod cmd)
        ]

{-------------------------------------------------------------------------------
  Interpret command line
-------------------------------------------------------------------------------}

connParams :: Cmdline -> ConnParams
connParams cmd = def {
      connTracer =
        if cmdDebug cmd
          then contramap show threadSafeTracer
          else connTracer def
    , connCompression =
        case cmdCompression cmd of
          Just alg -> Compr.require alg
          Nothing  -> connCompression def
    , connDefaultTimeout =
        Timeout Second . TimeoutValue <$> cmdTimeout cmd
    }


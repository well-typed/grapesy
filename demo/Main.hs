-- | Demo client
--
-- See @docs/demo.md@ for documentation.
module Main (main) where

import Control.Monad
import Control.Tracer
import Data.Default

import Network.GRPC.Client
import Network.GRPC.Compression qualified as Compresion

import Demo.Driver.Cmdline
import Demo.Driver.DelayOr
import Demo.Driver.Logging

import Demo.GRPC.Protobuf.Greeter          qualified as IO.Greeter
import Demo.GRPC.Protobuf.Pipes.RouteGuide qualified as Pi.RouteGuide
import Demo.GRPC.Protobuf.RouteGuide       qualified as IO.RouteGuide

{-------------------------------------------------------------------------------
  Application entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmd <- getCmdline

    withConnection (connParams cmd) $ \conn ->
      case cmdMethod cmd of
        SomeMethod SGreeter (SSayHello names) ->
          forM_ names $ IO.Greeter.sayHello conn (callParams cmd)
        SomeMethod SGreeter (SSayHelloStreamReply name) ->
          IO.Greeter.sayHelloStreamReply conn (callParams cmd) name
        SomeMethod SRouteGuide (SGetFeature p) ->
          IO.RouteGuide.getFeature conn (callParams cmd) p
        SomeMethod SRouteGuide (SListFeatures r) ->
          if cmdUsePipes cmd
            then Pi.RouteGuide.listFeatures conn (callParams cmd) r
            else IO.RouteGuide.listFeatures conn (callParams cmd) r
        SomeMethod SRouteGuide (SRecordRoute ps) ->
          if cmdUsePipes cmd
            then Pi.RouteGuide.recordRoute conn (callParams cmd) $ yieldAll ps
            else IO.RouteGuide.recordRoute conn (callParams cmd) =<< execAll ps
        SomeMethod SRouteGuide (SRouteChat notes) ->
          if cmdUsePipes cmd
            then Pi.RouteGuide.routeChat conn (callParams cmd) $ yieldAll notes
            else IO.RouteGuide.routeChat conn (callParams cmd) =<< execAll notes

{-------------------------------------------------------------------------------
  Interpret command line
-------------------------------------------------------------------------------}

connParams :: Cmdline -> ConnParams
connParams cmd = defaults {
      connTracer =
        if cmdDebug cmd
          then contramap show threadSafeTracer
          else connTracer defaults
    , connUpdateCompression =
        case cmdCompression cmd of
          Just alg -> Compresion.forceAlgorithm alg
          Nothing  -> connUpdateCompression defaults
    }
  where
    defaults :: ConnParams
    defaults = defaultConnParams $ cmdAuthority cmd

callParams :: Cmdline -> CallParams
callParams cmd = def{
      callTimeout = Timeout Second . TimeoutValue <$> cmdTimeout cmd
    }


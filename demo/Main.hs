-- | Demo client
--
-- See @docs/demo.md@ for documentation.
module Main (main) where

import Control.Tracer
import Data.Default

import Network.GRPC.Client
import Network.GRPC.Spec

import Demo.Driver.Cmdline
import Demo.Driver.DelayOr
import Demo.Driver.Logging

import Demo.GRPC.Protobuf.Greeter          qualified as Greeter
import Demo.GRPC.Protobuf.Pipes.RouteGuide qualified as Pipes.RouteGuide
import Demo.GRPC.Protobuf.RouteGuide       qualified as RouteGuide

{-------------------------------------------------------------------------------
  Application entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmd <- getCmdline

    connect cmd $ \conn ->
      case cmdMethod cmd of
        SomeMethod SGreeter (SSayHello name) ->
          Greeter.sayHello conn (meta cmd) name
        SomeMethod SGreeter (SSayHelloStreamReply name) ->
          Greeter.sayHelloStreamReply conn (meta cmd) name
        SomeMethod SRouteGuide (SGetFeature p) ->
          RouteGuide.getFeature conn (meta cmd) p
        SomeMethod SRouteGuide (SListFeatures r) ->
          if cmdUsePipes cmd
            then Pipes.RouteGuide.listFeatures conn (meta cmd) r
            else RouteGuide.listFeatures conn (meta cmd) r
        SomeMethod SRouteGuide (SRecordRoute ps) ->
          if cmdUsePipes cmd
            then Pipes.RouteGuide.recordRoute conn (meta cmd) (yieldAll ps)
            else RouteGuide.recordRoute conn (meta cmd) =<< execAll ps
        SomeMethod SRouteGuide (SRouteChat notes) ->
          if cmdUsePipes cmd
            then Pipes.RouteGuide.routeChat conn (meta cmd) (yieldAll notes)
            else RouteGuide.routeChat conn (meta cmd) =<< execAll notes

{-------------------------------------------------------------------------------
  Interpret command line
-------------------------------------------------------------------------------}

connect :: Cmdline -> (Connection -> IO a) -> IO a
connect cmd = withConnection (connectionTracer cmd) Http (cmdAuthority cmd)

connectionTracer :: Show a => Cmdline -> Tracer IO a
connectionTracer cmd
  | cmdDebug cmd = contramap show threadSafeTracer
  | otherwise    = nullTracer

-- | Request meta-information
meta :: Cmdline -> RequestMeta
meta cmd = def{
      requestTimeout = Timeout Second . TimeoutValue <$> cmdTimeout cmd
    }


-- | Demo client
--
-- See @docs/demo.md@ for documentation.
module Main (main) where

import Control.Concurrent
import Control.Exception
import System.IO

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr

import Demo.Client.Cmdline
import Demo.Client.Util.DelayOr qualified as DelayOr

import Demo.Client.API.Core.Greeter                  qualified as Core.Greeter
import Demo.Client.API.Core.NoFinal.Greeter          qualified as NoFinal.Greeter
import Demo.Client.API.Core.RouteGuide               qualified as Core.RouteGuide
import Demo.Client.API.StreamType.Conduit.RouteGuide qualified as Conduit.RouteGuide
import Demo.Client.API.StreamType.IO.Greeter         qualified as IO.Greeter
import Demo.Client.API.StreamType.IO.Ping            qualified as IO.Ping
import Demo.Client.API.StreamType.IO.RouteGuide      qualified as IO.RouteGuide
import Demo.Client.API.StreamType.MonadStack.Greeter qualified as CanCallRPC.Greeter

{-------------------------------------------------------------------------------
  Application entry point
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- For easier debugging
    cmd <- getCmdline
    withConnection (connParams cmd) (cmdServer cmd) $
      DelayOr.forM_ (cmdMethods cmd) . dispatch cmd

dispatch :: Cmdline -> Connection -> SomeMethod -> IO ()
dispatch cmd conn method =
    case method of
      SomeMethod (SSayHello name) ->
        case cmdAPI cmd of
          StreamTypeIO ->
            IO.Greeter.sayHello conn name
          StreamTypeMonadStack ->
            CanCallRPC.Greeter.sayHello conn name
          CoreNoFinal ->
            NoFinal.Greeter.sayHello conn name
          _otherwise ->
            unsupportedMode
      SomeMethod (SSayHelloStreamReply name) ->
        case cmdAPI cmd of
          Core ->
            Core.Greeter.sayHelloStreamReply conn name
          StreamTypeIO ->
            IO.Greeter.sayHelloStreamReply conn name
          StreamTypeMonadStack ->
            CanCallRPC.Greeter.sayHelloStreamReply conn name
          _otherwise ->
            unsupportedMode
      SomeMethod (SSayHelloBidiStream names) ->
        case cmdAPI cmd of
          Core ->
            Core.Greeter.sayHelloBidiStream conn names
          _otherwise ->
            unsupportedMode
      SomeMethod (SGetFeature p) ->
        case cmdAPI cmd of
          StreamTypeIO ->
            IO.RouteGuide.getFeature conn p
          _otherwise ->
            unsupportedMode
      SomeMethod (SListFeatures r) ->
        case cmdAPI cmd of
          StreamTypeConduit ->
            Conduit.RouteGuide.listFeatures conn r
          StreamTypeIO ->
            IO.RouteGuide.listFeatures conn r
          Core ->
            Core.RouteGuide.listFeatures conn r
          _otherwise ->
            unsupportedMode
      SomeMethod (SRecordRoute ps) ->
        case cmdAPI cmd of
          StreamTypeConduit ->
            Conduit.RouteGuide.recordRoute conn ps
          StreamTypeIO ->
            IO.RouteGuide.recordRoute conn ps
          _otherwise ->
            unsupportedMode
      SomeMethod (SRouteChat notes) ->
        case cmdAPI cmd of
          StreamTypeConduit ->
            Conduit.RouteGuide.routeChat conn notes
          StreamTypeIO ->
            IO.RouteGuide.routeChat conn notes
          _otherwise ->
            unsupportedMode
      SomeMethod (SPing msg) ->
        case cmdAPI cmd of
          StreamTypeIO ->
            IO.Ping.ping conn msg
          _otherwise ->
            unsupportedMode
  where
    unsupportedMode :: IO a
    unsupportedMode = throwIO $ userError $ concat [
          "Mode "
        , show (cmdAPI cmd)
        , " not supported for "
        , show method
        ]

{-------------------------------------------------------------------------------
  Interpret command line
-------------------------------------------------------------------------------}

connParams :: Cmdline -> ConnParams
connParams cmd = def {
      connCompression =
        case cmdCompression cmd of
          Just alg -> Compr.only alg
          Nothing  -> connCompression def
    , connDefaultTimeout =
        Timeout Second . TimeoutValue <$> cmdTimeout cmd
    , connReconnectPolicy =
        exponentialBackoff waitFor 1.5 (1, 2) 10
    }
  where
    waitFor :: Int -> IO ()
    waitFor delay = do
        putStrLn $ "Disconnected. Reconnecting after " ++ show delay ++ "μs"
        threadDelay delay
        putStrLn "Reconnecting now."



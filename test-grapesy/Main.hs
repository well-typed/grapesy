module Main (main) where

{-
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Void
import Debug.Trace

import Network.GRPC.Util.Parser
import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Thread

data Debug = Debug
data DebugIn
data DebugOut

instance DataFlow DebugIn where
  data Headers    DebugIn = InHeaders deriving (Show)
  type Message    DebugIn = Void
  type NoMessages DebugIn = ()
  type Trailers   DebugIn = ()

instance DataFlow DebugOut where
  data Headers    DebugOut = OutHeaders deriving (Show)
  type Message    DebugOut = Void
  type NoMessages DebugOut = ()
  type Trailers   DebugOut = ()

instance IsSession Debug where
  type Inbound Debug  = DebugIn
  type Outbound Debug = DebugOut

  parseInboundTrailers  _ _ = return ()
  buildOutboundTrailers _ _ = []
  parseMsg              _ _ = ParserError "Never parsing anything"
  buildMsg              _ _ = absurd

data RecvMessageLoopDied = RecvMessageLoopDied
  deriving (Show, Exception)

main :: IO ()
main = do
    channel :: Channel Debug <- initChannel

    forkThread (channelInbound channel) newEmptyTMVarIO $ \unmask stVar -> do
      regular <- initFlowStateRegular InHeaders
      atomically $ putTMVar stVar $ FlowStateRegular regular
      traceIO "threadBody 1"
      threadDelay 5_000_000
      throw $ RecvMessageLoopDied

    x <- atomically $ recv channel

    print x

-}

import Test.Tasty

import Test.Prop.Dialogue                     qualified as Dialogue
import Test.Prop.Serialization                qualified as Serialization
import Test.Sanity.StreamingType.NonStreaming qualified as StreamingType.NonStreaming
import Test.Sanity.HalfClosedLocal            qualified as HalfClosedLocal

main :: IO ()
main = defaultMain $ testGroup "grapesy" [
      testGroup "Sanity" [
          HalfClosedLocal.tests
        , testGroup "StreamingType" [
              StreamingType.NonStreaming.tests
            ]
        ]
    , testGroup "Prop" [
          Serialization.tests
        , Dialogue.tests
        ]
    ]


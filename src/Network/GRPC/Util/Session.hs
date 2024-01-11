-- | Session interface
--
-- A \"session\" is a series of messages exchanged by two nodes in a network;
-- we might be a client and our peer might be a server, or we might be a
-- server and our peer might be a client. Here we provide a abstraction which
--
-- * takes care of concurrency issues
-- * is typed, with different types for inbound and outbound headers, messages
--   and trailers
-- * works the same way whether we are a client or a server.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Util.Session qualified as Session
module Network.GRPC.Util.Session (
    -- * Session API
    DataFlow(..)
  , FlowStart(..)
  , IsSession(..)
  , InitiateSession(..)
  , AcceptSession(..)
    -- ** Raw request/response info
  , RequestInfo(..)
  , ResponseInfo(..)
    -- ** Exceptions
  , PeerException(..)
    -- * Channel
  , Channel -- opaque
    -- ** Working with an open channel
  , getInboundHeaders
  , send
  , recv
  , RecvAfterFinal(..)
  , SendAfterFinal(..)
    -- ** Closing
  , waitForOutbound
  , close
  , ChannelUncleanClose(..)
  , ChannelDiscarded(..)
  , ChannelException(..)
  , ChannelAborted(..)
    -- ** Construction
    -- *** Client
  , ConnectionToServer(..)
  , setupRequestChannel
    -- *** Server
  , ConnectionToClient(..)
  , setupResponseChannel
    -- ** Status
  , ChannelStatus(..)
  , FlowStatus(..)
  , checkChannelStatus
  , isChannelHealthy
    -- ** Logging
  , DebugMsg(..)
  ) where

import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Session.Client
import Network.GRPC.Util.Session.Server

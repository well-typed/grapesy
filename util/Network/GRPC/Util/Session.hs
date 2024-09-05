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
  , NoTrailers(..)
    -- ** Raw request/response info
  , RequestInfo(..)
  , ResponseInfo(..)
    -- ** Exceptions
  , PeerException(..)
    -- * Channel
  , Channel(..)
    -- ** Working with an open channel
  , getInboundHeaders
  , send
  , recvBoth
  , recvEither
  , RecvFinal(..)
  , RecvAfterFinal(..)
  , SendAfterFinal(..)
    -- ** Closing
  , waitForOutbound
  , close
  , ChannelDiscarded(..)
  , ChannelAborted(..)
    -- ** Half-closing
  , AllowHalfClosed(..)
    -- ** Construction
    -- *** Client
  , ConnectionToServer(..)
  , CancelRequest
  , setupRequestChannel
    -- *** Server
  , ConnectionToClient(..)
  , setupResponseChannel
  ) where

import Network.GRPC.Util.Session.API
import Network.GRPC.Util.Session.Channel
import Network.GRPC.Util.Session.Client
import Network.GRPC.Util.Session.Server

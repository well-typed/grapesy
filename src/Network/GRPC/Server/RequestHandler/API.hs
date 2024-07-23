-- | Definition of 'RequestHandler'
--
-- Defined in a separate module to avoid cyclic module dependencies.
--
-- Intended for unqualified import.
module Network.GRPC.Server.RequestHandler.API (
    RequestHandler
  , requestHandlerToServer
  ) where

import Control.Exception

import Network.HTTP2.Server qualified as HTTP2

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | HTTP2 request handler
type RequestHandler a =
     (forall x. IO x -> IO x)
  -> HTTP2.Request
  -> (HTTP2.Response -> IO ())
  -> IO a

-- | Construct @http2@ handler
requestHandlerToServer ::
     RequestHandler ()
     -- ^ Request handler
     --
     -- We can assume in 'requestHandlerToServer' that the handler will not
     -- throw any exceptions(doing so will cause @http2@ to reset the stream,
     -- which is not always the right thing to do; see detailed comments in
     -- 'acceptCall'). It is the responsibility of 'serverTopLevel' (prior to
     -- calling 'requestHandlerToServer') to catch any remaining exceptions.
  -> HTTP2.Server
requestHandlerToServer handler req _aux respond =
    -- We start by masking asynchronous exceptions. It is possible that
    -- http2 kills us /before/ this call to @mask@, but if it does, no harm
    -- is done: nothing has happened yet, so nothing is interrupted.
    mask $ \unmask ->
      handler unmask req (\resp -> respond resp [])

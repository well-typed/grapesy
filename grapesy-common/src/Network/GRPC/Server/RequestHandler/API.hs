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

import Network.HTTP.Semantics.Server qualified as Server
import Network.GRPC.Util.GHC

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | HTTP2 request handler
type RequestHandler a =
     (forall x. IO x -> IO x)
  -> Server.Request
  -> (Server.Response -> IO ())
  -> IO a

-- | Construct @http2@ handler
requestHandlerToServer ::
     RequestHandler ()
     -- ^ Request handler
     --
     -- We can assume in 'requestHandlerToServer' that the handler will not
     -- throw any exceptions (doing so will cause @http2@ to reset the stream,
     -- which is not always the right thing to do; see detailed comments in
     -- 'runHandler'). It is the responsibility of 'serverTopLevel' (prior to
     -- calling 'requestHandlerToServer') to catch any remaining exceptions.
  -> Server.Server
requestHandlerToServer handler req _aux respond =
    -- We start by masking asynchronous exceptions. Ideally http2 would start
    -- the thread with exceptions /already/ masked; the fact that it doesn't
    -- means that it is possible that
    --
    -- 1. @http2@ kills the thread /before/ the handler begins
    -- 2. @http2@ kills the thread /after/ the handler completes
    --
    -- Either way no harm is done; to avoid confusion, however, we will label
    -- the thread only /inside/ of the @mask@; in case (1) we'd get the label
    -- as it appears in HTTP2 ("H2 response sender for stream"); in case (2) we'd
    -- get the "done" label we set here.
    mask $ \unmask -> do
      labelThisThread "grapesy:requestHandler"
      handler unmask req (\resp -> respond resp [])
      -- /Only/ label if the handler itself didn't throw any exceptions
      labelThisThread "grapesy:requestHandler:done"

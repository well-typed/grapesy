-- | Definition of 'RequestHandler'
--
-- Defined in a separate module to avoid cyclic module dependencies.
--
-- Intended for unqualified import.
module Network.GRPC.Server.RequestHandler.API (
    RequestHandler
  , requestHandlerToServer
  ) where

import Control.Monad.XIO (XIO', NeverThrows)
import Control.Monad.XIO qualified as XIO
import Network.HTTP2.Server qualified as HTTP2

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | HTTP2 request handler
type RequestHandler e a = HTTP2.Request -> (HTTP2.Response -> IO ()) -> XIO' e a

-- | Construct @http2@ handler
requestHandlerToServer ::
     RequestHandler NeverThrows ()
     -- ^ Request handler
     --
     -- Handlers should never throw exceptions, as doing so will cause @http2@
     -- to reset the stream, which is not always the right thing to do (see
     -- detailed comments in 'acceptCall').
  -> HTTP2.Server
requestHandlerToServer handler req _aux respond =
    XIO.run $ handler req (\resp -> respond resp [])

-- | Protobuf handlers
--
-- These are used by both the client and the server API, although that use is
-- dual: in the client API we /provide/ handlers for the user to use, and in the
-- server API we /require/ handlers from the user.
--
-- However, /this/ module is NOT part of the public API. Instead:
--
-- * "Network.GRPC.Client.Protobuf" exports the types (opaque) along with
--   functions to /execute/ the handlers.
--
--   Here the user selects a particular type of handler (@nonStreaming@,
--   @clientStreaming@, etc.). To ensure that they choose the /right/ type for
--   the particular RPC call, the execution functions impose a constraint on the
--   'MethodStreamingType'.
--
--   By having the user choose the type of RPC call, it becomes very clear what
--   kind of arguments are expected.
--
-- * "Network.GRPC.Server.Protobuf" exports the types (opaque) along with
--   functions to /construct/ the handlers.
--
--   In this case we are declaring a set of handlers for a particular service,
--   and the /service/ dictates what kind of handlers we expect. Instead of
--   relying on a constraint, we therefore use a type family to choose the
--   right type of handler; this improves type inference for the user.
--
-- In the rest of the library we then don't have to worry that handlers are
-- matched with the right type of RPC, resulting in a more uniform design.
module Network.GRPC.Common.Protobuf (
    -- * Handler types
    NonStreamingHandler(..)
  , ClientStreamingHandler(..)
  , ServerStreamingHandler(..)
  , BiDiStreamingHandler(..)
  , HandlerFor
  ) where

import Data.Kind
import Data.ProtoLens.Service.Types
import GHC.TypeLits

import Network.GRPC.Util.StreamElem

{-------------------------------------------------------------------------------
  Handler types
-------------------------------------------------------------------------------}

newtype NonStreamingHandler m serv meth = NonStreamingHandler (
         MethodInput serv meth
      -> m (MethodOutput serv meth)
    )

newtype ClientStreamingHandler m serv meth = ClientStreamingHandler (
         m (StreamElem () (MethodInput serv meth))
      -> m (MethodOutput serv meth)
    )

newtype ServerStreamingHandler m serv meth = ServerStreamingHandler (
         MethodInput serv meth
      -> (MethodOutput serv meth -> m ())
      -> m ()
    )

newtype BiDiStreamingHandler m serv meth = BiDiStreamingHandler (
         m (StreamElem () (MethodInput serv meth))
      -> (MethodOutput serv meth -> m ())
      -> m ()
    )

type family HandlerFor (typ :: StreamingType) :: (Type -> Type) -> Type -> Symbol -> Type where
  HandlerFor NonStreaming    = NonStreamingHandler
  HandlerFor ClientStreaming = ClientStreamingHandler
  HandlerFor ServerStreaming = ServerStreamingHandler
  HandlerFor BiDiStreaming   = BiDiStreamingHandler


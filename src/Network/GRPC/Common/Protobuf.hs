module Network.GRPC.Common.Protobuf (
    -- * Handler type
    Handler
  , StreamingHandler
    -- * Singleton for 'StreamingType'
  , SStreamingType(..)
  , IsStreamingType(..)
  ) where

import Data.Kind
import Data.ProtoLens.Service.Types

import Network.GRPC.Spec.CustomMetadata
import Network.GRPC.Util.StreamElem

{-------------------------------------------------------------------------------
  Handler types

  We use

  > StreamElem () (MethodInput serv meth)

  for inputs, but we do not use

  > StreamElem [HTTP.Header] (MethodOutput serv method)

  for outputs:  when we receive the final output, we cannot /tell/ if it's the
  final output or not. See detailed discussion in "Network.GRPC.Client" on
  "Final messages and trailers".

  TODO: We need to rethink the metadata. We currently return the metadata from
  the trailers, but we don't return the metadata from the initial response, nor
  do we allow to set initial metadata for the request. This seems inconsistent;
  I think the right answer here is to delegate all handling of metadata of any
  kind to the general interface and remove it from here, but needs some thought.
-------------------------------------------------------------------------------}

type family Handler m serv meth where
  Handler m serv meth =
    StreamingHandler m serv meth (MethodStreamingType serv meth)

type family StreamingHandler m serv meth typ where
  StreamingHandler m serv meth NonStreaming =
       MethodInput serv meth
    -> m (MethodOutput serv meth, [CustomMetadata])

  StreamingHandler m serv meth ClientStreaming =
       m (StreamElem () (MethodInput serv meth))
    -> m (MethodOutput serv meth, [CustomMetadata])

  StreamingHandler m serv meth ServerStreaming =
       MethodInput serv meth
    -> (MethodOutput serv meth -> m ()) -> m [CustomMetadata]

  StreamingHandler m serv meth BiDiStreaming =
       m (StreamElem () (MethodInput serv meth))
    -> (MethodOutput serv meth -> m ()) -> m [CustomMetadata]

{-------------------------------------------------------------------------------
  Singleton for 'StreamingType'
-------------------------------------------------------------------------------}

data SStreamingType :: StreamingType -> Type where
  SNonStreaming    :: SStreamingType 'NonStreaming
  SClientStreaming :: SStreamingType 'ClientStreaming
  SServerStreaming :: SStreamingType 'ServerStreaming
  SBiDiStreaming   :: SStreamingType 'BiDiStreaming

class IsStreamingType (t :: StreamingType) where
  isStreamingType :: SStreamingType t

instance IsStreamingType NonStreaming    where isStreamingType = SNonStreaming
instance IsStreamingType ClientStreaming where isStreamingType = SClientStreaming
instance IsStreamingType ServerStreaming where isStreamingType = SServerStreaming
instance IsStreamingType BiDiStreaming   where isStreamingType = SBiDiStreaming



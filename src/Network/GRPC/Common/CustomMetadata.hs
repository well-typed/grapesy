-- | Custom metadata
--
-- Clients can include custom metadata in the initial request to the server, and
-- servers can include custom metadata boh in the initial response to the client
-- as well as in the response trailers.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.CustomMetadata (CustomMetadata(..))
-- > import Network.GRPC.Common.CustomMetadata qualified as Metadata
module Network.GRPC.Common.CustomMetadata (
    CustomMetadata(..)
  ) where

import Network.GRPC.Spec.CustomMetadata

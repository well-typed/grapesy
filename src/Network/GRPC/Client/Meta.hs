-- | Meta-information we maintain about an open connection
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Client.Meta (Meta)
-- > import Network.GRPC.Client.Meta qualified as Meta
module Network.GRPC.Client.Meta (
    -- * Definition
    Meta(..)
  , init
  , update
  ) where

import Prelude hiding (init)

import Control.Monad.Catch
import Data.List.NonEmpty (NonEmpty)

import Network.GRPC.Common.Compression (Compression, CompressionId)
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Information about on open connection
data Meta = Meta {
      -- | Compression algorithm used for sending messages to the server
      --
      -- Nothing if the compression negotation has not yet happened.
      outboundCompression :: Maybe Compression
    }
  deriving stock (Show)

-- | Initial connection state
init :: Meta
init = Meta {
      outboundCompression = Nothing
    }

{-------------------------------------------------------------------------------
  Update
-------------------------------------------------------------------------------}

-- | Update 'Meta' given response headers
--
-- Returns the updated 'Meta'.
update :: MonadThrow m => Compr.Negotation -> ResponseHeaders -> Meta -> m Meta
update compr hdrs meta =
         Meta
           <$> updateCompression
                 compr
                 (responseAcceptCompression hdrs)
                 (outboundCompression meta)

-- Update choice of compression, if necessary
--
-- We have four possibilities:
--
-- a. Compression algorithms have already been set
-- b. We chose from the list of server reported supported algorithms
-- c. We rejected all of the server reported supported algorithms
-- d. The server didn't report which algorithms are supported
updateCompression ::
     MonadThrow m
  => Compr.Negotation
  -> Maybe (NonEmpty CompressionId)
  -> Maybe Compression -> m (Maybe Compression)
updateCompression negotation accepted = go
  where
    go (Just compr) = return $ Just compr              -- (a)
    go Nothing      =
        case Compr.choose negotation <$> accepted of
          Just (Right compr) -> return $ Just compr    -- (b)
          Just (Left err)    -> throwM err             -- (c)
          Nothing            -> return Nothing         -- (d)

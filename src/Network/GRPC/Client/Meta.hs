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
init :: Maybe Compression -> Meta
init initCompr = Meta {
      outboundCompression = initCompr
    }

{-------------------------------------------------------------------------------
  Update
-------------------------------------------------------------------------------}

-- | Update 'Meta' given response headers
--
-- Returns the updated 'Meta'.
update ::
     MonadThrow m
  => Compr.Negotation -> ResponseHeaders' HandledSynthesized -> Meta -> m Meta
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
-- a. We chose from the list of server reported supported algorithms
-- b. The server didn't report which algorithms are supported
-- c. Compression algorithms have already been set
-- d. We could not parse the list of compression algorithms sent by the server
updateCompression :: forall m.
     MonadThrow m
  => Compr.Negotation
  -> Either (InvalidHeaders HandledSynthesized) (Maybe (NonEmpty CompressionId))
  -> Maybe Compression -> m (Maybe Compression)
updateCompression negotation (Right accepted) = go
  where
    go :: Maybe Compression -> m (Maybe Compression)
    go Nothing      = case Compr.choose negotation <$> accepted of
                        Just compr -> return $ Just compr  -- (a)
                        Nothing    -> return Nothing       -- (b)
    go (Just compr) = return $ Just compr                  -- (c)
updateCompression _ (Left _invalid) = return               -- (d)
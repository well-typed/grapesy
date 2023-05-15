-- | Information about on open connection
--
-- This is an internal type, it is not part of the public API.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Client.Connection.Meta (ConnMeta)
-- > import Network.GRPC.Client.Connection.Meta qualified as ConnMeta
module Network.GRPC.Client.Connection.Meta (
    -- * Definition
    ConnMeta(..)
    -- * Construction
  , init
  , UnsupportedCompression(..)
  , update
    -- * Queries
  ) where

import Prelude hiding (init)

import Control.Exception
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import Network.GRPC.Spec
import Network.GRPC.Spec.Compression (Compression (..), CompressionId)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Information about on open connection
data ConnMeta = ConnMeta {
      -- | Compression algorithm for outbound messages
      --
      -- Nothing if the server has not yet reported which algorithms it
      -- supports. Making this case explicit helps to cross-check our supported
      -- algorithms with the server supported algorithms only once.
      outboundCompression :: Maybe Compression
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Initial connection state
init :: ConnMeta
init = ConnMeta {
      outboundCompression = Nothing
    }

-- | Update 'ConnMeta' given response headers
update ::
     NonEmpty Compression
  -> Headers
  -> ConnMeta -> Either UnsupportedCompression ConnMeta
update ourSupported hdrs meta = do
    -- Update choice compression, if necessary
    --
    -- We have four possibilities:
    --
    -- a. Compression algorithms have already been set
    -- b. We chose from the list of server reported supported algorithms
    -- c. We rejected all of the server reported supported algorithms
    -- d. The server didn't report which algorithms are supported
    outboundCompression <-
      case ( outboundCompression meta
           , headerAcceptCompression hdrs
           ) of
        (Just alreadySet, _) ->
          return $ Just alreadySet                                       -- (a)
        (Nothing, Just serverSupported) -> do
           let isSupported :: Compression -> Bool
               isSupported = (`elem` serverSupported) . compressionId
           case NE.filter isSupported ourSupported of
              c:_ -> Right $ Just c                                      -- (b)
              []  -> Left $ UnsupportedCompression serverSupported       -- (c)
        (Nothing, Nothing) ->
           return Nothing                                                -- (d)

    return ConnMeta{
        outboundCompression
      }

-- | Exception thrown when we were unable to pick a preferred compression
data UnsupportedCompression = UnsupportedCompression {
      serverSupportedCompression :: [CompressionId]
    }
  deriving stock (Show)
  deriving anyclass (Exception)

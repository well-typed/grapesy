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
  , updateForResponse
    -- * Queries
  , compression
  ) where

import Prelude hiding (init)

import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.SOP

import Network.GRPC.Client.Connection.Params
import Network.GRPC.Compression (UnsupportedCompression)
import Network.GRPC.Compression qualified as Compression
import Network.GRPC.Spec.HTTP2.Response (ResponseHeaders)
import Network.GRPC.Spec.HTTP2.Response qualified as Response

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Information about on open connection
data ConnMeta = ConnMeta {
      -- | Compression preference, if set
      --
      -- Nothing if the server has not yet reported which algorithms it
      -- supports.
      preferredCompression :: Maybe Compression.Preference
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Initial connection state
init :: ConnMeta
init = ConnMeta {
      preferredCompression = Nothing
    }

-- | Update 'ConnMeta' given response headers
updateForResponse ::
     ConnParams
  -> ResponseHeaders I
  -> ConnMeta -> Either UnsupportedCompression ConnMeta
updateForResponse params hdrs meta = do
    -- Update choice compression, if necessary
    --
    -- We have four possibilities:
    --
    -- a. Compression algorithms have already been set
    -- b. We chose from the list of server reported supported algorithms
    -- c. We rejected all of the server reported supported algorithms
    -- d. The server didn't report which algorithms are supported
    preferredCompression <-
      case ( preferredCompression meta
           , connUpdateCompression params <$>
               unI (Response.acceptCompression hdrs)
           ) of
        (Just alreadySet, _)            -> return $ Just alreadySet -- (a)
        (Nothing, Just (Right updated)) -> return $ Just updated    -- (b)
        (Nothing, Just (Left err))      -> throwError err           -- (c)
        (Nothing, Nothing)              -> return Nothing           -- (d)

    return ConnMeta{
        preferredCompression
      }

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Preferred compression algorithms, if set, or suitable defauls otherwise
compression :: ConnParams -> ConnMeta -> Compression.Preference
compression ConnParams{connInitCompression} ConnMeta{preferredCompression} =
    fromMaybe initCompression preferredCompression
  where
    initCompression :: Compression.Preference
    initCompression = Compression.Preference {
          preferOutbound = Compression.identity
        , preferInbound  = connInitCompression
        }

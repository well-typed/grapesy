-- | Utilities for working with headers
module Network.GRPC.Common.Headers (
    HasRequiredHeaders(..)
  , RequiredHeaders(..)
  , verifyRequired
  , verifyAll
  , verifyAllIf
  ) where

import Data.Functor.Identity
import Data.Kind
import Data.Void

import Network.GRPC.Spec
import Network.GRPC.Util.HKD (Undecorated, Checked)
import Network.GRPC.Util.HKD qualified as HKD

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Required headers
--
-- Required headers are headers that @grapesy@ needs to know in order to
-- function. For example, we /need/ to know which compression algorithm the peer
-- is using for their messages to us.
class HKD.Traversable h => HasRequiredHeaders h where
  data RequiredHeaders h :: Type
  requiredHeaders :: h (Checked e) -> Either e (RequiredHeaders h)

-- | Like 'requiredHeaders', but for already verified headers
requiredHeadersVerified ::
     HasRequiredHeaders h
  => h Undecorated -> RequiredHeaders h
requiredHeadersVerified =
    either absurd id . requiredHeaders . HKD.map noError . HKD.decorate
  where
    noError :: Identity a -> Either Void a
    noError = Right . runIdentity

-- | Validate only the required headers
--
-- By default, we only check those headers @grapesy@ needs to function.
verifyRequired ::
     HasRequiredHeaders h
  => h (Checked e) -> Either e (RequiredHeaders h)
verifyRequired = requiredHeaders

-- | Validate /all/ headers
--
-- Validate all headers; we do this only if
-- 'Network.GRPC.Client.connVerifyHeaders' (on the client) or
-- 'Network.GRPC.Server.serverVerifyHeaders' (on the server) is enabled.
verifyAll :: forall h e.
     HasRequiredHeaders h
  => h (Checked e) -> Either e (h Undecorated, RequiredHeaders h)
verifyAll = fmap aux . HKD.sequence
  where
    aux :: h Undecorated -> (h Undecorated, RequiredHeaders h)
    aux verifyd = (verifyd, requiredHeadersVerified verifyd)

-- | Convenience wrapper, conditionally verifying all headers
verifyAllIf ::
     HasRequiredHeaders h
  => Bool -> h (Checked e) -> Either e (RequiredHeaders h)
verifyAllIf False = verifyRequired
verifyAllIf True  = fmap snd . verifyAll

{-------------------------------------------------------------------------------
  Request
-------------------------------------------------------------------------------}

instance HasRequiredHeaders RequestHeaders_ where
  data RequiredHeaders RequestHeaders_ = RequiredRequestHeaders {
        requiredRequestCompression :: Maybe CompressionId
      , requiredRequestTimeout     :: Maybe Timeout
      }

  requiredHeaders requestHeaders =
      RequiredRequestHeaders
        <$> requestCompression requestHeaders
        <*> requestTimeout     requestHeaders

{-------------------------------------------------------------------------------
  Response
-------------------------------------------------------------------------------}

instance HasRequiredHeaders ResponseHeaders_ where
  data RequiredHeaders ResponseHeaders_ = RequiredResponseHeaders {
      requiredResponseCompression :: Maybe CompressionId
    }

  requiredHeaders responseHeaders =
      RequiredResponseHeaders
        <$> responseCompression responseHeaders

{-------------------------------------------------------------------------------
  Trailers-Only
-------------------------------------------------------------------------------}

instance HasRequiredHeaders TrailersOnly_ where
  data RequiredHeaders TrailersOnly_ = NoRequiredTrailers
  requiredHeaders _ = pure NoRequiredTrailers


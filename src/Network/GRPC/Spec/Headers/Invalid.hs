{-# LANGUAGE OverloadedStrings #-}

-- | Dealing with invalid headers
module Network.GRPC.Spec.Headers.Invalid (
    InvalidHeaders(..)
  , InvalidHeader(..)
    -- * Construction
  , invalidHeader
  , missingHeader
  , unexpectedHeader
  , invalidHeaderSynthesize
  , throwInvalidHeader
    -- * Synthesized errors
  , HandledSynthesized
  , handledSynthesized
  , dropSynthesized
  , mapSynthesized
  , mapSynthesizedM
  , throwSynthesized
    -- * Utility
  , invalidHeaders
  , prettyInvalidHeaders
  , statusInvalidHeaders
  ) where

import Control.Monad.Except
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Builder qualified as ByteString (Builder)
import Data.ByteString.UTF8 qualified as BS.UTF8
import Data.CaseInsensitive qualified as CI
import Data.Foldable (asum)
import Data.Functor.Identity
import Data.Maybe (fromMaybe, mapMaybe)
import Network.HTTP.Types qualified as HTTP

import Network.GRPC.Spec.Status
import Network.GRPC.Util.HKD (Checked)
import Network.GRPC.Util.HKD qualified as HKD

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Invalid headers
--
-- This is used for request headers, response headers, and response trailers.
newtype InvalidHeaders e = InvalidHeaders {
      getInvalidHeaders :: [InvalidHeader e]
    }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Invalid header
--
-- This corresponds to a single \"raw\" HTTP header. It is possible that a
-- particular field of, say, 'Network.GRPC.Spec.Headers.Request.RequestHeaders'
-- corresponds to /multiple/ 'InvalidHeader', when the value of that field is
-- determined by combining multiple HTTP headers. A special case of this is the
-- field for unrecognized headers (see
-- 'Network.GRPC.Spec.Headers.Request.requestUnrecognized',
-- 'Network.GRPC.Spec.Headers.Response.responseUnrecognized', etc.), which
-- collects /all/ unrecognized headers in one field (and has value @()@ if there
-- are none).
--
-- For some invalid headers the gRPC spec mandates a specific HTTP status;
-- if this status is not specified, then we use 400 Bad Request.
data InvalidHeader e =
    -- | We failed to parse this header
    --
    -- We record the original header and the reason parsing failed.
    InvalidHeader (Maybe HTTP.Status) HTTP.Header String

    -- | Missing header (header that should have been present but was not)
  | MissingHeader (Maybe HTTP.Status) HTTP.HeaderName

    -- | Unexpected header (header that should not have been present but was)
  | UnexpectedHeader HTTP.HeaderName

    -- | Synthesize gRPC exception
    --
    -- This will be instantiated to 'Network.GRPC.Spec.GrpcException' after
    -- parsing, and to 'HandledSynthesized' once synthesized errors have been
    -- handled. See 'HandledSynthesized' for more details.
    --
    -- We record both the actual error and the synthesized error.
  | InvalidHeaderSynthesize e (InvalidHeader HandledSynthesized)
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

invalidHeader :: Maybe HTTP.Status -> HTTP.Header -> String -> InvalidHeaders e
invalidHeader status hdr err = wrapOne $ InvalidHeader status hdr err

missingHeader :: Maybe HTTP.Status -> HTTP.HeaderName -> InvalidHeaders e
missingHeader status name = wrapOne $ MissingHeader status name

unexpectedHeader :: HTTP.HeaderName -> InvalidHeaders e
unexpectedHeader name = wrapOne $ UnexpectedHeader name

invalidHeaderSynthesize ::
     e
  -> InvalidHeader HandledSynthesized
  -> InvalidHeaders e
invalidHeaderSynthesize e orig = wrapOne $ InvalidHeaderSynthesize e orig

throwInvalidHeader ::
     MonadError (InvalidHeaders e) m
  => HTTP.Header
  -> Either String a
  -> m a
throwInvalidHeader _   (Right a)  = return a
throwInvalidHeader hdr (Left err) = throwError $ invalidHeader Nothing hdr err

{-------------------------------------------------------------------------------
  Synthesized errors
-------------------------------------------------------------------------------}

-- | Indicate that all synthesized errors have been handled
--
-- For some headers the gRPC spec mandates a specific gRPC error that should
-- be synthesized when the header is invalid. We use 'HandledSynthesized'
-- in types to indicate that all errors that should have been synthesized have
-- already been thrown.
--
-- For example, 'Network.GRPC.Spec.RequestHeaders'' 'HandledSynthesized'
-- indicates that these request headers may still contain errors for some
-- headers, but no errors for which the spec mandates that we synthesize a
-- specific gRPC exception.
data HandledSynthesized

instance Show HandledSynthesized where
  show = handledSynthesized

instance Eq HandledSynthesized where
  x == _ = handledSynthesized x

handledSynthesized :: HandledSynthesized -> a
handledSynthesized x = case x of {}

-- | Drop all synthesized errors, leaving just the original
dropSynthesized :: InvalidHeaders e -> InvalidHeaders HandledSynthesized
dropSynthesized = \(InvalidHeaders es) ->
    InvalidHeaders $ map aux es
  where
    aux :: InvalidHeader e -> InvalidHeader HandledSynthesized
    aux (InvalidHeader status (name, value) err) =
        InvalidHeader status (name, value) err
    aux (MissingHeader status name) =
        MissingHeader status name
    aux (UnexpectedHeader name) =
        UnexpectedHeader name
    aux (InvalidHeaderSynthesize _ orig) =
        orig

mapSynthesizedM :: forall m e e'.
     Monad m
  => (e -> m e')
  ->    InvalidHeaders e
  -> m (InvalidHeaders e')
mapSynthesizedM f = \(InvalidHeaders es) ->
    InvalidHeaders <$> go [] es
  where
    go :: [InvalidHeader e'] -> [InvalidHeader e] -> m [InvalidHeader e']
    go acc []     = pure $ reverse acc
    go acc (x:xs) =
        case x of
          InvalidHeader status (name, value) err ->
            go (InvalidHeader status (name, value) err : acc) xs
          MissingHeader status name ->
            go (MissingHeader status name : acc) xs
          UnexpectedHeader name ->
            go (UnexpectedHeader name : acc) xs
          InvalidHeaderSynthesize e orig -> do
            e' <- f e
            go (InvalidHeaderSynthesize e' orig : acc) xs

mapSynthesized :: (e -> e') -> InvalidHeaders e -> InvalidHeaders e'
mapSynthesized f = runIdentity . mapSynthesizedM (Identity . f)

throwSynthesized ::
     (HKD.Traversable h, Monad m)
  => (forall a. GrpcException -> m a)
  ->    h (Checked (InvalidHeaders GrpcException))
  -> m (h (Checked (InvalidHeaders HandledSynthesized)))
throwSynthesized throw =
    HKD.traverse $
      either
        (fmap Left  . mapSynthesizedM throw)
        (fmap Right . return)

{-------------------------------------------------------------------------------
  Utility
-------------------------------------------------------------------------------}

-- | Extract all invalid headers
invalidHeaders :: InvalidHeaders e -> [HTTP.Header]
invalidHeaders = \invalid ->
    case dropSynthesized invalid of
      InvalidHeaders es -> mapMaybe aux es
  where
    aux :: InvalidHeader HandledSynthesized -> Maybe HTTP.Header
    aux (InvalidHeader _status hdr _) = Just hdr
    aux MissingHeader{}               = Nothing
    aux UnexpectedHeader{}            = Nothing
    aux (InvalidHeaderSynthesize e _) = handledSynthesized e

prettyInvalidHeaders :: InvalidHeaders HandledSynthesized -> ByteString.Builder
prettyInvalidHeaders = mconcat . map go . getInvalidHeaders
  where
    go :: InvalidHeader HandledSynthesized -> ByteString.Builder
    go (InvalidHeader _status (name, value) err) = mconcat [
          "Invalid header '"
        , Builder.byteString (CI.original name)
        , "' with value '"
        , Builder.byteString value
        , "': "
        , Builder.byteString $ BS.UTF8.fromString err
        , "\n"
        ]
    go (MissingHeader _status name) = mconcat [
          "Missing header '"
        , Builder.byteString (CI.original name)
        , "'\n"
        ]
    go (UnexpectedHeader name) = mconcat [
          "Unexpected header '"
        , Builder.byteString (CI.original name)
        , "'\n"
        ]
    go (InvalidHeaderSynthesize e _orig) =
        handledSynthesized e

-- | HTTP status to report
--
-- If there are multiple headers, each of which with a mandated status, we
-- just use the first; the spec is essentially ambiguous in this case.
statusInvalidHeaders :: InvalidHeaders HandledSynthesized -> HTTP.Status
statusInvalidHeaders (InvalidHeaders hs) =
    fromMaybe HTTP.badRequest400 $ asum $ map getStatus hs
  where
    getStatus :: InvalidHeader HandledSynthesized -> Maybe HTTP.Status
    getStatus (InvalidHeader status _ _)        = status
    getStatus (MissingHeader status _)          = status
    getStatus (UnexpectedHeader _)              = Nothing
    getStatus (InvalidHeaderSynthesize e _orig) = handledSynthesized e

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

wrapOne :: InvalidHeader e -> InvalidHeaders e
wrapOne = InvalidHeaders . (:[])

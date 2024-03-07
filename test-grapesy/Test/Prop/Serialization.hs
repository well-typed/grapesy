{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Prop.Serialization (tests) where

import Control.Monad
import Control.Monad.Except (Except, runExcept)
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base64 qualified as BS.Strict.Base64
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compr
import Network.GRPC.Spec

import Test.Util.Awkward

tests :: TestTree
tests = testGroup "Test.Prop.Serialization" [
      testGroup "Base64" [
          testProperty "unpadded" $ \(Awkward value) ->
            encodingNotPadded value
        , testProperty "acceptPadded" $
            roundtrip buildUnpadded parseBinaryValue
        , testProperty "roundtrip" $
            roundtrip buildBinaryValue parseBinaryValue
        ]
    , testGroup "Headers" [
          testProperty "CustomMetadata" $
            roundtrip buildCustomMetadata parseCustomMetadata
        , testProperty "Timeout" $
            roundtrip buildTimeout parseTimeout
        , testProperty "RequestHeaders" $
            roundtrip (buildRequestHeaders unknown)
                      (parseRequestHeaders unknown)
        ]
    ]
  where
    unknown = Proxy @(UnknownRpc (Just "serv") (Just "meth"))

-- TODO: Roundtrips for ResponseHeaders
-- TODO: Roundtrips for ProperTrailers
-- TODO: Roundtrips for TrailersOnly

{-------------------------------------------------------------------------------
  Auxiliary: binary headers
-------------------------------------------------------------------------------}

-- | The gRPC spec mandates we should not /create/ unpadded values
encodingNotPadded :: BinaryValue -> Bool
encodingNotPadded = BS.Strict.Char8.all (/= '=') . buildBinaryValue

-- | The gRPC spec mandates we must /accept/ unpadded values
buildUnpadded :: BinaryValue -> BS.Strict.Char8.ByteString
buildUnpadded = BS.Strict.Base64.encode . getBinaryValue

{-------------------------------------------------------------------------------
  Roundtrip tests
-------------------------------------------------------------------------------}

roundtrip :: forall e a b.
     (Eq a, Eq e, Show a, Show b, Show e)
  => (a -> b) -> (b -> Except e a) -> Awkward a -> Property
roundtrip there back (Awkward a) =
    counterexample (show b) $
      runExcept (back b) === Right a
  where
    b :: b
    b = there a

{-------------------------------------------------------------------------------
  Arbitrary instances

  We do not provide shrinkers for each definition; they should be defined if
  and when a test breaks.
-------------------------------------------------------------------------------}

instance Arbitrary (Awkward HeaderValue) where
  arbitrary = fmap Awkward $
      oneof [
          BinaryHeader <$> awkward
        , AsciiHeader  <$> awkward
        ]

instance Arbitrary (Awkward HeaderName) where
  arbitrary = Awkward <$> suchThatMap (getAwkward <$> arbitrary) safeHeaderName

instance Arbitrary (Awkward BinaryValue) where
  arbitrary = arbitraryAwkward BinaryValue
  shrink    = shrinkAwkward    BinaryValue getBinaryValue

instance Arbitrary (Awkward AsciiValue) where
  arbitrary = Awkward <$> suchThatMap (getAwkward <$> arbitrary) safeAsciiValue

instance Arbitrary (Awkward RequestHeaders) where
  arbitrary = Awkward <$> do
      requestTimeout             <- awkward
      requestMetadata            <- awkward
      requestCompression         <- awkward
      requestAcceptCompression   <- awkward
      requestContentType         <- awkward
      requestMessageType         <- arbitrary
      requestIncludeTE           <- arbitrary
      requestTraceContext        <- awkward
      return $ RequestHeaders{
          requestTimeout
        , requestMetadata
        , requestCompression
        , requestAcceptCompression
        , requestContentType
        , requestMessageType
        , requestIncludeTE
        , requestTraceContext
        }
  shrink h@(Awkward h') = concat [
        shrinkAwkward (\x -> h'{requestTimeout             = x}) requestTimeout             h
      , shrinkAwkward (\x -> h'{requestMetadata            = x}) requestMetadata            h
      , shrinkAwkward (\x -> h'{requestCompression         = x}) requestCompression         h
      , shrinkAwkward (\x -> h'{requestAcceptCompression   = x}) requestAcceptCompression   h
      , shrinkAwkward (\x -> h'{requestContentType         = x}) requestContentType         h
      , shrinkRegular (\x -> h'{requestMessageType         = x}) requestMessageType         h
      , shrinkRegular (\x -> h'{requestIncludeTE           = x}) requestIncludeTE           h
      , shrinkAwkward (\x -> h'{requestTraceContext        = x}) requestTraceContext        h
      ]

instance Arbitrary (Awkward Timeout) where
  arbitrary = fmap Awkward $
      Timeout <$> awkward <*> awkward

instance Arbitrary (Awkward TimeoutUnit) where
  arbitrary = Awkward <$> elements [
        Hour
      , Minute
      , Second
      , Millisecond
      , Microsecond
      , Nanosecond
      ]

instance Arbitrary (Awkward TimeoutValue) where
  arbitrary = fmap Awkward $
      TimeoutValue <$> arbitrary `suchThat` isValidTimeoutValue
  shrink (Awkward (TimeoutValue x)) =
      map (Awkward . TimeoutValue) $ shrink x

instance Arbitrary (Awkward CompressionId) where
  arbitrary = Awkward <$> oneof [
        pure Compr.Identity
      , pure Compr.GZip
      , pure Compr.Deflate
      , pure Compr.Snappy
      , Custom <$> awkward `suchThatMap` validCompressionId
      ]
  shrink (Awkward cid) = Awkward <$>
      case cid of
        Compr.Identity -> []
        Compr.GZip     -> [Compr.Identity]
        Compr.Deflate  -> [Compr.Identity]
        Compr.Snappy   -> [Compr.Identity]
        Compr.Custom x -> concat [
            [Compr.Identity]
          , mapMaybe (fmap Compr.Custom . validCompressionId) $ shrink x
          ]

instance Arbitrary (Awkward ContentType) where
  -- We don't generate
  --
  -- > ContentTypeOverride "application/grpc"
  --
  -- as this would be parsed as @ContentTypeDefault@
  arbitrary = Awkward <$>
      oneof [
          pure $ ContentTypeDefault
        , (\format -> ContentTypeOverride $ defaultRpcContentType format)
            <$> awkward `suchThatMap` validFormat
        ]
  shrink (Awkward ct) = Awkward <$>
      case ct of
        ContentTypeDefault     -> []
        ContentTypeOverride bs -> concat [
              [ContentTypeDefault]
            , [ ContentTypeOverride $ defaultRpcContentType format'
              | Just format <- [BS.Strict.Char8.stripPrefix ("application/grpc+") bs]
              , format' <- mapMaybe (validFormat . getAwkward) $ shrink (Awkward format)
              ]
            ]

instance Arbitrary (Awkward TraceContext) where
  arbitrary = Awkward <$> do
      traceContextTraceId <- awkward
      traceContextSpanId  <- awkward
      traceContextOptions <- awkward
      return TraceContext {
          traceContextTraceId
        , traceContextSpanId
        , traceContextOptions
        }

instance Arbitrary (Awkward TraceId) where
  arbitrary = Awkward <$> do
      tid <- replicateM 16 arbitrary -- length is fixed
      return $ TraceId $ BS.Strict.pack tid

instance Arbitrary (Awkward SpanId) where
  arbitrary = Awkward <$> do
      tid <- replicateM 8 arbitrary -- length is fixed
      return $ SpanId $ BS.Strict.pack tid

instance Arbitrary (Awkward TraceOptions) where
  arbitrary = Awkward <$> do
      traceOptionsSampled <- arbitrary
      return TraceOptions {
          traceOptionsSampled
        }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

validCompressionId :: String -> Maybe String
validCompressionId cid =
    case filter (not . forbiddenChar) cid of
      ""   -> Nothing
      cid' -> Just cid'
  where
    forbiddenChar :: Char -> Bool
    forbiddenChar c = or [
          isSpace c
        , c `elem` [',']
        ]

validFormat :: Strict.ByteString -> Maybe Strict.ByteString
validFormat format =
    case BS.Strict.Char8.filter (not . forbiddenChar) format of
      ""   -> Nothing
      cid' -> Just cid'
  where
    forbiddenChar :: Char -> Bool
    forbiddenChar c = or [
          isSpace c
        , c `elem` [';']
        ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Prop.Serialization (tests) where

import Control.Lens ((.~))
import Control.Monad
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Base64 qualified as BS.Strict.Base64
import Data.ByteString.Char8 qualified as BS.Strict.Char8
import Data.CaseInsensitive qualified as CI
import Data.Char (isSpace)
import Data.Function
import Data.List (nubBy, uncons, intersperse)
import Data.Maybe (mapMaybe)
import Data.ProtoLens.Labels ()
import Data.ProtoLens.Message
import Data.Proxy
import Data.Text.Lazy qualified as Text.Lazy
import Data.TreeDiff (ToExpr, ediff, ansiWlEditExpr)
import Data.Void (Void)
import Network.HTTP.Types qualified as HTTP
import Prettyprinter (Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter.Render.Terminal qualified as PP.Ansi
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck

import Network.GRPC.Spec
import Network.GRPC.Spec.Serialization

import Test.Util.Awkward
import Test.Util.Orphans ()

tests :: TestTree
tests = testGroup "Test.Prop.Serialization" [
      testGroup "Base64" [
          testProperty "roundtrip" $
            roundtrip buildBinaryValue parseBinaryValue
        , testProperty "emitUnpadded" $ \(Awkward value) ->
            binaryNotPadded value
        , testProperty "acceptPadded" $
            roundtrip buildBinaryPadded parseBinaryValue
        , testProperty "acceptCommas" $ \cs ->
            roundtripWith
              (showIntermediate .*. labelHasMultipleChunks)
              (buildBinaryChunked cs)
              parseBinaryValue
        ]
    , testGroup "Headers" [
          testProperty "CustomMetadata" $
            roundtrip buildCustomMetadata parseCustomMetadata
        , testProperty "Timeout" $
            roundtrip buildTimeout parseTimeout
        , testProperty "Pushback" $
            roundtrip @Void buildPushback parsePushback
        , testProperty "RequestHeaders" $
            roundtrip (buildRequestHeaders rpc)
                      (parseRequestHeaders rpc)
        , testProperty "ResponseHeaders" $
            roundtrip (buildResponseHeaders rpc)
                      (parseResponseHeaders rpc)
        , testProperty "ProperTrailers" $
            roundtrip buildProperTrailers
                      (parseProperTrailers rpc)
        , testProperty "TrailersOnly" $
            roundtrip (buildTrailersOnly' rpc)
                      (parseTrailersOnly  rpc)
        ]
    , testGroup "Duplicates" [
          testProperty "RequestHeaders" $ \dups ->
            roundtripWith
              (showIntermediate .*. labelDups)
              (introduceDups dups . buildRequestHeaders rpc)
              (                     parseRequestHeaders rpc)
        , testProperty "ResponseHeaders" $ \dups ->
            roundtripWith
              (showIntermediate .*. labelDups)
              (introduceDups dups . buildResponseHeaders rpc)
              (                     parseResponseHeaders rpc)
        , testProperty "ProperTrailers" $ \dups ->
            roundtripWith
              (showIntermediate .*. labelDups)
              (introduceDups dups . buildProperTrailers)
              (                     parseProperTrailers rpc)
        , testProperty "TrailersOnly" $ \dups ->
            roundtripWith
              (showIntermediate .*. labelDups)
              (introduceDups dups . buildTrailersOnly' rpc)
              (                     parseTrailersOnly  rpc)
        ]
    , testGroup "Padding" [
          testProperty "RequestHeaders" $ \padding ->
            roundtripWith
              (showIntermediate .*. labelPadding)
              (introducePadding padding . buildRequestHeaders rpc)
              (                           parseRequestHeaders rpc)
        , testProperty "ResponseHeaders" $ \padding ->
            roundtripWith
              (showIntermediate .*. labelPadding)
              (introducePadding padding . buildResponseHeaders rpc)
              (                           parseResponseHeaders rpc)
        , testProperty "ProperTrailers" $ \padding ->
            roundtripWith
              (showIntermediate .*. labelPadding)
              (introducePadding padding . buildProperTrailers)
              (                           parseProperTrailers rpc)
        , testProperty "TrailersOnly" $ \padding ->
            roundtripWith
              (showIntermediate .*. labelPadding)
              (introducePadding padding . buildTrailersOnly' rpc)
              (                           parseTrailersOnly  rpc)
        ]
    ]
  where
    rpc = Proxy @(RawRpc "serv" "meth")

{-------------------------------------------------------------------------------
  Binary headers
-------------------------------------------------------------------------------}

-- | The gRPC spec mandates we should not /create/ padded values
binaryNotPadded :: Strict.ByteString -> Bool
binaryNotPadded = BS.Strict.Char8.all (/= '=') . buildBinaryValue

-- | The gRPC spec mandates we must /accept/ padded values
--
-- We cannot call 'buildBinaryValue' here, because it creates padded values
-- (as tested with 'binaryNotPadded').
buildBinaryPadded :: Strict.ByteString -> Strict.ByteString
buildBinaryPadded = BS.Strict.Base64.encode

-- | Build binary value by encoding chunks separately and joining the results
--
-- See 'parseBinaryValue' for detailed discussion.
--
-- We depend on the presence of commas here; see discussion in 'introduceDups',
-- with the corresponding statistics relevant to this case collected by
-- 'labelHasMultipleChunks'.
buildBinaryChunked :: [Bool] -> Strict.ByteString -> Strict.ByteString
buildBinaryChunked ds value =
    mconcat . intersperse "," $
      map buildBinaryValue chunks
  where
    chunks :: [Strict.ByteString]
    (chunks, _) =  splitHeaderValueBS ds value

labelHasMultipleChunks :: (a, Strict.ByteString) -> Property -> Property
labelHasMultipleChunks (_, bs) =
    tabulate "has multiple chunks" [
        show $ ',' `BS.Strict.Char8.elem` bs
      ]

{-------------------------------------------------------------------------------
  Duplicates in Custom-Metadata

  The spec mandates:

  > Custom-Metadata header order is not guaranteed to be preserved except for
  > values with duplicate header names. Duplicate header names may have their
  > values joined with "," as the delimiter and be considered semantically
  > equivalent.

  We need to be careful with whitespace around the delimeter here; the spec
  mandates

  > ASCII-Value should not have leading or trailing whitespace. If it contains
  > leading or trailing whitespace, it may be stripped.

  Since we use an internal representation that does not allow for duplicates, we
  test this by introducing duplicates in the /serialized/ form.
-------------------------------------------------------------------------------}

-- | Introduce duplicte headers
--
-- We introduce duplicates by splitting existing custom-metadata at existing
-- limiters (so that the roundtrip still passes), /if/ the corresponding 'Bool'
-- value is 'True' (so that we can shrink towards not introducing duplicates).
--
-- NOTE: Our generator for strict bytestrings generates commas with relatively
-- high probability, which ensures that we have enough source material here to
-- generate duplicate headers. We should keep an eye on the statistics however
-- to ensure that this continues to be the case (see 'labelDups').
introduceDups :: [Bool] -> [HTTP.Header] -> [HTTP.Header]
introduceDups = \dups -> concat . flip evalState dups . mapM go
  where
    go :: HTTP.Header -> State [Bool] [HTTP.Header]
    go hdr@(name, value)
      -- Don't split reserved headers (only metadata)
      | Nothing <- safeHeaderName (CI.foldedCase name)
      = return [hdr]

      -- We can split ASCII or binary headers
      | otherwise
      = state $ \dups -> first (map (name,)) $ splitHeaderValueBS dups value

splitHeaderValueBS ::
     [Bool]
  -> Strict.ByteString
  -> ([Strict.ByteString], [Bool])
splitHeaderValueBS ds =
      first (map BS.Strict.Char8.pack)
    . splitHeaderValue ds
    . BS.Strict.Char8.unpack

-- | Split a header value
--
-- We split the header value at @","@ boundaries, /provided/ that the comma is
-- not preceded or followed by whitespace (otherwise that whitespace would be
-- lost, since individual headers are trimmed).
--
-- Examples:
--
-- > splitHeaderValue []                "abc,def,ghi"  == (["abc,def,ghi"]     , [])
-- > splitHeaderValue [True]            "abc,def,ghi"  == (["abc","def,ghi"]   , [])
-- > splitHeaderValue [False]           "abc,def,ghi"  == (["abc,def,ghi"]     , [])
-- > splitHeaderValue [False,True]      "abc,def,ghi"  == (["abc,def","ghi"]   , [])
-- > splitHeaderValue [True,True]       "abc,def,ghi"  == (["abc","def","ghi"] , [])
-- > splitHeaderValue [True,False,True] "abc,def"      == (["abc","def"]       , [False,True])
-- > splitHeaderValue [True]            "abc, def,ghi" == (["abc, def","ghi"]  , [])
-- > splitHeaderValue [True]            "abc ,def,ghi" == (["abc ,def","ghi"]  , [])
splitHeaderValue ::
     [Bool]  -- ^ Allowed splits (useful to shrink towards splitting less)
  -> String  -- ^ String to split
  -> ([String], [Bool])
splitHeaderValue = go []
  where
    go ::
         [Char] -- Accumulated chunk, in reverse order
      -> [Bool] -- Allowed splits left
      -> String -- String left to process
      -> ([String], [Bool])
    go acc []     xs     = finalize acc [] xs
    go acc ds     []     = finalize acc ds []
    go acc (d:ds) (x:xs)
       | canSplit, d     = first (reverse acc :) $ go []         ds  xs
       | canSplit, not d =                         go (x:acc)    ds  xs
       | otherwise       =                         go (x:acc) (d:ds) xs
      where
        prevIsSpace, nextIsSpace, canSplit :: Bool
        prevIsSpace = maybe False (isSpace . fst) $ uncons acc
        nextIsSpace = maybe False (isSpace . fst) $ uncons xs
        canSplit    = x == ',' && not prevIsSpace && not nextIsSpace

    finalize :: [Char] -> [Bool] -> String -> ([String], [Bool])
    finalize acc ds xs = ([reverse acc ++ xs], ds)

labelDups :: (a, [HTTP.Header]) -> Property -> Property
labelDups (_a, headers) =
    tabulate "has duplicate headers" [
        show $ length headers /= length (nubBy ((==) `on` fst) headers)
      ]

{-------------------------------------------------------------------------------
  Roundtrip tests
-------------------------------------------------------------------------------}

roundtrip :: forall e a b.
     (Eq a, ToExpr a, Show b, Show e)
  => (a -> b)             -- ^ There
  -> (b -> Except e a)    -- ^ and back again
  -> Awkward a -> Property
roundtrip = roundtripWith showIntermediate

roundtripWith :: forall e a b.
     (Eq a, ToExpr a, Show e)
  => ((a, b) -> Property -> Property) -- ^ Statistics, metadata, ..
  -> (a -> b)             -- ^ There
  -> (b -> Except e a)    -- ^ and back again
  -> Awkward a -> Property
roundtripWith modProp there back (Awkward a) =
    modProp (a, b) $
      let ma' = runExcept (back b)
      in case ma' of
           Left err ->
                 counterexample (show err) False
           Right a'
             | a == a' ->
                 property True
             | otherwise ->
                 counterexample (renderDoc $ ansiWlEditExpr $ ediff a a') False
   where
     b :: b
     b = there a

showIntermediate :: Show b => (a, b) -> Property -> Property
showIntermediate (_, b) = counterexample (show b)

(.*.) ::
     ((a, b) -> Property -> Property)
  -> ((a, b) -> Property -> Property)
  -> ((a, b) -> Property -> Property)
(.*.) f g (a, b) = f (a, b) . g (a, b)

{-------------------------------------------------------------------------------
  Padding
-------------------------------------------------------------------------------}

newtype Padding = Padding {
      getPadding :: Strict.ByteString
    }
  deriving Show

instance Arbitrary Padding where
  arbitrary = do
      n <- choose (0, 5)
      Padding . BS.Strict.Char8.pack <$> replicateM n (elements " \t")
  shrink =
        map (Padding . BS.Strict.pack)
      . shrinkList (const []) -- only remove elements from the list
      . BS.Strict.unpack
      . getPadding

newtype PaddingPerHeader = PaddingPerHeader {
      -- | Left and right padding for each header
      getPaddingPerHeader :: [(Padding, Padding)]
    }
  deriving stock (Show)

instance Arbitrary PaddingPerHeader where
  arbitrary = sized $ \sz -> do
      n      <- choose (0, sz)
      PaddingPerHeader <$> replicateM n genPadding
    where
      -- Most of the time, no padding
      genPadding :: Gen (Padding, Padding)
      genPadding = frequency [
          (7, (,) <$> pure (Padding "") <*> pure (Padding ""))
        , (1, (,) <$> arbitrary         <*> pure (Padding ""))
        , (1, (,) <$> pure (Padding "") <*> arbitrary)
        , (1, (,) <$> arbitrary         <*> arbitrary)
        ]


  shrink =
        map PaddingPerHeader
      . shrinkList shrink
      . getPaddingPerHeader

introducePadding :: PaddingPerHeader -> [HTTP.Header] -> [HTTP.Header]
introducePadding = \(PaddingPerHeader padding) ->
    flip evalState padding . mapM go
  where
    go :: HTTP.Header -> State [(Padding, Padding)] HTTP.Header
    go (name, value) = state $ \case
      [] ->
        ((name, value), [])
      (l, r) : padding' ->
        ((name, getPadding l <> value <> getPadding r), padding')

labelPadding :: (a, [HTTP.Header]) -> Property -> Property
labelPadding (_a, headers) =
    tabulate "has padding" [
        show $ any (hasPadding . snd) headers
      ]
  where
    hasPadding :: Strict.ByteString -> Bool
    hasPadding bs = or [
          not . BS.Strict.null $ BS.Strict.Char8.takeWhile    isSpace bs
        , not . BS.Strict.null $ BS.Strict.Char8.takeWhileEnd isSpace bs
        ]

{-------------------------------------------------------------------------------
  Arbitrary instances

  We do not yet provide shrinkers for each definition; they should be defined if
  and when a test breaks.
-------------------------------------------------------------------------------}

instance Arbitrary (Awkward CustomMetadata) where
  arbitrary = Awkward <$> do
      name <- genName
      awkward `suchThatMap` safeCustomMetadata name
    where
      genName :: Gen HeaderName
      genName = oneof [
            getAwkward <$> arbitrary
          , fmap (<> "-bin") $ getAwkward <$> arbitrary
          ] `suchThatMap` safeHeaderName

  -- For now we shrink only the value
  shrink (Awkward (CustomMetadata name value)) =
      mapMaybe (fmap Awkward . safeCustomMetadata name) $ shrink value

instance Arbitrary (Awkward CustomMetadataMap) where
  arbitrary = Awkward <$>
      customMetadataMapFromList <$> awkward
  shrink =
        map (Awkward . customMetadataMapFromList . map getAwkward)
      . shrink
      . (map Awkward . customMetadataMapToList . getAwkward)

instance Arbitrary (Awkward RequestHeaders) where
  arbitrary = Awkward <$> do
      requestTimeout             <- awkward
      requestCompression         <- awkward
      requestAcceptCompression   <- awkward
      requestContentType         <- Just <$> awkward -- cannot be missing
      requestMessageType         <- Just <$> awkward -- cannot be missing
      requestUserAgent           <- awkward
      requestIncludeTE           <- arbitrary
      requestTraceContext        <- awkward
      requestPreviousRpcAttempts <- awkward
      requestMetadata            <- awkward
      return $ RequestHeaders{
          requestTimeout
        , requestCompression
        , requestAcceptCompression
        , requestContentType
        , requestMessageType
        , requestUserAgent
        , requestIncludeTE
        , requestTraceContext
        , requestPreviousRpcAttempts
        , requestMetadata
        , requestUnrecognized = ()
        }
  shrink h@(Awkward h') = concat [
        shrinkAwkward (\x -> h'{requestTimeout             = x}) requestTimeout             h
      , shrinkAwkward (\x -> h'{requestCompression         = x}) requestCompression         h
      , shrinkAwkward (\x -> h'{requestAcceptCompression   = x}) requestAcceptCompression   h
      , shrinkAwkward (\x -> h'{requestUserAgent           = x}) requestUserAgent           h
      , shrinkRegular (\x -> h'{requestIncludeTE           = x}) requestIncludeTE           h
      , shrinkAwkward (\x -> h'{requestTraceContext        = x}) requestTraceContext        h
      , shrinkAwkward (\x -> h'{requestPreviousRpcAttempts = x}) requestPreviousRpcAttempts h
      , shrinkAwkward (\x -> h'{requestMetadata            = x}) requestMetadata            h
      ]

instance Arbitrary (Awkward ResponseHeaders) where
  arbitrary = Awkward <$> do
      responseCompression       <- awkward
      responseAcceptCompression <- awkward
      responseContentType       <- Just <$> awkward
      responseMetadata          <- awkward
      return ResponseHeaders {
          responseCompression
        , responseAcceptCompression
        , responseContentType
        , responseMetadata
        , responseUnrecognized = ()
        }

  shrink h@(Awkward h') = concat [
        shrinkAwkward (\x -> h'{responseCompression       = x}) responseCompression       h
      , shrinkAwkward (\x -> h'{responseAcceptCompression = x}) responseAcceptCompression h
      , shrinkAwkward (\x -> h'{responseMetadata          = x}) responseMetadata          h
      ]

instance Arbitrary (Awkward ProperTrailers) where
  arbitrary = Awkward <$> do
      properTrailersGrpcStatus     <- awkward
      properTrailersGrpcMessage    <- awkward
      properTrailersStatusDetails  <- awkward
      properTrailersPushback       <- awkward
      properTrailersOrcaLoadReport <- awkward
      properTrailersMetadata       <- awkward
      return $ ProperTrailers{
          properTrailersGrpcStatus
        , properTrailersGrpcMessage
        , properTrailersStatusDetails
        , properTrailersPushback
        , properTrailersOrcaLoadReport
        , properTrailersMetadata
        , properTrailersUnrecognized = ()
        }

  shrink h@(Awkward h') = concat [
        shrinkAwkward (\x -> h'{properTrailersGrpcStatus     = x}) properTrailersGrpcStatus     h
      , shrinkAwkward (\x -> h'{properTrailersGrpcMessage    = x}) properTrailersGrpcMessage    h
      , shrinkAwkward (\x -> h'{properTrailersMetadata       = x}) properTrailersMetadata       h
      , shrinkAwkward (\x -> h'{properTrailersPushback       = x}) properTrailersPushback       h
      , shrinkAwkward (\x -> h'{properTrailersOrcaLoadReport = x}) properTrailersOrcaLoadReport h
      ]

instance Arbitrary (Awkward TrailersOnly) where
  arbitrary = Awkward <$> do
      trailersOnlyContentType <- Just <$> awkward
      trailersOnlyProper      <- awkward
      return $ TrailersOnly {
           trailersOnlyContentType
         , trailersOnlyProper
        }

  shrink h@(Awkward h') = concat [
        shrinkAwkward (\x -> h'{trailersOnlyContentType = x}) trailersOnlyContentType  h
      , shrinkAwkward (\x -> h'{trailersOnlyProper      = x}) trailersOnlyProper       h
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
        pure Identity
      , pure GZip
      , pure Deflate
      , pure Snappy
      , Custom <$> awkward `suchThatMap` validCompressionId
      ]
  shrink (Awkward cid) = Awkward <$>
      case cid of
        Identity -> []
        GZip     -> [Identity]
        Deflate  -> [Identity]
        Snappy   -> [Identity]
        Custom x -> concat [
            [Identity]
          , mapMaybe (fmap Custom . validCompressionId) $ shrink x
          ]

instance Arbitrary (Awkward ContentType) where
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

instance Arbitrary (Awkward MessageType) where
  arbitrary = Awkward <$>
      oneof [
          pure $ MessageTypeDefault
        , MessageTypeOverride <$> awkward `suchThat` validMessageType
        ]

  shrink (Awkward mt) = Awkward <$>
      case mt of
        MessageTypeDefault    -> []
        MessageTypeOverride x -> concat [
            [MessageTypeDefault]
          , [ MessageTypeOverride x'
            | x' <- shrink x
            , validMessageType x'
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

instance Arbitrary (Awkward GrpcStatus) where
  arbitrary = Awkward <$>
      elements [
          GrpcOk
        , GrpcError GrpcCancelled
        , GrpcError GrpcUnknown
        , GrpcError GrpcInvalidArgument
        , GrpcError GrpcDeadlineExceeded
        , GrpcError GrpcNotFound
        , GrpcError GrpcAlreadyExists
        , GrpcError GrpcPermissionDenied
        , GrpcError GrpcResourceExhausted
        , GrpcError GrpcFailedPrecondition
        , GrpcError GrpcAborted
        , GrpcError GrpcOutOfRange
        , GrpcError GrpcUnimplemented
        , GrpcError GrpcInternal
        , GrpcError GrpcUnavailable
        , GrpcError GrpcDataLoss
        , GrpcError GrpcUnauthenticated
        ]

instance Arbitrary (Awkward Pushback) where
  arbitrary = Awkward <$>
      oneof [
          RetryAfter <$> arbitrary
        , pure DoNotRetry
        ]

instance Arbitrary (Awkward OrcaLoadReport) where
  arbitrary = Awkward <$> do
      -- @rps@ is a deprecated field, we omit it from the test
      cpuUtilization         <- awkward
      memUtilization         <- awkward
      requestCost            <- awkward
      utilization            <- awkward
      rpsFractional          <- awkward
      eps                    <- awkward
      namedMetrics           <- awkward
      applicationUtilization <- awkward
      return $
        defMessage
          & #cpuUtilization         .~ cpuUtilization
          & #memUtilization         .~ memUtilization
          & #requestCost            .~ requestCost
          & #utilization            .~ utilization
          & #rpsFractional          .~ rpsFractional
          & #eps                    .~ eps
          & #namedMetrics           .~ namedMetrics
          & #applicationUtilization .~ applicationUtilization

{-------------------------------------------------------------------------------
  Generating valid values
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
      ""     -> Nothing
      "grpc" -> Nothing -- Would be parsed as @ContentTypeDefault@
      cid'   -> Just cid'
  where
    forbiddenChar :: Char -> Bool
    forbiddenChar c = or [
          isSpace c
        , c `elem` [';']
        ]

validMessageType :: Strict.ByteString -> Bool
validMessageType "Void" = False -- Would be parsed as @MessageTypeDefault@
validMessageType _      = True

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

renderDoc :: Doc AnsiStyle -> String
renderDoc =
      Text.Lazy.unpack
    . PP.Ansi.renderLazy
    . resetTerminal
    . PP.layoutPretty PP.defaultLayoutOptions
  where
    -- Reset the vivid red from tasty
    resetTerminal :: PP.SimpleDocStream AnsiStyle -> PP.SimpleDocStream AnsiStyle
    resetTerminal = PP.SAnnPush $ PP.Ansi.color PP.Ansi.Black

buildTrailersOnly' :: IsRPC rpc => Proxy rpc -> TrailersOnly -> [HTTP.Header]
buildTrailersOnly' rpc = buildTrailersOnly (Just . chooseContentType rpc)

type instance RequestMetadata          (RawRpc "serv" "meth") = NoMetadata
type instance ResponseInitialMetadata  (RawRpc "serv" "meth") = NoMetadata
type instance ResponseTrailingMetadata (RawRpc "serv" "meth") = NoMetadata

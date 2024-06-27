{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans () where

import Data.CaseInsensitive
import Data.TreeDiff

import Network.GRPC.Spec

import Test.Util.Protobuf

{-------------------------------------------------------------------------------
  ToExpr (tree-diff)
-------------------------------------------------------------------------------}

instance ToExpr a => ToExpr (CI a) where
  toExpr = toExpr . foldedCase

instance ToExpr OrcaLoadReport where
  toExpr = messageToExpr

deriving anyclass instance ToExpr CompressionId
deriving anyclass instance ToExpr ContentType
deriving anyclass instance ToExpr CustomMetadata
deriving anyclass instance ToExpr CustomMetadataMap
deriving anyclass instance ToExpr GrpcError
deriving anyclass instance ToExpr GrpcStatus
deriving anyclass instance ToExpr HeaderName
deriving anyclass instance ToExpr MessageType
deriving anyclass instance ToExpr ProperTrailers
deriving anyclass instance ToExpr Pushback
deriving anyclass instance ToExpr RequestHeaders
deriving anyclass instance ToExpr ResponseHeaders
deriving anyclass instance ToExpr SpanId
deriving anyclass instance ToExpr Timeout
deriving anyclass instance ToExpr TimeoutUnit
deriving anyclass instance ToExpr TimeoutValue
deriving anyclass instance ToExpr TraceContext
deriving anyclass instance ToExpr TraceId
deriving anyclass instance ToExpr TraceOptions
deriving anyclass instance ToExpr TrailersOnly


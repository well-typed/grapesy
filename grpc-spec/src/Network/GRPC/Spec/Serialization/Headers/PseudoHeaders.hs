{-# LANGUAGE OverloadedStrings #-}

module Network.GRPC.Spec.Serialization.Headers.PseudoHeaders (
    RawResourceHeaders(..)
  , InvalidResourceHeaders(..)
  , buildResourceHeaders
  , parseResourceHeaders
  ) where

import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)

import Network.GRPC.Spec
import Network.GRPC.Spec.Util.ByteString

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

-- | Raw (serialized) form of t'ResourceHeaders'
data RawResourceHeaders = RawResourceHeaders {
      rawPath   :: Strict.ByteString  -- ^ Serialized 'resourcePath'
    , rawMethod :: Strict.ByteString  -- ^ Serialized 'resourceMethod'
    }
  deriving (Show)

-- | Invalid resource headers
--
-- See 'parseResourceHeaders'
data InvalidResourceHeaders =
    InvalidMethod Strict.ByteString
  | InvalidPath Strict.ByteString
  deriving stock (Show)

-- | Serialize t'ResourceHeaders' (pseudo headers)
buildResourceHeaders :: ResourceHeaders -> RawResourceHeaders
buildResourceHeaders ResourceHeaders{resourcePath, resourceMethod} =
    RawResourceHeaders {
        rawMethod = case resourceMethod of Post -> "POST"
      , rawPath   = mconcat [
                        "/"
                      , pathService resourcePath
                      , "/"
                      , pathMethod resourcePath
                      ]
      }

-- | Parse t'ResourceHeaders' (pseudo headers)
parseResourceHeaders ::
     RawResourceHeaders
  -> Either InvalidResourceHeaders ResourceHeaders
parseResourceHeaders RawResourceHeaders{rawMethod, rawPath} = do
    resourceMethod <-
      case rawMethod of
        "POST"     -> return Post
        _otherwise -> throwError $ InvalidMethod rawMethod

    resourcePath <-
      case BS.Strict.split (ascii '/') rawPath of
        ["", service, method] ->
          return $ Path service method
        _otherwise ->
          throwError $ InvalidPath rawPath

    return ResourceHeaders{resourceMethod, resourcePath}

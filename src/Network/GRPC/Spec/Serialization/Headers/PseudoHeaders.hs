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
import Network.GRPC.Util.ByteString

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

data RawResourceHeaders = RawResourceHeaders {
      rawPath   :: Strict.ByteString
    , rawMethod :: Strict.ByteString
    }
  deriving (Show)

data InvalidResourceHeaders =
    InvalidMethod Strict.ByteString
  | InvalidPath Strict.ByteString
  deriving stock (Show)

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

-- | Parse pseudo headers
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

{-# LANGUAGE OverloadedStrings #-}

-- | HTTP connections
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Spec.HTTP2.Connection (Scheme(..), Authority(..))
-- > import Network.GRPC.Spec.HTTP2.Connection qualified as Connection
module Network.GRPC.Spec.HTTP2.Connection (
    -- * Types
    Scheme(..)
  , Authority(..)
    -- * Connection
  , authority
  , method
  , path
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8

import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Scheme = Http | Https
  deriving stock (Show)

-- | HTTP authority
data Authority = Authority {
      authorityUserInfo :: Maybe String
    , authorityHost     :: String
    , authorityPort     :: Word
    }
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Connections
-------------------------------------------------------------------------------}

-- | HTTP authority
--
-- From the HTTP2 spec <https://www.rfc-editor.org/rfc/rfc3986#section-3.2>:
--
-- > authority = [ userinfo "@" ] host [ ":" port ]
--
-- The spec also mandates the use of UTF8
-- <https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2>.
--
-- (The example from the gRPC spec just has the hostname here.)
authority :: Authority -> Strict.ByteString
authority auth = mconcat [
      case authorityUserInfo auth of
        Just userInfo -> BS.Strict.UTF8.fromString userInfo <> "@"
        Nothing       -> mempty
    , BS.Strict.UTF8.fromString $ authorityHost auth
    , ":"
    , BS.Strict.C8.pack $ show (authorityPort auth)
    ]

-- | Method is always POST for gRPC
method :: Strict.ByteString
method = "POST"

-- | Path
path :: IsRPC rpc => rpc -> Strict.ByteString
path rpc = "/" <> serviceName rpc <> "/" <> methodName rpc

{-# LANGUAGE OverloadedStrings #-}

-- | Part of the gRPC spec that maps to HTTP2 pseudo-headers
--
-- Intended for unqualified import.
module Network.GRPC.Spec.PseudoHeaders (
    -- * Definition
    ServerHeaders(..)
  , ResourceHeaders(..)
  , PseudoHeaders(..)
    -- ** Individual headers
  , Method(..)
  , Scheme(..)
  , Authority(..)
  , Path(..)
    -- * Raw headers
  , RawServerHeaders(..)
  , RawResourceHeaders(..)
  , RawPseudoHeaders(..)
    -- * Construction
  , buildServerHeaders
  , buildResourceHeaders
  , rpcPath
    -- * Parsing
  , InvalidPseudoHeaders(..)
  , parsePseudoHeaders
  ) where

import Control.Monad.Except
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.C8
import Data.ByteString.UTF8 qualified as BS.Strict.UTF8
import Data.Hashable (Hashable)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Text.Read (readMaybe)

import Network.GRPC.Spec.PercentEncoding qualified as PercentEncoding
import Network.GRPC.Spec.RPC
import Network.GRPC.Util.ByteString

{-------------------------------------------------------------------------------
  Definition

  This is not intended to be a general definition of pseudo-headers in HTTP2,
  but rather a reflection of how these pseudo-headers are used in gRPC.
-------------------------------------------------------------------------------}

-- | Partial pseudo headers: identify the server, but not a specific resource
data ServerHeaders = ServerHeaders {
      serverScheme    :: Scheme
    , serverAuthority :: Authority
    }
  deriving stock (Show)

-- | Request pseudo-methods
--
-- <https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2.3>
data ResourceHeaders = ResourceHeaders {
      resourceMethod :: Method
    , resourcePath   :: Path
    }
  deriving stock (Show)

-- | All pseudo-headers
data PseudoHeaders = PseudoHeaders {
      serverHeaders   :: ServerHeaders
    , resourceHeaders :: ResourceHeaders
    }
  deriving stock (Show)

-- | Method
--
-- The only method supported by gRPC is @POST@.
--
-- See also <https://datatracker.ietf.org/doc/html/rfc7231#section-4>.
data Method = Post
  deriving stock (Show)

-- | Scheme
--
-- See <https://datatracker.ietf.org/doc/html/rfc3986#section-3.1>.
data Scheme = Http | Https
  deriving stock (Show)

-- | HTTP authority
--
--  As per the HTTP2 spec, this does not include @userinfo@:
--
-- > The authority MUST NOT include the deprecated "userinfo" subcomponent for
-- > "http" or "https" schemed URIs.
--
-- See also <https://datatracker.ietf.org/doc/html/rfc3986#section-3.2>.
data Authority = Authority {
      -- | Hostname
      authorityHost :: String

      -- | TCP port
    , authorityPort :: Word
    }
  deriving stock (Show)

-- | Path
--
-- The gRPC spec specifies:
--
-- > Path → ":path" "/" Service-Name "/" {method name} # But see note below.
--
-- Moreover, it says:
--
-- > Path is case-sensitive. Some gRPC implementations may allow the Path format
-- > shown above to be overridden, but this functionality is strongly
-- > discouraged. gRPC does not go out of its way to break users that are using
-- > this kind of override, but we do not actively support it, and some
-- > functionality (e.g., service config support) will not work when the path is
-- > not of the form shown above.
--
-- We don't support these non-standard paths at all.
data Path = Path {
      pathService :: Text
    , pathMethod  :: Text
    }
  deriving stock (Show, Eq)
  deriving stock (GHC.Generic)
  deriving anyclass (Hashable)

{-------------------------------------------------------------------------------
  Raw headers
-------------------------------------------------------------------------------}

data RawServerHeaders = RawServerHeaders {
      rawScheme    :: Strict.ByteString
    , rawAuthority :: Strict.ByteString
    }

data RawResourceHeaders = RawResourceHeaders {
      rawPath   :: Strict.ByteString
    , rawMethod :: Strict.ByteString
    }

data RawPseudoHeaders = RawPseudoHeaders {
      rawServerHeaders   :: RawServerHeaders
    , rawResourceHeaders :: RawResourceHeaders
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

buildServerHeaders :: ServerHeaders -> RawServerHeaders
buildServerHeaders ServerHeaders{serverScheme, serverAuthority} =
    RawServerHeaders {
        rawAuthority =
          -- The spec mandates the use of UTF8
          -- <https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2>
          mconcat [
              BS.Strict.UTF8.fromString $ authorityHost serverAuthority
            , ":"
            , BS.Strict.C8.pack $ show $ authorityPort serverAuthority
            ]
      , rawScheme =
          case serverScheme of
            Http  -> "http"
            Https -> "https"
      }

buildResourceHeaders :: ResourceHeaders -> RawResourceHeaders
buildResourceHeaders ResourceHeaders{resourcePath, resourceMethod} =
    RawResourceHeaders {
        rawMethod = case resourceMethod of Post -> "POST"
      , rawPath   = mconcat [
                        "/"
                      , PercentEncoding.encode $ pathService resourcePath
                      , "/"
                      , PercentEncoding.encode $ pathMethod resourcePath
                      ]
      }

rpcPath :: IsRPC rpc => Proxy rpc -> Path
rpcPath proxy = Path (serviceName proxy) (methodName proxy)

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

data InvalidPseudoHeaders =
    InvalidScheme Strict.ByteString
  | InvalidAuthority Strict.ByteString
  | InvalidMethod Strict.ByteString
  | InvalidPath Strict.ByteString

-- | Parse pseudo headers
--
-- TODOs:
--
-- * Technically the Authority header is optional in gRPC, but we currently
--   require it.
-- * Even if an Authority header is present, it does not need to include a port
--   number, but we currently require it.
parsePseudoHeaders ::
     RawPseudoHeaders
  -> Either InvalidPseudoHeaders PseudoHeaders
parsePseudoHeaders RawPseudoHeaders{
        rawServerHeaders   = RawServerHeaders{rawScheme, rawAuthority}
      , rawResourceHeaders = RawResourceHeaders{rawMethod, rawPath}
      } = do
    serverScheme <-
      case rawScheme of
        "http"     -> return Http
        "https"    -> return Https
        _otherwise -> throwError $ InvalidScheme rawScheme

    serverAuthority <-
      case BS.Strict.split (ascii ':') rawAuthority of
        [host, port]
            | Just port' <- readMaybe (BS.Strict.C8.unpack port) ->
          return $ Authority (BS.Strict.UTF8.toString host) port'
        _otherwise ->
          throwError $ InvalidAuthority rawAuthority

    resourceMethod <-
      case rawMethod of
        "POST"     -> return Post
        _otherwise -> throwError $ InvalidMethod rawMethod

    resourcePath <-
      case BS.Strict.split (ascii '/') rawPath of
        ["", service, method]
            | Right service' <- PercentEncoding.decode service
            , Right method'  <- PercentEncoding.decode method  ->
          return $ Path service' method'
        _otherwise ->
          throwError $ InvalidPath rawPath

    return PseudoHeaders{
        serverHeaders   = ServerHeaders{serverScheme, serverAuthority}
      , resourceHeaders = ResourceHeaders{resourceMethod, resourcePath}
      }

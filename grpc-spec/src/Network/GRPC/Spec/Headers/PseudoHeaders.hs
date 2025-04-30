-- | Part of the gRPC spec that maps to HTTP2 pseudo-headers
--
-- Intended for unqualified import.
module Network.GRPC.Spec.Headers.PseudoHeaders (
    -- * Definition
    ServerHeaders(..)
  , ResourceHeaders(..)
  , PseudoHeaders(..)
    -- ** Individual headers
  , Method(..)
  , Scheme(..)
  , Address(..)
  , Path(..)
  , rpcPath
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.Hashable
import Data.Proxy
import Network.Socket (HostName, PortNumber)

import Network.GRPC.Spec.RPC

{-------------------------------------------------------------------------------
  Definition

  This is not intended to be a general definition of pseudo-headers in HTTP2,
  but rather a reflection of how these pseudo-headers are used in gRPC.
-------------------------------------------------------------------------------}

-- | Partial pseudo headers: identify the server, but not a specific resource
data ServerHeaders = ServerHeaders {
      serverScheme  :: Scheme
    , serverAddress :: Address
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

-- | Address
--
-- The address of a server to connect to. This is not standard gRPC
-- nomenclature, but follows convention such as adopted by
-- [grpcurl](https://github.com/fullstorydev/grpcurl) and
-- [grpc-client-cli](https://github.com/vadimi/grpc-client-cli), which
-- distinguish between the /address/ of a server to connect to (hostname and
-- port), and the (optional) HTTP /authority/, which is an (optional) string to
-- be included as the HTTP2
-- [:authority](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2)
-- [pseudo-header](https://datatracker.ietf.org/doc/html/rfc7540#section-8.1.2.3).
data Address = Address {
      -- | Hostname
      addressHost :: HostName

      -- | TCP port
    , addressPort :: PortNumber

      -- | Authority
      --
      -- When the authority is not specified, it defaults to @addressHost@.
      --
      -- This is used both for the HTTP2 @:authority@ pseudo-header as well
      -- as for TLS SNI (if using a secure connection).
      --
      -- Although the HTTP(2) specification allows the authority to include a
      -- port number, and many servers can accept this, this will /not/ work
      -- with TLS, and it is therefore recommended not to include a port number.
      -- Note that the HTTP2 spec explicitly /disallows/ the authority to
      -- include @userinfo@.
    , addressAuthority :: Maybe String
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
      pathService :: Strict.ByteString
    , pathMethod  :: Strict.ByteString
    }
  deriving stock (Show, Eq)

instance Hashable Path where
  hashWithSalt salt Path{pathService, pathMethod} =
      hashWithSalt salt (pathService, pathMethod)

-- | Construct path
rpcPath :: IsRPC rpc => Proxy rpc -> Path
rpcPath proxy = Path (rpcServiceName proxy) (rpcMethodName proxy)

{-# LANGUAGE OverloadedStrings #-}

-- | TLS utilities
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Util.TLS (ServerValidation(..))
-- > import Network.GRPC.Util.TLS qualified as Util.TLS
module Network.GRPC.Util.TLS (
    -- * Certificate store
    CertificateStoreSpec(..)
  , certStoreFromSystem
  , certStoreFromCerts
  , certStoreFromPath
  , loadCertificateStore
    -- * Configuration
    -- ** Parameters
  , ServerValidation(..)
    -- ** Common to server and client
  , debugParams
  , supported
    -- ** Hooks (configured for HTTP2)
  , serverHooks
  , clientHooks
    -- ** Shared parameters
  , serverShared
  , clientShared
  ) where

import Control.Exception
import Data.ByteString qualified as Strict (ByteString)
import Data.Default
import Data.X509 qualified as X509
import Data.X509.CertificateStore qualified as X509
import Data.X509.Validation qualified as X509
import Network.TLS qualified as TLS
import Network.TLS.Extra qualified as TLS
import System.Environment
import System.X509 qualified as X509

{-------------------------------------------------------------------------------
  Certificate store
-------------------------------------------------------------------------------}

-- | Certificate store specification (for certificate validation)
--
-- This is a deep embedding, describing how to construct a certificate store.
-- The actual construction happens in 'loadCertificateStore'.
--
-- There are three primitive ways to construct a 'CertificateStore':
-- 'certStoreFromSystem', 'certStoreFromCerts', and 'certStoreFromPath'; please
-- refer to the corresponding documentation.
--
-- You can also combine 'CertificateStore's through the 'Monoid' instance.
data CertificateStoreSpec =
    CertStoreEmpty
  | CertStoreAppend CertificateStoreSpec CertificateStoreSpec
  | CertStoreFromSystem
  | CertStoreFromCerts [X509.SignedCertificate]
  | CertStoreFromPath FilePath
  deriving (Show)

instance Semigroup CertificateStoreSpec where
  (<>) = CertStoreAppend

instance Monoid CertificateStoreSpec where
  mempty = CertStoreEmpty

-- | Use the system's certificate store
certStoreFromSystem :: CertificateStoreSpec
certStoreFromSystem = CertStoreFromSystem

-- | Construct a certificate store with the given certificates
certStoreFromCerts :: [X509.SignedCertificate] -> CertificateStoreSpec
certStoreFromCerts = CertStoreFromCerts

-- | Load certificate store from disk
--
-- The path may point to single file (multiple PEM formatted certificates
-- concanated) or directory (one certificate per file, file names are hashes
-- from certificate).
certStoreFromPath :: FilePath -> CertificateStoreSpec
certStoreFromPath = CertStoreFromPath

-- | Load the certificate store
loadCertificateStore :: CertificateStoreSpec -> IO X509.CertificateStore
loadCertificateStore = go
  where
    go :: CertificateStoreSpec -> IO X509.CertificateStore
    go  CertStoreEmpty         = return mempty
    go (CertStoreAppend c1 c2) = (<>) <$> go c1 <*> go c2
    go  CertStoreFromSystem    = X509.getSystemCertificateStore
    go (CertStoreFromCerts cs) = return $ X509.makeCertificateStore cs
    go (CertStoreFromPath fp)  = X509.readCertificateStore fp >>= \case
                                   Nothing -> throwIO $ NoCertificatesAtPath fp
                                   Just cs -> return cs

data LoadCertificateStoreException =
    NoCertificatesAtPath FilePath
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Parameters
-------------------------------------------------------------------------------}

-- | How does the client want to validate the server?
data ServerValidation =
    -- | Validate the server
    --
    -- The 'CertificateStore' is a collection of trust anchors. If 'Nothing'
    -- is specified, the system certificate store will be used.
    ValidateServer CertificateStoreSpec

    -- | Skip server validation
    --
    -- WARNING: This is dangerous. Although communication with the server will
    -- still be encrypted, you cannot be sure that the server is who they claim
    -- to be.
  | NoServerValidation
  deriving (Show)

{-------------------------------------------------------------------------------
  Configuration common to server and client
-------------------------------------------------------------------------------}

debugParams :: IO TLS.DebugParams
debugParams = do
    keyLogFile <- lookupEnv "SSLKEYLOGFILE"
    return def {
          TLS.debugKeyLogger =
            case keyLogFile of
              Nothing -> \_   -> return ()
              Just fp -> \str -> appendFile fp (str ++ "\n")
        }

supported :: TLS.Supported
supported = def {
      TLS.supportedCiphers = TLS.ciphersuite_default
    }

{-------------------------------------------------------------------------------
  Hooks

  We override these to do ALPN (application level protocol negotiation): the
  server will choose only @h2@ (HTTP2), and the client will suggest only @h2@.
-------------------------------------------------------------------------------}

serverHooks :: TLS.ServerHooks
serverHooks = def {
      TLS.onALPNClientSuggest = Just alpn
    }
  where
    -- Application level protocol negotation
    alpn :: [Strict.ByteString] -> IO Strict.ByteString
    alpn protocols
      | "h2" `elem` protocols = return "h2"
      | otherwise = throwIO $ ProtocolNegotationFailed protocols

clientHooks :: ServerValidation -> TLS.ClientHooks
clientHooks validation = def {
      TLS.onSuggestALPN       = alpn
    , TLS.onServerCertificate =
        case validation of
          ValidateServer _   -> X509.validateDefault
          NoServerValidation -> \_ _ _ _ -> return []
    }
  where
    alpn :: IO (Maybe [Strict.ByteString])
    alpn = return $ Just ["h2"]

data ProtocolNegotationFailed =
    -- | The only protocol we support is HTTP2.
    --
    -- We list which protocols the client listed as supported.
    ProtocolNegotationFailed [Strict.ByteString]
  deriving stock (Show)
  deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Shared parameters

  The server wants credentials, the client wants a means to validate those
  credentials.
-------------------------------------------------------------------------------}

serverShared :: TLS.Credentials -> TLS.Shared
serverShared creds = def {
      TLS.sharedCredentials = creds
    }

clientShared :: ServerValidation -> IO TLS.Shared
clientShared validation = do
    caStore <-
      case validation of
        ValidateServer storeSpec ->
          loadCertificateStore storeSpec
        NoServerValidation ->
          return mempty
    return def {
        TLS.sharedCAStore = caStore
      }
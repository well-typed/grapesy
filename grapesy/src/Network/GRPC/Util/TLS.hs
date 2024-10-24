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
  , validationCAStore
    -- ** Common to server and client
  , SslKeyLog(..)
  , keyLogger
  ) where

import Control.Exception
import Data.Default.Class
import Data.X509 qualified as X509
import Data.X509.CertificateStore qualified as X509
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

validationCAStore :: ServerValidation -> IO X509.CertificateStore
validationCAStore (ValidateServer storeSpec) = loadCertificateStore storeSpec
validationCAStore NoServerValidation         = return mempty

{-------------------------------------------------------------------------------
  Configuration common to server and client
-------------------------------------------------------------------------------}

-- | SSL key log file
--
-- An SSL key log file can be used by tools such as Wireshark to decode TLS
-- network traffic. It is used for debugging only.
data SslKeyLog =
    -- | Don't use a key log file
    SslKeyLogNone

    -- | Use the specified path
  | SslKeyLogPath FilePath

    -- | Use the @SSLKEYLOGFILE@ environment variable to determine the key log
    --
    -- This is the default.
  | SslKeyLogFromEnv
  deriving (Show, Eq)

instance Default SslKeyLog where
  def = SslKeyLogFromEnv

keyLogger :: SslKeyLog -> IO (String -> IO ())
keyLogger sslKeyLog = do
    keyLogFile <- case sslKeyLog of
                    SslKeyLogNone    -> return $ Nothing
                    SslKeyLogPath fp -> return $ Just fp
                    SslKeyLogFromEnv -> lookupEnv "SSLKEYLOGFILE"
    return $
      case keyLogFile of
        Nothing -> \_   -> return ()
        Just fp -> \str -> appendFile fp (str ++ "\n")

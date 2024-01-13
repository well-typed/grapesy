module Network.GRPC.Util.PrettyVal (
    -- * Deriving-via support
    StrictByteString_IsString(..)
  , StrictByteString_Binary(..)
  ) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString qualified as BS.Strict
import Data.ByteString.Internal qualified as BS.Strict
import Data.Coerce
import Data.Proxy
import Data.String
import GHC.TypeLits
import Text.Show.Pretty

{-------------------------------------------------------------------------------
  Deriving-via support
-------------------------------------------------------------------------------}

newtype StrictByteString_IsString a = StrictByteString_IsString a

instance ( Coercible a Strict.ByteString
         , IsString a
         ) => PrettyVal (StrictByteString_IsString a) where
  prettyVal (StrictByteString_IsString x) =
      -- The 'ByteString' 'IsString' instance is defined using 'packChars',
      -- so here we do the opposite
      String . show $ BS.Strict.unpackChars (co x)
    where
      co :: a -> Strict.ByteString
      co = coerce

newtype StrictByteString_Binary (constr :: Symbol) a = StrictByteString_Binary a

instance ( Coercible a Strict.ByteString
         , KnownSymbol constr
         ) => PrettyVal (StrictByteString_Binary constr a) where
  prettyVal (StrictByteString_Binary x) =
      Con (symbolVal (Proxy @constr)) [
          Con "pack" [prettyVal $ BS.Strict.unpack (co x)]
        ]
    where
      co :: a -> Strict.ByteString
      co = coerce

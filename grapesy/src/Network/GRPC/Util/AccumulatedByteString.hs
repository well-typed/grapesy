-- | Accumulate strict bytestrings
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Util.AccumulatedByteString (AccumulatedByteString)
-- > import Network.GRPC.Util.AccumulatedByteString qualified as AccBS
module Network.GRPC.Util.AccumulatedByteString (
    AccumulatedByteString -- opaque
    -- * Construction
  , init
  , append
    -- * Query
  , length
    -- * Destruction
  , splitAt
  ) where

import Prelude hiding (init, splitAt, length)

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Accumulate strict bytestrings until we have enough bytes
data AccumulatedByteString = AccBS {
       -- | Total accumulated length
       accLength  :: !Word

       -- | Accumulated bytestrings, in reverse order
     , accStrings :: [Strict.ByteString]
     }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

init :: Maybe Strict.ByteString -> AccumulatedByteString
init Nothing   = AccBS 0 []
init (Just bs) = AccBS (fromIntegral $ BS.Strict.length bs) [bs]

append :: AccumulatedByteString -> Strict.ByteString -> AccumulatedByteString
append AccBS{accLength, accStrings} bs = AccBS{
      accLength  = accLength + fromIntegral (BS.Strict.length bs)
    , accStrings = bs : accStrings
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

length :: AccumulatedByteString -> Word
length = accLength

{-------------------------------------------------------------------------------
  Destruction
-------------------------------------------------------------------------------}

-- | Split the accumulated bytestrings after @n@ bytes
--
-- Preconditions:
--
-- 1. The accumulated length must be @>= n@
-- 2. The accumulated length without the most recently added bytestring
--    must be @<= n@
--
-- These two preconditions together imply that all accumulated bytestrings
-- except the most recent will be required.
splitAt ::
     Word -- ^ @n@
  -> AccumulatedByteString
  -> (Strict.ByteString, Maybe Strict.ByteString)
splitAt n AccBS{accLength, accStrings} =
    if leftoverLen == 0 then
      (BS.Strict.concat $ reverse accStrings, Nothing)
    else
      -- If @leftoverLen > 0@ then @accLength > 0@: we must have some strings
      case accStrings of
        []              -> error "splitAt: invalid AccumulatedByteString"
        mostRecent:rest ->
          let neededLen =
                -- cannot underflow due to precondition (2)
                BS.Strict.length mostRecent - fromIntegral leftoverLen
              (needed, leftover) =
                BS.Strict.splitAt neededLen mostRecent
          in ( BS.Strict.concat $ reverse (needed : rest)
             , if BS.Strict.null leftover
                 then Nothing
                 else Just leftover
             )
  where
    -- cannot underflow due to precondition (1)
    leftoverLen :: Word
    leftoverLen = accLength - n


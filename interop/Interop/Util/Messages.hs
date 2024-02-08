-- | Utilities for working with the message types from the interop testsuite
module Interop.Util.Messages (
    -- * BoolValue
    boolValue
    -- * Payload
  , payloadOfZeroes
  , payloadOfType
  , clearPayload
  ) where

import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Char8 qualified as BS.Strict.Char8

import Interop.API
import Interop.Util.Exceptions

{-------------------------------------------------------------------------------
  BoolValue
-------------------------------------------------------------------------------}

boolValue :: Bool -> BoolValue
boolValue b = defMessage & #value .~ b

{-------------------------------------------------------------------------------
  Payload
-------------------------------------------------------------------------------}

payloadOfZeroes :: Int -> Payload
payloadOfZeroes sz = defMessage & #body .~ BS.Strict.pack (replicate sz 0)

payloadOfType :: Integral size => PayloadType -> size -> IO Payload
payloadOfType type' size = do
    body <-
      case type' of
        COMPRESSABLE ->
          return $ BS.Strict.pack (replicate (fromIntegral size) 0)
        PayloadType'Unrecognized x ->
          assertUnrecognized x
    return $
      defMessage
        & #type' .~ type'
        & #body  .~ body

clearPayload ::
     ( HasField a "payload" b
     , HasField b "body" Strict.ByteString
     )
  => a -> a
clearPayload x = x & #payload . #body .~ BS.Strict.Char8.pack replacement
  where
    replacement :: String
    replacement = concat [
          "<<payload of "
        , show (BS.Strict.length (x ^. #payload . #body))
        , " bytes>>"
        ]

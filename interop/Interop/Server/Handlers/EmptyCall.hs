module Interop.Server.Handlers.EmptyCall (handleEmptyCall) where

import Data.ProtoLens

import Proto.Src.Proto.Grpc.Testing.Empty

-- | Handle @TestService.EmptyCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#emptycall>
handleEmptyCall :: Empty -> IO Empty
handleEmptyCall _ = return defMessage


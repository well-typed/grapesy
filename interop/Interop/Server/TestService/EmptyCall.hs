module Interop.Server.TestService.EmptyCall (handleEmptyCall) where

import Network.GRPC.Common.Protobuf

import Proto.Empty

-- | Handle @TestService.EmptyCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#emptycall>
handleEmptyCall :: Empty -> IO Empty
handleEmptyCall _ = return defMessage


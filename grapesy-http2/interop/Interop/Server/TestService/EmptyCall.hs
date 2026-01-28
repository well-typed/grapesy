module Interop.Server.TestService.EmptyCall (handle) where

import Network.GRPC.Common.Protobuf

import Proto.API.Interop

-- | Handle @TestService.EmptyCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#emptycall>
handle :: Proto Empty -> IO (Proto Empty)
handle _ = return defMessage


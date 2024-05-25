module Interop.Server.TestService.EmptyCall (handle) where

import Interop.API

-- | Handle @TestService.EmptyCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#emptycall>
handle :: Proto Empty -> IO (Proto Empty)
handle _ = return defMessage


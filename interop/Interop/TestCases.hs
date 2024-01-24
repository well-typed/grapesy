-- | Re-export all testcases
--
-- The test cases are described in the gRPC documentation at
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md>;
-- the relevant @.proto@ definitions are
--
-- * @grpc-repo/src/proto/grpc/testing/test.proto@ (main service definition)
-- * @grpc-repo/src/proto/grpc/testing/messages.proto@ (most message types)
-- * @grpc-repo/src/proto/grpc/testing/empty.proto@
module Interop.TestCases (module X) where

import Interop.TestCase.CacheableUnary as X
module Test.Sanity.Disconnect where

-- Server disconnects:
-- 1. Current call and concurrent calls fail with ServerDisconnected
-- 2. Future calls (on reconnect, which happens transparently) succeed
--
-- Client disconnects:
-- 1. All handlers dealing with that particular client (and only those handlers)
--    should get a ClientDisconnected exception.
# Revision history for grapesy

## 1.1.0 -- 2025-05-23

* Overhaul `ReconnectPolicy` to allow making `ReconnectDecision`s after running
  some `IO` action.
* Introduce `ReconnectDecision`, which specifies the `ReconnectTo` target, the
  `OnConnection` action to run upon reconnection, and the next
  `ReconnectPolicy`.
* Add `OnConnection` actions to `ConnParams` and `ReconnectDecision` so that
  users can track whether a connection is actually connected.

## 1.0.1 -- 2025-04-01

* Support unix sockets [#275, Sjoerd Visscher]

## 1.0.0 -- 2025-01-22

* First released version.

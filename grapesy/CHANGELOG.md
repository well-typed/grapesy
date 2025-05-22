# Revision history for grapesy

## 1.1.0 -- 2025-05-23

* Add `OnConnection` actions to `ConnParams` and `ReconnectPolicy`
* Change `ReconnectAfter` to contain just an `IO` action that returns a
  `ReconnectDecision`.
* Introduce `ReconnectDecision`, which specifies the `ReconnectTo` target, the
  `OnConnection` action to run upon reconnection, and the next
  `ReconnectPolicy`.

## 1.0.1 -- 2025-04-01

* Support unix sockets [#275, Sjoerd Visscher]

## 1.0.0 -- 2025-01-22

* First released version.

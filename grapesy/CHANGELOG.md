# Revision history for grapesy

## 1.2.0 -- 2026-09-02

* Use `http2-5.4.4` (which brings in `crypton-1.1.*`).
* Improve handling of of `http2` exceptions
* Reduce test-suite flakiness
* `recvNextOutputElem` checks trailing metadata [#369]
* Send `RST_STREAM` even if client has already sent their final message [#372].
  The RST_STREAM tells the server that the client is no longer interested in
  receiving any more messages from the server; it's therefore independent from
  whether or not the client has sent _its_ final message _to_ the server.
* The set of trailers included in a response no longer need to be static per
  RPC, but can vary based on the request; see
  `setResponseInitialMetadataAndTrailers` [#375]
* HTTP `Trailer` header (which announces which trailers a server might send)
  can now be inspected by clients
* Improve documentation of `exponentialBackoff` [#332, Mako Bates]
* Support for GHC 9.12 and 9.14
* Various other bounds [#348, Erik de Castro Lopo; and others]
* Test against v1.83 of the official gRPC interop tests [#371]

## 1.1.1 -- 2025-10-09

* Support `openConnection/closeConnection`

## 1.1.0 -- 2025-07-17

* User-specified actions on connection/disconnect/reconnect [#280]
  - Overhaul `ReconnectPolicy` to allow making `ReconnectDecision`s after
    running some `IO` action.
  - Introduce `ReconnectDecision`, which specifies the `ReconnectTo` target, the
    `OnConnection` action to run upon reconnection, and the next
    `ReconnectPolicy`.
  - Add `OnConnection` actions to `ConnParams` and `ReconnectDecision` so that
    users can track whether a connection is actually connected.
* `Network.GRPC.Common.Protobuf` does not import from `Data.ProtoLens.Labels`,
  to avoid problems with `lens` [#283, Leonid Onokhov].
  NOTE: Users who want to rely on this will also need
  https://github.com/google/proto-lens/pull/515.
* Lower bound on `http2-tls` changed to 0.4.9 [#289]

## 1.0.1 -- 2025-04-01

* Support unix sockets [#275, Sjoerd Visscher]

## 1.0.0 -- 2025-01-22

* First released version.

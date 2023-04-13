# Haskell library providing [`gRPC`](https://grpc.io/) client and server

The `grapesy` Haskell library provides a `gRPC` server and client.

This is work in progress; it is **NOT YET READY FOR USE**.

It currently depends on an unreleased version of `http2`; see
[`cabal.project`](cabal.project).

## Documentation

* [Description of the demo](demo.md)
* [Project status](status.md)

## Comparison to related packages

* Unlike
  [`http2-client-grpc`](https://hackage.haskell.org/package/http2-client-grpc),
  `grapesy` uses the HTTP2 client from
  [`http2`](https://hackage.haskell.org/package/http2) rather than from
  [`http2-client`](https://hackage.haskell.org/package/http2-client).
* Unlike [`grpc-haskell`](https://hackage.haskell.org/package/grpc-haskell), it
  does not make use of any C or C++ libraries (native Haskell only).

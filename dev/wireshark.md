# Debugging with Wireshark

## Setup

Wireshark supports `gRPC` and can decode Protobuf; see
https://grpc.io/blog/wireshark/.

Relevant Wireshark options (the demo server uses 50051 for insecure
communication and 50052 for secure communication):

* `Protocols/HTTP2/TCP port(s)`: add 50051

* `Protocols/HTTP/SSL/TLS Ports`: add 50052

* `Protocols/Protobuf/Protobuf search paths`: make sure that the directory
  containing the `.proto` definitions for the examples is listed here:

  - /path/to/grpc-repo/examples/protos

  For each of these you should enable "Load all files"

* `Preferences`/`Protocols`/`TLS`/`(Pre-)-Master-Secret log filename`:
  Set this to the same path that you set the `SSLKEYLOGFILE` environment
  variable to (both the `grapesy` server and client both respect this
  environment variable).

When monitoring network traffic, you might then want to use a filter

```
tcp.port == 50051 || tcp.port == 50052
```

Or to only see HTTP2 traffic:

```
(tcp.port == 50051 || tcp.port == 50052) && http2
```

This is less useful when debugging connectivity issues, of course.

## Hints

* If you see a TCP `SYN` followed immediately by a TCP `RST`, this may be a sign
  that the server is listening on the wrong interface; if so, this can be
  solved by having the server listen on _all_ interfaces (`0.0.0.0`).

  Another symptom of this (especially when the server runs inside Docker) is
  when the client TLS handshake fails with `HandshakeFailed Error_EOF`.
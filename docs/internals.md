# Internals

This document will eventually provide a high-level overview of the iternals
of the library.

## Relevant specifications

* [HTTP2](https://datatracker.ietf.org/doc/html/rfc7540)
* [gRPC](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
* [Protobuf](https://protobuf.dev/reference/protobuf/proto3-spec/)

## Debugging

### Wireshark

Wireshark supports `gRPC` and can decode Protobuf; see
https://grpc.io/blog/wireshark/.

Relevant Wireshark options:

* `Protocols/HTTP2/TCP port(s)`: add 50051 here (the port used by the
  example servers).
* `Protocols/Protobuf/Protobuf search paths`: make sure that the directory
  containing the `.proto` definitions for the examples is listed here.


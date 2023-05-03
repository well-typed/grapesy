# Internals

This document will eventually provide a high-level overview of the iternals
of the library.

## Relevant specifications

* [HTTP2](https://datatracker.ietf.org/doc/html/rfc7540)
* [gRPC](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md)
* [Protobuf](https://protobuf.dev/reference/protobuf/proto3-spec/)

### gRPC spec

The gRPC spec uses ABNF syntax <https://www.rfc-editor.org/rfc/rfc5234>.
Summary:

* Ordered sequence: `A B C`
* Alternatives: `A / B / C`
* Repetition: `*A` for zero-or-more, and `1*A` for one-or-more
* Optional: `[A]`

In addition (not conform ABNF), the gRPC spec also uses `?A` to mean optional.

## Debugging

### Wireshark

Wireshark supports `gRPC` and can decode Protobuf; see
https://grpc.io/blog/wireshark/.

Relevant Wireshark options:

* `Protocols/HTTP2/TCP port(s)`: add 50051 here (the port used by the
  example servers).
* `Protocols/Protobuf/Protobuf search paths`: make sure that the directory
  containing the `.proto` definitions for the examples is listed here.


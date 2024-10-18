# Internals

This document will eventually provide a high-level overview of the internals
of the library.

## Relevant specifications

* [HTTP1](https://datatracker.ietf.org/doc/html/rfc7231)

  Although gRPC is based on HTTP2, the HTTP2 standard itself refers back to
  the HTTP1 standard for things like error codes.

* [HTTP2](https://datatracker.ietf.org/doc/html/rfc7540)

* [URI](https://datatracker.ietf.org/doc/html/rfc3986)

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

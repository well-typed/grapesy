# Debugging

## Wireshark configuration

Wireshark supports `gRPC` and can decode Protobuf; see
https://grpc.io/blog/wireshark/.

Relevant Wireshark options (the demo server uses 50051 for insecure
communication and 50052 for secure communication):

* `Protocols/HTTP2/TCP port(s)`: add 50051
* `Protocols/HTTP/SSL/TLS Ports`: add 50052
* `Protocols/Protobuf/Protobuf search paths`: make sure that the directory
  containing the `.proto` definitions for the examples is listed here.
* `Preferences`/`Protocols`/`TLS`/`(Pre-)-Master-Secret log filename`:
  Set this to the same path that you set the `SSLKEYLOGFILE` environment
  variable to (both the `grapesy` server and client both respect this
  environment variable).

When monitoring network traffic, you might then want to use a filter

```
tcp.port == 50051 || tcp.port == 50052
```

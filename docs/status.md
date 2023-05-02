# Status

Currently this document is mostly intended for internal use, recording an
ever growing (and hopefully eventually shrinking) list of things to do.

## Unimplemented

- [ ] Compression schemes

- [ ] (As part of compression schemes:) When we send the first message to the
      server, it will respond with the list of compression schemes it supports.
      We should store this as part of the connection state. (For the C++ example
      servers from the official repo this is `deflate` and `gzip`.)

- [ ] We should think about transparent retries (maybe test with nginx).
      See https://nuvalence.io/insights/lessons-learned-investigating-goaways-in-grpc-apis/
      and https://github.com/grpc/proposal/blob/master/A6-client-retries.md .

- [ ] Authentication (ALTS)

- [ ] `Network.GRPC.Client.Request` currently uses one-place buffers. We should
      replace these with `n`-place buffers, for configurable `n`.

## Bugs

- [ ] An exception in the parts of `grapesy` client implementation will
      currently cause the client to block indefinitely (if they are waiting for
      a response). We should propagate these exceptions.

      Status: I think this is fixed, but needs tests.

- [ ] Related: if the function provided to `biDirStreaming` et al. throws
      an exception, we similarly block indefinitely (this can happen for example
      if we have a final unexpected delay, see 'execAll'). The exception is
      moreover swallowed.

      Status: I think this is fixed, but needs tests.

- [ ] When we run an RPC call against a server that does not implement it,

      - we sometimes get `ResponseUnexpectedContentLength 0`
        (e.g., run `sayHelloStreamReply` against `route_guide_server.py`)
      - we sometimes get `StreamErrorIsReceived NoError`
        (e.g., run `routeChat` against `wait_for_ready_with_client_timeout_example_server.py`)

- [ ] When we run the `SayHelloStreamReply` client against the example C++
      server, which does not implement it, we just seem to stall; should use
      Wireshark to see if that is correct, or whether we are getting some output
      from the server.

- [ ] We should set the HTTP2 `Trailers` header. The GRPC spec doesn't stipulate
      this, but perhaps it should (open issue about this at
      https://github.com/grpc/grpc/issues/29540); it feels like good HTTP
      hygiene.

## Bugs in `http2`

- [x] `Client` type is not general enough
      https://github.com/kazu-yamamoto/http2/pull/72

- [x] Concurrency bug in `sendRequest`
      https://github.com/kazu-yamamoto/http2/issues/73 /
      https://github.com/kazu-yamamoto/http2/pull/74

- [ ] `RST_STREAM` with `NO_ERROR` should not throw `StreamErrorIsReceived`
      https://github.com/kazu-yamamoto/http2/issues/76

      In the context of `gRPC`, this can be important: if the client sends a
      message to the server but does not mark it as `Final`, but the server
      _does_ consider the conversation as being finished, the server can send
      a `RST_STREAM` frame to the client.

## Testing

- [ ] Test against the
      [`gRPC interop test suite`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md).

- [ ] We should test with a proxy in the middle, as I'm seeing lots of
      StackOverflow questions etc. about things going wrong with proxies.

- [ ] We should have an example of a proper chat client. When we do, we should
      also think carefully about termination: everything should shut down
      cleanly, with a minimum of care required in the implementation of the
      chat client itself.

- [ ] We should stress-test the server and the client while we limit the ghc
      heap (and stack?), to ensure there are no memory leaks.

- [ ] We should stress-test against existing servers.

- [ ] Probably not that useful, but postman has support for testing gRPC
      https://learning.postman.com/docs/sending-requests/grpc/test-examples/.
      Might be useful to try out, just in terms of compliance testing.

## Demo

- [X] Update demo once we have specialized functions for the various
      Protobuf communication patterns.
- [ ] Update demo when we have a server-side implementation, too.
- [X] Update demo for conduit

## Things to figure out/read

- [ ] The Python example server for `SayHelloStreamReply`
      is specifically designed to test a gRPC feature called
      [Wait for Ready semantics](https://github.com/grpc/grpc/blob/master/doc/wait-for-ready.md).
      We should figure out what that is and if we need to support it.
      The server also provides an `initial-md` custom header for this request;
      unsure if this is related (and are there "non-initial-md" values..?).

- [ ] https://www.youtube.com/watch?v=Yw4rkaTc0f8 1:01:00 talks about pros of
      gRPC, and mentions "progress feedback", which i guess is just
      bidirectional communication? It also mentions also "cancel requests" and
      says that it depends on HTTP2. We need to ensure that that works.
      (PROTOCOL_HTTP2.md explicitly mentions CANCEL(8))

- [ ] Should look at the API of Gabriella's library and see if we are missing
      anthing. https://hackage.haskell.org/package/grpc-haskell

- [ ] Notes on performance:
      - https://grpc.io/docs/guides/performance/
      - https://grpc.io/docs/guides/benchmarking/
      - https://ably.com/blog/grpc-stream-performance
        * "Crushed hopes" -- we should do similar tests
        * "With 1000 publishers, .." -- these insights might be relevant

- [ ] Keep-alive pings?
      Briefly mentioned in https://learn.microsoft.com/en-us/aspnet/core/grpc/performance?view=aspnetcore-7.0 .
      Looking at Wireshark however I'm already seing these pings; it seems
      like `http2` is taking care of it for us. But should make sure.

- [ ] Buffers...?
      https://medium.com/engineering-at-palo-alto-networks/dataflow-and-grpc-using-at-scale-9612303dfe0b
      says "gRPC uses buffers on the client and server side. It is important to
      tune these to your use case." Not sure if/how this is relevant to us.

- [ ] Some references on gRPC error handling we should read/scan through:
      - https://grpc.io/docs/guides/error/
      - https://anthonygiretti.com/2022/08/28/asp-net-core-6-handling-grpc-exception-correctly-server-side/
      - https://programmingpercy.tech/blog/grpc-interceptors/
  https://medium.com/utility-warehouse-technology/advanced-grpc-error-usage-1b37398f0ff4
      - https://pkg.go.dev/google.golang.org/grpc/codes
      - https://cloud.google.com/apis/design
        (This one is not gRPC specific, but does mention gRPC)
      - https://www.baeldung.com/grpcs-error-handling

- [ ] Some references on back pressure.
      I _think_ we are handling back pressure ok already, but still, there
      might be some insights here, or at least some things to test.
      - https://groups.google.com/g/grpc-io/c/ePChS4cyEOc?pli=1
      - https://medium.com/@georgeleung_7777/seamless-backpressure-handling-with-grpc-kotlin-a6f99cab4b95
      - https://stackoverflow.com/questions/72424145/how-to-do-server-side-backpressure-in-grpc
      - https://www.quora.com/How-do-I-implement-flow-control-and-back-pressure-in-a-GRPC

- [ ] Some references on flow control we should read/scan through
      - https://medium.com/engineering-at-palo-alto-networks/dataflow-and-grpc-using-at-scale-9612303dfe0b
      - https://www.reddit.com/r/grpc/comments/10y6cza/comment/j9xo8fx/?utm_source=reddit&utm_medium=web2x&context=3

## Nice-to-haves

- [ ] Protobuf parsing (deserialization) as implemented in `proto-lens`
      depends on strict bytestrings. It would be nicer if this would use
      lazy bytestrings. There is an existing ticket open about this
      on the `proto-lens` repo: https://github.com/google/proto-lens/issues/62;
      it's not that easy to fix, because the first thing their parser does is
      turn the strict bytestring into a C pointer to the raw bytes and then
      proceeds from there.

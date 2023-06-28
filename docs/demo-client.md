# Demo client

The code for the demo can be found in [`demo/Main.hs`](/demo-client/Main.hs). It
is based on the examples from the [gRPC docs](https://grpc.io/docs/). We will
mostly refer to the [Python version]https://grpc.io/docs/languages/python/) of
these examples, as it is the most complete, although we will also provide some
pointers for testing against the [C++
version](https://grpc.io/docs/languages/cpp/).

In the explanations below, `grpc-repo` refers to a local checkout of the [gRPC
repo](https://github.com/grpc/grpc). These instructions are based on commit hash
[358bfb5](https://github.com/grpc/grpc/commit/358bfb581feeda5bf17dd3b96da1074d84a6ef8d).

## Protobuf

All of these examples use the Protobuf instantiation of `gRPC`. On the Haskell
side, `.proto` files are compiled using
[`proto-lens-protoc`](https://hackage.haskell.org/package/proto-lens-protoc).
This uses type-level symbols to refer to service methods; one minor pecularity
here is that it turns the first letter of the method name to lowercase. For
example, `helloworld.Greeter.SayHello` becomes

```haskell
Protobuf @Greeter @"sayHello"
```

## Debugging

The demo client supports a `--debug` command line argument. If enabled, you will
get verbose debug logs of the communication between the client and the server.

```
cabal run demo-client -- --debug sayHello --name 'John'
```

## TLS

See https://grpc.io/docs/guides/auth/#python for the changes required to the
Python code to enable TLS.

To connect over TLS, run the client with `--secure`:

```
cabal run demo-client -- sayHello --name 'John' --secure
```

This will use the systems's standard certificate store to validate the server's
TLS certificate. For example, if this is run against the demo server, which uses
a self-signed certificate, the above will result in

```
demo-client: HandshakeFailed (Error_Protocol ("certificate has unknown CA",True,UnknownCa))
```

There are two ways to address this. We can disable certificate validation
altogether:

```
cabal run demo-client -- sayHello \
  --name 'John' \
  --secure \
  --no-server-validation
```

or we can define our own roots; for example, we can declare the demo server's
own certificate as a root:

```
cabal run demo-client -- sayHello \
  --name 'John' \
  --secure \
  --cert-store-from-path data/grpc-demo.cert
```

## Quick start

### `helloworld.Greeter.SayHello`

Haskell implementation: `greeterSayHello`.

Assumes running server:

* If testing against example Python server:

  ```
  grpc-repo/examples/python/helloworld$ python3 greeter_server.py
  ```

* If testing against example C++ server:

  ```
  grpc-repo/examples/cpp/helloworld/cmake/build$ ./greeter_server
  ```

Run client with

```
cabal run demo-client -- sayHello --name 'John'
```

The output will be slightly different depending on which server you use.

### Testing compression

Unlike most demos, the `sayHello` example can also send _multiple_ messages:

```
cabal run demo-client -- sayHello --name 'John' --name 'Alice'
```

This can be interesting for example to verify that compression is working
propertly (the first message will be sent without compression since it's not
yet unknown what compression schemes the server supports, but subsequent
messages should then be compressed). However, it takes a bit of effort to make
the server actually compress anything:

* You need to enable compression in the server; for the Python one, see
  https://github.com/grpc/grpc/blob/master/examples/python/compression/README.md
* You need to send a "name" that is long enough that the server actually
  bothers with compression at all.
* You can also use the `--gzip` command line flag to tell the server to /only/
  use GZip compression.

For example:

```
cabal run demo-client -- --gzip sayHello \
  --name 'John' \
  --name '0xxxxxxxxxx1xxxxxxxxxx2xxxxxxxxxx3xxxxxxxxxx4xxxxxxxxxx5xxxxxxxxxx'
```

Of course, the compression is transparent to the user, but you can observe in
in Wireshark.

### Dealing with unterminated streams

Normally the last message to the server is marked as `END_STREAM`. If this is
not the case, the server can still respond, but they will additionally send a
`RST_STREAM` frame to the client to indicate that the stream has ended.
To test this we can intentionally omit the `END_STREAM` marker:

```
cabal run demo-client -- --core-dont-mark-final sayHello --name 'John'
```

Wireshark can be used to observe the `END_STREAM` marker being sent
(note that this depends on https://github.com/kazu-yamamoto/http2/pull/78).

### `helloworld.Greeter.SayHelloStreamReply`

Haskell implementation: `greeterSayHelloStreamReply`.

This is only implemented by a _single_ example server (the Python one). Run it
with

```
grpc-repo/examples/python/wait_for_ready$
  python3 wait_for_ready_with_client_timeout_example_server.py
```

Run client with

```
cabal run demo-client -- sayHelloStreamReply --name 'John'
```

The Python server has some intentional delays in there, so the streaming will be
relatively slow (delays on the order of seconds). Moreover, this example also
sends some metadata in the initial request. Metadata is not available using the
standard Protobuf API in grapesy; instead, we need to use the full client API.
The demo does this in the `Core` implementation:

```
cabal run demo-client -- --core sayHelloStreamReply --name 'John'
```

This should output something like:

```
# pause..
[AsciiHeader (HeaderName "initial-md") (AsciiValue "initial-md-value")]
# pause..
{message: "Hello John times 0"}
{message: "Hello John times 1"}
{message: "Hello John times 2"}
[]
```

## Route guide

Instructions below assume a running server:

```
grpc-repo/examples/python/route_guide$ python3 route_guide_server.py
```

or

```
grpc-repo/examples/cpp/route_guide$ cmake/build/route_guide_server
```

All methods except `getFeature` support the `--protobuf-pipes` option to use the
[`pipes` interface](/src/Network/GRPC/Client/Protobuf/Pipes.hs).

### `routeguide.RouteGuide.GetFeature` (non-streaming RPC)

Haskell implementation: `routeGuideGetFeature`.

Run client:

```
cabal run demo-client -- getFeature --latitude 409146138 --longitude -746188906
```

(for a location where there exists a feature), or

```
cabal run demo-client -- getFeature --latitude 0 --longitude 0
```

for a location where there is not.

### `routeguide.RouteGuide.ListFeatures` (server-side streaming)

Haskell implementation: `routeGuideListFeatures`.

Run client:

```
cabal run demo-client -- listFeatures \
    --lo-latitude   400000000 \
    --lo-longitude -750000000 \
    --hi-latitude   420000000 \
    --hi-longitude -730000000
```

#### Edge case: no features

An important edge case here is the case where there are zero features in
the specified rectangle:

```
cabal run demo-client -- --debug listFeatures \
  --lo-latitude  0 \
  --lo-longitude 0 \
  --hi-latitude  0 \
  --hi-longitude 0
```

This is important because this triggers the gRPC `Trailers-Only` case despite
not being an error (strictly speaking this is not conform the gRPC spec).

This test can also be run with `--core`, in which case it shows the received
metadata. This is important, even though that metadata is empty, because it
requires some special case to treat that metadata correctly in this case; see
discussion in `Network.GRPC.Spec.Response`.

### `routeguide.RouteGuide.RecordRuite` (client-side streaming)

Haskell implementation: `routeGuideRecordRoute`.

Run client:

```
cabal run demo-client -- recordRoute \
  --latitude 404306372 --longitude -741079661 \
  --delay 0.5                                 \
  --latitude 406109563 --longitude -742186778 \
  --delay 0.5                                 \
  --latitude 417951888 --longitude -748484944 \
  --delay 0.5                                 \
  --latitude 411236786 --longitude -744070769 \
  --delay 0.5                                 \
  --latitude 409224445 --longitude -748286738 \
  --delay 0.5                                 \
  --latitude 407586880 --longitude -741670168 \
  --delay 0.5                                 \
  --latitude 415736605 --longitude -742847522 \
  --delay 0.5                                 \
  --latitude 411236786 --longitude -744070769 \
  --delay 0.5                                 \
  --latitude 400106455 --longitude -742870190 \
  --delay 0.5                                 \
  --latitude 404615353 --longitude -745129803
```

This should report something like

```
{point_count: 10 feature_count: 10 distance: 667689 elapsed_time: 4}
```

#### Edge case: empty route

One important edge case here is the empty route:

```
cabal run demo-client -- --debug recordRoute
```

(This is effectively the equivalent of the  `Trailers-Only` case from the "no
features" edge case above, except of course that gRPC does not actually support
request trailers. Nonetheless, similar scenario.)

### `routeguide.RouteGuide.RouteChat` (bidirectional streaming)

Haskell implementation: `routeGuideRouteChat`.

This example is a bit strange; despite the name "chat" it does not actually
allow for communication between clients. Instead, the server will keep a history
of all notes received, and whenever it receives another note, it will output all
previous notes at that same location. So if you run the client with something
like this:

```
cabal run demo-client -- routeChat \
  --latitude 0 --longitude 0 'A' \
  --delay 1                      \
  --latitude 0 --longitude 0 'B' \
  --delay 1                      \
  --latitude 0 --longitude 0 'C' \
  --delay 1                      \
  --latitude 0 --longitude 0 'D' \
  --delay 1                      \
  --latitude 0 --longitude 1 'E' \
  --delay 1                      \
  --latitude 0 --longitude 1 'F' \
  --delay 1                      \
  --latitude 0 --longitude 1 'G' \
  --delay 1                      \
  --latitude 0 --longitude 0 'H'
```

you will get this output:

```
Sending {location { } message: "A"}
Delay 1.0s

Sending {location { } message: "B"}
Delay 1.0s
{location { } message: "A"}

Sending {location { } message: "C"}
Delay 1.0s
{location { } message: "A"}
{location { } message: "B"}

Sending {location { } message: "D"}
Delay 1.0s
{location { } message: "A"}
{location { } message: "B"}
{location { } message: "C"}

Sending {location { longitude: 1 } message: "E"}
Delay 1.0s

Sending {location { longitude: 1 } message: "F"}
Delay 1.0s
{location { longitude: 1 } message: "E"}

Sending {location { longitude: 1 } message: "G"}
Delay 1.0s
{location { longitude: 1 } message: "E"}
{location { longitude: 1 } message: "F"}

Sending {location { } message: "H"}
{location { } message: "A"}
{location { } message: "B"}
{location { } message: "C"}
{location { } message: "D"}
```

**Note**: The C++ implementation of this server seems buggy: new clients that
connect sometimes (randomly?) get state from old clients. The Python client does
not do this.



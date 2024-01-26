# Demo server

The demo server implements the `Greeter` (`helloworld.proto`) and `RouteGuide`
(`route_guide.proto`) examples from the gRPC repo.

## Running the server

Start the server with

```
cabal run demo-server -- \
  --port-insecure 50051 \
  --port-secure   50052 --host-secure 0.0.0.0 \
  --tls-pub  data/grpc-demo.cert \
  --tls-priv data/grpc-demo.priv
```

It is occassionally useful to run the server without `cabal` (for example, when
you want to use a priviliged port). To do this (without installing the server
with `cabal install`), you can run:

```
sudo grapesy_datadir=data \
  `find dist-newstyle/ -name demo-server -type f` \
  --disable-insecure \
  --port-secure 443 \
  --tls-pub  data/grpc-demo.cert \
  --tls-priv data/grpc-demo.priv
```

## Debugging

You can pass `--debug` to the server to get additional debugging output.

## Testing the server

See https://grpc.io/docs/guides/auth/#python for the changes required to the
Python code to enable TLS.

### `Greeter`

To test the server against a `Greeter` client, can run the Python one:

```
grpc-repo/examples/python/helloworld$ python3 greeter_client.py
Will try to greet world ...
Greeter client received: Hello, you!
```

and

```
grpc-repo/examples/python/wait_for_ready$ python3 wait_for_ready_with_client_timeout_example_client.py
Greeter client received initial metadata: key=initial-md value=initial-md-value
received initial metadata before time out!
Greeter client received: Hello you times 0
Greeter client received: Hello you times 1
Greeter client received: Hello you times 2
```

### `RouteGuide`

Run the Python client:

```
grpc-repo/examples/python/route_guide$ python3 route_guide_client.py
```

(output omitted).

### Compression

Currently server-side compression can be verified simply by running the Python
hello-world client (and then looking at the communication in Wireshark), because
the server applies compression independent of whether that saves space or not.

### Trailers-Only shortcut

A normal gRPC response looks like

```
<headers>
<messages>
<trailers>
```

If there are no messages, then this whole thing collapses to just a set of
trailers (or headers; the distinction is no longer relevant in this case); the
gRPC specification refers to this as `Trailers-Only`. The spec says that this
should _only_ be used in error cases, but in practice some servers also use this
for normal cases. For example, the Python implementation of the `ListFeatures`
method will use `Trailers-Only` in the case that the list of features is empty.

The Protobuf-specific wrappers in `grapesy` will not use `Trailers-Only` except
in the case of errors, conforming to the spec; however, it is possible to use
the lower-level server API to get the behaviour exibited by the Python example
implementation. The command line flag `--trailers-only-shortcut` enables this
for the demo server. The difference in server operation can only be observed
with Wireshark; a request for a list of features in the rectangle `(0, 0, 0, 0)`
(which is empty) will

* result in three HTTP frames in the normal case (`HEADERS`, empty `DATA` to
  separate headers from trailers, and another `HEADERS` frame with the trailers)
* result in a single HTTP `HEADERS` frame when using `--trailers-only-shortcut`

Note that this behaviour does _NOT_ conform the gRPC spec, so not all clients
may support it.

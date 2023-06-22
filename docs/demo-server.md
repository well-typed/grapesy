# Demo server

The demo server implements the `Greeter` (`helloworld.proto`) and `RouteGuide`
(`route_guide.proto`) examples from the gRPC repo.

## Running the server

Start the server with

```
cabal run demo-server -- \
  --port-insecure 50051 \
  --port-secure   50052 \
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
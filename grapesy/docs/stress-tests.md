# Stress Tests

The stress tests are intended to test the performance of `grapesy`, primarily
ensuring that memory usage stays constant even under extreme load. The building
blocks of the test are the stress test server and client applications. These can
be ran manually as separate processes using (e.g.):

```
cabal run test-stress -- server ...
cabal run test-stress -- client ...
```

However, to automate the specific extreme performance scenarios we're interested
in, we also have a driver application that will automically spin up a selection
of servers and clients we care about:

```
cabal run test-stress -- driver
```

The driver will automically kill and restart certain clients and servers to test
"unstable" performance scenarios (e.g. do we leak memory when we attempt to
reconnect to disconnected servers?).

To enable verbose debugging traces of any of these components, pass `-v` as a
top-level option, e.g.:

```
cabal run test-stress -- -v client ...
```

**Warning:** Passing `-v` and `driver` will result in *a lot* of output.

The rest of this document will explain how to use each of these components on
the command-line and the specifics regarding what each of them is doing.

# The API

The stress test client and server communicate via the four major types of RPCs:

1. **Non-streaming:** Client sends one message, server sends one message back.
2. **Client streaming:** Client sends `N` messages, server sends one message
   back.
3. **Server streaming:** Client sends one message specifying `N`, server sends
   `N` messages back.
4. **Bidirectional streaming:** Client sends one messages specifying `N`, server
   and client take turns sending one message back and forth until each has sent
   `N` messages.

These are the "atoms" of communication between the stress test clients and
servers. The messages sent back and forth are random lazy bytestrings ranging in
length from 128 to 256 bytes (see [here](../test-stress/Test/Stress/Common.hs)).

# The Server

The server is the simplest to run. Simply specify the port it should bind to and
whether it should use TLS. For example, to start a secure server on port 50051:

```
cabal run test-stress -- server --secure --port 50051
```

Use the `--help` flag to see all available options.

By default, it will use the certificates and keys in the [`../data`](../data/)
directory, just like the demo server. See the [demo server's
documentation](./demo-server.md) for more information.

# The Client

The client takes options that specify the server it should connect to, how many
times it should connect to the server, and what calls it should execute on those
connections. For example, to run a client that opens 3 connections to an
insecure server at port 50051 and makes a client streaming call with 1234
messages and a server streaming call with 500 messages, repeating those calls on
each connection 10 times.

```bash
cabal run test-stress -- client \
    --port 50051 \
    --num-connections 3 \
    --num-calls 5 \
    --client-streaming 1234 \
    --server-streaming 500
```

Clients also support running each connection concurrently via the `--concurrent`
option. Clients can connect to secure servers (using the default certificates)
using the `--secure` option, but can be configured to use non-default
certificates via other command line options just like the demo client. See the
`--help` client option and the [demo client's documentation](./demo-client.md)
for more information.

# The Driver

The driver spawns a variety of servers and clients in separate processes, and
runs for a total of 60 seconds. Each process is run with a specific heap limit
(via the `-M` RTS flag), and the application will terminate with a non-zero exit
code if any of the processes are killed with a `heap overflow` exception.

## Servers

The driver spawns four total server processes. Each server is either secure or
insecure, and either stable or unstable. Secure servers require TLS, insecure
require non-TLS. Unstable servers are killed and restarted intermittently,
stable servers are left running for the duration of the driver's execution.

## Clients

The driver spawns 56 total client processes. Similar to the servers, each client
is either secure or insecure and stable or unstable. Each client only
communicates with one of the servers. Obviously, (in)secure clients only
communicate with an (in)secure servers. Each client-server pair only
communicates in one of the following "patterns":

* **Many connections:** Open a connection, make a single non-streaming call,
  repeat indefinitely. Think of this as calling `withConnection` over and over.
* **Many non-streaming calls:** Open a connection. Make a single non-streaming
  call, repeat indefinitely. Think of this as calling `withRPC` and sending a
  single message back and forth on a single connection over and over.
* **Client streaming:** Open a connection. Make a non-stop client streaming
  call.
* **Many client streaming calls:** Open a connection. Make a client streaming
  call with a few messages, repeat indefinitely.
* **Server streaming:** Same as client streaming, but server sends messages
  non-stop.
* **Many server streaming calls:** Same as client streaming, but server streams.
* **Bidirectional streaming:** Same as client streaming, but both client and
  server send messages indefinitely.
* **Many bidirectional streaming calls:** Same as client streaming, but both
  client and server stream messages.

## Summary chart generation

The stress test driver can optionally create summary heap profile charts for the
stable components after the test is finished by passing the `--gen-charts` flag.
This will cause each stable component to emit an event log with heap profiling
events. The driver will parse the event logs and generate SVG plots of the
memory usage over time.

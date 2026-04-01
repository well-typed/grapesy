# Debugging with Wireshark

## Setup

Wireshark supports `gRPC` and can decode Protobuf; see
https://grpc.io/blog/wireshark/.

Relevant Wireshark options (the demo server uses 50051 for insecure
communication and 50052 for secure communication):

* `Protocols/HTTP2/TCP port(s)`: add 50051

* `Protocols/HTTP/SSL/TLS Ports`: add 50052

* `Protocols/Protobuf/Protobuf search paths`: include
  `/path/to/grapesy/dev/grpc-proto`. This contains a slightly modified version
  of various `.proto` files from the official gRPC repo, specifically for use
  in Wireshark.  Enable "Load all files".

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

## `tshark`

It is sometimes useful to use a CLI rather than the GUI, especially when there
are non-deterministic test failures, and we want to repeatedly start and stop
capture.

* List available interfaces:

  ```
  dumpcap -D
  ```

* Capture traffic to and from 50051

  ```
  dumpcap -i lo -f "port 50051" -w capture.pcapng
  ```

* Example of repeatedly running a test until failure, capturing each test
  separately:

  ```bash
  #!/bin/bash

  for i in $(seq -f "%03g" 1 100)
  do
    echo "Test run $i"
    dumpcap -i lo -f "port 50051" -w capture-$i.pcapng &
    DUMPCAP=$!
    cabal run -- test-grapesy -p multipleMsgs
    TESTRESULT=$?
    sleep 0.5
    kill -SIGINT $DUMPCAP
    if [ $TESTRESULT -ne 0 ]
    then
      break
    fi
  done
  ```

  The call to `sleep` is necessary to give `dumpcap` a chance to actually
  write the packets (I don't know of a cleaner way to do this).

  Note: the tests in the Grapesy test suite use random ports by default; they
  must be modified before they will use a static port such as 50051.

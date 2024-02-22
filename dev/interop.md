# gRPC interoperability tests

The gRPC repo defines a set of interoperability tests; relevant documentation:

* https://github.com/grpc/grpc/blob/master/tools/run_tests/README.md
* https://github.com/grpc/grpc/blob/master/tools/interop_matrix/README.md
* https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md

In this document we describe how to setup and run these tests, and provide
some suggestions for debugging.

TODO: This document needs cleaning up.

## Setup

### Set up the git repo

Clone the `grapesy` branch of the gRPC repo:

```bash
git clone git@github.com:edsko/grpc -b grapesy ./grpc-repo
```

The `grapesy` branch is currently based off tag `v1.60.0`.

Then get all submodules:

```bash
grpc-repo$ git submodule update --init --recursive
```

Unlike the [official
instructions](https://grpc.io/docs/languages/python/quickstart/), this does
_not_ do a shallow checkout. This is important: the tests will fail otherwise
with an error such as this one:

```
Submodule 'third_party/abseil-cpp' (https://github.com/abseil/abseil-cpp.git) registered for path 'third_party/abseil-cpp'
Cloning into '/var/local/git/grpc/third_party/abseil-cpp'...
fatal: reference repository '/var/local/jenkins/grpc/third_party/abseil-cpp' is shallow
fatal: clone of 'https://github.com/abseil/abseil-cpp.git' into submodule path '/var/local/git/grpc/third_party/abseil-cpp' failed
```

### Install Python

```bash
apt-get install python3 python3-pip python3-setuptools python3-yaml
```

### Python virtual environment

Create Python virtual environment (https://docs.python.org/3/library/venv.html)

```bash
grpc-repo$ python3 -m venv ./python-venv
```

Then activate the virtual environment by prepending `python-venv/bin` to your
PATH (see https://docs.python.org/3/library/venv.html#how-venvs-work); one
option is to create an `.envrc` file in `grpc-repo` with

```bash
export PATH=/path/to/grpc-repo/python-venv/bin:$PATH
export MY_INSTALL_DIR=/path/to/grpc-repo/local
```

(the `MY_INSTALL_DIR` is only necessary to locally compile C++ programs;
see https://grpc.io/docs/languages/cpp/quickstart/).

### Dependencies

Install the required Python dependencies:

```bash
python3 -m pip install --upgrade \
  six==1.16.0 \
  google-auth==1.23.0 \
  google-api-python-client==1.12.8 \
  oauth2client==4.1.0
```

(these version numbers match the ones used in
`./tools/dockerfile/interoptest/grpc_interop_python/Dockerfile`).

Note: the Docker file in the repo also pins `pip` itself, but to quite an old
version (`python3 -m pip install --upgrade pip==19.3.1`); not sure if that is
needed (and actually results in some errors).

### IPv6

The tests get confused when IPv4 is enabled on the host; you should disable IPv6
and then restart the Docker daemon. On Linux this can be done with

```bash
grapesy$ sudo dev/disable-ipv6.sh
```

I have found no other way to solve this problem (some details in this script).

### Check that it works

```bash
grpc_repo$ tools/run_tests/run_interop_tests.py -l python -s c++ --use_docker
```

Setting up the Docker containers takes something between 5-10 minutes.

## Running the tests

### Commit all changes

Make sure all changes you want to test are committed:

* Any changes to the `grpc-repo` must be committed (locally)
* Make sure that the `grapesy` subrepo in the `grpc-repo` points to the
  `grapesy` branch you want to test.

### Server reachability

To test whether the `grapesy` server can be reached at all, we can ping it:

```bash
cabal run grapesy-interop -- --ping
```

### TLS

You can pass `--use_tls=false` to disable TLS; the official Interop tests
support this command line argument also. To ping a server over TLS, run

```bash
cabal run grapesy-interop -- --ping \
  --server_host_override=foo.test.google.fr \
  --use_test_ca=true
```

The server respects the `SSLKEYLOGFILE` environment variable, which can be
useful for Wireshark debugging; see [/dev/wireshark.md](/dev/wireshark.md) for
some suggestions on how to setup Wireshark.

### To rebuild the Docker image with the `grapesy` deps

When the `grapesy` dependencies change (and especially when it requires newer
versions of packages that aren't yet known in the cabal package database),
you need to rebuild the Docker image.

```bash
grapesy/dev/grapesy-deps-docker$ docker build . -t edsko/grapesy-deps:latest
```

then upload it

```bash
$ docker push edsko/grapesy-deps:latest
```

## Testing `grapesy` as a server

### Automatic execution

To run the tests automatically:

```bash
tools/run_tests/run_interop_tests.py -l python -s grapesy --use_docker
tools/run_tests/run_interop_tests.py -l c++    -s grapesy --use_docker
tools/run_tests/run_interop_tests.py -l go     -s grapesy --use_docker
tools/run_tests/run_interop_tests.py -l java   -s grapesy --use_docker
```

This will require a directory structure like this:

```
parent
  |
  +---- grpc-repo    https://github.com/grpc/grpc
  |
  +---- grpc-go      https://github.com/grpc/grpc-go
  |
  \---- grpc-java    https://github.com/grpc/grpc-java
```

### Manual execution

During development however it might be more convenient to use the `--manual`
flag, which creates a bash script that executes the individual tests:

Create the docker containers:

```bash
tools/run_tests/run_interop_tests.py -l go -s grapesy --use_docker --manual
```

(the `go` Interop tests are the quickest to build, by quite a margin).

Start the server

```bash
bash interop_server_cmds.sh
```

Make a note of which port was exposed locally:

```bash
docker ps
```

Then run the client commands:

```bash
SERVER_PORT=... bash interop_client_cmds.sh
```

If the tests fail, it may be more convenient to run the server directly on the
host, bypassing docker:

```bash
grapesy$ cabal run grapesy-interop -- --server
```

By default, the server will use TLS and run on port 50052.

## Testing `grapesy` as a client

### Automatic execution

To run the tests automatically:

```bash
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s go     --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s python --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s c++    --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s java   --use_docker
```

### Manual execution

During development, manual execution is more convenient, and the `grapesy`
client can be run outside of docker.

```bash
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s java --use_docker --manual
```

Start the server

```bash
grpc-repo$ bash ./interop_server_cmds.sh
```

Take a note of the port

```bash
$ docker ps
CONTAINER ID   IMAGE               ..  PORTS
833c9f2a84ea   grpc_interop_go:..  ..  0.0.0.0:32768->8080/tcp
```

and run the client:

```bash
grpc-repo$ SERVER_PORT=32768 bash ./interop_client_cmds.sh
```

Alternatively, you can also run the tests manually:

```bash
grapesy$ cabal run grapesy-interop -- \
  --client \
  --port=32768 \
  --server_host_override=foo.test.google.fr \
  --use_test_ca=true \
  --test_case=empty_unary
```

Omit the `--test_case` argument to run all tests.


## Testing against local grapesy build

### Building the reference clients/servers

First prepare all the reference servers and clients by running the following
in the `grpc-repo`:

```bash
tools/run_tests/run_interop_tests.py -l java -s java --use_docker --manual &&
  mv interop_server_cmds.sh java_server.sh &&
  mv interop_client_cmds.sh java_client.sh

tools/run_tests/run_interop_tests.py -l c++ -s c++ --use_docker --manual &&
  mv interop_server_cmds.sh cxx_server.sh &&
  mv interop_client_cmds.sh cxx_client.sh

tools/run_tests/run_interop_tests.py -l go -s go --use_docker --manual &&
  mv interop_server_cmds.sh go_server.sh &&
  mv interop_client_cmds.sh go_client.sh

tools/run_tests/run_interop_tests.py -l python -s python --use_docker --manual &&
  mv interop_server_cmds.sh python_server.sh &&
  mv interop_client_cmds.sh python_client.sh
```

### Running the grapesy client

Choose one of the servers to run

```bash
grpc-repo$ bash ./cxx_server.sh
```

Take a note of the server port

```bash
docker ps
```

Can now run individual tests (you'll need to specify the right port):

```bash
grapesy$ cabal run grapesy-interop -- \
  --client \
  --port=32770 \
  --server_host_override=foo.test.google.fr \
  --use_test_ca=true \
  --test_case=empty_unary
```

or omit the `--test_case` argument to run al tests.


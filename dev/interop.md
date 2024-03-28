# gRPC interoperability tests

The gRPC repo defines a set of interoperability tests; relevant documentation:

* https://github.com/grpc/grpc/blob/master/tools/run_tests/README.md
* https://github.com/grpc/grpc/blob/master/tools/interop_matrix/README.md
* https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md

In this document we describe how to setup and run these tests.

## Running grapesy against itself

To run the interop tests with a `grapesy` client against a `grapesy` server,
there is no need for a checkout of the official gRPC repo. Simply start the
server:

```bash
grapesy$ cabal run grapesy-interop -- --server
```

and run the tests

```bash
grapesy$ cabal run grapesy-interop -- --client
```

Alternatively, you can also run everything in one process.

```bash
grapesy$ cabal run grapesy-interop -- --self-test
```

All tests should pass.

## Running development `grapesy` against reference implementation

> [!NOTE]
> At the time of writing version
> [v1.62.0](https://github.com/grpc/grpc/releases/tag/v1.62.0) is the most
> recent (released Feb 20, 2024).

In this section we will describe how to run a development `grapesy` server or
client (that is, a local checkout of the git repository, run simply using
`cabal run`) against a reference implementation. For this you will need a
checkout of the [official gRPC repo](https://github.com/grpc/grpc/).

```bash
$ git clone https://github.com/grpc/grpc.git -b v1.62.0 ./grpc-repo
grpc-repo$ git switch -c v1.62.0
grpc-repo$ git submodule update --init --recursive
```

For some languages, the gRPC implementation lives in a separate repository.

```bash
$ git clone https://github.com/grpc/grpc-java.git -b v1.62.0
grpc-java$ git switch -c v1.62.0

$ git clone https://github.com/grpc/grpc-go.git -b v1.62.0
grpc-go$ git switch -c v1.62.0
```

These need to be checked out alongside `grpc-repo`, so that you end up with
something like this:

```
parent
  |
  +---- grpc-repo    https://github.com/grpc/grpc
  |
  +---- grpc-go      https://github.com/grpc/grpc-go
  |
  \---- grpc-java    https://github.com/grpc/grpc-java
```

> [!WARNING]
> Unlike the [QuickCheck
> instructions](https://grpc.io/docs/languages/python/quickstart/), this does
> _not_ do a shallow checkout. This is important: the tests will fail otherwise
> with an error such as this one:

```
Submodule 'third_party/abseil-cpp' (https://github.com/abseil/abseil-cpp.git) registered for path 'third_party/abseil-cpp'
Cloning into '/var/local/git/grpc/third_party/abseil-cpp'...
fatal: reference repository '/var/local/jenkins/grpc/third_party/abseil-cpp' is shallow
fatal: clone of 'https://github.com/abseil/abseil-cpp.git' into submodule path '/var/local/git/grpc/third_party/abseil-cpp' failed
```

> [!WARNING]
> The official interop tests fail when IPv6 is enabled on the host machine. For
> convenience, there is a script in the `grapesy` repo at
> [/dev/disable-ipv6.sh](/dev/disable-ipv6.sh) that can be used to disable IPv6
> on Linux machines. If IPv6 is enabled, tests will fail with an error such as
> the one below (this error is in the test _infrastructure_, independent from
> the reference client or server):

```
Traceback (most recent call last):
  File "../grpc-repo/tools/run_tests/run_interop_tests.py", line 1583, in <module>
    job.mapped_port(_DEFAULT_SERVER_PORT),
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "../grpc-repo/tools/run_tests/python_utils/dockerjob.py", line 165, in mapped_port
    return docker_mapped_port(self._container_name, port)
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "../grpc-repo/tools/run_tests/python_utils/dockerjob.py", line 58, in docker_mapped_port
    return int(output.split(":", 2)[1])
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ValueError: invalid literal for int() with base 10: '32768\n['
```

### Test infrastructure dependencies

The gRPC interop test infrastructure is written in Python, and requires some
dependencies. For convenience, in this section we describe how to set this up
on Linux. First, you will need Python:

```bash
$ apt-get install python3 python3-pip python3-setuptools python3-yaml
```

Create a Python virtual environment
(https://docs.python.org/3/library/venv.html)

```bash
grpc-repo$ python3 -m venv ./python-venv
```

Activate the virtual environment by prepending `python-venv/bin` to your
PATH (see https://docs.python.org/3/library/venv.html#how-venvs-work); one
option is to create an `.envrc` file in `grpc-repo` with

```bash
export PATH=/path/to/grpc-repo/python-venv/bin:$PATH
```

(you can also use `python-venv/bin/activate` but simply setting the path is
enough, and if you use the `.envrc` file, it will be activated automatically).

Finally, install the required Python dependencies:

```bash
grpc-repo$ python3 -m pip install --upgrade \
  six==1.16.0 \
  google-auth==1.23.0 \
  google-api-python-client==1.12.8 \
  oauth2client==4.1.0
```

(these version numbers match the ones used in
`tools/dockerfile/interoptest/grpc_interop_go/Dockerfile` and others).

> [!NOTE]
> The Docker file in the repo also pins `pip` itself, but to quite an old
> version (19.3.1). I found that this old version results in some errors, and
> had no problems with the default version (23.2 at the time of writing).

As a sanity check, you can try to run the Python reference client against the
Python reference server:

```bash
grpc_repo$ tools/run_tests/run_interop_tests.py -l go -s go --use_docker
```

All tests should pass.

### Build all clients and servers

You can now prepare all reference clients and servers:

First prepare all the reference servers and clients by running the following
in the `grpc-repo`:

```bash
grpc-repo$ tools/run_tests/run_interop_tests.py -l python -s python --use_docker --manual &&
  mv interop_server_cmds.sh python_server.sh &&
  mv interop_client_cmds.sh python_client.sh

grpc-repo$ tools/run_tests/run_interop_tests.py -l c++ -s c++ --use_docker --manual &&
  mv interop_server_cmds.sh cxx_server.sh &&
  mv interop_client_cmds.sh cxx_client.sh

grpc-repo$ tools/run_tests/run_interop_tests.py -l go -s go --use_docker --manual &&
  mv interop_server_cmds.sh go_server.sh &&
  mv interop_client_cmds.sh go_client.sh

grpc-repo$ tools/run_tests/run_interop_tests.py -l java -s java --use_docker --manual &&
  mv interop_server_cmds.sh java_server.sh &&
  mv interop_client_cmds.sh java_client.sh
```

### Running `grapesy` as a client

Start the various servers (I find it useful to give each its own terminal):

```bash
grpc-repo$ bash ./python_server.sh
grpc-repo$ bash ./cxx_server.sh
grpc-repo$ bash ./go_server.sh
grpc-repo$ bash ./java_server.sh
```

Take note of the port numbers that were assigned to them:

```bash
$ docker ps --format 'table {{.Image}}\t{{.Ports}}'
IMAGE                                                      PORTS
grpc_interop_java:0ac5c6f1-9835-45ee-9664-ad9925a41680     0.0.0.0:32771->8080/tcp
grpc_interop_go:b2a668d2-718c-4c66-b0e6-f7f6111974a6       0.0.0.0:32770->8080/tcp
grpc_interop_cxx:288ee67a-f462-40d1-b5a2-a29be5562fb2      0.0.0.0:32769->8080/tcp
grpc_interop_python:f558f1af-9ee5-41bc-8a6b-0b84a1a9e87a   0.0.0.0:32768->8080/tcp
```

We can now run the `grapesy` client against each reference server:

```bash
grapesy$ cabal run grapesy-interop -- --client \
  --server_port=32768 \
  --skip_compression # Python

grapesy$ cabal run grapesy-interop -- --client \
  --server_port=32769 # C++

grapesy$ cabal run grapesy-interop -- --client \
  --server_port=32770 \
  --skip_compression # Go

grapesy$ cabal run grapesy-interop -- --client \
  --server_port=32771 \
  --skip_client_compression \
  --skip_test=server_compressed_streaming \
  --skip_test=timeout_on_sleeping_server # Java
```

All tests should pass. All servers can be terminated using

```bash
$ docker stop $(docker ps -q)
```


> [!NOTE]
> Not all reference servers support all features, motivating the various
> `--skip-xyz` command line flags. To verify which features are unsupported,
> see `grpc-repo/tools/run_tests/run_interop_tests.py`, find the corresponding
> language definition (for example, `class PythonLanguage`), and then look at
> `unimplemented_test_cases_server`.
>
> The only exception is `timeout_on_sleeping_server` for Java: it seems that the
> server does not conform to the gRPC specification here, and simply closes the
> connection without sending `DEADLINE_EXCEEDED` to the client. The Java
> _client_ does not notice this because (it seems) it imposes a _local_ timeout
> also, and doesn't even _connect_ to the server: the test even passes without
> the server running at all. (And indeed, the other clients don't seem precise
> enough here: the `grapesy` server was passing this test before it implemented
> timeouts _at all_.)

It is also possible to only run one specific test case, for example:

```bash
grapesy$ cabal run grapesy-interop -- --client \
  --server_port=32771 \
  --test_case=timeout_on_sleeping_server
```

For Wireshark debugging it is sometimes useful to have the server run on the
host's own network (rather than isolated in a Docker network) on a specific
port. To do this, edit the corresponding script and replace the `-p 8080`
Docker parameter by `--net=host` (and optionally change the server's `--port`
flag also). You may also wish to disable TLS: the `grapesy` client and server
support `SSLKEYLOGFILE` environment variable, but the reference implementations
do not, making it impossible to debug any network traffic in Wireshark.

### Running `grapesy` as a server

To run `grapsey` as as server, run

```bash
grapesy$ cabal run grapesy-interop -- --server
```

By default, this will run the server on port 50052 (though this can be changed
using `--port`). To test if the server can be reached at all, `grapesy-interop`
has a `--ping` mode:

```bash
grapesy$ cabal run grapesy-interop -- --ping
```

To run the reference clients:

```bash
grpc-repo$ SERVER_PORT=50052 bash ./python_client.sh
grpc-repo$ SERVER_PORT=50052 bash ./cxx_client.sh
grpc-repo$ SERVER_PORT=50052 bash ./go_client.sh
grpc-repo$ SERVER_PORT=50052 bash ./java_client.sh
```

Only the ORCA tests are expected to fail (you may wish to disable those by
editing the client scripts).

## Running automatic tests

So far we've been running the interop tests in "manual" mode. To run them
in automatic mode, we need to integrate `grapesy` with the gRPC testing
infrastructure. This is done in a clone of the repo:

```bash
grpc-repo$ git remote add edsko git@github.com:edsko/grpc.git
grpc-repo$ git fetch --all
grpc-repo$ git checkout --track edsko/grapesy
grpc-repo$ git submodule update --init
```

_Everything_ that you want to test by running the automatic tests
**must be committed to the repository** (because the first thing the tests will
do is clone the local repo inside the Docker container); this includes the
checkout of `grapesy` you want to test, which is why it's included as a subrepo.

### Running `grapesy` as a server

To run `grapesy` against a reference server, run

```bash
grpc-repo$ tools/run_tests/run_interop_tests.py -l python -s grapesy --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l c++    -s grapesy --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l go     -s grapesy --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l java   -s grapesy --use_docker
```

All tests should pass.

### Running `grapesy` as a client

To run `grapesy` against a reference client, run

```bash
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s python --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s c++    --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s go     --use_docker
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s java   --use_docker
```

The only failures we currently expect are (non-deterministic) TLS handshake
failures. It's unclear at present where these are coming from (these don't seem
to happen when `grapesy` is run locally instead of in Docker).

### Debugging

To debug issues with the automatic tests, it's useful to switch back to
manual mode. For example,

```bash
grpc-repo$ tools/run_tests/run_interop_tests.py -l grapesy -s python --use_docker --manual &&
  mv interop_server_cmds.sh grapesy_to_python-server.sh &&
  mv interop_client_cmds.sh grapesy_to_python-client.sh
```

Run the server (in this example, Python):

```bash
grpc-repo$ bash ./grapesy_to_python-server.sh
```

Take a note of the server port:

```bash
$ docker ps --format 'table {{.Image}}\t{{.Ports}}'
IMAGE                                                      PORTS
grpc_interop_python:8f4411cd-383b-4d58-b855-e4d52ade4372   0.0.0.0:32781->8080/tcp
```

and run the client (in this case, `grapesy`):

```bash
grpc-repo$ SERVER_PORT=32781 bash ./grapesy_to_python-client.sh
```

This still uses `grapesy` inside of docker rather than locally, so is a more
faithful reproduction of what happens with the automatic tests. Nonetheless,
the TLS handshake failures common in the automatic test runs don't seem to
happen here, so the setup is not identical.
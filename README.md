# `grapesy`: industrial strength [gRPC][grpc:website] library for Haskell

Haskell library providing [`gRPC`][grpc:website] client and server.

## Interoperability matrix

The `gRPC` framework comes with a test suite that [tests interoperability
between implementations][grpc:interop] as well compliance to the `gRPC`
specification. As the table below shows, `grapesy` passes all of these tests,
apart from the Open Request Cost Aggregation (ORCA) tests, which is [not yet
supported][grapesy:issue:72-orca] by grapesy.

### Legend and version information

Legend:

* ✅ passed
* ❌ failed
* ❔ not supported by the reference (or reference does not conform to the gRPC
  specification)
* 🚫 not supported by `grapesy`

We last tested against version
[v1.81.0](https://github.com/grpc/grpc/releases/tag/v1.81.0) (released May 29,
2026).

### `grapesy` server versus reference client

| Test                          | Python | C++  | Go | Java | grapesy |
| ----------------------------- | ------ | ---- | -- | ---- | ------- |
| `cancel_after_begin`          | ✅      | ✅    | ✅  | ✅    | ✅       |
| `cancel_after_first_response` | ✅      | ✅    | ✅  | ✅    | ✅       |
| `client_compressed_streaming` | ❔      | ✅    | ❔  | ✅    | ✅       |
| `client_compressed_unary`     | ❔      | ✅    | ❔  | ✅    | ✅       |
| `client_streaming`            | ✅      | ✅    | ✅  | ✅    | ✅       |
| `custom_metadata`             | ✅      | ✅    | ✅  | ✅    | ✅       |
| `empty_stream`                | ✅      | ✅    | ✅  | ✅    | ✅       |
| `empty_unary`                 | ✅      | ✅    | ✅  | ✅    | ✅       |
| `large_unary`                 | ✅      | ✅    | ✅  | ✅    | ✅       |
| `orca_oob`                    | 🚫      | 🚫    | 🚫  | 🚫    | 🚫       |
| `orca_per_rpc`                | 🚫      | 🚫    | 🚫  | 🚫    | 🚫       |
| `ping_pong`                   | ✅      | ✅    | ✅  | ✅    | ✅       |
| `server_compressed_streaming` | ❔      | ✅    | ❔  | ✅    | ✅       |
| `server_compressed_unary`     | ❔      | ✅    | ❔  | ✅    | ✅       |
| `server_streaming`            | ✅      | ✅    | ✅  | ✅    | ✅       |
| `special_status_message`      | ✅      | ❔    | ✅  | ✅    | ✅       |
| `status_code_and_message`     | ✅      | ✅    | ✅  | ✅    | ✅       |
| `timeout_on_sleeping_server`  | ✅      | ✅    | ✅  | ✅    | ✅       |
| `unimplemented_method`        | ✅      | ✅    | ✅  | ✅    | ✅       |
| `unimplemented_service`       | ✅      | ✅    | ✅  | ✅    | ✅       |

### `grapesy` client versus reference server

| Test                          | Python | C++  | Go | Java | grapesy |
| ----------------------------- | ------ | ---- | -- | ---- | ------- |
| `cancel_after_begin`          | ✅      | ✅    | ✅  | ✅    | ✅       |
| `cancel_after_first_response` | ✅      | ✅    | ✅  | ✅    | ✅       |
| `client_compressed_streaming` | ❔      | ✅    | ❔  | ❔    | ✅       |
| `client_compressed_unary`     | ❔      | ✅    | ❔  | ❔    | ✅       |
| `client_streaming`            | ✅      | ✅    | ✅  | ✅    | ✅       |
| `custom_metadata`             | ✅      | ✅    | ✅  | ✅    | ✅       |
| `empty_stream`                | ✅      | ✅    | ✅  | ✅    | ✅       |
| `empty_unary`                 | ✅      | ✅    | ✅  | ✅    | ✅       |
| `large_unary`                 | ✅      | ✅    | ✅  | ✅    | ✅       |
| `orca_oob`                    | 🚫      | 🚫    | 🚫  | 🚫    | 🚫       |
| `orca_per_rpc`                | 🚫      | 🚫    | 🚫  | 🚫    | 🚫       |
| `ping_pong`                   | ✅      | ✅    | ✅  | ✅    | ✅       |
| `server_compressed_streaming` | ❔      | ✅    | ❔  | ❔    | ✅       |
| `server_compressed_unary`     | ❔      | ✅    | ❔  | ✅    | ✅       |
| `server_streaming`            | ✅      | ✅    | ✅  | ✅    | ✅       |
| `special_status_message`      | ✅      | ✅    | ✅  | ✅    | ✅       |
| `status_code_and_message`     | ✅      | ✅    | ✅  | ✅    | ✅       |
| `timeout_on_sleeping_server`  | ✅      | ✅    | ✅  | ❔    | ✅       |
| `unimplemented_method`        | ✅      | ✅    | ✅  | ✅    | ✅       |
| `unimplemented_service`       | ✅      | ✅    | ✅  | ✅    | ✅       |

### Unsupported tests

There are additional tests that are not supported by `grapesy`, but since these
are not supported by _any_ of the reference implementations that we tested, we
did not consider them any further. The full list is:

* [`cacheable_unary`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#cacheable_unary)
* [`channel_soak`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#channel_soak)
* [`compute_engine_channel_credentials`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptionsmd#compute_engine_channel_credentials)
* [`compute_engine_creds`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#compute_engine_creds)
* [`concurrent_large_unary`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#concurrent_large_unary)
* [`google_default_credentials`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#google_default_credentials)
* [`jwt_token_creds`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#jwt_token_creds)
* [`long_lived_channel`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#long_lived_channel)
* [`oauth2_auth_token`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#oauth2_auth_token)
* [`per_rpc_creds`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#per_rpc_creds)
* [`rpc_soak`](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#rpc_soak)

## Memory profiles

Screenshots of the "Area Chart" display of the
[`eventlog2html`](https://github.com/mpickering/eventlog2html) output for
selected RPC communication patterns.

### Many connections

Client opens many connections and does a single non-streaming RPC.

Client profile:

![Client many connections
profile](/assets/profiles/many-connections-100000-client.png)

Server profile:

![Server many connections
profile](/assets/profiles/many-connections-100000-server.png)

### Many calls

Client opens a single connection and does many RPCs.

Client profile:

![Client many connections
profile](/assets/profiles/many-calls-client.png)

Server profile:

![Server many connections
profile](/assets/profiles/many-calls-server.png)

### Many messages

Client opens a single connection and does a single RPC that sends many messages.

Client profile:

![Client many connections
profile](/assets/profiles/many-messages-client.png)

Server profile:

![Server many connections
profile](/assets/profiles/many-messages-server.png)

## Benchmarks

<table>
  <tr>
    <td><strong>Run type</strong></td>
    <td colspan=2 style="text-align: center"><strong>Linux (RPCs/s)</strong></td>
    <td colspan=2 style="text-align: center"><strong>OSX (RPCs/s)</strong></td>
  </tr>
  <tr>
    <td></td>
    <td><strong>Grapesy</strong></td>
    <td><strong>Java</strong></td>
    <td><strong>Grapesy</strong></td>
    <td><strong>Java</strong></td>
  </tr>
  <tr>
    <td>sequential protobuf</td>
    <td>4654.500</td>
    <td>7169.283</td>
    <td>2919.783</td>
    <td>1907.350</td>
  </tr>
  <tr>
    <td>concurrent protobuf</td>
    <td>3326.283</td>
    <td>10908.650</td>
    <td>5032.950</td>
    <td>4571.750</td>
  </tr>
  <tr>
    <td>sequential json</td>
    <td>2371.233</td>
    <td>2427.900</td>
    <td>909.417</td>
    <td>1574.817</td>
  </tr>
  <tr>
    <td>concurrent json</td>
    <td>3019.733</td>
    <td>6143.567</td>
    <td>1151.017</td>
    <td>3563.867</td>
  </tr>
</table>


[grpc:website]: https://grpc.io/
[grpc:interop]: https://github.com/grpc/grpc/tree/master/tools/interop_matrix
[grapesy:issue:72-orca]: https://github.com/well-typed/grapesy/issues/72


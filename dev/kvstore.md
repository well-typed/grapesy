# Key-value client/server example

This benchmark is described in

* <https://grpc.io/blog/optimizing-grpc-part-1/>
* <https://grpc.io/blog/grpc-with-json/>

## Ideal performance

Run the benchmark with

```bash
$ cabal run grapesy-kvstore
```

Ideal performance: all write operations take 50 ms, and all read operations take
10ms; since we do read operations 25% of the time, we expect an average of

0.75 * 50 + 0.25 * 10 = 40ms

per operation, for a theoretical maximum of 25 RPCs/sec (the Java reference does
24.2).

We should also measure the performance when we disable the simulated workload
(which would then measure the actual RPC overhead more directly):

```bash
$ cabal run grapesy-kvstore -- --no-work-simulation
```

The Java reference does roughly 4700 RPCs/sec.

## Profiling

### Time

To profile, add the following lines to `cabal.project.local`:

```
package grapesy
  profiling: true

package *
  profiling-detail: late
```

(Late cost centre profiling requires ghc 9.4 or higher; this also requires a
recent version of `cabal`.)

Run with

```bash
$ cabal run grapesy-kvstore -- --no-work-simulation +RTS -pj
```

Then load the resulting `grapesy-kvstore.prof` into Speedscope (and select
the "Left Heavy" view). Alternatively, run

```bash
$ cabal run grapesy-kvstore -- --no-work-simulation +RTS -p
```

to generate a human-readable profile instead.

### Memory

For memory profiling, run with

```bash
$ cabal run grapesy-kvstore -- --no-work-simulation +RTS -hy -l-agu
```

and then process the eventlog with

```bash
$ eventlog2html grapesy-kvstore.eventlog
```

and open `grapesy-kvstore.eventlog.html` in your browser.

### Wireshark

To monitor the traffic in Wireshark, set a capture filter for

```
port 50051
```

and then capture the loopback device.

### Eventlog

The KVStore implementation issues some eventlog events; run with

```bash
$ cabal run grapesy-kvstore -- --no-work-simulation +RTS -l
```

then inspect the eventlog, for example with Threadscope

```bash
$ threadscope ./grapesy-kvstore.eventlog
```

or with

```bash
$ ghc-events show grapesy-kvstore.eventlog
```

A useful tool for inspecting the eventlog is
[ghc-events-util](https://github.com/edsko/ghc-events-util), which can be used
to filter events and show the time intervals between events.

### Adding foreign calls to the eventlog

To trace the cost of foreign calls, it can be useful to use the
[trace-foreign-calls](https://github.com/well-typed/trace-foreign-calls) plugin.
See the corresponding `README.md` for detailed instructions, but briefly: first
install the plugin into a fresh `cabal` store:

```bash
$ trace-foreign-calls$ cabal --store-dir=/tmp/cabal-plugin-store-grapesy install --lib trace-foreign-calls
```

Then run with

```bash
grapesy$ cabal run --project-file cabal.project.plugin \
  grapesy-kvstore -- --no-work-simulation +RTS -l
```

### `perf` (CPU cycles/on CPU)

In order to figure out what is happening during the time that the Haskell
profile records as `SYSTEM`, a C-level profile may be useful. Add these lines
to `cabal.project.local` to compile with DWARF information (see
https://www.well-typed.com/blog/2020/04/dwarf-1/):

```
package *
  debug-info: 3
```

Then create a profile with

```bash
$ perf record $(find . -name grapesy-kvstore -type f) --no-work-simulation
```

You can then inspect the profile with

```bash
$ perf report
```

See https://www.well-typed.com/blog/2020/04/dwarf-4/ for additional info.

Note 1: You may need to lower paranoia settings for `perf`

```bash
$ sudo sysctl kernel.kptr_restrict=0
$ sudo sysctl kernel.perf_event_paranoid=-1
```

Note 2: to figure out what is included under SYSTEM, can grep the ghc
codebase for `CC_SYSTEM` / `CCS_SYSTEM`.

### `strace`

To debug performance with strace, it is useful to enable the `strace` cabal
flag:

```
package grapesy
  flags: +strace
```

This causes user eventlog events (emitted by `traceEventIO`) to also be written
to `/dev/null`, causing them to show up in an `strace` log. Run with

```bash
grapesy$ strace -f -o strace.log -r \
  $(find . -name grapesy-kvstore -type f) --no-work-simulation
```

The `-f` parameter (`--follow-forks`) is needed to see the events from both the
server and the client; alternatively, you could use `-ff` (shorthand for
`--follow-forks --output-separately`) to write them to separate files.

Another useful `strace` parameter is `-e`, for filtering events, for example:

```
grapesy$ strace -f -o strace.log -r -e write \
  $(find . -name grapesy-kvstore -type f) --no-work-simulation
```

This will show blocks such as

```
991445      0.000533 write(15, "CLIENT start CREATE\n", 20) = 20
991449      0.004599 write(15, "HANDLER start CREATE\n", 21) = 21
991449      0.000043 write(15, "HANDLER stop  CREATE\n", 21) = 21
991445      0.041041 write(15, "CLIENT stop CREATE\n", 19) = 19
```

where we can see that the communication from the client to the handler took
46ms, and communication back to the client another 41ms.
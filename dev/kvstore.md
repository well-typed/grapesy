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
$ cabal run grapesy-kvstore -- --disable-work-simulation
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
$ cabal run grapesy-kvstore -- +RTS -pj
```

Then load the resulting `grapesy-kvstore.prof` into Speedscope (and select
the "Left Heavy" view). Alternatively, run

```bash
$ cabal run grapesy-kvstore -- +RTS -p
```

to generate a human-readable profile instead.

### Memory

For memory profiling, run with

```bash
$ cabal run grapesy-kvstore -- +RTS -hy -l-agu
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

### `perf` (CPU cycles/on CPU)

In order to figure out what is happening during the time that the Haskell
profile records as `SYSTEM`, a C-level profile may be useful. Add these lines
to `cabal.project.local` to compile with DWARF information (see
https://www.well-typed.com/blog/2020/04/dwarf-1/):

```
package *
  debug-info: 2
```

Then create a profile with

```bash
$ perf record $(find . -name grapesy-kvstore -type f)
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

### `perf` (wallclock/off CPU)

The above @record@ command records CPU cycles; to record wallclock instead
(useful for debugging blocking behaviour), you can use

```bash
$ perf sched record (find . -name grapesy-kvstore -type f)
```

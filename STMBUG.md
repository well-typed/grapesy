This version of the branch demonstrates the STM bug.

To reproduce, remove the workaround from `Network.GRPC.Util.Concurrency` by defining

```haskell
atomically :: forall a. STM a -> IO a
atomically = STM.atomically
```

and then run with

```
while (cabal run test-grapesy -- -p earlyTermination3 2>log); do date; done
```

You should hit the "blocked indefinitely" very quickly (usually already on the
first run). 

Critical pieces of code:

* The "blocked indefinitely" exception is raised by running a call to `recv`
  (src/Network/GRPC/Util/Session/Channel.hs). 

* This first gets the "thread interface" (call to `getThreadInterface`) which,
  when this is first called, will return an `TMVar` containing `FlowStateRegular`
  of another `TMVar`. It is the call to `takeTMVar` on that second `TMVar` that
  is blocking.

* It is true that the thread that is meant to write to that `TMVar` dies in the
  test case. That thread is spawned in src/Network/GRPC/Util/Session/Client.hs,
  specifically in `forkRequest`.

* However, the thread body is wrapped with `threadBody`
  (src/Network/GRPC/Util/Thread.hs), and then it dies, the final call to
  `writeTVar` in `threadBody` will write the new status to the `TVar`
  containing that thread status.

* I have confirmed that this write actually happens, _but_ the call to `recv`
  (which as one of the first things that it does is _read_ that thread status)
  is not retried. _If_ it would be retried (which is basically what my awful
  `atomically` hack does), then it would notice that the thread status has
  indeed changed and it will throw an appropriate exception ("connection
  closed") rather than that "blocked indefinitely" exception.

Related reports:

* https://gitlab.haskell.org/ghc/ghc/-/issues/9401 /
  https://github.com/simonmar/async/issues/14
  Different to our case: this is about _two_ threads both being deadlocked, and
  an attempt to resolve the deadlock by catching `BlockedIndefinitelyOnSTM`.
  This is not the case for us (indeed, one of the threads involved runs to
  completion).

  Another example: https://gitlab.haskell.org/ghc/ghc/-/issues/10241

  https://gitlab.haskell.org/ghc/ghc/-/issues/10793 is similar: two
  threads that are both deadlocked.

* https://gitlab.haskell.org/ghc/ghc/-/issues/11001
  This is to do with the other reference to the `T(M)Var` being from a
  finalizer. Also not the case in our example.

  

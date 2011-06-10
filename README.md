Rx for Haskell
==============

Attempt to implement the Reactive Extensions API in Haskell. Just for
the sake of it.

Status
------

- Observable, Observer and Disposable defined, working in the IO Monad
- "PushCollection" is a simple implementation of Observable
- In "read rx", the Observer has three methods (OnNext, OnEnd, OnError). My version has just one (OnNext)
- No combinators yet

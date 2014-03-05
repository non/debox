## Debox

### Overview

Debox provides specialized mutable collections that don't box.

For performance reasons, Debox's types are not compatible with Scala's
collections framework (although conversions are possible). You may find that
Debox's structures provide more reliable performance than Scala's mutable
collections.

Debox is available for Scala 2.10, and depends on Spire.

### Set up

*Debox is not yet published, so these instructions do not yet apply.*

If you have a Scala 2.10 which uses SBT, add the following to `build.sbt`:

```
resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

libraryDependencies += "org.spire-math" %% "debox" % "0.3.0"
```

### Debox Types

#### Buffer

`debox.Buffer` is an indexed data structure backed by an array which
can be appended to. It corresponds to `collection.mutable.ArrayBuffer`
but has better performance due to specialization. It can also wrap
instances of `Array` to provide specialized versions of foreach and
map, which are a bit faster.

Buffers can grow internally. Appending, such as `+=` and `++=`, and
removing and from the end of the buffer, as `pop` does, will be fast
operations. Other operations (like adding to the middle of the buffer
with `insert`) may require internal copying.

Example usage:

```scala
import debox.Buffer

val buf = Buffer.empty[Int]
buf += 1
buf += 1
buf += 2
buf.foreach { n => println(n) } // prints 1, 1, 2

val child = buf.copy
child += 3
child(0) = 999
child.toString // Buffer(999, 1, 2, 3)

val buf ++= child
buf.pop
buf.sum  // uses spire
```

#### Set

`debox.set.Set` corresponds to `collection.mutable.Set` (there is not
currently an immutable version). It uses implicit `debox.Hash` instances to
determine how to hash items, and uses `debox.Buckets` to store the members.
The hashing is done via an open addressing scheme which is similar to the one
used by Python.

#### Map

`debox.map.Map` corresponds to `collection.mutable.Map` (there is not
currently an immutable version). Like `debox.set.Set`, it uses `debox.Hash` to
determine how to hash key values, which it stores in `debox.Buckets` (values
are stored directly in an `Array`). It uses the same hashing strategy as
`debox.set.Set`.

### Benchmarks

Most of Debox has been developed in tandem with aggressive benchmarking using
Caliper and other tools.

The benchmarks can be run from SBT:

    erik@hex ~/w/debox $ sbt
    [info] Loading project definition from /home/erik/w/debox/project
    [info] Set current project to debox (in build file:/home/erik/w/debox/)
    [info] Set current project to debox (in build file:/home/erik/w/debox/)
    > project benchmark
    [info] Set current project to benchmark (in build file:/home/erik/w/debox/)
    > run

### Disclaimers and Provisos

Unlike many Java (and Scala?) projects, Debox is not interested in
hiding its internals beyond what is convenient. To aid inlining, most
internals are public. This does not mean that users should modify them
directly--attempting to manually update the structures could produce
non-deterministic effects.

### Disclaimers

Debox aims to achieve the best possible performance through use of features
like specialization, macros, arrays, etc. All other concerns (such as
visibility, subtyping relationships, type signatures, etc.) are secondary.

Debox chooses not to provide methods whose implementations are
guaranteed to be slow. Rather than trying to provide every possibly
useful method, the goal is to provide core functionality which can be
implemented efficiently and which plays to the data structure's
strengths.

It's possible that in the future Debox will use a system of imports to
optionally add "slow" methods which have been left off the current API.
Since Debox is not at a 1.0 release yet, the API may change from
version to version. Debox makes no source- or binary-compatibility
guarantees.

Criticisms, suggestions and patches are all welcome, as are
benchmarking results (especially surprsing ones)!

### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the COPYING
file.

Copyright Erik Osheim, 2012-2014.

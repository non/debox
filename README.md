## Debox

### Overview

Debox provides specialized mutable collections that don't box.

For performance reasons, Debox's types are not compatible with Scala's
collections framework (although conversions are possible). You may find
that Debox's structures provide more reliable performance than Scala's
mutable collections.

Debox is available for Scala 2.10 and 2.11.

### Set up

If you have a project using Scala 2.10 and SBT, add the following to
your `build.sbt` file:

```
resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

libraryDependencies += "org.spire-math" %% "debox" % "0.7.0"
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

Large buffers which have most of their elements removed will still
maintain a large underlying array. Use `compact` to reclaim unnecessary
memory in these situations.

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

`debox.Set` corresponds to `collection.mutable.Set`, but without
boxing. The hashing is done via an open addressing scheme with
re-hashing, which is derived from the strategy used by Python. See
*Hashing Strategy* for a more complete description.

Sets are required to maintain extra space to ensure fast average lookup
times. Sets will tend to use 33-66% of the underlying storage.

Large sets which have most of their elements removed will still
maintain a large underlying array. Use `compact` to reclaim unnecessary
memory in these situations.

Example usage:

```scala
import debox.Set

val set = Set.empty[Int]
set += 1
set += 1
set += 2
set(0) // false
set(1) // true

val child = buf.copy
child += 3
child += 999
child.size == 4 // true

val other = Set(2, 3, 4)
set &= other
set(1) // false
set(2) // true
set.size == 1 // true
```

#### Map

`debox.Map` corresponds to `collection.mutable.Map`, but without
boxing. As with `debox.Set` the hashing is done via an open addressing
scheme with re-hashing, which is derived from the strategy used by
Python. See *Hashing Strategy* for a more complete description.

Maps are required to maintain extra space to ensure fast average lookup
times. Maps will tend to use 33-66% of the underlying storage.

Large maps which have most of their elements removed will still
maintain a large underlying array. Use `compact` to reclaim unnecessary
memory in these situations.

Unlike Scala Maps (which store a key and value together as a `Tuple2`),
Debox stores keys and values in separate arrays. This makes iterating
over keys or values separately faster, but means that operations which
treat a map as a sequence of tuples are slower and/or not supported.

Example usage:

```scala
import debox.Map

val m = Map.empty[String, Int]
m("boris") = 1887
m("bela") = 1880
m("bela") = 1882
m.size // 2

m.contains("bela")   // true
m.contains("donald") // false
m.contains(12345)    // compile-time error!

m.get("bela") // Some(1882)
m.get("donald") // None

m("boris")  // 1887
m("bela")   // 1882
m("donald") // debox.KeyNotFoundException

m ++= Map("christopher" -> 1922, "vincent" -> 1911)
m.keysSet // Set(christopher, vincent, bela, boris), order is arbitrary

val b = m.mapValues(year => "born in %d" format year)
b // Map(boris -> born in 1887, bela -> born in 1882, ...)
```

### Hashing Strategy

The hashing used in `Set` and `Map` works as follows:

1. Get the item's hashcode (retrieved by the `##` operator) as `i`.
2. Mask this by the underlying array's max index to get `j`.
  1. If slot `j` is free, use it and return.
  2. Else, re-hash `i` and repeat.

The re-hashing strategy uses `perturbation` (initialized to the
original hashcode) as well as the current `i` value. The transition can
be expressed as:

```scala
i = (i << 2) + i + perturbation + 1
perturbation >>= 5
```

For a much more detailed treatment, you can read the comments on
CPython's `dictobject.c`
[here](http://hg.python.org/cpython/file/56c346e9ae4d/Objects/dictobject.c#l106).

### Benchmarks

Most of Debox has been developed in tandem with aggressive benchmarking
using Caliper and other tools.

The benchmarks can be run from SBT via `benchmark/run`.

### Disclaimers and Provisos

Debox aims to achieve the best possible performance through use of features
like specialization, macros, arrays, etc. All other concerns (such as
visibility, subtyping relationships, type signatures, etc.) are secondary.

Unlike many Java (and Scala?) projects, Debox is not interested in
hiding its internals beyond what is convenient. To aid inlining, most
internals are public. This does not mean that users should modify them
directly--attempting to manually update the structures could produce
non-deterministic effects.

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
benchmarking results (especially surprising ones)!

### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the COPYING
file.

Copyright Erik Osheim, 2012-2014.

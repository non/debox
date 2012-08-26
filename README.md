## Debox

### Overview

Debox is a Scala library providing simple collections which do not box.

It is not intended to replace Scala's built-in collections but to complement
them in cases where performance is more important than genericity. In
particular, these collection classes do not extend any class, nor any trait
that is not specialized. This means that they are not instances of
Traversable, Seq, or any other standard Scala collection interface.

Debox currently targets Scala 2.10 and SBT 0.12.0.

### Types

#### Buffer

`debox.buffer.Buffer` is an indexed data structure backed by an array which
can be appended to. It corresponds to `collection.mutable.ArrayBuffer`
but has better performance due to specialization. It can also wrap instances
of `Array` to provide specialized versions of foreach and map.

It comes in two flavors: `debox.buffer.Mutable` and `debox.buffer.Immutable`.
The former supports mutation via things like `update`, `insert`, and `append`
(and can automatically grow itself efficiently when needed) whereas the
immutable version is able to do structural sharing and supports lazily applied
methods like `reverse`, `map`, and so on.

#### Buckets

`debox.Buckets` is a low-level data structure representing an array which can
tell when particular elements are unset (information provided via implicit
`debox.Unset` instances). There are two flavors: `debox.MarkedBuckets` uses a
placeholder value (e.g. `null`) to mark an empty bucket, whereas
`debox.BitmaskBuckets` uses a separate bitmask array to track which buckets
are empty.

These classes are designed primarily to be used by other data structures, such
as `Set` and `Map`

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

#### Vector

`debox.vector.Vector` corresponds to `collection.immutable.Vector`. It uses a
similar strategy (immutable tree with a branching factor of 32) to try to
achieve "effectively" constant-time access (given a maximum of 2^31 items, the
maximum tree depth is 6).

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

### Future Work

Current shortcomings of Debox include:

  1. ad-hoc packages and names
  2. arbitrary-seeming APIs
  3. (necessary) code duplication
  4. lack of consistent mutable/immutable support
  5. missing useful methods
  6. low test coverage

Probably the most important thing to do is to come up with a principled
approach to naming these types (given that most of them clash with existing
Scala types) as well as a good rule of thumb for which methods to include (or
exclude) for each.

Code duplication can be (potentially) solved via good use of macros and
reification and type classes would be a useful way to create the appearance of
an inheritance hierarchy. Both of these approaches should be experiemented
with.

### Disclaimers

Debox aims to achieve the best possible performance through use of features
like specialization, macros, arrays, etc. All other concerns (such as
visibility, subtyping relationships, type signatures, etc.) are secondary.

The existing API is still quite experimental, and no guarantees about source
or binary compatibility should be assumed between versions.

Criticisms, suggestions and patches are all welcome, as are benchmarking
numbers (especially surprsing ones)! 

### Copyright and License

All code is available to you under the MIT license, available at
http://opensource.org/licenses/mit-license.php and also in the COPYING file.

Copyright Erik Osheim, 2012.

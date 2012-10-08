package debox.map

import debox._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => spec}

class InvalidSizes(k: Int, v: Int) extends Exception("%s, %s" format (k, v))
class MapOverflow(n: Int) extends Exception("size %s exceeds max" format n)
class NotFound(k: String) extends Exception("key %s was not found" format k)

object Map {
  /**
   * Create an empty Map.
   */
  def empty[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash,
    @spec(Int, Long, Double, AnyRef) B:ClassTag
  ] = new Map(new Array[A](8), new Array[B](8), new Array[Int](1), 0, 0)

  /**
   * Create a Map preallocated to a particular size.
   *
   * Note that the internal representation may allocate more space than
   * requested to satisfy the requirements of internal alignment. Map uses
   * arrays whose lengths are powers of two.
   */
  def ofDim[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash,
    @spec(Int, Long, Double, AnyRef) B:ClassTag
  ](n:Int) = {
    val sz = Util.nextPowerOfTwo(n)
    if (sz < 1) throw new MapOverflow(n)
    val m = (sz + 15) >> 4
    new Map(new Array[A](sz), new Array[B](sz), new Array[Int](m), 0, 0)
  }

  /**
   * Create an empty Map.
   */
  def apply[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash,
    @spec(Int, Long, Double, AnyRef) B:ClassTag
  ]() = empty[A, B]

  /**
   * Create a map from an array of keys and another array of values.
   */
  def apply[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash,
    @spec(Int, Long, Double, AnyRef) B:ClassTag
  ](ks:Array[A], vs:Array[B]) = {
    if (ks.length != vs.length) throw new InvalidSizes(ks.length, vs.length)
    val map = ofDim[A, B](ks.length)
    val limit = ks.length - 1
    @inline @tailrec def loop(i:Int) {
      map(ks(i)) = vs(i)
      if (i < limit) loop(i + 1)
    }
    loop(0)
    map
  }
}

final class Map[
  @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash,
  @spec(Int, Long, Double, AnyRef) B:ClassTag
] protected[debox] (
  ks:Array[A], vs:Array[B], bs:Array[Int], n:Int, u:Int
) extends Function1[A, B] {

  // set internals
  var keys:Array[A] = ks // keys track set/unset
  var vals:Array[B] = vs // values mirror keys
  var buckets:Array[Int] = bs // buckets track defined/used (2-bits per item)
  var len:Int = n // number of defined items in map
  var used:Int = u // number of buckets used in map

  def getBuckets: Array[Int] = buckets

  // hashing internals
  var mask = keys.length - 1 // size - 1, used for hashing
  var limit = (keys.length * 0.65).toInt // point at which we should resize

  final def length:Int = len

  final def update(key:A, value:B) {
    @inline @tailrec def loop(i:Int, perturbation:Int) {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 0) {
        keys(j) = key
        vals(j) = value
        Util.set(buckets, j)
        used += 1
        len += 1
        if (used > limit) resize()
      } else if (status == 2) {
        keys(j) = key
        vals(j) = value
        Util.set(buckets, j)
        len += 1
      } else if (keys(j) == key) {
        vals(j) = value
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def remove(key:A) {
    @inline @tailrec def loop(i:Int, perturbation:Int) {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 3 && keys(j) == key) {
        Util.unset(buckets, j)
        len -= 1
      } else if (status == 0) {
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def copy:Map[A, B] =
    new Map(keys.clone, vals.clone, buckets.clone, len, used)

  final def contains(key:A):Boolean = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Boolean = {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 0) {
        false
      } else if (status == 3 && keys(j) == key) {
        true
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def apply(key:A): B = {
    @inline @tailrec def loop(i:Int, perturbation:Int): B = {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 0) {
        throw new NotFound(key.toString)
      } else if (status == 3 && keys(j) == key) {
        vals(j)
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def get(key:A):Option[B] = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Option[B] = {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 0) {
        None
      } else if (status == 3 && keys(j) == key) {
        Some(vals(j))
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def foreach(f: (A, B) => Unit) {
    @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
      if (((b >> shift) & 3) == 3) {
        f(keys(i), vals(i))
        if (shift < 30) inner(i + 1, b, shift + 2, count + 1) else count + 1
      } else {
        if (shift < 30) inner(i + 1, b, shift + 2, count) else count
      }
    }

    @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int) {
      if (count < len) outer(i + 16, k + 1, inner(i, buckets(k), 0, count), len)
    }
    outer(0, 0, 0, len)
  }

  final def hash(key:A, _mask:Int, _keys:Array[A], _buckets:Array[Int]):Int = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Int = {
      val j = i & _mask
      if (Util.status(_buckets, j) == 3 && _keys(j) != key) {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      } else {
        j
      }
    }
    val i = Hash[A].hash(key) & 0x7fffffff
    loop(i, i)
  }

  final def resize(): Unit2[A, B] = {
    val size = keys.length
    val factor = if (size < 10000) 4 else 2
    
    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextkeys = new Array[A](nextsize)
    val nextvals = new Array[B](nextsize)
    val nextbs = new Array[Int]((nextsize + 15) >> 4)

    @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
      if (((b >> shift) & 3) == 3) {
        val key = keys(i)
        val j = hash(key, nextmask, nextkeys, nextbs)
        nextkeys(j) = key
        nextvals(j) = vals(i)
        Util.set(nextbs, j)
        if (shift < 30) inner(i + 1, b, shift + 2, count + 1) else count + 1
      } else {
        if (shift < 30) inner(i + 1, b, shift + 2, count) else count
      }
    }

    @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int) {
      if (count < len) outer(i + 16, k + 1, inner(i, buckets(k), 0, count), len)
    }
    outer(0, 0, 0, len)

    keys = nextkeys
    vals = nextvals
    buckets = nextbs

    mask = nextmask
    limit *= factor

    new Unit2[A, B]
  }
}

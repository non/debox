package debox

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.syntax.cfor._

class MapOverflow(n: Int) extends Exception("size %s exceeds max" format n)
class NotFound(k: String) extends Exception("key %s was not found" format k)

object Map {

  /**
   * Create an empty Map.
   */
  def empty[@sp(Int, Long, Double, AnyRef) A: ClassTag, @sp(Int, Long, Double, AnyRef) B: ClassTag]: Map[A, B] =
    new Map(new Array[A](8), new Array[B](8), new Array[Byte](8), 0, 0)

  /**
   * Create a Map preallocated to a particular size.
   *
   * Note that the internal representation may allocate more space than
   * requested to satisfy the requirements of internal alignment. Map uses
   * arrays whose lengths are powers of two.
   */
  def ofSize[@sp(Int, Long, Double, AnyRef) A: ClassTag, @sp(Int, Long, Double, AnyRef) B: ClassTag](n: Int): Map[A, B] = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw new MapOverflow(n)
      case 0 => 8
      case n => n
    }
    new Map(new Array[A](sz), new Array[B](sz), new Array[Byte](sz), 0, 0)
  }

  def apply[@sp(Int, Long, Double, AnyRef) A: ClassTag, @sp(Int, Long, Double, AnyRef) B: ClassTag](pairs: (A, B)*): Map[A, B] = {
    val map = empty[A, B]
    pairs.foreach { case (a, b) => map(a) = b }
    map
  }

  /**
   * Create a map from an array of keys and another array of values.
   */
  def fromArrays[@sp(Int, Long, Double, AnyRef) A: ClassTag, @sp(Int, Long, Double, AnyRef) B: ClassTag](ks: Array[A], vs: Array[B]): Map[A, B] = {
    if (ks.length != vs.length) throw new InvalidSizes(ks.length, vs.length)
    val map = ofSize[A, B](ks.length)
    val limit = ks.length - 1
    @inline @tailrec def loop(i: Int) {
      map(ks(i)) = vs(i)
      if (i < limit) loop(i + 1)
    }
    loop(0)
    map
  }
}

final class Map[@sp(Int, Long, Double, AnyRef) A: ClassTag, @sp(Int, Long, Double, AnyRef) B: ClassTag] protected[debox] (ks: Array[A], vs: Array[B], bs: Array[Byte], n: Int, u: Int) {

  // set internals
  var keys: Array[A] = ks       // slots for keys
  var vals: Array[B] = vs       // slots for values
  var buckets: Array[Byte] = bs // buckets track defined/used slots
  var len: Int = n              // number of defined slots
  var used: Int = u             // number of used slots (used >= len)

  // hashing internals
  var mask = keys.length - 1             // size - 1, used for hashing
  var limit = (keys.length * 0.65).toInt // point at which we should grow

  final def size: Int = len

  final def update(key: A, value: B): Unit = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Unit = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        keys(j) = key
        vals(j) = value
        buckets(j) = 3
        len += 1
        used += 1
        if (used > limit) grow()
      } else if (status == 2 && !contains(key)) {
        keys(j) = key
        vals(j) = value
        buckets(j) = 3
        len += 1
      } else if (keys(j) == key) {
        vals(j) = value
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  final def remove(key: A) {
    @inline @tailrec def loop(i: Int, perturbation: Int) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        buckets(j) = 2
        len -= 1
      } else if (status == 0) {
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  final def copy: Map[A, B] = new Map(keys.clone, vals.clone, buckets.clone, len, used)

  private[this] def absorb(that: Map[A, B]): Unit = {
    keys = that.keys
    vals = that.vals
    buckets = that.buckets
    len = that.len
    used = that.used
    mask = that.mask
    limit = that.limit
  }

  final def contains(key: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) false
      else if (status == 3 && keys(j) == key) true
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  final def apply(key: A): B = {
    @inline @tailrec def loop(i: Int, perturbation: Int): B = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) throw new NotFound(key.toString)
      else if (status == 3 && keys(j) == key) vals(j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  final def get(key: A): Option[B] = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Option[B] = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) None
      else if (status == 3 && keys(j) == key) Some(vals(j))
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  final def keysSet: Set[A] = new Set(keys.clone, buckets.clone, len, used)

  final def valuesArray: Array[B] = {
    val bs = new Array[B](len)
    var j = 0
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) { bs(j) = vals(i); j += 1 }
    }
    bs
  }

  final def foreach(f: (A, B) => Unit): Unit = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) f(keys(i), vals(i))
    }
  }

  final def foreachKey(f: A => Unit): Unit = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) f(keys(i))
    }
  }

  final def foreachValue(f: B => Unit): Unit = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) f(vals(i))
    }
  }

  final def grow(): Unit2[A, B] = {
    val next = keys.length * (if (keys.length < 10000) 4 else 2)
    val map = Map.ofSize[A, B](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) map(keys(i)) = vals(i)
    }
    absorb(map)
    new Unit2[A, B]
  }
}

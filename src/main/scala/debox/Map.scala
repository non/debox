package debox

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra.Monoid
import spire.syntax.cfor._
import spire.syntax.monoid._

class MapOverflow(n: Int) extends Exception("size %s exceeds max" format n)
class NotFound(k: String) extends Exception("key %s was not found" format k)

object Map {

  /**
   * Create an empty Map.
   * 
   * Map.empty[Int, String]
   */
  def empty[@sp(Int, Long, AnyRef) A: ClassTag, @sp(Boolean, Int, Long, Double, AnyRef) B: ClassTag]: Map[A, B] =
    new Map(new Array[A](8), new Array[B](8), new Array[Byte](8), 0, 0)

  /**
   * Create a Map preallocated to a particular size.
   *
   * Note that the internal representation may allocate more space than
   * requested to satisfy the requirements of internal alignment. Map uses
   * arrays whose lengths are powers of two.
   * 
   * Map.ofSize[Int, String](100)
   */
  def ofSize[@sp(Int, Long, AnyRef) A: ClassTag, @sp(Boolean, Int, Long, Double, AnyRef) B: ClassTag](n: Int): Map[A, B] = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw new MapOverflow(n)
      case 0 => 8
      case n => n
    }
    new Map(new Array[A](sz), new Array[B](sz), new Array[Byte](sz), 0, 0)
  }

  /**
   * Create a new literal map.
   * 
   * Map(1 -> "cat", 2 -> "dog", 3 -> "fish")
   */
  def apply[@sp(Int, Long, AnyRef) A: ClassTag, @sp(Boolean, Int, Long, Double, AnyRef) B: ClassTag](pairs: (A, B)*): Map[A, B] =
    fromIterable(pairs)

  /**
   * Create a map from an array of keys and another array of values.
   * 
   * Map(Array(1,2,3), Array("cat", "dog", "fish"))
   */
  def fromArrays[@sp(Int, Long, AnyRef) A: ClassTag, @sp(Boolean, Int, Long, Double, AnyRef) B: ClassTag](ks: Array[A], vs: Array[B]): Map[A, B] = {
    if (ks.length != vs.length) throw new InvalidSizes(ks.length, vs.length)
    val map = ofSize[A, B](ks.length)
    cfor(0)(_ < ks.length, _ + 1) { i => map(ks(i)) = vs(i) }
    map
  }

  /**
   * Create a map from an iterable of tuples.
   * 
   * Map(List((1, "cat"), (2, "dog"), (3, "fish")))
   */
  def fromIterable[@sp(Int, Long, AnyRef) A: ClassTag, @sp(Boolean, Int, Long, Double, AnyRef) B: ClassTag](pairs: Iterable[(A, B)]): Map[A, B] = {
    val result = empty[A, B]
    // work around compiler bug with foreach here
    val it = pairs.iterator
    while (it.hasNext) { result += it.next }
    result
  }
}

final class Map[@sp(Int, Long, AnyRef) A, @sp(Boolean, Int, Long, Double, AnyRef) B] protected[debox] (ks: Array[A], vs: Array[B], bs: Array[Byte], n: Int, u: Int)(implicit val cta: ClassTag[A], val ctb: ClassTag[B]) {

  // set internals
  var keys: Array[A] = ks       // slots for keys
  var vals: Array[B] = vs       // slots for values
  var buckets: Array[Byte] = bs // buckets track defined/used slots
  var len: Int = n              // number of defined slots
  var used: Int = u             // number of used slots (used >= len)

  // hashing internals
  var mask = keys.length - 1             // size - 1, used for hashing
  var limit = (keys.length * 0.65).toInt // point at which we should grow

  override def equals(that: Any): Boolean = that match {
    case that: Map[_, _] =>
      size == that.size &&
      cta == that.cta && ctb == that.ctb &&
      forall(that.asInstanceOf[Map[A, B]].containsItem)
    case _ =>
      false
  }

  override def hashCode: Int = fold(0)(_ ^ _.## ^ _.##)

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Map(")
    var first = true
    foreach { (k, v) =>
      if (first) first = false else sb.append(", ")
      sb.append(k.toString)
      sb.append(" -> ")
      sb.append(v.toString)
    }
    sb.append(")")
    sb.toString
  }

  final def size: Int = len

  final def +=(kv: (A, B)): Unit = update(kv._1, kv._2)

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

  final def clear: Unit = absorb(Map.empty[A, B])

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

  final def containsItem(key: A, value: B): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) false
      else if (status == 3 && keys(j) == key) vals(j) == value
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

  final def getOrElse(key: A, fallback: B): B = {
    @inline @tailrec def loop(i: Int, perturbation: Int): B = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) fallback
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

  final def ++=(that: Map[A, B])(implicit ev: Monoid[B]): Unit =
    if (this.size < that.size) {
      transformValues((k, v) => v |+| that.getOrElse(k, ev.id))
    } else {
      that.foreach { (k, v) =>
        this(k) = this.getOrElse(k, ev.id) |+| v
      }
    }

  final def ++=(that: Iterable[(A, B)])(implicit ev: Monoid[B]): Unit =
    that.foreach { case (k, v) =>
      update(k, getOrElse(k, ev.id) |+| v)
    }

  final def --=(that: Set[A]): Unit =
    if (this.size < that.size) {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && that(keys(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    } else {
      that.foreach(a => this.remove(a))
    }

  final def --=(that: Iterable[A]): Unit =
    that.foreach(a => this.remove(a))

  final def filterInPlace(that: Set[A]): Unit =
    if (this.size < that.size) {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && !that(keys(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    } else {
      val m = Map.empty[A, B]
      that.foreach(k => m(k) = this(k))
      absorb(m)
    }

  final def keysSet: Set[A] = new Set(keys.clone, buckets.clone, len, used)

  final def valuesArray: Array[B] = {
    val result = new Array[B](len)
    var j = 0
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) { result(j) = vals(i); j += 1 }
    }
    result
  }

  final def mapItems[@sp (Int, Long, AnyRef) C: ClassTag, @sp (Boolean, Int, Long, Double, AnyRef) D: ClassTag](f: (A, B) => (C, D)): Map[C, D] = {
    val result = Map.empty[C, D]
    foreach((a, b) => result += f(a, b))
    result
  }

  final def mapKeys[@sp (Int, Long, AnyRef) C: ClassTag](f: A => C): Map[C, B] = {
    val result = Map.empty[C, B]
    foreach((a, b) => result(f(a)) = b)
    result
  }

  final def mapValues[@sp (Boolean, Int, Long, Double, AnyRef) C: ClassTag](f: B => C): Map[A, C] =
    new Map[A, C](keys.clone, vals.map(f), buckets.clone, len, used)

  final def mapToArray[@sp (Int, Long, Double) C: ClassTag](f: (A, B) => C): Array[C] = {
    val result = new Array[C](len)
    var j = 0
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        result(j) = f(keys(i), vals(i))
        j += 1
      }
    }
    result
  }

  final def flatMap[@sp (Int, Long, AnyRef) C: ClassTag, @sp (Boolean, Int, Long, Double, AnyRef) D: ClassTag](f: (A, B) => Map[C, D]): Map[C, D] = {
    val result = Map.empty[C, D]
    foreach((k, v) => f(k, v).foreach((c, d) => result(c) = d))
    result
  }

  final def foreach(f: (A, B) => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i => if (buckets(i) == 3) f(keys(i), vals(i)) }

  final def foreachKey(f: A => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i => if (buckets(i) == 3) f(keys(i)) }

  final def foreachValue(f: B => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i => if (buckets(i) == 3) f(vals(i)) }

  final def transformValues(f: (A, B) => B): Unit = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) vals(i) = f(keys(i), vals(i))
    }
  }

  def fold[@sp(Int, Long, Double, AnyRef) C](init: C)(f: (C, A, B) => C): C = {
    var result = init
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) result = f(result, keys(i), vals(i))
    }
    result
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

  def forall(p: (A, B) => Boolean) =
    loopWhile(p) == -1

  def exists(p: (A, B) => Boolean) =
    loopUntil(p) != -1

  def find(p: (A, B) => Boolean): Option[(A, B)] =
    loopUntil(p) match {
      case i if i >= 0 => Some((keys(i), vals(i)))
      case _ => None
    }

  def findAll(p: (A, B) => Boolean): Map[A, B] = {
    val out = Map.empty[A, B]
    foreach((a, b) => if (p(a, b)) out(a) = b)
    out
  }

  def loopWhile(p: (A, B) => Boolean): Int = {
    @inline @tailrec def loop(i: Int, limit: Int): Int = {
      if (buckets(i) == 3 && !p(keys(i), vals(i))) i
      else if (i < limit) loop(i + 1, limit)
      else -1
    }
    loop(0, buckets.length - 1)
  }

  def loopUntil(p: (A, B) => Boolean): Int = {
    @inline @tailrec def loop(i: Int, limit: Int): Int = {
      if (buckets(i) == 3 && p(keys(i), vals(i))) i
      else if (i < limit) loop(i + 1, limit)
      else -1
    }
    loop(0, buckets.length - 1)
  }

}

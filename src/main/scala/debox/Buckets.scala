package debox

import scala.{specialized => spec}

/**
 * Buckets is designed to be a (relatively) low-level of using arrays while
 * also tracking whether a given element is "unset" or not. This is easy when
 * you are willing to use a value (e.g. null, 0, Double.NaN, Int.MinValue) as
 * a "marker" and hard(er) otherwise.
 *
 * In the case where we have a marker value, we just allocate a value array and
 * use that particular value to track whether an item is set or not.
 *
 * In the case where we have no marker, we also allocate an array of integers
 * where each bit of each integer corresponds to an item in the value array.
 *
 * There are some implementation details. Like many classes in this projects,
 * we avoid access controls (for now) to avoid interfering with specialization.
 * We also need to make sure mention the specialized type parameter (usually A)
 * in every method prototype, to "trick" specialization into working.
 */

object Buckets {
  final def empty[@spec A:Unset:Manifest]: Buckets[A] = Buckets.ofDim[A](8)

  final def ofDim[@spec A:Unset:Manifest](n:Int): Buckets[A] = Unset[A] match {
    case MarkedUnset(mark) => new MarkedBuckets(Array.fill(n)(mark), mark)
    case NoUnset => new BitmaskBuckets(Array.ofDim[A](n), Array.ofDim[Int]((n + 31) >> 5))
  }
}

sealed trait Buckets[@spec A] {
  protected[this] def arr: Array[A]

  def length: Int

  def apply(i:Int): A

  def set(i:Int, a:A): Unit
  def unset(i:Int): A
  def isSet(i:Int, a:A): Boolean
  def isUnset(i:Int, a:A): Boolean

  def hasItemAt(i:Int, a:A): Boolean

  def copy: Buckets[A]

  //def hash(item:A, m:Int): Int

  override def toString = {
    val len = length
    val ss = Array.ofDim[String](len)
    var i = 0
    while (i < len) {
      val a = arr(i)
      if (isSet(i, a)) ss(i) = a.toString
      else ss(i) = "<>"
      i += 1
    }
    ss.mkString ("Buckets(", ", ", ")")
  }

  def foreach(f:A => Unit): Unit
  //final def foreach(f:A => Unit) {
  //  val as = arr
  //  var i = 0
  //  val len = length
  //  while (i < len) {
  //    val a = as(i)
  //    if (isSet(i, a)) f(a)
  //    i += 1
  //  }
  //}

  final def map[@spec B:Manifest:Unset](f:A => B): Buckets[B] = {
    val buckets = Buckets.empty[B]
    val as = arr
    var i = 0
    val len = length
    while (i < len) {
      val a = as(i)
      if (isSet(i, a)) buckets.set(i, f(a))
      i += 1
    }
    buckets
  }
}

final class MarkedBuckets[@spec A](as:Array[A], mark:A) extends Buckets[A] {
  final val nul = mark

  final def arr = as
  @inline final def length: Int = as.length

  @inline final def apply(i:Int): A = as(i)
  final def set(i:Int, a:A): Unit = as(i) = a
  final def unset(i:Int): A = { val a = as(i); as(i) = nul; nul }
  @inline final def isSet(i:Int, a:A): Boolean = a != nul
  @inline final def isUnset(i:Int, a:A): Boolean = a == nul
  final def hasItemAt(i:Int, a:A): Boolean = a == as(i)

  final def copy = new MarkedBuckets(as.clone, mark)

  final def foreach(f:A => Unit) {
    val as = arr
    var i = 0
    val len = length
    while (i < len) {
      val a = as(i)
      if (a != nul) f(a)
      i += 1
    }
  }
}

final class BitmaskBuckets[@spec A](as:Array[A], mask:Array[Int]) extends Buckets[A] {
  final val nul = null.asInstanceOf[A] // this breaks if specialization is off

  final def arr = as
  @inline final def length: Int = as.length

  @inline final def apply(i:Int): A = as(i)
  final def set(i:Int, a:A): Unit = { as(i) = a; mask(i >> 5) |= (1 << (i & 31)) }
  final def unset(i:Int): A = { mask(i >> 5) &= ~(1 << (i & 31)); nul }
  @inline final def isSet(i:Int, a:A): Boolean = (mask(i >> 5) & (1 << (i & 31))) != 0
  @inline final def isUnset(i:Int, a:A): Boolean = (mask(i >> 5) & (1 << (i & 31))) == 0
  final def hasItemAt(i:Int, a:A): Boolean = a == as(i) && isSet(i, a)

  final def copy = new BitmaskBuckets(as.clone, mask.clone)

  final def foreach(f:Function[A, Unit]): Unit = {
    var i = 0
    var j = 0
    var count = 0
    val limit = mask.length - 1
    val _len = length

    while (count < _len && j < limit) {
      val b = mask(j)

      if ((b & 0x01) != 0) { f(as(i)); count += 1 }
      if ((b & 0x02) != 0) { f(as(i + 1)); count += 1 }
      if ((b & 0x04) != 0) { f(as(i + 2)); count += 1 }
      if ((b & 0x08) != 0) { f(as(i + 3)); count += 1 }
      if ((b & 0x10) != 0) { f(as(i + 4)); count += 1 }
      if ((b & 0x20) != 0) { f(as(i + 5)); count += 1 }
      if ((b & 0x40) != 0) { f(as(i + 6)); count += 1 }
      if ((b & 0x80) != 0) { f(as(i + 7)); count += 1 }

      if ((b & 0x0100) != 0) { f(as(i + 8)); count += 1 }
      if ((b & 0x0200) != 0) { f(as(i + 9)); count += 1 }
      if ((b & 0x0400) != 0) { f(as(i + 10)); count += 1 }
      if ((b & 0x0800) != 0) { f(as(i + 11)); count += 1 }
      if ((b & 0x1000) != 0) { f(as(i + 12)); count += 1 }
      if ((b & 0x2000) != 0) { f(as(i + 13)); count += 1 }
      if ((b & 0x4000) != 0) { f(as(i + 14)); count += 1 }
      if ((b & 0x8000) != 0) { f(as(i + 15)); count += 1 }

      if ((b & 0x010000) != 0) { f(as(i + 16)); count += 1 }
      if ((b & 0x020000) != 0) { f(as(i + 17)); count += 1 }
      if ((b & 0x040000) != 0) { f(as(i + 18)); count += 1 }
      if ((b & 0x080000) != 0) { f(as(i + 19)); count += 1 }
      if ((b & 0x100000) != 0) { f(as(i + 20)); count += 1 }
      if ((b & 0x200000) != 0) { f(as(i + 21)); count += 1 }
      if ((b & 0x400000) != 0) { f(as(i + 22)); count += 1 }
      if ((b & 0x800000) != 0) { f(as(i + 23)); count += 1 }

      if ((b & 0x01000000) != 0) { f(as(i + 24)); count += 1 }
      if ((b & 0x02000000) != 0) { f(as(i + 25)); count += 1 }
      if ((b & 0x04000000) != 0) { f(as(i + 26)); count += 1 }
      if ((b & 0x08000000) != 0) { f(as(i + 27)); count += 1 }
      if ((b & 0x10000000) != 0) { f(as(i + 28)); count += 1 }
      if ((b & 0x20000000) != 0) { f(as(i + 29)); count += 1 }
      if ((b & 0x40000000) != 0) { f(as(i + 30)); count += 1 }
      if ((b & 0x80000000) != 0) { f(as(i + 31)); count += 1 }

      i += 32
      j += 1
    }

    while (count < _len && i < limit) {
      val a = as(i)
      if (isSet(i, a)) { f(a); count += 1 }
      i += 1
    }
  }
}

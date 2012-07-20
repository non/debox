package debox

import scala.{specialized => spec}

/**
 * Buckets are designed to be a (relatively) low-level array-like data
 * structure while also tracking whether a given element is "unset" or not.
 * This is easy when one is willing to use a value (e.g. null, 0, Double.NaN,
 * Int.MinValue) as a "marker" and slightly harder otherwise.
 *
 * In the case where we have a marker value, we just allocate an array and use
 * that particular value to track whether a cell is set or not.
 *
 * In the case where we have no marker, we also allocate an array of integers
 * where each bit of each integer corresponds to an item in the value array
 * (0 for unset, 1 for set).
 *
 * There are some implementation details. Like many classes in this project,
 * we avoid access controls (for now) to avoid interfering with specialization.
 * We also need to make sure to mention the specialized type parameter in every
 * method prototype, to "trick" specialization into working.
 *
 * End users should probably prefer a class based on Buckets.
 */
sealed trait Buckets[@spec A] {

  /**
   * The total number of buckets (both set and unset).
   */
  def length: Int

  /**
   * Access a particular bucket. If the bucket is unset then the return value
   * is not defined.
   */
  def apply(i:Int): A

  /**
   * Mark bucket 'i' as holding the value 'a'.
   */
  def set(i:Int, a:A): Unit

  /**
   * Mark bucket 'i' as being unset (holding no value).
   */
  def unset(i:Int): A

  /**
   * Test if bucket 'i' is set.
   *
   * The parameter 'a' should be set to the value of bucket 'i', although in
   * some cases it won't be used. This parameter is necessary to work around
   * specialization bugs (as of Scala 2.10).
   */
  def isSet(i:Int, a:A): Boolean

  /**
   * Test if bucket 'i' is unset.
   *
   * The parameter 'a' should be set to the value of bucket 'i', although in
   * some cases it won't be used. This parameter is necessary to work around
   * specialization bugs (as of Scala 2.10).
   */
  def isUnset(i:Int, a:A): Boolean

  /**
   * Test to see if bucket 'i' holds the value 'a'.
   */
  def hasItemAt(i:Int, a:A): Boolean

  /**
   * Test to see if bucket 'i' does not hold the value 'a'.
   */
  def notItemAt(i:Int, a:A): Boolean

  /**
   * Copy these buckets to a new object, as efficiently as possible.
   */
  def copy: Buckets[A]

  /**
   * Run function 'f' on every bucket with a set value.
   *
   * Unset buckets are skipped.
   */
  def foreach(f:A => Unit): Unit

  /**
   * Create a new bucket object, where every existing bucket with a set value
   * is mapped through the function 'f'.
   *
   * Buckets which are unset in this object will be unset in the result.
   */
  def map[@spec B:Manifest:Unset](f:A => B): Buckets[B]
}

/**
 * The Buckets object provides some useful factory methods.
 */
object Buckets {
  final def empty[@spec A:Unset:Manifest]: Buckets[A] = Buckets.ofDim[A](8)

  final def ofDim[@spec A:Unset:Manifest](n:Int): Buckets[A] = Unset[A] match {
    case MarkedUnset(mark) => new MarkedBuckets(Array.fill(n)(mark), mark)
    case NoUnset => new BitmaskBuckets(Array.ofDim[A](n), Array.ofDim[Int]((n + 31) >> 5))
  }
}


final class MarkedBuckets[@spec A] protected[debox] (as:Array[A], mark:A) extends Buckets[A] {
  final val nul = mark

  @inline final def length: Int = as.length
  @inline final def apply(i:Int): A = as(i)

  final def set(i:Int, a:A): Unit = as(i) = a
  final def unset(i:Int): A = { val a = as(i); as(i) = nul; null.asInstanceOf[A] }
  @inline final def isSet(i:Int, a:A): Boolean = a != nul
  @inline final def isUnset(i:Int, a:A): Boolean = a == nul

  final def hasItemAt(i:Int, a:A): Boolean = a == as(i)
  final def notItemAt(i:Int, a:A): Boolean = a != as(i)

  final def copy = new MarkedBuckets(as.clone, mark)

  final def foreach(f:A => Unit) {
    var i = 0
    val len = length
    while (i < len) {
      val a = as(i)
      if (a != nul) f(a)
      i += 1
    }
  }

  final def map[@spec B:Manifest:Unset](f:A => B): Buckets[B] = {
    val buckets = Buckets.empty[B]
    var i = 0
    val len = length
    while (i < len) {
      val a = as(i)
      if (a != nul) buckets.set(i, f(a))
      i += 1
    }
    buckets
  }
}

final class BitmaskBuckets[@spec A] protected[debox] (as:Array[A], mask:Array[Int]) extends Buckets[A] {

  @inline final def length: Int = as.length
  @inline final def apply(i:Int): A = as(i)

  final def set(i:Int, a:A): Unit = { as(i) = a; mask(i >> 5) |= (1 << (i & 31)) }
  final def unset(i:Int): A = { mask(i >> 5) &= ~(1 << (i & 31)); null.asInstanceOf[A] }
  @inline final def isSet(i:Int, a:A): Boolean = (mask(i >> 5) & (1 << (i & 31))) != 0
  @inline final def isUnset(i:Int, a:A): Boolean = (mask(i >> 5) & (1 << (i & 31))) == 0

  final def hasItemAt(i:Int, a:A): Boolean = isSet(i, a) && a == as(i)
  final def notItemAt(i:Int, a:A): Boolean = isUnset(i, a) || a != as(i)

  final def copy = new BitmaskBuckets(as.clone, mask.clone)

  final def foreach(f:Function[A, Unit]): Unit = {
    var i = 0
    var j = 0
    val limit = mask.length - 1
    val _len = length

    while (j < limit) {
      val b = mask(j)

      if ((b & 0x00000001) != 0) f(as(i))
      if ((b & 0x00000002) != 0) f(as(i + 1))
      if ((b & 0x00000004) != 0) f(as(i + 2))
      if ((b & 0x00000008) != 0) f(as(i + 3))
      if ((b & 0x00000010) != 0) f(as(i + 4))
      if ((b & 0x00000020) != 0) f(as(i + 5))
      if ((b & 0x00000040) != 0) f(as(i + 6))
      if ((b & 0x00000080) != 0) f(as(i + 7))

      if ((b & 0x00000100) != 0) f(as(i + 8))
      if ((b & 0x00000200) != 0) f(as(i + 9))
      if ((b & 0x00000400) != 0) f(as(i + 10))
      if ((b & 0x00000800) != 0) f(as(i + 11))
      if ((b & 0x00001000) != 0) f(as(i + 12))
      if ((b & 0x00002000) != 0) f(as(i + 13))
      if ((b & 0x00004000) != 0) f(as(i + 14))
      if ((b & 0x00008000) != 0) f(as(i + 15))

      if ((b & 0x00010000) != 0) f(as(i + 16))
      if ((b & 0x00020000) != 0) f(as(i + 17))
      if ((b & 0x00040000) != 0) f(as(i + 18))
      if ((b & 0x00080000) != 0) f(as(i + 19))
      if ((b & 0x00100000) != 0) f(as(i + 20))
      if ((b & 0x00200000) != 0) f(as(i + 21))
      if ((b & 0x00400000) != 0) f(as(i + 22))
      if ((b & 0x00800000) != 0) f(as(i + 23))

      if ((b & 0x01000000) != 0) f(as(i + 24))
      if ((b & 0x02000000) != 0) f(as(i + 25))
      if ((b & 0x04000000) != 0) f(as(i + 26))
      if ((b & 0x08000000) != 0) f(as(i + 27))
      if ((b & 0x10000000) != 0) f(as(i + 28))
      if ((b & 0x20000000) != 0) f(as(i + 29))
      if ((b & 0x40000000) != 0) f(as(i + 30))
      if ((b & 0x80000000) != 0) f(as(i + 31))

      i += 32
      j += 1
    }

    while (i < _len) {
      val a = as(i)
      if (isSet(i, a)) f(a)
      i += 1
    }
  }

  final def map[@spec B:Manifest:Unset](f:A => B): Buckets[B] = {
    val buckets = Buckets.empty[B]
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

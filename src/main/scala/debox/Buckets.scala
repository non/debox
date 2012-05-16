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
  def empty[@spec A:Unset:Manifest]: Buckets[A] = Buckets.ofDim[A](8)

  def ofDim[@spec A:Unset:Manifest](n:Int): Buckets[A] = {
    val arr = Array.ofDim[A](n)
    Unset[A].get match {
      case Some(mark) => new MarkedBuckets(arr, mark)
      case None => new BitmaskBuckets(arr, Array.ofDim[Int]((n + 31) / 32))
    }
  }
}

trait Buckets[@spec A] {
  def arr: Array[A]
  def length: Int = arr.length
  def set(i:Int, a:A): Unit
  def unset(i:Int): A
  def isSet(i:Int, a:A): Boolean
  def foreach(f:A => Unit) {
    val as = arr
    var i = 0
    val len = length
    while (i < len) {
      val a = as(i)
      if (isSet(i, a)) f(a)
      i += 1
    }
  }
  def map[@spec B:Manifest:Unset](f:A => B): Buckets[B] = {
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

class MarkedBuckets[@spec A](val arr:Array[A], mark:A) extends Buckets[A] {
  def set(i:Int, a:A): Unit = arr(i) = a
  def unset(i:Int): A = { val a = arr(i); arr(i) = mark; a }
  def isSet(i:Int, a:A): Boolean = a == mark
}

class BitmaskBuckets[@spec A](val arr:Array[A], mask:Array[Int]) extends Buckets[A] {
  def set(i:Int, a:A): Unit = { arr(i) = a; mask(i / 32) |= (1 << (i % 32)) }
  def unset(i:Int): A = { mask(i / 32) &= ~(1 << (i % 32)); arr(i) }
  def isSet(i:Int, a:A): Boolean = (mask(i / 32) & (1 << (i % 32))) != 0
}

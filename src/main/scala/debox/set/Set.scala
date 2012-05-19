package debox.set

import debox._
import debox.buffer.Buffer

import scala.{specialized => spec}

object Set {
  def empty[@spec A:Manifest:Unset]: Set[A] = ofDim[A](8)

  def ofDim[@spec A](n:Int)(implicit m:Manifest[A], u:Unset[A]): Set[A] = {
    if (n > 1073741824) sys.error("invalid size: %s" format n)
    var i = 8
    while (i < n) i << 1
    u match {
      case mu @ MarkedUnset(a) => new MarkedSet(Array.fill(n)(a), 0, (n + 31) >> 5)(m, mu)
      case nu => new BitmaskSet(Array.ofDim[A](n), Array(0), 0, (n + 31) >> 5)(m, nu)
    }
  }

  def apply[@spec A:Manifest:Unset](): Set[A] = empty[A]

  def apply[@spec A:Manifest:Unset](as:Array[A]) = {
    val n = scala.math.min(as.length * 3 / 2, 8)
    val s = ofDim[A](n)
    var i = 0
    val len = as.length
    while (i < len) {
      s.add(as(i))
      i += 1
    }
    s
  }

  def apply[@spec A:Manifest:Unset](as:Buffer[A]) = {
    val s = ofDim[A](as.length * 3 / 2)
    var i = 0
    val len = as.length
    while (i < len) {
      s.add(as(i))
      i += 1
    }
    s
  }
}

trait Set[@spec A] {
  implicit def m:Manifest[A]
  implicit def u:Unset[A]

  def length: Int

  def apply(item:A): Boolean

  def contains(item:A): Boolean = apply(item)

  def add(item:A): Boolean

  def update(as:Array[A]) {
    var i = 0
    val len = as.length
    while (i < len) {
      add(as(i))
      i += 1
    }
  }

  def remove(item:A): Boolean

  def copy:Set[A]

  def union(that:Set[A]): Set[A] = {
    if (length < that.length) return that.union(this)
    val result:Set[A] = this.copy
    that.foreach(a => result.add(a))
    result
  }

  def intersection(that:Set[A]): Set[A] = {
    if (length > that.length) return that.intersection(this)
    val result = Set.empty[A]
    this.foreach(a => if (that.contains(a)) result.add(a))
    result
  }

  def toList = {
    var lst = List[A]()
    foreach(a => lst = a :: lst)
    lst
  }

  def toArray = {
    var i = 0
    val arr = Array.ofDim[A](length)
    foreach {
      a =>
      arr(i) = a
      i += 1
    }
    arr
  }

  def toBuffer = debox.buffer.Mutable.unsafe(toArray)

  def map[@spec B:Manifest:Unset](f:A => B): Set[B]

  def foreach(f:Function[A, Unit]): Unit
}

final class MarkedSet[@spec A](as:Array[A], n:Int, s:Int)(implicit val m:Manifest[A], val u:MarkedUnset[A]) extends Set[A] {
  var buckets:Array[A] = as // buckets to store things in
  var len:Int = n // number of buckets used
  final val nul = u.nul

  @inline final def length = len
  @inline final def apply(item:A): Boolean = hasItemAt(item, hash(item, mask, buckets))
  final def hasItemAt(item:A, i:Int): Boolean = item == buckets(i)
  final def notItemAt(item:A, i:Int): Boolean = item != buckets(i)

  def add(item:A): Boolean = {
    val i = hash(item, mask, buckets)
    if (hasItemAt(item, i)) return false
    buckets(i) = item
    len += 1
    if (len > limit) resize()
    true
  }

  def remove(item:A): Boolean = {
    val i = hash(item, mask, buckets)
    if (notItemAt(item, i)) return false
    buckets(i) = nul
    len -= 1
    // TODO: maybe shrink the underlying array?
    true
  }

  def copy:Set[A] = new MarkedSet(buckets.clone, len, size)

  def map[@spec B:Manifest:Unset](f:A => B): Set[B] = {
    implicit val mu = MarkedUnset(f(nul))
    val set = new MarkedSet(Array.ofDim[B](size), 0, size)
    foreach(a => set.add(f(a)))
    set
  }

  def foreach(f:Function[A, Unit]): Unit = {
    var i = 0
    while (i < len) {
      if (buckets(i) != nul) f(buckets(i))
      i += 1
    }
  }

  // hashing internals
  var size = s // number of buckets, should be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  def hash(item:A, mask:Int, bs:Array[A]):Int = {
    var i = item.hashCode & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask

      // if this index is empty
      if (bs(j) == nul || bs(j) == item) return j

      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    -1
  }

  def resize():A = {
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextbuckets = Array.ofDim[A](nextsize)

    if (nextsize < 0) sys.error("oh no!")

    var i = 0
    while (i < len) {
      val item = buckets(i)
      if (item != nul) {
        val j = hash(item, nextmask, nextbuckets)
        nextbuckets(j) = item
      }
      i += 1
    }

    size = nextsize
    mask = nextmask
    buckets = nextbuckets
    limit = (size * 0.65).toInt

    null.asInstanceOf[A]
  }
}



final class BitmaskSet[@spec A](as:Array[A], ps:Array[Int], n:Int, s:Int)(implicit val m:Manifest[A], val u:Unset[A]) extends Set[A] {
  var buckets:Array[A] = as // buckets to store things in
  var present:Array[Int] = ps // keep track of which buckets are used
  var len:Int = n // number of buckets used

  final val nul:A = null.asInstanceOf[A]

  @inline final def length = len
  @inline final def apply(item:A): Boolean = hasItemAt(item, hash(item, mask, buckets, present))
  final def hasItemAt(item:A, i:Int): Boolean = {
    item == buckets(i) && (item != nul || occupied(present, i))
  }
  final def notItemAt(item:A, i:Int): Boolean = {
    item != buckets(i) || (item == nul && vacant(present, i))
  }

  final def add(item:A): Boolean = {
    val i = hash(item, mask, buckets, present)
    if (hasItemAt(item, i)) return false
    buckets(i) = item
    present(i >> 5) |= (1 << (i & 31))
    len += 1
    if (len > limit) resize()
    true
  }

  def remove(item:A): Boolean = {
    val i = hash(item, mask, buckets, present)
    if (notItemAt(item, i)) return false
    buckets(i) = nul
    present(i >> 5) &= ~(1 << (i & 31))
    len -= 1
    // TODO: maybe shrink the underlying arrays?
    true
  }

  def copy:Set[A] = new BitmaskSet(buckets.clone, present.clone, len, size)

  def map[@spec B:Manifest:Unset](f:A => B): Set[B] = {
    val set = new BitmaskSet(Array.ofDim[B](size), Array.ofDim[Int](present.length), 0, size)
    foreach(a => set.add(f(a)))
    set
  }

  def foreach(f:Function[A, Unit]): Unit = {
    var i = 0
    while (i < len) {
      if (occupied(present, i)) f(buckets(i))
      i += 1
    }
  }

  // hashing internals
  var size = s // number of buckets, should be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  @inline final def occupied(pres:Array[Int], i:Int) = (pres(i >> 5) & (1 << (i & 31))) != 0
  @inline final def vacant(pres:Array[Int], i:Int) = (pres(i >> 5) & (1 << (i & 31))) == 0

  def hash(item:A, mask:Int, bs:Array[A], ps:Array[Int]):Int = {
    var i = item.hashCode & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions

    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask

      // if the index is empty
      if (vacant(ps, j) || bs(j) == item) return j

      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    -1
  }

  def resize():A = {
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextbuckets = Array.ofDim[A](nextsize)
    val nextpresent = Array.ofDim[Int]((nextsize + 31) >> 5)

    if (nextsize < 0) sys.error("oh no!")

    var i = 0
    while (i < len) {
      if (occupied(present, i)) {
        val item = buckets(i)
        val j = hash(item, nextmask, nextbuckets, nextpresent)
        nextbuckets(j) = item
        nextpresent(j >> 5) |= (1 << (j & 31))
      }
      i += 1
    }

    size = nextsize
    mask = nextmask
    buckets = nextbuckets
    present = nextpresent
    limit = (size * 0.65).toInt
    nul
  }
}

package debox.set

import debox.Unset
import debox.buffer.Buffer

import scala.{specialized => spec}

object Set {
  def empty[@spec A:Manifest:Unset]: Set[A] = ofDim[A](8)

  def ofDim[@spec A:Manifest:Unset](n:Int): Set[A] = Unset[A].get match {
    case Some(a) => new MarkedSet(Array.fill(n)(a), 0, (n + 31) / 32)
    case None => new BitmaskSet(Array.ofDim[A](n), Array(0), 0, (n + 31) / 32)
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

  def iterate(f:Function[Int, Unit]): Unit
}

final class MarkedSet[@spec A](as:Array[A], n:Int, s:Int)(implicit val m:Manifest[A], val u:Unset[A]) extends Set[A] {
  println("built marked set")
  var buckets:Array[A] = as // buckets to store things in
  var len:Int = n // number of buckets used

  def length = len

  def apply(item:A): Boolean = hasItemAt(item, hash(item, mask, buckets))

  def hasItemAt(item:A, i:Int): Boolean = item == buckets(i)

  def add(item:A): Boolean = {
    val i = hash(item, mask, buckets)
    if (hasItemAt(item, i)) {
      false
    } else {
      buckets(i) = item
      len += 1
      if (len > limit) resize()
      true
    }
  }

  def remove(item:A): Boolean = {
    val i = hash(item, mask, buckets)
    if (hasItemAt(item, i)) {
      buckets(i) = null.asInstanceOf[A]
      len -= 1
      // TODO: maybe we want to shrink the underlying arrays in some cases
      true
    } else {
      false
    }
  }

  def copy:Set[A] = new MarkedSet(buckets.clone, len, size)

  def map[@spec B:Manifest:Unset](f:A => B): Set[B] = {
    val set = new MarkedSet(Array.ofDim[B](size), 0, size)
    foreach(a => set.add(f(a)))
    set
  }

  def foreach(f:Function[A, Unit]): Unit = iterate(i => f(buckets(i)))

  def iterate(f:Function[Int, Unit]): Unit = {
    var i = 0
    while (i < length) {
      if (buckets(i) != null) f(i)
      i += 1
    }
  }

  // hashing internals
  var size = s // number of buckets, should be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  def hash(item:A, mask:Int, bs:Array[A]):Int = {
    var i = item.hashCode // get a hash code for the item
    var j = i & mask // ensure j is positive and within [0, size]
    
    // return if we have a vacant bucket, or our own bucket
    if (bs(j) == null || bs(j) == item) return j
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    var perturbation = i
    while (true) {
      i = (i << 2) + i + perturbation + 1
      j = i & mask
    
      // again, return if we have a vacant (or owned) bucket
      if (bs(j) == null || bs(j) == item) return j
    
      perturbation >>= 5
    }
    
    // should never happen
    sys.error("should not happen")
    -1
  }

  def resize() {
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextbuckets = Array.ofDim[A](nextsize)

    if (nextsize < 0) sys.error("oh no!")

    foreach {
      item =>
      val i = hash(item, nextmask, nextbuckets)
      nextbuckets(i) = item
    }

    size = nextsize
    mask = nextmask
    buckets = nextbuckets
    limit = (size * 0.65).toInt
  }
}



final class BitmaskSet[@spec A](as:Array[A], ps:Array[Int], n:Int, s:Int)(implicit val m:Manifest[A], val u:Unset[A]) extends Set[A] {
  println("built bitmask set")
  var buckets:Array[A] = as // buckets to store things in
  var present:Array[Int] = ps // keep track of which buckets are used
  var len:Int = n // number of buckets used

  def length = len

  def apply(item:A): Boolean = hasItemAt(item, hash(item, mask, buckets, present))

  def hasItemAt(item:A, i:Int): Boolean = item == buckets(i) && occupied(present, i)

  def add(item:A): Boolean = {
    val i = hash(item, mask, buckets, present)
    if (hasItemAt(item, i)) {
      false
    } else {
      buckets(i) = item
      present(i / 32) |= (1 << (i % 32))
      len += 1
      if (len > limit) resize()
      true
    }
  }

  def remove(item:A): Boolean = {
    val i = hash(item, mask, buckets, present)
    if (hasItemAt(item, i)) {
      buckets(i) = null.asInstanceOf[A]
      present(i / 32) &= ~(1 << (i % 32))
      len -= 1
      // TODO: maybe we want to shrink the underlying arrays in some cases
      true
    } else {
      false
    }
  }

  def copy:Set[A] = new BitmaskSet(buckets.clone, present.clone, len, size)

  def map[@spec B:Manifest:Unset](f:A => B): Set[B] = {
    val set = new BitmaskSet(Array.ofDim[B](size), Array.ofDim[Int](present.length), 0, size)
    foreach(a => set.add(f(a)))
    set
  }

  def foreach(f:Function[A, Unit]): Unit = iterate(i => f(buckets(i)))

  def iterate(f:Function[Int, Unit]): Unit = {
    var i = 0
    var m = 0
    var p = 0

    // this is the main loop that will handle all but the last 0-31 slots.
    val limit = size - 31
    while (i < limit) {
      p = present(i / 32)
      m = 1
      while (m != 0) {
        if ((p & m) != 0) f(i)
        m = m << 1
        i += 1
      }
    }

    // if length is not a multiple of 32, we want the rest here.
    if (i < size) {
      p = present(i / 32)
      m = 1
      while (i < size) {
        if ((p & m) != 0) f(i)
        m = m << 1
        i += 1
      }
    }
  }

  // hashing internals
  var size = s // number of buckets, should be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  def occupied(pres:Array[Int], i:Int) = (pres(i / 32) & (1 << (i % 32))) != 0
  def vacant(pres:Array[Int], i:Int) = (pres(i / 32) & (1 << (i % 32))) == 0

  def hash(item:A, mask:Int, bs:Array[A], ps:Array[Int]):Int = {
    var i = item.hashCode // get a hash code for the item
    var j = i & mask // ensure j is positive and within [0, size]
    
    // return if we have a vacant bucket, or our own bucket
    if (vacant(ps, j) || bs(j) == item) return j
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    var perturbation = i
    while (true) {
      i = (i << 2) + i + perturbation + 1
      j = i & mask
    
      // again, return if we have a vacant (or owned) bucket
      if (vacant(ps, j) || bs(j) == item) return j
    
      perturbation >>= 5
    }
    
    // should never happen
    sys.error("should not happen")
    -1
  }

  def resize() {
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextbuckets = Array.ofDim[A](nextsize)
    val nextpresent = Array.ofDim[Int]((nextsize + 31) / 32)

    if (nextsize < 0) sys.error("oh no!")

    foreach {
      item =>
      val i = hash(item, nextmask, nextbuckets, nextpresent)
      nextbuckets(i) = item
      nextpresent(i / 32) |= (1 << (i % 32))
    }

    size = nextsize
    mask = nextmask
    buckets = nextbuckets
    present = nextpresent
    limit = (size * 0.65).toInt
  }
}

package debox.set

import debox._
import debox.buffer.Buffer

import scala.{specialized => spec}

object Set2 {
  def size(n:Int) = {
    var sz = 8
    var limit = 5
    while (n < limit) {
      if (sz < 10000) sz <<= 2 else sz <<= 1
      limit = (sz * 0.65).toInt
      if (sz <= 0) sys.error("overflow")
    }
    sz
  }

  def empty[@spec A:Manifest:Unset] = new Set2(Buckets.ofDim[A](8), 0, 8)

  def apply[@spec A:Manifest:Unset](): Set2[A] = empty[A]

  def apply[@spec A:Manifest:Unset](as:Array[A]) = {
    val len = as.length
    val sz = size(len)
    val s = new Set2(Buckets.ofDim[A](sz), 0, sz)
    var i = 0
    while (i < len) {
      s.add(as(i))
      i += 1
    }
    s
  }

  def apply[@spec A:Manifest:Unset](as:Buffer[A]) = {
    val len = as.length
    val sz = size(len)
    val s = new Set2(Buckets.ofDim[A](sz), len, sz)
    var i = 0
    while (i < len) {
      s.add(as(i))
      i += 1
    }
    s
  }
}

final class Set2[@spec A:Manifest:Unset](as:Buckets[A], n:Int, s:Int) extends Set[A] {
  final def u = null // xyz
  final def m = null // xyz

  var buckets:Buckets[A] = as // buckets to store things in
  var len:Int = n // number of buckets used

  final def length: Int = len

  //final def apply(item:A): Boolean = buckets.hasItemAt(hash(item, mask, buckets), item)

  final def hasItemAt(item:A, i:Int): Boolean = buckets.hasItemAt(i, item)

  final def notItemAt(item:A, i:Int): Boolean = {
    val itm = buckets(i)
    itm != item || buckets.isUnset(i, itm)
  }

  //def contains(item:A): Boolean = apply(item)

  final def add(item:A): Boolean = {
    val i = hash(item, mask, buckets)
    if (i >= buckets.length) sys.error("no no no")
    if (hasItemAt(item, i)) return false
    buckets.set(i, item)
    len += 1
    if (len > limit) resize()
    true
  }

  //def update(as:Array[A]) {
  //  var i = 0
  //  val len = as.length
  //  while (i < len) {
  //    add(as(i))
  //    i += 1
  //  }
  //}

  final def remove(item:A): Boolean = {
    val i = hash(item, mask, buckets)
    if (notItemAt(item, i)) return false
    buckets.unset(i)
    len -= 1
    // TODO: maybe shrink the underlying array?
    true
  }

  final def copy:Set2[A] = new Set2(buckets.copy, len, size)

  final def union(that:Set2[A]): Set2[A] = {
    if (length < that.length) return that.union(this)
    val result:Set2[A] = this.copy
    that.foreach(a => result.add(a))
    result
  }

  final def intersection(that:Set2[A]): Set2[A] = {
    if (length > that.length) return that.intersection(this)
    val result = Set2.empty[A]
    this.foreach(a => if (that.contains(a)) result.add(a))
    result
  }

  override def toList = {
    var lst = List[A]()
    foreach(a => lst = a :: lst)
    lst
  }
  
  override def toArray = {
    var i = 0
    val arr = Array.ofDim[A](length)
    foreach {
      a =>
      arr(i) = a
      i += 1
    }
    arr
  }

  override def toBuffer = debox.buffer.Mutable.unsafe(toArray)

  final def map[@spec B:Manifest:Unset](f:A => B): Set2[B] = {
    val set = new Set2(Buckets.ofDim[B](size), 0, size)
    foreach(a => set.add(f(a)))
    set
  }

  final def foreach(f:Function[A, Unit]): Unit = buckets.foreach(f)
  //  var i = 0
  //  var count = 0
  //  while (count < len) {
  //    var item = buckets(i)
  //    if (buckets.isSet(i, item)) {
  //      f(item)
  //      count += 1
  //    }
  //    i += 1
  //  }
  //}

  override def toString = {
    var as = List[A]()
    foreach {
      a => as = a :: as
    }
    as.mkString("Set2(", ", ", ")")
  }

  // hashing internals
  var size = s // number of buckets, should be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  final def hash(item:A, mask:Int, bs:Buckets[A]):Int = {
    var i = item.## & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask

      // if this index is empty, or equal to our value already
      //val slot = bs(j)
      //if (slot == item || bs.isUnset(j, slot)) return j
      if (bs(j) == item || bs.isUnset(j, bs(j))) return j

      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    -1
  }

  final def apply(item:A):Boolean = {
    var i = item.## & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    val bs = buckets
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask

      // if this index is empty, or equal to our value already
      val slot = bs(j)
      if (bs.isUnset(j, slot)) return false
      if (slot == item) return true

      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    false
  }

  final def resize():A = {
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextbuckets = Buckets.ofDim[A](nextsize)

    if (nextsize < 0) sys.error("oh no!")

    var i = 0
    var count = 0
    while (count < len) {
      //if (i >= buckets.length)
      //  sys.error("i=%s bs=%s count=%s len=%s nbs=%s" format (i, buckets, count, len, nextbuckets))
      val item = buckets(i)
      if (buckets.isSet(i, item)) {
        //nextbuckets.set(nextbuckets.hash(item, nextmask), item)
        nextbuckets.set(hash(item, nextmask, nextbuckets), item)
        count += 1
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

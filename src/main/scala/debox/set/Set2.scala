package debox.set

import debox._
import debox.buffer.Buffer

import scala.{specialized => spec}

object Set2 {
  def empty[@spec A:Manifest:Unset] = new Set2(Buckets.ofDim[A](8), 0, 8)

  def apply[@spec A:Manifest:Unset]():Set2[A] = empty[A]

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

  private def size(n:Int) = {
    var sz = 8
    var limit = 5
    while (n < limit) {
      if (sz < 10000) sz <<= 2 else sz <<= 1
      limit = (sz * 0.65).toInt
      if (sz <= 0) sys.error("overflow")
    }
    sz
  }
}

final class Set2[@spec A:Manifest:Unset] protected[debox] (as:Buckets[A], n:Int, s:Int) extends Function1[A, Boolean] {

  // set internals
  var buckets:Buckets[A] = as // buckets to store things in
  var len:Int = n // number of buckets used

  // hashing internals
  var size = s // number of buckets, must be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  final def length:Int = len

  final def update(item:A, b:Boolean):Unit = if (b) add(item) else remove(item)

  final def add(item:A):Boolean = {
    val i = hash(item, mask, buckets)
    if (buckets.hasItemAt(i, item)) return false
    buckets.set(i, item)
    len += 1
    if (len > limit) resize()
    true
  }

  final def remove(item:A):Boolean = {
    val i = hash(item, mask, buckets)
    if (buckets.notItemAt(i, item)) return false
    buckets.unset(i)
    len -= 1
    // TODO: maybe shrink the underlying array?
    true
  }

  final def copy:Set2[A] = new Set2(buckets.copy, len, size)

  final def union(that:Set2[A]):Set2[A] = {
    if (length < that.length) return that.union(this)
    val result:Set2[A] = copy
    that.foreach(a => result.add(a))
    result
  }

  final def intersection(that:Set2[A]):Set2[A] = {
    if (length > that.length) return that.intersection(this)
    val result = Set2.empty[A]
    this.foreach(a => if (that(a)) result.add(a))
    result
  }

  def toList:List[A] = {
    var lst = List[A]()
    foreach(a => lst = a :: lst)
    lst
  }
  
  def toArray:Array[A] = {
    var i = 0
    val arr = Array.ofDim[A](length)
    foreach {
      a =>
      arr(i) = a
      i += 1
    }
    arr
  }

  def toBuffer:Buffer[A] = debox.buffer.Mutable.unsafe(toArray)

  final def map[@spec B:Manifest:Unset](f:A => B):Set2[B] = {
    val set = new Set2(Buckets.ofDim[B](size), 0, size)
    foreach(a => set.add(f(a)))
    set
  }

  final def foreach(f:Function[A, Unit]):Unit = buckets.foreach(f)

  override def toString:String = toList.mkString("Set2(", ", ", ")")

  final def hash(item:A, mask:Int, bs:Buckets[A]):Int = {
    var i = item.## & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask

      // if this index is empty, or equal to our value already
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

    var i = 0
    var count = 0
    while (count < len) {
      val item = buckets(i)
      if (buckets.isSet(i, item)) {
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

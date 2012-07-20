package debox.set

import debox._
import debox.buffer.Buffer

import scala.{specialized => spec}

/**
 *
 */
//object Set {
//  def empty[@spec A:Manifest:Unset]: Set[A] = ofDim[A](8)
//
//  def ofDim[@spec A](n:Int)(implicit m:Manifest[A], u:Unset[A]): Set[A] = {
//    if (n > 1073741824) sys.error("invalid size: %s" format n)
//    var i = 8
//    while (i < n) i << 1
//    u match {
//      case mu @ MarkedUnset(a) => new MarkedSet(Array.fill(n)(a), 0, (n + 31) >> 5)(m, mu)
//      case nu => new BitmaskSet(Array.ofDim[A](n), Array(0), 0, (n + 31) >> 5)(m, nu)
//    }
//  }
//
//  def apply[@spec A:Manifest:Unset](): Set[A] = empty[A]
//
//  def apply[@spec A:Manifest:Unset](as:Array[A]) = {
//    val n = scala.math.min(as.length * 3 / 2, 8)
//    val s = ofDim[A](n)
//    var i = 0
//    val len = as.length
//    while (i < len) {
//      s.add(as(i))
//      i += 1
//    }
//    s
//  }
//
//  def apply[@spec A:Manifest:Unset](as:Buffer[A]) = {
//    val s = ofDim[A](as.length * 3 / 2)
//    var i = 0
//    val len = as.length
//    while (i < len) {
//      s.add(as(i))
//      i += 1
//    }
//    s
//  }
//}

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

  //def union(that:Set[A]): Set[A] = {
  //  if (length < that.length) return that.union(this)
  //  val result:Set[A] = this.copy
  //  that.foreach(a => result.add(a))
  //  result
  //}
  //
  //def intersection(that:Set[A]): Set[A] = {
  //  if (length > that.length) return that.intersection(this)
  //  val result = Set.empty[A]
  //  this.foreach(a => if (that.contains(a)) result.add(a))
  //  result
  //}

  def toList = {
    var lst = List[A]()
    foreach(a => lst = a :: lst)
    lst
  }

  def toArray = {
    var i = 0
    val len = length
    val arr = Array.ofDim[A](len)
    foreach {
      a =>
      arr(i) = a
      i += 1
    }
    arr
  }

  def toBuffer = debox.buffer.Mutable.unsafe(toArray)

  override def toString = toArray.mkString("Set(", ", ", ")")

  def map[@spec B:Manifest:Unset](f:A => B): Set[B]

  def foreach(f:Function[A, Unit]): Unit
}

//final class MarkedSet[@spec A](as:Array[A], n:Int, s:Int)(implicit val m:Manifest[A], val u:MarkedUnset[A]) extends Set[A] {
//  var buckets:Array[A] = as // buckets to store things in
//  var len:Int = n // number of buckets used
//  final val nul = u.nul
//
//  final def length = len
//  final def apply(item:A): Boolean = hasItemAt(item, hash(item, mask, buckets))
//  @inline final def hasItemAt(item:A, i:Int): Boolean = item == buckets(i)
//  @inline final def notItemAt(item:A, i:Int): Boolean = item != buckets(i)
//
//  final def add(item:A): Boolean = {
//    val i = hash(item, mask, buckets)
//    if (hasItemAt(item, i)) return false
//    buckets(i) = item
//    len += 1
//    if (len > limit) resize()
//    true
//  }
//
//  final def remove(item:A): Boolean = {
//    val i = hash(item, mask, buckets)
//    if (notItemAt(item, i)) return false
//    buckets(i) = nul
//    len -= 1
//    // TODO: maybe shrink the underlying array?
//    true
//  }
//
//  final def copy:Set[A] = new MarkedSet(buckets.clone, len, size)
//
//  final def map[@spec B](f:A => B)(implicit m:Manifest[B], u:Unset[B]): Set[B] = u match {
//    case MarkedUnset(nul) => mapNul(f)(m, MarkedUnset[B](nul))
//    case NoUnset => mapBitmask(f)(m, NoUnset)
//  }
//  
//  final def mapNul[@spec B:Manifest:MarkedUnset](f:A => B): Set[B] = {
//    val bs = Array.ofDim[B](size)
//    val set = new MarkedSet(bs, len, size)
//    foreach(a => set.store(f(a), mask, bs))
//    set
//  }
//  
//  final def mapBitmask[@spec B:Manifest:Unset](f:A => B): Set[B] = {
//    val bs = Array.ofDim[B](size)
//    val ps = Array.ofDim[Int]((size + 31) >> 5)
//    val set = new BitmaskSet(bs, ps, len, size)
//    foreach(a => set.store(f(a), mask, bs, ps))
//    set
//  }
//
//  final def foreach(f:Function[A, Unit]): Unit = {
//    var i = 0
//    var count = 0
//    val _len = len
//    val n = nul
//    while (count < _len) {
//      if (buckets(i) != n) {
//        f(buckets(i))
//        count += 1
//      }
//      i += 1
//    }
//  }
//
//  // hashing internals
//  var size = s // number of buckets, should be a power of 2
//  var mask = s - 1 // size-1, used for hashing
//  var limit = (s * 0.65).toInt // point at which we should resize
//
//
//  final def hash(item:A, mask:Int, bs:Array[A]):Int = {
//    var i = item.## & 0x7fffffff // positive hash code for the item
//    var perturbation = i // perturbation helps avoid collisions
//    val n = nul
//
//    // while there are collisions, we have to keep modifying our index to find
//    // new addresses. the open addressing scheme here is inspired by python's.
//    while (true) {
//      // j is the index we're going to try.
//      val j = i & mask
//
//      // if this index holds our item, or is empty, return the index.
//      val itm = bs(j)
//      if (itm == n || itm == item) return j
//
//      // otherwise, find a new index to try.
//      i = (i << 2) + i + perturbation + 1
//      perturbation >>= 5
//    }
//    -1 // should never happen
//  }
//
//  final def store(item:A, mask:Int, bs:Array[A]) {
//    bs(hash(item, mask, bs)) = item
//  }
//
//  final def resize():A = {
//    val factor = if (size < 10000) 4 else 2
//
//    val nextsize = size * factor
//    val nextmask = nextsize - 1
//    val nextbuckets = Array.fill(nextsize)(nul)
//
//    if (nextsize < 0) sys.error("oh no!")
//
//    var i = 0
//    var count = 0
//    val _len = len
//    val n = nul
//    while (count < _len) {
//      val item = buckets(i)
//      if (item != n) {
//        store(item, nextmask, nextbuckets)
//        count += 1
//      }
//      i += 1
//    }
//
//    size = nextsize
//    mask = nextmask
//    buckets = nextbuckets
//    limit = (size * 0.65).toInt
//
//    null.asInstanceOf[A]
//  }
//}
//
//
//
//final class BitmaskSet[@spec A](as:Array[A], ps:Array[Int], n:Int, s:Int)(implicit val m:Manifest[A], val u:Unset[A]) extends Set[A] {
//  var buckets:Array[A] = as // buckets to store things in
//  var present:Array[Int] = ps // keep track of which buckets are used
//  var len:Int = n // number of buckets used
//
//  final val nul:A = null.asInstanceOf[A]
//
//  final def length = len
//  final def apply(item:A): Boolean = hasItemAt(item, hash(item, mask, buckets, present))
//  @inline final def hasItemAt(item:A, i:Int) = {
//    item == buckets(i) && (present(i >> 5) & (1 << (i & 31))) != 0
//  }
//
//  @inline final def notItemAt(item:A, i:Int) = {
//    item != buckets(i) || (present(i >> 5) & (1 << (i & 31))) == 0
//  }
//
//  final def add(item:A): Boolean = {
//    val i = hash(item, mask, buckets, present)
//    if (hasItemAt(item, i)) return false
//    buckets(i) = item
//    present(i >> 5) |= (1 << (i & 31))
//    len += 1
//    if (len > limit) resize()
//    true
//  }
//
//  final def remove(item:A): Boolean = {
//    val i = hash(item, mask, buckets, present)
//    if (notItemAt(item, i)) return false
//    present(i >> 5) &= ~(1 << (i & 31))
//    len -= 1
//    // TODO: maybe shrink the underlying arrays?
//    true
//  }
//
//  final def copy:Set[A] = new BitmaskSet(buckets.clone, present.clone, len, size)
//
//  final def map[@spec B](f:A => B)(implicit m:Manifest[B], u:Unset[B]): Set[B] = u match {
//    case MarkedUnset(nul) => mapNul(f)(m, MarkedUnset[B](nul))
//    case NoUnset => mapBitmask(f)(m, NoUnset)
//  }
//  
//  final def mapNul[@spec B:Manifest:MarkedUnset](f:A => B): Set[B] = {
//    val bs = Array.ofDim[B](size)
//    val set = new MarkedSet(bs, len, size)
//    foreach(a => set.store(f(a), mask, bs))
//    set
//  }
//  
//  final def mapBitmask[@spec B:Manifest:Unset](f:A => B): Set[B] = {
//    val bs = Array.ofDim[B](size)
//    val ps = Array.ofDim[Int]((size + 31) >> 5)
//    val set = new BitmaskSet(bs, ps, len, size)
//    foreach(a => set.store(f(a), mask, bs, ps))
//    set
//  }
//
//  final def foreach(f:Function[A, Unit]): Unit = {
//    var i = 0
//    var j = 0
//    var count = 0
//    val limit = present.length - 1
//    val _len = len
//
//    while (count < _len && j < limit) {
//      val b = present(j)
//
//      if ((b & 0x01) != 0) { f(buckets(i)); count += 1 }
//      if ((b & 0x02) != 0) { f(buckets(i + 1)); count += 1 }
//      if ((b & 0x04) != 0) { f(buckets(i + 2)); count += 1 }
//      if ((b & 0x08) != 0) { f(buckets(i + 3)); count += 1 }
//      if ((b & 0x10) != 0) { f(buckets(i + 4)); count += 1 }
//      if ((b & 0x20) != 0) { f(buckets(i + 5)); count += 1 }
//      if ((b & 0x40) != 0) { f(buckets(i + 6)); count += 1 }
//      if ((b & 0x80) != 0) { f(buckets(i + 7)); count += 1 }
//
//      if ((b & 0x0100) != 0) { f(buckets(i + 8)); count += 1 }
//      if ((b & 0x0200) != 0) { f(buckets(i + 9)); count += 1 }
//      if ((b & 0x0400) != 0) { f(buckets(i + 10)); count += 1 }
//      if ((b & 0x0800) != 0) { f(buckets(i + 11)); count += 1 }
//      if ((b & 0x1000) != 0) { f(buckets(i + 12)); count += 1 }
//      if ((b & 0x2000) != 0) { f(buckets(i + 13)); count += 1 }
//      if ((b & 0x4000) != 0) { f(buckets(i + 14)); count += 1 }
//      if ((b & 0x8000) != 0) { f(buckets(i + 15)); count += 1 }
//
//      if ((b & 0x010000) != 0) { f(buckets(i + 16)); count += 1 }
//      if ((b & 0x020000) != 0) { f(buckets(i + 17)); count += 1 }
//      if ((b & 0x040000) != 0) { f(buckets(i + 18)); count += 1 }
//      if ((b & 0x080000) != 0) { f(buckets(i + 19)); count += 1 }
//      if ((b & 0x100000) != 0) { f(buckets(i + 20)); count += 1 }
//      if ((b & 0x200000) != 0) { f(buckets(i + 21)); count += 1 }
//      if ((b & 0x400000) != 0) { f(buckets(i + 22)); count += 1 }
//      if ((b & 0x800000) != 0) { f(buckets(i + 23)); count += 1 }
//
//      if ((b & 0x01000000) != 0) { f(buckets(i + 24)); count += 1 }
//      if ((b & 0x02000000) != 0) { f(buckets(i + 25)); count += 1 }
//      if ((b & 0x04000000) != 0) { f(buckets(i + 26)); count += 1 }
//      if ((b & 0x08000000) != 0) { f(buckets(i + 27)); count += 1 }
//      if ((b & 0x10000000) != 0) { f(buckets(i + 28)); count += 1 }
//      if ((b & 0x20000000) != 0) { f(buckets(i + 29)); count += 1 }
//      if ((b & 0x40000000) != 0) { f(buckets(i + 30)); count += 1 }
//      if ((b & 0x80000000) != 0) { f(buckets(i + 31)); count += 1 }
//
//      i += 32
//      j += 1
//    }
//
//    while (count < _len) {
//      if (occupied(present, i)) { f(buckets(i)); count += 1 }
//      i += 1
//    }
//  }
//
//  // hashing internals
//  var size = s // number of buckets, should be a power of 2
//  var mask = s - 1 // size-1, used for hashing
//  var limit = (s * 0.65).toInt // point at which we should resize
//
//  @inline final def occupied(pres:Array[Int], i:Int) = (pres(i >> 5) & (1 << (i & 31))) != 0
//  @inline final def vacant(pres:Array[Int], i:Int) = (pres(i >> 5) & (1 << (i & 31))) == 0
//
//  final def hash(item:A, mask:Int, bs:Array[A], ps:Array[Int]):Int = {
//    var i = item.## & 0x7fffffff // positive hash code for the item
//    var perturbation = i // perturbation helps avoid collisions
//
//    // while there are collisions, we have to keep modifying our index to find
//    // new addresses. the open addressing scheme here is inspired by python's.
//    while (true) {
//      // j is the index we're going to try
//      val j = i & mask
//
//      // if the index is empty
//      if (bs(j) == item || (ps(j >> 5) & (1 << (j & 31))) == 0) return j
//
//      // otherwise, find a new index to try
//      i = (i << 2) + i + perturbation + 1
//      perturbation >>= 5
//    }
//    
//    // should never happen
//    -1
//  }
//
//  final def store(item:A, mask:Int, bs:Array[A], ps:Array[Int]) {
//    val j = hash(item, mask, bs, ps)
//    bs(j) = item
//    ps(j >> 5) |= (1 << (j & 31))
//  }
//
//  final def resize():A = {
//    val factor = if (size < 10000) 4 else 2
//
//    val nextsize = size * factor
//    val nextm = nextsize - 1
//    val nextbs = Array.ofDim[A](nextsize)
//    val nextps = Array.ofDim[Int]((nextsize + 31) >> 5)
//
//    if (nextsize < 0) sys.error("oh no!")
//
//    var i = 0
//    var count = 0
//    val _len = len
//    while (count < _len) {
//      if (occupied(present, i)) {
//        store(buckets(i), nextm, nextbs, nextps)
//        count += 1
//      }
//      i += 1
//    }
//
//    size = nextsize
//    mask = nextm
//    buckets = nextbs
//    present = nextps
//    limit = (size * 0.65).toInt
//    nul
//  }
//}

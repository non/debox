package debox.set

import debox._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => spec}

class InvalidSizes(k: Int, v: Int) extends Exception("%s, %s" format (k, v))
class SetOverflow(n: Int) extends Exception("size %s exceeds max" format n)

object Set {
  def empty[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash
  ] = new Set(new Array[A](8), new Array[Int](1), 0, 0)

  def ofDim[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash
  ](n:Int) = {
    val sz = Util.nextPowerOfTwo(n)
    if (sz < 1) throw new SetOverflow(n)
    val m = (sz + 15) >> 4
    new Set(new Array[A](sz), new Array[Int](m), 0, 0)
  }

  def apply[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash
  ]():Set[A] = empty[A]

  def apply[
    @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash
  ](as:Array[A]) = {
    val set = ofDim[A](as.length)
    val limit = as.length - 1
    @inline @tailrec def loop(i:Int) {
      set.add(as(i))
      if (i < limit) loop(i + 1)
    }
    loop(0)
    set
  }
}

final class Set[
  @spec(Int, Long, Double, AnyRef) A:ClassTag:Hash
] protected[debox] (
  as:Array[A], bs:Array[Int], n:Int, u:Int
) extends Function1[A, Boolean] {

  var items:Array[A] = as
  var buckets:Array[Int] = bs
  var len:Int = n
  var used:Int = u

  def getBuckets: Array[Int] = buckets

  // hashing internals
  var mask = items.length - 1 // size-1, used for hashing
  var limit = (items.length * 0.65).toInt // point at which we should resize

  final def length:Int = len

  final def update(item:A, b:Boolean) = if (b) add(item) else remove(item)

  final def add(item:A):Boolean = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Boolean = {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 0) {
        items(j) = item
        Util.set(buckets, j)
        used += 1
        len += 1
        if (used > limit) resize()
        true
      } else if (status == 2) {
        items(j) = item
        Util.set(buckets, j)
        len += 1
        true
      } else if (items(j) == item) {
        false
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(item) & 0x7fffffff
    loop(i, i)
  }

  final def remove(item:A):Boolean = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Boolean = {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 3 && items(j) == item) {
        Util.unset(buckets, j)
        len -= 1
        true
      } else if (status == 0) {
        false
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(item) & 0x7fffffff
    loop(i, i)
  }

  final def copy:Set[A] = new Set(items.clone, buckets.clone, len, used)

  final def apply(item:A):Boolean = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Boolean = {
      val j = i & mask
      val status = Util.status(buckets, j)
      if (status == 0) {
        false
      } else if (status == 3 && items(j) == item) {
        true
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = Hash[A].hash(item) & 0x7fffffff
    loop(i, i)
  }

  final def foreach(f: A => Unit) {
    @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
      if (((b >> shift) & 3) == 3) {
        f(items(i))
        if (shift < 30) inner(i + 1, b, shift + 2, count + 1) else count + 1
      } else {
        if (shift < 30) inner(i + 1, b, shift + 2, count) else count
      }
    }

    @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int) {
      if (count < len) outer(i + 16, k + 1, inner(i, buckets(k), 0, count), len)
    }
    outer(0, 0, 0, len)
  }

  final def hash(item:A, _mask:Int, _items:Array[A], _buckets:Array[Int]):Int = {
    @inline @tailrec def loop(i:Int, perturbation:Int): Int = {
      val j = i & _mask
      if (Util.status(_buckets, j) == 3 && _items(j) != item) {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      } else {
        j
      }
    }
    val i = Hash[A].hash(item) & 0x7fffffff
    loop(i, i)
  }

  final def resize(): Unit1[A] = {
    val size = items.length
    val factor = if (size < 10000) 4 else 2
    
    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextitems = new Array[A](nextsize)
    val nextbs = new Array[Int]((nextsize + 15) >> 4)

    @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
      if (((b >> shift) & 3) == 3) {
        val item = items(i)
        val j = hash(item, nextmask, nextitems, nextbs)
        nextitems(j) = item
        Util.set(nextbs, j)
        if (shift < 30) inner(i + 1, b, shift + 2, count + 1) else count + 1
      } else {
        if (shift < 30) inner(i + 1, b, shift + 2, count) else count
      }
    }

    @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int) {
      if (count < len) outer(i + 16, k + 1, inner(i, buckets(k), 0, count), len)
    }
    outer(0, 0, 0, len)

    items = nextitems
    buckets = nextbs

    mask = nextmask
    limit *= factor

    new Unit1[A]
  }
}

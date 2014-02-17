package debox

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.syntax.cfor._

/**
 * Set is a mutable hash set, with open addressing and double hashing.
 * 
 * Set provides constant-time membership tests, and amortized
 * constant-time addition and removal. One underlying array stores
 * items, and another tracks which buckets are used and defined.
 *
 * When the type A is known (or the caller is specialized on A),
 * Set[A] will store the values in an unboxed array.
 */
final class Set[@sp A] protected[debox](as: Array[A], bs: Array[Byte], n: Int, u: Int)(implicit val ct: ClassTag[A]) { lhs =>

  // set machinery
  var items: Array[A] = as      // slots for items
  var buckets: Array[Byte] = bs // buckets track defined/used slots
  var len: Int = n              // number of defined slots
  var used: Int = u             // number of used slots (used >= len)

  // hashing internals
  var mask = buckets.length - 1             // size-1, used for hashing
  var limit = (buckets.length * 0.65).toInt // point at which we should grow

  override def equals(that: Any): Boolean = that match {
    case that: Set[_] =>
      if (size != that.size || ct != that.ct) return false
      val s = that.asInstanceOf[Set[A]]
      forall(s.apply)
    case _ =>
      false
  }

  override def hashCode: Int = fold(0xdeadd065)(_ ^ _.##)

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Set(")
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) {
      sb.append(items(i).toString)
      i += 1
    }
    while (i < buckets.length) {
      if (buckets(i) == 3) {
        sb.append(", ")
        sb.append(items(i).toString)
      }
      i += 1
    }
    sb.append(")")
    sb.toString
  }

  final def size: Int = len
  final def isEmpty: Boolean = len == 0
  final def nonEmpty: Boolean = len > 0

  final def apply(item: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        false
      } else if (status == 3 && items(j) == item) {
        true
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = item.## & 0x7fffffff
    loop(i, i)
  }

  final def copy: Set[A] = new Set(items.clone, buckets.clone, len, used)

  final def clear: Unit = absorb(Set.empty[A])

  private[this] def absorb(that: Set[A]): Unit = {
    items = that.items
    buckets = that.buckets
    len = that.len
    used = that.used
    mask = that.mask
    limit = that.limit
  }

  def +=(item: A): Boolean = add(item)

  final def add(item: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 3) {
        if (items(j) == item)
          false
        else
          loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      } else if (status == 2 && apply(item)) {
        false
      } else {
        items(j) = item
        buckets(j) = 3
        len += 1
        if (status == 0) {
          used += 1
          if (used > limit) grow()
        }
        true
      }
    }
    val i = item.## & 0x7fffffff
    loop(i, i)
  }

  def ++(items: Iterable[A]): Set[A] = {
    val set = copy
    set ++= items
    set
  }

  def ++=(items: Iterable[A]): Unit = addAll(items)

  def addAll(items: Iterable[A]): Unit = items.foreach(add)

  def remove(item: A): Boolean = this -= item

  final def -=(item: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && items(j) == item) {
        buckets(j) = 2
        len -= 1
        if ((len >> 2) < used) shrink()
        true
      } else if (status == 0) {
        false
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = item.## & 0x7fffffff
    loop(i, i)
  }

  final def update(item: A, b: Boolean) =
    if (b) add(item) else remove(item)

  def foreach(f: A => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) f(items(i))
    }

  def map[@sp(Int, Long, Double, AnyRef) B: ClassTag](f: A => B): Set[B] = {
    val out = Set.empty[B]
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) out.add(f(items(i)))
    }
    out
  }

  def fold[@sp(Int, Long, Double, AnyRef) B](init: B)(f: (B, A) => B): B = {
    var result = init
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) result = f(result, items(i))
    }
    result
  }

  final def grow(): Unit1[A] = {
    val next = buckets.length * (if (buckets.length < 10000) 4 else 2)
    val set = Set.ofSize[A](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) set += items(i)
    }
    absorb(set)
    new Unit1[A]
  }

  final def shrink(): Unit1[A] = {
    val set = Set.ofSize[A](len * 2)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) set += items(i)
    }
    absorb(set)
    new Unit1[A]
  }

  // For a lot of the following methods, there are size tests to try
  // to make sure we're looping over the smaller of the two
  // sets. Some things to keep in mind:
  //
  // 1. System.arraycopy is way faster than a loop
  // 2. We want to avoid copying a large object and then shrinking it
  // 3. Methods ending in = are not symmetric, the others are
  //
  // So where possible we'd like to be looping over a smaller set,
  // doing membership tests against a larger set.

  def union(rhs: Set[A]): Set[A] = lhs | rhs

  /**
   * Return new set which is the union of lhs and rhs.
   * 
   * The new set will contain all members of lhs and rhs.
   */
  def |(rhs: Set[A]): Set[A] =
    if (lhs.size >= rhs.size) {
      val out = lhs.copy
      out |= rhs
      out
    } else {
      val out = rhs.copy
      out |= lhs
      out
    }

  /**
   * Add all members of rhs to lhs.
   */
  def |=(rhs: Set[A]): Unit =
    if (lhs.size >= rhs.size) {
      cfor(0)(_ < rhs.buckets.length, _ + 1) { i =>
        if (rhs.buckets(i) == 3) lhs += rhs.items(i)
      }
    } else {
      val out = rhs.copy
      out |= lhs
      lhs.absorb(out)
    }

  def intersection(rhs: Set[A]): Set[A] = this & rhs

  def &(rhs: Set[A]): Set[A] =
    if (lhs.size <= rhs.size) {
      val out = lhs.copy
      out &= rhs
      out
    } else {
      val out = rhs.copy
      out &= lhs
      out
    }

  /**
   * Remove any member of this which is not in rhs.
   */
  def &=(rhs: Set[A]): Unit =
    if (lhs.size <= rhs.size) {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && !rhs(items(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
      if ((len >> 2) < used) shrink()
    } else {
      val out = rhs.copy
      out &= lhs
      lhs.absorb(out)
    }

  def --=(rhs: Set[A]): Unit =
    if (lhs.size >= rhs.size) {
      cfor(0)(_ < rhs.buckets.length, _ + 1) { i =>
        if (rhs.buckets(i) == 3) lhs -= rhs.items(i)
      }
    } else {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && rhs(items(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    }

  def difference(rhs: Set[A]): Set[A] = lhs -- rhs
  
  def --(rhs: Set[A]): Set[A] = {
    val out = lhs.copy
    out --= rhs
    out
  }
  
  def count(p: A => Boolean) =
    fold(0)((n, a) => if (p(a)) n + 1 else n)

  def forall(p: A => Boolean) =
    loopWhile(p) == -1

  def exists(p: A => Boolean) =
    loopUntil(p) != -1

  def find(p: A => Boolean): Option[A] =
    loopUntil(p) match {
      case i if i >= 0 => Some(items(i))
      case _ => None
    }

  def findAll(p: A => Boolean): Set[A] = {
    val out = Set.empty[A]
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) out += items(i)
    }
    out
  }

  def partition(p: A => Boolean): (Set[A], Set[A]) = {
    val no = Set.empty[A]
    val yes = Set.empty[A]
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val a = items(i)
        if (p(a)) yes += a else no += a
      }
    }
    (no, yes)
  }

  def loopWhile(p: A => Boolean): Int = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3 && !p(items(i))) return i
    }
    -1
  }

  def loopUntil(p: A => Boolean): Int = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3 && p(items(i))) return i
    }
    -1
  }

  def iterator: Iterator[A] = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    new Iterator[A] {
      var index = i
      def hasNext: Boolean = index < buckets.length
      def next: A = {
        val item = items(index)
        index += 1
        while (index < buckets.length && buckets(index) != 3) index += 1
        item
      }
    }
  }

  def toIterable: Iterable[A] =
    new Iterable[A] {
      override def size: Int = lhs.size
      def iterator: Iterator[A] = lhs.iterator
      override def foreach[U](f: A => U): Unit = lhs.foreach(a => f(a))
    }
}


object Set {
  def empty[@sp A: ClassTag] = new Set(new Array[A](8), new Array[Byte](8), 0, 0)

  def ofSize[@sp A: ClassTag](n: Int) = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw DeboxOverflowError(n)
      case 0 => 8
      case n => n
    }
    new Set(new Array[A](sz), new Array[Byte](sz), 0, 0)
  }

  def apply[@sp A: ClassTag](as: A*): Set[A] = fromIterable(as)

  def fromArray[@sp A: ClassTag](as: Array[A]): Set[A] = {
    val n = spire.math.max(8, as.length + as.length / 2)
    val set = ofSize[A](n)
    cfor(0)(_ < as.length, _ + 1)(i => set.add(as(i)))
    set
  }

  def fromIterable[@sp A: ClassTag](as: Iterable[A]): Set[A] = {
    val set = empty[A]
    set ++= as
    set
  }
}

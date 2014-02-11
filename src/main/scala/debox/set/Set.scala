package debox.set

import debox._

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.syntax.cfor._

class InvalidSizes(k: Int, v: Int) extends Exception("%s, %s" format (k, v))
class SetOverflow(n: Int) extends Exception("size %s exceeds max" format n)

object Set {
  def empty[@sp A: ClassTag] = new Set(new Array[A](8), new Array[Byte](8), 0, 0)

  def ofSize[@sp A: ClassTag](n: Int) = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw new SetOverflow(n)
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

final class Set[@sp A] protected[debox](as: Array[A], bs: Array[Byte], n: Int, u: Int)(implicit val ct: ClassTag[A]) extends Function1[A, Boolean] {

  // set machinery
  var items: Array[A] = as
  var buckets: Array[Byte] = bs
  var len: Int = n
  var used: Int = u

  // hashing internals
  var mask = items.length - 1 // size-1, used for hashing
  var limit = (items.length * 0.65).toInt // point at which we should grow

  override def equals(that: Any): Boolean = that match {
    case that: Set[_] =>
      size == that.size && ct == that.ct && forall(that.asInstanceOf[Set[A]].apply)
    case _ =>
      false
  }

  override def hashCode: Int = fold(0)(_ ^ _.##)

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Set(")
    var first = true
    foreach { a =>
      if (first) first = false else sb.append(", ")
      sb.append(a.toString)
    }
    sb.append(")")
    sb.toString
  }

  final def size: Int = len

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

  def -=(item: A): Boolean = remove(item)

  final def remove(item: A): Boolean = {
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

  def foreach(f: A => Unit) {
    @inline @tailrec def loop(i: Int, count: Int, limit: Int): Unit =
      if (count >= limit) ()
      else if (buckets(i) == 3) { f(items(i)); loop(i + 1, count + 1, limit) }
      else loop(i + 1, count, limit)
    loop(0, 0, size)
  }

  def map[@sp(Int, Long, Double, AnyRef) B: ClassTag](f: A => B): Set[B] = {
    val out = Set.empty[B]
    foreach(a => out.add(f(a)))
    out
  }

  def fold[@sp(Int, Long, Double, AnyRef) B](init: B)(f: (B, A) => B): B = {
    var result = init
    cfor(0)(_ < items.length, _ + 1) { i =>
      if (buckets(i) == 3) result = f(result, items(i))
    }
    result
  }

  final def grow(): Unit1[A] = {
    val next = items.length * (if (items.length < 10000) 4 else 2)
    val set = Set.ofSize[A](next)
    cfor(0)(_ < items.length, _ + 1) { i =>
      if (buckets(i) == 3) set += items(i)
    }
    absorb(set)
    new Unit1[A]
  }

  final def shrink(): Unit1[A] = {
    val set = Set.ofSize[A](len * 2)
    cfor(0)(_ < items.length, _ + 1) { i =>
      if (buckets(i) == 3) set += items(i)
    }
    absorb(set)
    new Unit1[A]
  }

  def |(that: Set[A]): Set[A] = union(that)

  def union(that: Set[A]): Set[A] = {
    if (size > that.size) return that.union(this)
    val out = that.copy
    foreach(out.add)
    out
  }

  def &(that: Set[A]): Set[A] = intersection(that)
  
  def intersection(that: Set[A]): Set[A] = {
    if (size < that.size) return that.intersection(this)
    val out = Set.empty[A]
    foreach(a => if (that(a)) out.add(a))
    out
  }

  def --=(that: Set[A]): Unit =
    if (this.size > that.size) {
      that.foreach(remove)
    } else {
      cfor(0)(_ < items.length, _ + 1) { i =>
        if (buckets(i) == 3 && !that(items(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    }

  def --(that: Set[A]): Set[A] = difference(that)
  
  def difference(that: Set[A]): Set[A] = {
    val out = Set.empty[A]
    foreach(a => if (!that(a)) out.add(a))
    out
  }
  
  def extend(that: Set[A]): Unit =
    foreach(add)

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
    foreach(a => if (p(a)) out.add(a))
    out
  }

  def partition(p: A => Boolean): (Set[A], Set[A]) = {
    val no = Set.empty[A]
    val yes = Set.empty[A]
    foreach(a => if (p(a)) yes.add(a) else no.add(a))
    (no, yes)
  }

  def loopWhile(p: A => Boolean): Int = {
    @inline @tailrec def loop(i: Int, limit: Int): Int = {
      if (buckets(i) == 3 && !p(items(i))) i
      else if (i < limit) loop(i + 1, limit)
      else -1
    }
    loop(0, items.length - 1)
  }

  def loopUntil(p: A => Boolean): Int = {
    @inline @tailrec def loop(i: Int, limit: Int): Int = {
      if (buckets(i) == 3 && p(items(i))) i
      else if (i < limit) loop(i + 1, limit)
      else -1
    }
    loop(0, items.length - 1)
  }
}

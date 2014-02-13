package debox

// TODO:
// 1. ensure size is always a power of two (maybe?)
// 2. consider trying to reduce arraycopying via checks
// 3. make sure everything is implemented directly

import scala.reflect.ClassTag

import language.experimental.macros

import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => spec}

import spire.syntax.cfor._

object Buffer {
  def empty[@spec A: ClassTag]: Buffer[A] =
    ofSize[A](8)

  def ofSize[@spec A: ClassTag](n: Int): Buffer[A] =
    new Buffer(new Array[A](Util.nextPowerOfTwo(n)), 0)

  def fill[@spec A: ClassTag](n: Int)(a: A): Buffer[A] =
    unsafe(Array.fill(n)(a))

  def unsafe[@spec A: ClassTag](as: Array[A]): Buffer[A] =
    new Buffer(as, as.length)

  def apply[A: ClassTag](as: A*): Buffer[A] =
    unsafe(as.toArray)
    
  def fromArray[@spec A: ClassTag](as: Array[A]): Buffer[A] =
    new Buffer(as.clone, as.length)

  def fromIterable[@spec A: ClassTag](as: Iterable[A]): Buffer[A] =
    unsafe(as.toArray)
}

final class Buffer[@spec A](as: Array[A], n: Int)(implicit val ct: ClassTag[A]) {
  var elems: Array[A] = as
  var len: Int = n

  override def equals(that: Any): Boolean = that match {
    case b: Buffer[_] =>
      if (length != b.length || ct != b.ct) return false
      val buf = b.asInstanceOf[Buffer[A]]
      val limit = len
      cfor(0)(_ < limit, _ + 1) { i =>
        if (elems(i) != buf.elems(i)) return false
      }
      true
    case _ =>
      false
  }

  override def hashCode: Int = {
    var code: Int = 0xf457f00d
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i => code = (code * 19) + elems(i).## }
    code
  }

  override def toString =
    if (length == 0) {
      "Buffer()"
    } else {
      val limit = len
      val sb = new StringBuilder()
      sb.append("Buffer(")
      sb.append(apply(0))
      cfor(1)(_ < limit, _ + 1) { i =>
        sb.append(",")
        sb.append(elems(i))
      }
      sb.append(")")
      sb.toString
    }

  final def copy: Buffer[A] = new Buffer(elems.clone, len)

  private[this] def absorb(that: Buffer[A]): Unit = {
    elems = that.elems
    len = that.len
  }

  def unsafeArray: Array[A] = elems

  def length: Int = len
  def isEmpty: Boolean = len == 0
  def nonEmpty: Boolean = len > 0

  def apply(i: Int): A = elems(i)

  def update(i: Int, a: A): Unit = elems(i) = a

  def +=(a: A): Unit = append(a)

  def append(a: A): Unit = {
    val n = len
    if (n >= elems.length) resize(nextSize(n))
    elems(n) = a
    len = n + 1
  }

  def insert(i: Int, a: A): Unit = {
    resizeIfNecessary(1)
    System.arraycopy(elems, i, elems, i + 1, len - i)
    elems(i) = a
    len += 1
  }

  def prepend(a: A): Unit = {
    resizeIfNecessary(1)
    System.arraycopy(elems, 0, elems, 1, len)
    elems(0) = a
    len += 1
  }

  def ++=(as: Array[A]): Unit = extend(as)

  def ++=(buf: Buffer[A]): Unit = extend(buf)

  def ++=(as: Iterable[A]): Unit = as.foreach(append)

  def extend(as: Array[A]): Unit = splice(len, as)

  def extend(buf: Buffer[A]): Unit = splice(len, buf)

  def extend(as: Iterable[A]): Unit = as.foreach(append)

  def splice(i: Int, as: Array[A]): Unit = {
    val n = as.length
    resizeIfNecessary(n)
    System.arraycopy(elems, i, elems, i + n, len - i)
    System.arraycopy(as, 0, elems, i, n)
    len += n
  }

  def splice(i: Int, as: Buffer[A]): Unit = {
    val n = as.length
    resizeIfNecessary(n)
    System.arraycopy(elems, i, elems, i + n, len - i)
    System.arraycopy(as.elems, 0, elems, i, n)
    len += n
  }

  def prependAll(as: Array[A]): Unit = splice(0, as)

  def prependAll(buf: Buffer[A]): Unit = splice(0, buf)

  private final def nextSize(x: Int): Int = {
    val n = x << 1
    if (n < 0) sys.error("overflow") else n
  }

  final def resizeIfNecessary(n: Int): Unit = {
    val goal = len + n
    var x = elems.length
    while (x < goal) x = nextSize(x)
    resize(x)
  }

  final def resize(n: Int): Unit = {
    val arr = new Array[A](n)
    System.arraycopy(elems, 0, arr, 0, len)
    elems = arr
  }

  def remove(i: Int): A = {
    val last = len - 1
    if (i < last) {
      System.arraycopy(elems, i + 1, elems, i, last - i)
    }
    val a = elems(last)
    elems(last) = null.asInstanceOf[A]
    len = last
    a
  }

  def pop: A = {
    val last = len - 1
    val a = elems(last)
    len = last
    a
  }

  def clear: Unit = absorb(Buffer.empty[A])

  def toArray: Array[A] =
    Util.alloc(elems, 0, len)

  def slice(i: Int, j: Int): Buffer[A] = {
    val n = j - i
    val arr = new Array[A](nextSize(n))
    System.arraycopy(elems, i, arr, 0, n)
    new Buffer(arr, n)
  }

  def reverse: Buffer[A] = {
    val arr = new Array[A](elems.length)
    var i = 0
    var j = len - 1
    val limit = len
    while (i < limit) {
      arr(j) = elems(i)
      i += 1
      j -= 1
    }
    new Buffer(arr, len)
  }

  def map[@spec B: ClassTag](f: A => B): Buffer[B] = {
    val arr = new Array[B](len)
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i => arr(i) = f(elems(i)) }
    new Buffer(arr, len)
  }

  def toList = {
    @tailrec def f(i: Int, as: List[A]): List[A] =
      if (i < 0) as else f(i - 1, elems(i) :: as)
    f(length - 1, Nil)
  }

  def toVector: Vector[A] = {
    import scala.collection.immutable.VectorBuilder
    val b = new VectorBuilder[A]
    b.sizeHint(len)
    cfor(0)(_ < len, _ + 1) { i => b += elems(i) }
    b.result
  }

  def foreach(f: Function[A, Unit]): Unit = {
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i => f(elems(i)) }
  }
}

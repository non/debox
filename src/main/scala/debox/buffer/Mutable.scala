package debox.buffer

import scala.reflect.ClassTag

import debox.Util
import scala.math.{min, max}
import scala.{specialized => spec}

object Mutable {
  def safe[@spec A:ClassTag](as:Array[A]) = new Mutable(as.clone, as.length)
  def unsafe[@spec A:ClassTag](as:Array[A]) = new Mutable(as, as.length)
  def apply[@spec A:ClassTag](as:Array[A]) = unsafe(as)
  def empty[@spec A:ClassTag] = unsafe(Array.empty[A])
  def ofDim[@spec A:ClassTag](n:Int) = unsafe(new Array[A](n))
  def fill[@spec A:ClassTag](n:Int)(a:A) = unsafe(Array.fill(n)(a))
}

final class Mutable[@spec A:ClassTag](as:Array[A], n:Int) extends Buffer[A] {
  protected[this] var elems:Array[A] = as
  protected[this] var len:Int = n

  def unsafeArray = elems

  def length = len
  def toArray = Util.alloc(elems, 0, len)
  def slice(i:Int, j:Int) = Mutable.unsafe(Util.alloc(elems, i, j - i))
  def reverse = {
    val as = new Array[A](elems.length)
    var i = 0
    var j = len - 1
    val limit = len
    while (i < limit) {
      as(j) = elems(i)
      i += 1
      j -= 1
    }
    Mutable(as)
  }
  def map[@spec B:ClassTag](f:A => B) = Mutable.unsafe(as.map(f))
  def apply(i:Int) = elems(i)
  def get(i:Int) = if (i < len) Some(elems(i)) else None
  def update(i:Int, a:A):Unit = elems(i) = a
  def toImmutable = Immutable(as.clone)
  def toImmutableUnsafe = Immutable(as)
  def toMutable = new Mutable(as.clone, n)
  def toMutableUnsafe = this

  protected[this] def resizeIfNecessary(n:Int):Unit = {
    val x = elems.length
    if (len + n > x) {
      val x2 = if (x < 4) 8 else if (x <= 0x3fffffff) x * 2 else Int.MaxValue
      val as = new Array[A](x2)
      System.arraycopy(elems, 0, as, 0, len)
      elems = as
    }
  }

  def append(a:A):Unit = insert(len, a)
  def prepend(a:A):Unit = insert(0, a)
  def insert(i:Int, a:A):Unit = {
    resizeIfNecessary(1)
    System.arraycopy(elems, i, elems, i + 1, len - i)
    elems(i) = a
    len += 1
  }

  def prextend(as:Array[A]):Unit = splice(0, as)
  def extend(as:Array[A]):Unit = splice(len, as)
  def splice(i:Int, as:Array[A]):Unit = {
    val n = as.length
    resizeIfNecessary(as.length)
    System.arraycopy(elems, i, elems, i + as.length, len - i)
    System.arraycopy(as, 0, elems, i, as.length)
    len += as.length
  }

  def remove(i:Int):Unit = {
    System.arraycopy(elems, i + 1, elems, i, len - i - 1)
    len -= 1
  }
  def pop(i:Int):A = {
    val a = elems(i)
    remove(i)
    a
  }
}

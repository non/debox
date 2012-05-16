package debox.buffer

import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => spec}

object Buffer {
  def mapBufferToArray[@spec A, @spec B:Manifest](src:Buffer[A], s1:Int, len:Int)(f:A => B) = {
    val bs = Array.ofDim[B](len)
    var i = s1
    var j = 0
    var k = s1 + len
    while (i < k) {
      bs(j) = f(src(i))
      i += 1
      j += 1
    }
    bs
  }
}

trait Buffer[@spec A] {
  def toArray:Array[A]
  def toList = {
    @tailrec def f(i:Int, as:List[A]):List[A] = if (i < 0) as else f(i - 1, apply(i) :: as)
    f(length - 1, Nil)
  }
  def toVector:Vector[A] = Vector(toArray:_*)

  def length:Int

  def apply(i:Int):A
  def get(i:Int):Option[A]

  def slice(i:Int, j:Int):Buffer[A]
  def reverse: Buffer[A]

  def map[@spec B:Manifest](f:A => B): Buffer[B]

  def foreach(f:Function[A, Unit]): Unit = {
    var i = 0
    val len = length
    while (i < len) {
      f(apply(i))
      i += 1
    }
  }

  override def toString = {
    val sb = new StringBuilder()
    sb.append("Buffer(")
    if (length > 0) {
      sb.append(apply(0))
      var i = 1
      while (i < length) {
        sb.append(",")
        sb.append(apply(i))
        i += 1
      }
    }
    sb.append(")")
    sb.toString
  }

  def toImmutable:Immutable[A]
  def toImmutableUnsafe:Immutable[A]

  def toMutable:Mutable[A]
  def toMutableUnsafe:Mutable[A]
}

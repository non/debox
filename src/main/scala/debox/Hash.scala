package debox

import scala.{specialized => spec}

sealed trait Hash[@spec A] {
  def zero:A
  def hash(a:A):Int
}

trait UnitHash extends Hash[Unit] {
  def zero = ()
  def hash(a:Unit) = -1
}

trait BooleanHash extends Hash[Boolean] {
  def zero = false
  def hash(a:Boolean) = if (a) 1231 else 1237
}

trait ByteHash extends Hash[Byte] {
  def zero = 0.toByte
  def hash(a:Byte) = a.toInt
}

trait ShortHash extends Hash[Short] {
  def zero = 0.toShort
  def hash(a:Short) = a.toInt
}

trait IntHash extends Hash[Int] {
  def zero = 0
  def hash(a:Int) = a
}

trait LongHash extends Hash[Long] {
  def zero = 0L
  def hash(a:Long) = a.toInt
}

trait FloatHash extends Hash[Float] {
  def zero = 0.0F
  def hash(a:Float) = java.lang.Float.floatToRawIntBits(a)
}

trait DoubleHash extends Hash[Double] {
  def zero = 0.0
  def hash(a:Double) = java.lang.Double.doubleToRawLongBits(a).toInt
}

//trait AnyRefHash extends Hash[AnyRef] {
//  def zero = null.asInstanceOf[AnyRef]
//  def hash(a:AnyRef) = a.hashCode
//}

class RefHash[A] extends Hash[A] {
  def zero = null.asInstanceOf[A]
  def hash(a:A) = a.hashCode
}

object Hash {
  @inline final def apply[@spec A](implicit ev:Hash[A]) = ev

  implicit object UnitHash extends UnitHash
  implicit object BooleanHash extends BooleanHash
  implicit object ByteHash extends ByteHash
  implicit object ShortHash extends ShortHash
  implicit object IntHash extends IntHash
  implicit object LongHash extends LongHash
  implicit object FloatHash extends FloatHash
  implicit object DoubleHash extends DoubleHash
  //implicit object AnyRefHash extends AnyRefHash
  implicit def refHash[A <: AnyRef] = new RefHash[A]
}

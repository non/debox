package debox

/**
 * Unset is designed to communicate whether there is a "reserved" value of a
 * given type A that won't normally be used (e.g. null).
 * 
 * While this seems unsavory, it turns out to be really important when trying
 * to build and work with specialized array-based collections. When an unset
 * value is not available, extra work must be done to track whether a given
 * array element is "set" or not.
 * 
 * By default the only implicit available is "anyHasNoUnset" which provides a
 * NoUnset[A] instance. Unset.mark(a) can be used to create implicit instances,
 * and an implicit which uses null can be imported from Unset.Implicits.
 *
 * This class is not specialized... these methods should not be used in the
 * "critical path" but rather to determine which type of objects to build.
 */

trait Unset[A] {
  def get:Option[A]
  def exists:Boolean = get.isDefined
}

case class NoUnset[A]() extends Unset[A] { def get:Option[A] = None }
case class MarkedUnset[A](nul:A) extends Unset[A] { def get:Option[A] = Some(nul) }

object Unset extends LowPriorityImplicits {
  def apply[A](implicit u:Unset[A]):Unset[A] = u
  def marked[A](a:A):Unset[A] = MarkedUnset(a)

  object Implicits {
    implicit def anyrefHasNullUnset[A <: AnyRef]:Unset[A] = MarkedUnset(null.asInstanceOf[A])
  }
}

trait LowPriorityImplicits {
  implicit def anyHasNoUnset[A]:Unset[A] = NoUnset[A]()
}

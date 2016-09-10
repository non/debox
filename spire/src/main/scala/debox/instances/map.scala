package debox
package instances

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra._

trait MapInstances {

  /**
   * Provide an Eq[Map[A, B]] instance.
   *
   * Since Maps are so reliant on equality, and use hash codes
   * internally, the default equality is used to compare elements.
   */
  implicit def eqv[A, B] =
    new Eq[Map[A, B]] {
      def eqv(lhs: Map[A, B], rhs: Map[A, B]): Boolean = lhs == rhs
    }

  /**
   * Provide a Monoid[Map[A, B]].
   *
   * The maps are combined key-by-key, using |+| to merge values if
   * necessary.
   */
  implicit def monoid[@sp A: ClassTag, @sp B: ClassTag: Monoid] =
    new Monoid[Map[A, B]] {
      def id: Map[A, B] =
        Map.empty[A, B]
      def op(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] =
        syntax.map.MapOps(lhs) ++ rhs
    }

  /**
   * Provide an AdditiveMonoid[Map[A, B]].
   *
   * The maps are combined key-by-key, using + to merge values if
   * necessary.
   */
  implicit def additiveMonoid[@sp A: ClassTag, @sp B: ClassTag: AdditiveMonoid] =
    new AdditiveMonoid[Map[A, B]] {
      implicit val m: Monoid[B] = implicitly[AdditiveMonoid[B]].additive
      def zero: Map[A, B] =
        Map.empty[A, B]
      def plus(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] =
        syntax.map.MapOps(lhs) ++ rhs
    }

  /**
   * Provide an MultiplicativeMonoid[Map[A, B]].
   *
   * The maps are combined key-by-key, using * to merge values if
   * necessary.
   */
  implicit def multiplicativeMonoid[@sp A: ClassTag, @sp B: ClassTag: MultiplicativeMonoid] =
    new MultiplicativeMonoid[Map[A, B]] {
      implicit val m: Monoid[B] = implicitly[MultiplicativeMonoid[B]].multiplicative
      def one: Map[A, B] =
        Map.empty[A, B]
      def times(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] =
        syntax.map.MapOps(lhs) ++ rhs
    }
}

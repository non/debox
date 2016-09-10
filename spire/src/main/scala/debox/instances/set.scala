package debox
package instances

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra.{Eq, CMonoid}

trait SetInstances {

  /**
   * Provide a Eq[Set[A]] instance.
   *
   * Since Sets are so reliant on equality, and use hash codes
   * internally, the default equality is used to compare elements.
   */
  implicit def setEq[A]: Eq[Set[A]] =
    new Eq[Set[A]] {
      def eqv(lhs: Set[A], rhs: Set[A]): Boolean = lhs == rhs
    }

  /**
   * Provide a CMonoid[Set[A]] instance.
   *
   * Since element order is irrelevant, union is a commutative
   * operation. The empty set is the identity element.
   */
  implicit def setCMonoid[@sp A: ClassTag]: CMonoid[Set[A]] =
    new CMonoid[Set[A]] {
      def id: Set[A] = Set.empty[A]
      def op(lhs: Set[A], rhs: Set[A]): Set[A] = lhs | rhs
    }
}

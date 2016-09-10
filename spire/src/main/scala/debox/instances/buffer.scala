package debox
package instances

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

trait BufferInstances extends BufferInstances1 {

  /**
   * Provide an Order[Buffer[A]] instance.
   *
   * The empty buffer is considered "less-than" any non-empty buffer,
   * and non-empty buffers are compared lexicographically. Elemens are
   * compared using the given Order[A].
   */
  implicit def bufferOrder[@sp A: Order]: Order[Buffer[A]] =
    new Order[Buffer[A]] {
      def compare(lhs: Buffer[A], rhs: Buffer[A]): Int = {
        val (minLength, lastResult) =
          if (lhs.length < rhs.length) (lhs.length, -1)
          else if (lhs.length == rhs.length) (lhs.length, 0)
          else (rhs.length, 1)

        cfor(0)(_ < minLength, _ + 1) { i =>
          val n = lhs.elems(i) compare rhs.elems(i)
          if (n != 0) return n
        }
        lastResult
      }
    }

  /**
   * Provides a Monoid[Buffer[A]] instance.
   *
   * The identity value is an empty buffer, and the ++ operator is
   * used to concatenate two buffers without modifying their contents.
   */
  implicit def bufferMonoid[@sp A: ClassTag]: Monoid[Buffer[A]] =
    new Monoid[Buffer[A]] {
      def id: Buffer[A] = Buffer.empty[A]
      def op(lhs: Buffer[A], rhs: Buffer[A]): Buffer[A] = lhs ++ rhs
    }

  /**
   * AdditiveMonoid[Buffer[A]] which uses addition to combine buffers
   * in a pairwise fashion. The result will be the length of the
   * longer argument.
   */
  implicit def bufferAdditiveMonoid[@sp A: ClassTag: AdditiveMonoid]: AdditiveMonoid[Buffer[A]] =
    new AdditiveMonoid[Buffer[A]] {
      def zero: Buffer[A] = Buffer.empty[A]
      def plus(lhs: Buffer[A], rhs: Buffer[A]): Buffer[A] =
        if (lhs.length >= rhs.length) {
          val out = lhs.copy
          val es = rhs.elems
          val limit = rhs.length
          cfor(0)(_ < limit, _ + 1) { i =>
            out(i) = out(i) + es(i)
          }
          out
        } else {
          val out = rhs.copy
          val es = lhs.elems
          val limit = lhs.length
          cfor(0)(_ < limit, _ + 1) { i =>
            out(i) = es(i) + out(i)
          }
          out
        }
    }

  /**
   * MultiplicativeSemigroup[Buffer[A]] which uses multiplication to
   * combine buffers in a pairwise fashion. The result will be the
   * length of the shorter argument.
   */
  implicit def bufferMultiplicativeSemigroup[@sp A: ClassTag: MultiplicativeSemigroup]: MultiplicativeSemigroup[Buffer[A]] =
    new MultiplicativeSemigroup[Buffer[A]] {
      def times(lhs: Buffer[A], rhs: Buffer[A]): Buffer[A] =
        if (lhs.length <= rhs.length) {
          val out = lhs.copy
          val es = rhs.elems
          val limit = lhs.length
          cfor(0)(_ < limit, _ + 1) { i =>
            out(i) = out(i) * es(i)
          }
          out
        } else {
          val out = rhs.copy
          val es = lhs.elems
          val limit = rhs.length
          cfor(0)(_ < limit, _ + 1) { i =>
            out(i) = es(i) * out(i)
          }
          out
        }
    }
}

trait BufferInstances1 {

  /**
   * Provide an Eq[Buffer[A]] instance.
   *
   * This method uses the given Eq[A] to compare each element
   * pairwise. Buffers are required to be the same length.
   */
  implicit def bufferEq[@sp A: Eq]: Eq[Buffer[A]] =
    new Eq[Buffer[A]] {
      def eqv(lhs: Buffer[A], rhs: Buffer[A]): Boolean = {
        if (lhs.length != rhs.length) return false
        cfor(0)(_ < lhs.elems.length, _ + 1) { i =>
          if (lhs.elems(i) =!= rhs.elems(i)) return false
        }
        true
      }
    }
}

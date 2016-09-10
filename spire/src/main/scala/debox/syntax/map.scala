package debox
package syntax

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra.{Monoid, CMonoid}
import spire.syntax.all._

trait MapSyntax {

  implicit class MapOps[@sp(Int, Long, AnyRef) A, @sp B](lhs: Map[A, B]) {

    /**
     * Add the items from the given map to this map, combining values
     * using the provided Monoid[B].
     * 
     * This is an O(m) operation, where m is the size of the rhs.
     */
    final def ++=(rhs: Map[A, B])(implicit ev: Monoid[B]): Unit = {
      val z = ev.id
      rhs.foreach { (k, v) => lhs(k) = lhs.getOrElse(k, z) |+| v }
    }

    /**
     * Add the items from the given iterable to this map, combining
     * values using the provided Monoid[B].
     * 
     * This is an O(m) operation, where m is the size of the rhs.
     */
    final def ++=(rhs: Iterable[(A, B)])(implicit ev: Monoid[B]): Unit = {
      val z = ev.id
      rhs.foreach { case (k, v) => lhs(k) = lhs.getOrElse(k, z) |+| v }
    }

    /**
     * Combine the two maps into a new map, using the provided
     * CMonoid[B] to merge values for the same key.
     * 
     * This is an O(m+n) operation, where m and n are the size of the
     * maps.
     */
    final def ++(rhs: Map[A, B])(implicit ev: Monoid[B]): Map[A, B] = {
      val out = lhs.copy
      out ++= rhs
      out
    }

    /**
     * Transform this map into another map.
     * 
     * This method uses a function to go from an existing key to a new
     * key, and will add the new key with the existing value to the new
     * map.
     * 
     * If multiple invocations of the provided function return values
     * with the same key, the given commutative monoid will be used to
     * combine the values. This commutativity ensures that maps with the
     * same keys and values will always produce the same
     * result. However, if duplicate keys are produced, the resulting
     * map will be smaller than this map.
     * 
     * On average, this is an O(n) operation, where n is the size of the
     * map.
     */
    final def mapKeys[@sp (Int, Long, AnyRef) C: ClassTag](f: A => C)(implicit ev: CMonoid[B], ct: ClassTag[B]): Map[C, B] = {
      val result = Map.ofSize[C, B](lhs.size)
      val z = ev.id
      lhs.foreach { (a, b) =>
        val c = f(a)
        result(c) = result.getOrElse(c, z) |+| b
      }
      result.compact
      result
    }

    /**
     * Transform this map into another map.
     * 
     * This method uses a function to go from key and value to a pair,
     * which will be added to the map.
     * 
     * If multiple invocations of the provided function return values
     * with the same key, the given commutative monoid will be used to
     * combine the values. This commutativity ensures that maps with the
     * same keys and values will always produce the same
     * result. However, if duplicate keys are produced, the resulting
     * map will be smaller than this map.
     * 
     * On average, this is an O(n) operation, where n is the size of the
     * map.
     */
    final def mapItemsToMap[@sp (Int, Long, AnyRef) C: ClassTag, @sp D: ClassTag](f: (A, B) => (C, D))(implicit ev: CMonoid[D]): Map[C, D] = {
      val result = Map.ofSize[C, D](lhs.size)
      val z = ev.id
      lhs.foreach { (a, b) =>
        val (c, d) = f(a, b)
        result(c) = result.getOrElse(c, z) |+| d
      }
      result.compact
      result
    }
  }
}

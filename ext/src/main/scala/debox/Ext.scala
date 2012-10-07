package debox

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.{specialized => spec}

import language.experimental.macros

class MapOps[A, B](m: map.Map[A, B]) {
  def foreach_(f: (A, B) => Unit): Unit = macro Macros.foreach_[A, B]
}

object Ext {
  implicit def map2Ops[A, B](m: map.Map[A, B]) = new MapOps(m)
}

object Macros {
  final def foreach_[A, B](c:Context)(f: c.Expr[(A, B) => Unit]): c.Expr[Unit] = {
    import c.universe._
    val m = c.prefix.tree match {
      case Apply(TypeApply(_, _), List(m)) => c.Expr[map.Map[A, B]](m)
      case t => sys.error("bad tree %s" format t)
    }

    val tree = c.universe.reify {
      import scala.annotation.tailrec

      val buckets = m.splice.getBuckets
      val keys = m.splice.keys
      val vals = m.splice.vals
      @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
        if (((b >> shift) & 3) == 3) {
          f.splice(keys(i), vals(i))
          if (shift < 30) inner(i + 1, b, shift + 2, count + 1) else count + 1
        } else {
          if (shift < 30) inner(i + 1, b, shift + 2, count) else count
        }
      }
  
      @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int) {
        if (count < len) outer(i + 16, k + 1, inner(i, buckets(k), 0, count), len)
      }
      outer(0, 0, 0, m.splice.length)
    }

    new debox.map.Inliner[c.type](c).inlineAndReset(tree)

    tree
  }
}

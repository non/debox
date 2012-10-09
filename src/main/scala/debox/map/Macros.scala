package debox.map

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.{specialized => spec}

import language.experimental.macros

object Macros {
  def unpack[A, B](c:Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(m)), List(eva, evb)) =>
        (c.Expr[Map[A, B]](m), c.Expr[ClassTag[A]](eva), c.Expr[ClassTag[B]](evb))
      case t => sys.error("bad tree %s" format t)
    }
  }

  final def foreach[A, B](c:Context)(f: c.Expr[(A, B) => Unit]): c.Expr[Unit] = {
    import c.universe._
    val (m, eva, evb) = unpack(c)

    val expr = reify {
      import scala.annotation.tailrec
      
      val buckets = m.splice.getBuckets
      val keys = m.splice.keys
      val vals = m.splice.vals
      
      @inline @tailrec
      def loop(i: Int, count: Int, limit: Int) {
        val c = if (buckets(i) == 3) {
          f.splice(keys(i), vals(i))
          count + 1
        } else {
          count
        }
        if (c < limit) loop(c, i + 1, limit)
      }
      loop(0, 0, m.splice.length - 1)
    }

    new debox.Inliner[c.type](c).inlineAndReset(expr)
  }
}

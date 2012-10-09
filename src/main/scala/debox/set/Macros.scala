package debox.set

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.{specialized => spec}

import language.experimental.macros

object Macros {
  def unpack[A](c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(s)), List(ev)) =>
        (c.Expr[Set[A]](s), c.Expr[ClassTag[A]](ev))
      case t => {
        sys.error("bad tree %s (%s)" format (t, t.getClass.getName))
      }
    }
  }

  final def foreach[A](c:Context)(f: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._
    val (s, ev) = unpack(c)

    val expr = reify {
      import scala.annotation.tailrec

      val buckets = s.splice.getBuckets
      val items = s.splice.items

      @inline @tailrec
      def loop(i: Int, count: Int, limit: Int) {
        val c = if (buckets(i) == 3) { f.splice(items(i)); count + 1 } else count
        if (c < limit) loop(c, i + 1, limit)
      }
      loop(0, 0, s.splice.length - 1)
    }

    new debox.Inliner[c.type](c).inlineAndReset(expr)
  }
}

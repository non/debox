package debox

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.{specialized => spec}

import debox.map.Map
import debox.set.Set

import language.experimental.macros

object Macros {
  final def mapForeach[A, B](c:Context)(f: c.Expr[(A, B) => Unit]): c.Expr[Unit] = {
    import c.universe._
    val m = c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(m)), _) => c.Expr[Map[A, B]](m)
      //case Apply(TypeApply(_, _), List(m)) => c.Expr[Map[A, B]](m)
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
  }

  final def setForeach[A](c:Context)(f: c.Expr[A => Unit]): c.Expr[Unit] = {
    import c.universe._
    val s = c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(s)), _) => c.Expr[Set[A]](s)
      case t => {
        sys.error("bad tree %s (%s)" format (t, t.getClass.getName))
      }
    }

    val tree = c.universe.reify {
      import scala.annotation.tailrec

      val buckets = s.splice.getBuckets
      val items = s.splice.items
      @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
        if (((b >> shift) & 3) == 3) {
          f.splice(items(i))
          if (shift < 30) inner(i + 1, b, shift + 2, count + 1) else count + 1
        } else {
          if (shift < 30) inner(i + 1, b, shift + 2, count) else count
        }
      }
  
      @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int) {
        if (count < len) outer(i + 16, k + 1, inner(i, buckets(k), 0, count), len)
      }
      outer(0, 0, 0, s.splice.length)
    }

    new debox.map.Inliner[c.type](c).inlineAndReset(tree)
  }

  final def setFold[A, T:c.AbsTypeTag](c:Context)(init: c.Expr[T])(f: c.Expr[(T, A) => T]): c.Expr[T] = {
    import c.universe._
    val s = c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(s)), _) => c.Expr[Set[A]](s)
      case t => {
        sys.error("bad tree %s (%s)" format (t, t.getClass.getName))
      }
    }

    val tree = c.universe.reify {
      import scala.annotation.tailrec

      val buckets = s.splice.getBuckets
      val items = s.splice.items
      @inline @tailrec def inner(i: Int, b: Int, shift: Int, t:T): T = {
        if (((b >> shift) & 3) == 3) {
          val tt = f.splice(t, items(i))
          if (shift < 30) inner(i + 1, b, shift + 2, tt) else tt
        } else {
          if (shift < 30) inner(i + 1, b, shift + 2, t) else t
        }
      }
  
      @inline @tailrec def outer(i: Int, k: Int, len: Int, t: T): T = {
        if (i < len) 
          outer(i + 16, k + 1, len, inner(i, buckets(k), 0, t))
        else
          t
      }
      outer(0, 0, items.length, init.splice)
    }

    new debox.map.Inliner[c.type](c).inlineAndReset(tree)
  }

  final def setLoopWhile[A](c:Context)(p: c.Expr[A => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val s = c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(s)), _) => c.Expr[Set[A]](s)
      case t => {
        sys.error("bad tree %s (%s)" format (t, t.getClass.getName))
      }
    }

    val tree = c.universe.reify {
      import scala.annotation.tailrec

      val buckets = s.splice.getBuckets
      val items = s.splice.items
      @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
        if (((b >> shift) & 3) == 3) {
          if (p.splice(items(i))) {
            if (shift < 30)
              inner(i + 1, b, shift + 2, count + 1)
            else
              count + 1
          } else {
            -1
          }
        } else {
          if (shift < 30) inner(i + 1, b, shift + 2, count) else count
        }
      }
  
      @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int): Boolean = {
        if (count < len) {
          val c = inner(i, buckets(k), 0, count)
          if (c < 0) false else outer(i + 16, k + 1, c, len)
        } else {
          true
        }
      }
      outer(0, 0, 0, s.splice.length)
    }

    new debox.map.Inliner[c.type](c).inlineAndReset(tree)
  }

  final def setLoopUntil[A](c:Context)(p: c.Expr[A => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val s = c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(s)), _) => c.Expr[Set[A]](s)
      case t => {
        sys.error("bad tree %s (%s)" format (t, t.getClass.getName))
      }
    }

    val tree = c.universe.reify {
      import scala.annotation.tailrec

      val buckets = s.splice.getBuckets
      val items = s.splice.items
      @inline @tailrec def inner(i: Int, b: Int, shift: Int, count: Int): Int = {
        if (((b >> shift) & 3) == 3) {
          if (p.splice(items(i))) {
            -1 
          } else if (shift < 30) {
            inner(i + 1, b, shift + 2, count + 1)
          } else {
            count + 1
          }
        } else {
          if (shift < 30) inner(i + 1, b, shift + 2, count) else count
        }
      }
  
      @inline @tailrec def outer(i: Int, k: Int, count: Int, len: Int): Boolean = {
        if (count < len) {
          val c = inner(i, buckets(k), 0, count)
          if (c < 0) false else outer(i + 16, k + 1, c, len)
        } else {
          true
        }
      }
      outer(0, 0, 0, s.splice.length)
    }

    new debox.map.Inliner[c.type](c).inlineAndReset(tree)
  }

}

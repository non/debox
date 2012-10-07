package debox.map

import debox._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.{specialized => spec}

import language.experimental.macros

class Inliner[C <: Context with Singleton](val c:C) {
  import c.universe._
  
  def die(msg:String) = c.abort(c.enclosingPosition, msg)

  def inlineAndReset[T](expr: c.Expr[T]): c.Expr[T] =
    c.Expr[T](c resetAllAttrs inlineApplyRecursive(expr.tree))

  def inlineApplyRecursive(tree: Tree): Tree = {
    val ApplyName = newTermName("apply")

    class InlineTerm(name:TermName, value:Tree) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(`name`) => value
        case _ => super.transform(tree)
      }
    }

    object InlineApply extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Apply(Select(Function(params, body), ApplyName), args) =>
          if (params.length != args.length)
            die("bad arity: %s vs %s" format (params.length, args.length))
          
          params.zip(args).foldLeft(body) {
            case (body, (ValDef(_, name, _, _), arg)) =>
              new InlineTerm(name, arg).transform(body)
          }
          
        case _ => super.transform(tree)
      }
    }

    InlineApply.transform(tree)
  }
}


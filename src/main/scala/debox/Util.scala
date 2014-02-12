package debox

import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => spec}

import language.experimental.macros

import scala.reflect.ClassTag
import scala.reflect.macros.Context

object Util {

  /**
   * Given a src array, an offset, and a len, return a new array which
   * copies the relevant area of src.
   * 
   * This method does not do any size or bounds checking.
   */
  def alloc[@spec A: ClassTag](src: Array[A], offset: Int, len: Int) = {
    val as = new Array[A](len)
    System.arraycopy(src, offset, as, 0, len)
    as
  }

  /**
   * Efficient alternative to Array.apply.
   *
   * "As seen on scala-internals!"
   */
  def array[A](as: A*) = macro arrayMacro[A]

  /**
   * Takes in something like:
   *   ArrayUtil.alloc[Int](11, 22, 33, 44)(ct)
   *
   * and builds a tree like:
   *   {
   *     val arr:Array[Int] = ct.newArray(4)
   *     arr.update(0, 11)
   *     arr.update(1, 22)
   *     arr.update(2, 33)
   *     arr.update(3, 44)
   *     arr
   *   }
   */
  def arrayMacro[A: c.WeakTypeTag](c: Context)(as: c.Expr[A]*): c.Expr[Array[A]] = {
    import c.mirror._
    import c.universe._
    def const(x: Int) = Literal(Constant(x))
  
    val n = as.length
    val arr = newTermName("arr")
  
    val mod = Ident(staticModule("scala.reflect.ClassTag"))
    val att = implicitly[c.WeakTypeTag[A]]
    val ct = Apply(mod, List(c.reifyRuntimeClass(att.tpe)))
  
    val create = Apply(Select(ct, "newArray"), List(const(n)))
    val arrtpe = TypeTree(implicitly[c.WeakTypeTag[Array[A]]].tpe)
    val valdef = ValDef(Modifiers(), arr, arrtpe, create)
  
    val updates = (0 until n).map {
      i => Apply(Select(Ident(arr), "update"), List(const(i), as(i).tree))
    }
  
    val exprs = Seq(valdef) ++ updates ++ Seq(Ident(arr))
    val block = Block(exprs: _*)
  
    c.Expr[Array[A]](block)
  }

  /**
   * Given a number n, this method returns n if n is a power-of-two.
   * 
   * Otherwise, it returns the smallest power-of-two larger than n.
   */
  def nextPowerOfTwo(n: Int): Int = {
    val x = java.lang.Integer.highestOneBit(n)
    if (x == n) n else x * 2
  }
}

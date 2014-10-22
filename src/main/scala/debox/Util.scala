package debox

import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => sp}

import language.experimental.macros

import scala.reflect.ClassTag
import scala.reflect.macros.Context

case class DeboxOverflowError(n: Int) extends Exception("size %s exceeds max" format n)
class KeyNotFoundException(k: String) extends Exception("key %s was not found" format k)

class Unit1[@sp A]

class Unit2[@sp A, @sp B]

object Util {

  /**
   * Given a src array, an offset, and a len, return a new array which
   * copies the relevant area of src.
   * 
   * This method does not do any size or bounds checking.
   */
  def alloc[@sp A: ClassTag](src: Array[A], offset: Int, len: Int) = {
    val as = new Array[A](len)
    System.arraycopy(src, offset, as, 0, len)
    as
  }

  def bufferMacro[A: c.WeakTypeTag](c: Context)(as: c.Expr[A]*): c.Expr[debox.Buffer[A]] = {
    import c.mirror._
    import c.universe._
    val arr = arrayMacro(c)(as: _*)
    c.Expr[debox.Buffer[A]](q"debox.unsafe($arr)")
  }

  /**
   * Efficient alternative to Array.apply.
   *
   * "As seen on scala-internals!"
   */
  def array[A](as: A*): Array[A] = macro arrayMacro[A]

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
  
    val n = as.length
    val tpe = implicitly[c.WeakTypeTag[A]].tpe
    val valdef = q"val arr = new Array[$tpe]($n)"
    val updates = as.toList.zipWithIndex.map { case (a, i) =>
      q"arr($i) = ${a.tree}"
    }

    c.Expr[Array[A]](Block(valdef :: updates, q"arr"))
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

import Specializable._

/** Dummy specialization annotation. Used to disable specialization by changing imports in Map. */

class dummysp extends scala.annotation.StaticAnnotation {
  def this(dummy: Any*) = this()
}

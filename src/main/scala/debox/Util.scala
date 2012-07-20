package debox

import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => spec}

object Util {
  def alloc[@spec A:Manifest](src:Array[A], s1:Int, len:Int) = {
    val as = Array.ofDim[A](len)
    System.arraycopy(src, s1, as, 0, len)
    as
  }
}

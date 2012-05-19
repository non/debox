package debox

import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => spec}

object Util {
  def alloc[@spec A:Manifest](src:Array[A], s1:Int, len:Int) = {
    val as = Array.ofDim[A](len)
    copy(src, as, s1, 0, len)
    as
  }

  def copy[@spec A](src:Array[A], dst:Array[A], s1:Int, d1:Int, len:Int) {
    System.arraycopy(src, s1, dst, d1, len)
  }

  def rcopy[@spec A](src:Array[A], dst:Array[A], s1:Int, d1:Int, len:Int) {
    System.arraycopy(src, s1, dst, d1, len)
  }
}

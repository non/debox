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
    var i = s1
    var j = d1
    var k = s1 + len
    while (i < k) {
      dst(j) = src(i)
      i += 1
      j += 1
    }
  }

  def rcopy[@spec A](src:Array[A], dst:Array[A], s1:Int, d1:Int, len:Int) {
    var i = s1 + len - 1
    var j = d1 + len - 1
    while (i >= s1) {
      dst(j) = src(i)
      i -= 1
      j -= 1
    }
  }
}

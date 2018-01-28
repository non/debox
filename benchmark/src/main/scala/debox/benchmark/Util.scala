package debox

import scala.{specialized => sp}
import scala.reflect.ClassTag
import spire.syntax.cfor._

package object benchmark {
  def init[@sp A: ClassTag](size: Int)(init: => A): Array[A] = {
    val data = new Array[A](size)
    cfor(0)(_ < size, _ + 1)(i => data(i) = init)
    data
  }
}

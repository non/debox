package debox
package syntax

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra.Order

trait SetSyntax {

  implicit class SetOps[@sp A](set: Set[A]) {

    /**
     * Copy the set's elements into a sorted buffer.
     *
     * Elements will be arranged from lowest-to-highest.
     *
     * This is an O(n) operation, where n is the size of the set.
     */
    def toSortedBuffer(implicit o: Order[A], ct: ClassTag[A]): Buffer[A] = {
      val arr = set.toArray
      spire.math.Sorting.quickSort(arr)
      Buffer.fromArray(arr)
    }
  }
}

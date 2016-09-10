package debox
package syntax

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra._
import spire.math.QuickSort
import spire.syntax.all._

trait BufferSyntax {

  implicit class BufferOps[@sp A](buf: Buffer[A]) {

    /**
     * Add the buffer contents together, returning their sum.
     */
    def sum(implicit ev: AdditiveMonoid[A]): A = {
      val limit = buf.length
      var result: A = ev.zero
      cfor(0)(_ < limit, _ + 1) { i => result += buf.elems(i) }
      result
    }

    /**
     * Multiply the buffer contents together, returning their product.
     */
    def product(implicit ev: MultiplicativeMonoid[A]): A = {
      val limit = buf.length
      var result: A = ev.one
      cfor(0)(_ < limit, _ + 1) { i => result *= buf.elems(i) }
      result
    }

    /**
     * Find the p-norm of the buffer's contents.
     *
     * The p-norm generalizes notion of a length function.
     */
    def norm(p: Int)(implicit ev: Field[A], s: Signed[A], nr: NRoot[A]): A = {
      val limit = buf.length
      var result: A = ev.one
      cfor(0)(_ < limit, _ + 1) { i =>
        result += buf.elems(i).abs ** p
      }
      result nroot p
    }

    /**
     * Find the minimum value in this buffer.
     *
     * This method uses an instance of Spire's Order[A] type class to
     * compare the elements, to avoid boxing. If you want to use Scala's
     * Ordering, you can use compatibility code in Spire, or call
     * toIterable.min.
     */
    def min(implicit o: Order[A]): A = {
      if (buf.isEmpty) throw new UnsupportedOperationException()
      var result: A = buf.elems(0)
      val limit = buf.length
      cfor(1)(_ < limit, _ + 1) { i =>
        result = result min buf.elems(i)
      }
      result
    }

    /**
     * Find the maximum value in this buffer.
     *
     * This method uses an instance of Spire's Order[A] type class to
     * compare the elements, to avoid boxing. If you want to use Scala's
     * Ordering, you can use compatibility code in Spire, or call
     * toIterable.min.
     */
    def max(implicit o: Order[A]): A = {
      if (buf.isEmpty) throw new UnsupportedOperationException()
      var result: A = buf.elems(0)
      val limit = buf.length
      cfor(1)(_ < limit, _ + 1) { i =>
        result = result max buf.elems(i)
      }
      result
    }

    /**
     * Find the mean (average) value of this buffer's contents.
     */
    def mean(implicit ev: Field[A]): A = {
      if (buf.isEmpty) throw new UnsupportedOperationException()
      var result: A = ev.zero
      val limit = buf.length
      cfor(0)(_ < limit, _ + 1) { i =>
        result = (result * i / (i + 1)) + (buf.elems(i) / (i + 1))
      }
      result
    }

    /**
     * Sort the contents of the buffer.
     *
     * This method uses an instance of Spire's Order[A] type class to
     * compare the elements, to avoid boxing. If you want to use Scala's
     * Ordering, you can use compatibility code in Spire, or call
     * toIterable.min.
     */
    def sort(implicit o: Order[A], ct: ClassTag[A]): Unit =
      QuickSort.qsort(buf.elems, 0, buf.length - 1)

    /**
     * Return a sorted copy of the buffer.
     */
    def sorted(implicit o: Order[A], ct: ClassTag[A]): Buffer[A] = {
      val buf2 = buf.copy
      QuickSort.qsort(buf2.elems, 0, buf2.length - 1)
      buf2
    }
  }
}

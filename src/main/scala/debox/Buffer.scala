package debox

import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra._
import spire.math.QuickSort
import spire.syntax.all._

/**
 * Buffer is a mutable, indexed sequence of values.
 * 
 * Buffer wraps an underlying array, which provides constant-time
 * lookups, updates, and length checks. Values can be appended to or
 * popped from the end of the buffer in amortized constant time. Other
 * operations, such as insert, prepend, and will be linear.
 * 
 * In cases where the type A is known (or the caller is specialized on
 * A), Buffer[A] will store the values in an unboxed array, and will
 * not box values on access or update. To aid in specialization and to
 * avoid inheriting bogus methods, Buffer intentionally does not
 * implement any of Scala's collection traits.
 * 
 * For interop purposes, the toIterable method wraps a buffer in a
 * collections-compatible Iterable[A]. Buffer's iterator method
 * returns an Iterator[A], and the conversion methods .toArray,
 * toVector, and toList are also available.
 * 
 * To facilitate inlining, Buffer's internals are public. However, you
 * should refrain from accessing or modifying these values unless you
 * know what you are doing.
 * 
 * Furthermore, since Buffer is really only useful in cases where you
 * care about space efficiency and performance, Buffer declines to do
 * error-checking above what is provided by the underlying array, or
 * which is necessary to avoid corruption. This means that if you try
 * to access an element beyond the Buffer's length, you are not
 * guaranteed to get an exception (although you will in many
 * cases). This is by design. However, calls which modify the buffer
 * using an invalid index are guaranteed not to corrupt the buffer.
 * 
 * Finally, there is no attempt made to provide any kind of thread
 * safety or protection against concurrent updates. Modify a Buffer
 * during foreach, map, iterator, etc will produce undefined results.
 */
final class Buffer[@sp A](arr: Array[A], n: Int)(implicit val ct: ClassTag[A]) { lhs =>

  var elems: Array[A] = arr
  var len: Int = n

  /**
   * Check if two Buffers are equal.
   * 
   * Equal means the buffers have the same type (which is checked
   * using the ClassTag instances) and the same contents.
   * 
   * Comparing Buffers with any of Scala's collection types will
   * return false.
   */
  override def equals(that: Any): Boolean = that match {
    case b: Buffer[_] =>
      if (length != b.length || ct != b.ct) return false
      val buf = b.asInstanceOf[Buffer[A]]
      val limit = len
      cfor(0)(_ < limit, _ + 1) { i =>
        if (elems(i) != buf.elems(i)) return false
      }
      true
    case _ =>
      false
  }

  /**
   * Hash the contents of the buffer to an Int value.
   */
  override def hashCode: Int = {
    var code: Int = 0xf457f00d
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i => code = (code * 19) + elems(i).## }
    code
  }

  /**
   * Return a string representation of the contents of the buffer.
   */
  override def toString =
    if (length == 0) {
      "Buffer()"
    } else {
      val limit = len
      val sb = new StringBuilder()
      sb.append("Buffer(")
      sb.append(apply(0))
      cfor(1)(_ < limit, _ + 1) { i =>
        sb.append(",")
        sb.append(elems(i))
      }
      sb.append(")")
      sb.toString
    }

  /**
   * Copy the buffer's contents to a new buffer.
   */
  final def copy: Buffer[A] = new Buffer(elems.clone, len)

  /**
   * Aborb the given buffer's contents into this buffer.
   * 
   * This method does not copy the other buffer's contents. Thus, this
   * should only be used when there are no saved references to the
   * other Buffer. It is private, and exists primarily to simplify the
   * implementation of certain methods.
   */
  private[this] def absorb(that: Buffer[A]): Unit = {
    elems = that.elems
    len = that.len
  }

  /**
   * Given delta, a change in the buffer's length, determine if the
   * underlying array needs to be grown. If this is necessary, do
   * it. Otherwise, return.
   * 
   * This is an amortized O(1) operation; most calls will simply
   * return without growing.
   */
  private[this] def growIfNecessary(delta: Int): Unit1[A] = {
    val goal = len + delta
    val n = elems.length
    if (n >= goal) return null

    var x = if (n == 0) 8 else Util.nextPowerOfTwo(n + 1)
    while (x >= 0 && x < goal) x = Util.nextPowerOfTwo(x + 1)
    if (x < 0) throw DeboxOverflowError(x)
    grow(x)
    null
  }

  /**
   * Grow the underlying array to accomodate n elements.
   * 
   * In order to amortize the cost of growth, we need to double the
   * size of the underlying array each time, so that additional
   * resizes become less and less frequent as the buffer is added to.
   * 
   * Growing is an O(n) operation, where n is buffer.length.
   */
  private[this] def grow(n: Int): Unit1[A] = {
    val arr = new Array[A](n)
    System.arraycopy(elems, 0, arr, 0, len)
    elems = arr
    null
  }

  /**
   * Return the length of this Buffer as an Int.
   * 
   * Since Buffers wrap arrays, their size is limited to what a 32-bit
   * signed integer can represent. In general Buffer should only be
   * used for sequences that are small enough to easily fit into
   * contiguous memory--larger sequences would benefit from block
   * layouts, serialization to/from disk, and other strategies that
   * Buffer does not provide.
   * 
   * This is an O(1) operation.
   */
  def length: Int = len

  /**
   * Return true if the Buffer is empty, false otherwise.
   * 
   * This is an O(1) operation.
   */
  def isEmpty: Boolean = len == 0

  /**
   * Return true if the Buffer is non-empty, false otherwise.
   * 
   * This is an O(1) operation.
   */
  def nonEmpty: Boolean = len > 0

  /**
   * Return the value at element i.
   * 
   * As noted above, this method may throw an
   * ArrayIndexOutOfBoundsException if i is too large. However, if i
   * is larger than the buffer's length, but fits in the underlying
   * array, a garbage value will be returned instead. Be careful!
   * 
   * This is an O(1) operation.
   */
  def apply(i: Int): A = elems(i)

  /**
   * Update the value of element i.
   * 
   * This method has similar caveats to apply. If an illegal i value
   * is used, an ArrayIndexOutOfBoundsException may be thrown. If no
   * exception is thrown, the update will have been ignored. Under no
   * circumstances will an invalid index corrupt the buffer.
   * 
   * This is an O(1) operation.
   */
  def update(i: Int, a: A): Unit = elems(i) = a

  /**
   * This method is a synonym for append.
   */
  def append(a: A): Unit = this += a

  /**
   * Append a new value to the end of the buffer.
   * 
   * If there is no space left in the underlying array this method
   * will trigger a grow, increasing the underlying storage
   * capacity.
   * 
   * This is an amortized O(1) operation.
   */
  def +=(a: A): Unit = {
    val n = len
    if (n >= elems.length) grow(Util.nextPowerOfTwo(n + 1))
    elems(n) = a
    len = n + 1
  }

  /**
   * Insert a new value at index i.
   * 
   * For i values that are negative, or greater than the length of the
   * buffer, an exception will be thrown. If i == buffer.length, the
   * value will be appended to the end. Otherwise, this method will
   * shift the values at i and beyond forward to make room.
   * 
   * This is an O(n) operation, where n is buffer.length.
   */
  def insert(i: Int, a: A): Unit =
    if (i < 0 || i > len) {
      throw new IllegalArgumentException(i.toString)
    } else if (i == len) {
      append(a)
    } else {
      growIfNecessary(1)
      System.arraycopy(elems, i, elems, i + 1, len - i)
      elems(i) = a
      len += 1
    }

  /**
   * Insert a new value at the beginning of the buffer.
   * 
   * This method will shift the contents of the buffer forward to make
   * space for the new value.
   * 
   * This is an O(n) operation, where n is buffer.length.
   */
  def prepend(a: A): Unit = insert(0, a)

  /**
   * This is a synonym for ++.
   */
  def concat(buf: Buffer[A]): Buffer[A] = this ++ buf

  /**
   * Concatenate two buffers, returning a new buffer.
   * 
   * This method does not modify either input buffer, but allocates
   * and returns a new one.
   * 
   * This is an O(n+m) operation, where n and m are the lengths of the
   * input buffers.
   */
  def ++(buf: Buffer[A]): Buffer[A] = {
    val result = this.copy; result ++= buf; result
  }

  /**
   * This is a synonym for ++=.
   */
  def extend(arr: Array[A]): Unit = this ++= arr

  /**
   * This is a synonym for extend.
   */
  def extend(buf: Buffer[A]): Unit = this ++= buf

  /**
   * This is a synonym for extend.
   */
  def extend(items: Iterable[A]): Unit = this ++= items

  /**
   * Append the values in arr to the end of the buffer.
   * 
   * This method is an O(m) operation, where m is the length of arr.
   */
  def ++=(arr: Array[A]): Unit = splice(len, arr)

  /**
   * Append the values in buf to the end of the buffer.
   * 
   * This method is an O(m) operation, where m is the length of buf.
   */
  def ++=(buf: Buffer[A]): Unit = splice(len, buf)

  /**
   * Append the values in elems to the end of the buffer.
   * 
   * This method is an O(m) operation, where m is the length of items.
   */
  def ++=(items: Iterable[A]): Unit = items.foreach(append)

  /**
   * Splice the values in arr into the buffer at index i.
   * 
   * If i is negative or greater than buffer.length, an exception will
   * be thrown. If i is equal to buffer.length, the buffer will be
   * extended with arr. Otherwise, this method will shift the elements
   * at i and beyond forward to make room for arr's elements. Thus,
   * the size of the buffer will increase by arr.length.
   * 
   * This method is an O(m+n) operation, where m is the length of arr,
   * and n is the length of the buffer.
   */
  def splice(i: Int, arr: Array[A]): Unit =
    if (i < 0 || i > len) {
      throw new IllegalArgumentException(i.toString)
    } else {
      val n = arr.length
      growIfNecessary(n)
      if (i < len) System.arraycopy(elems, i, elems, i + n, len - i)
      System.arraycopy(arr, 0, elems, i, n)
      len += n
    }

  /**
   * Splice the values in buf into the buffer at index i.
   * 
   * If i is negative or greater than buffer.length, an exception will
   * be thrown. If i is equal to buffer.length, the buffer will be
   * extended with buf. Otherwise, this method will shift the elements
   * at i and beyond forward to make room for buf's elements. Thus,
   * the size of the buffer will increase by buf.length.
   * 
   * This method is an O(m+n) operation, where m is the length of buf,
   * and n is the length of the buffer.
   */
  def splice(i: Int, buf: Buffer[A]): Unit =
    if (i < 0 || i > len) {
      throw new IllegalArgumentException(i.toString)
    } else {
      val n = buf.length
      growIfNecessary(n)
      if (i < len) System.arraycopy(elems, i, elems, i + n, len - i)
      System.arraycopy(buf.elems, 0, elems, i, n)
      len += n
    }

  /**
   * Prepend the values from arr into the beginning of the buffer.
   * 
   * Like splice, this method will shift all the buffer's values back
   * to make room.
   * 
   * This method is an O(m+n) operation, where m is the length of arr,
   * and n is the lenght of the buffer.
   */
  def prependAll(arr: Array[A]): Unit = splice(0, arr)

  /**
   * Prepend the values from arr into the beginning of the buffer.
   * 
   * Like splice, this method will shift all the buffer's values back
   * to make room.
   * 
   * This method is an O(m+n) operation, where m is the length of arr,
   * and n is the lenght of the buffer.
   */
  def prependAll(buf: Buffer[A]): Unit = splice(0, buf)

  /**
   * Remove the element at i, returning the value removed.
   * 
   * This method verifies that the index i is valid; if not, it will
   * throw an exception.
   * 
   * This method is an O(n) operation, where n is buffer.length.
   * Removing the last element of the buffer is O(1) operation, and
   * can also be accomplished with pop.
   */
  def remove(i: Int): A = {
    val last = len - 1
    if (i < 0) {
      throw new IndexOutOfBoundsException(i.toString)
    } else if (i < last) {
      System.arraycopy(elems, i + 1, elems, i, last - i)
      val a = elems(last)
      elems(last) = null.asInstanceOf[A]
      len = last
      a
    } else if (i == last) {
      pop
    } else {
      throw new IndexOutOfBoundsException(i.toString)
    }
  }

  /**
   * Remove the last element, returning the value returned.
   * 
   * If the buffer is empty, this method throws an exception.
   * 
   * This method is an O(1) operation.
   */
  def pop: A =
    if (len > 0) {
      val last = len - 1
      val a = elems(last)
      len = last
      a
    } else {
      throw new IndexOutOfBoundsException("0")
    }

  /**
   * Clears the buffer's internal state.
   * 
   * After calling this method, the buffer's state is identical to
   * that obtained by calling Buffer.empty[A].
   * 
   * The previous array is not retained, and will become available for
   * garbage collection.
   * 
   * This is an O(1) operation.
   */
  def clear: Unit1[A] = { absorb(Buffer.empty[A]); null }

  /**
   * Compacts the buffer's internal array to remove extra free space.
   * 
   * This operation should be used it a buffer is not likely to grow
   * again, and the user wants to free any additional memory that may
   * be available.
   * 
   * In general, a buffer that has only grown will use 1-2x of its
   * apparent size. Buffers that have been reduced in size may be
   * using up to 4x the apparent size.
   */
  def compact: Unit1[A] = {
    if (len < elems.length) {
      val arr = new Array[A](len)
      System.arraycopy(elems, 0, arr, 0, len)
      elems = arr
    }
    null
  }

  /**
   * Return a new buffer which consists of the elements [i, j).
   * 
   * The slice is half-open: the resulting buffer will include element
   * i but not element j. In other words, the new buffer will have
   * length (j - i).
   * 
   * If i and j are not valid indices in the buffer, or if i > j, this
   * method will throw an exception.
   * 
   * This is an O(j - i) operation.
   */
  def slice(i: Int, j: Int): Buffer[A] = {
    if (0 > i || i > j || j > len)
      throw new IllegalArgumentException("(%s, %s)" format (i, j))
    val n = j - i
    val arr = new Array[A](n)
    System.arraycopy(elems, i, arr, 0, n)
    new Buffer(arr, n)
  }

  /**
   * Return a new buffer with this buffer's elements in reverse order.
   * 
   * This is an O(n) method, where n is buffer.length.
   */
  def reverse: Buffer[A] = {
    val arr = new Array[A](elems.length)
    var i = 0
    var j = len - 1
    val limit = len
    while (i < limit) {
      arr(j) = elems(i)
      i += 1
      j -= 1
    }
    new Buffer(arr, len)
  }

  /**
   * Return an iterator over this buffer's contents.
   * 
   * This method does not do any copying or locking. Thus, if the
   * buffer is modified while the iterator is "live" the results will
   * be undefined and probably bad.
   * 
   * Use this.copy.iterator to get a "clean" iterator if needed.
   * 
   * Creating the iterator is an O(1) operation.
   */
  def iterator: Iterator[A] =
    elems.iterator.take(len)

  /**
   * Loop over the buffer's contents, appying f to each element.
   * 
   * This is an O(n) operation, where n is the length of the buffer.
   */
  def foreach(f: Function[A, Unit]): Unit = {
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i => f(elems(i)) }
  }

  /**
   * Map this buffer's contents into a new buffer using f.
   * 
   * This is an O(n) operation, where n is the length of the buffer.
   */
  def map[@sp B: ClassTag](f: A => B): Buffer[B] = {
    val arr = new Array[B](len)
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i => arr(i) = f(elems(i)) }
    new Buffer(arr, len)
  }

  /**
   * Add the buffer contents together, returning their sum.
   */
  def sum(implicit ev: AdditiveMonoid[A]): A = {
    val limit = len
    var result: A = ev.zero
    cfor(0)(_ < limit, _ + 1) { i => result += elems(i) }
    result
  }

  /**
   * Multiply the buffer contents together, returning their product.
   */
  def product(implicit ev: MultiplicativeMonoid[A]): A = {
    val limit = len
    var result: A = ev.one
    cfor(0)(_ < limit, _ + 1) { i => result *= elems(i) }
    result
  }

  /**
   * Find the p-norm of the buffer's contents.
   * 
   * The p-norm generalizes notion of a length function.
   */
  def norm(p: Int)(implicit ev: Field[A], s: Signed[A], nr: NRoot[A]): A = {
    val limit = len
    var result: A = ev.one
    cfor(0)(_ < limit, _ + 1) { i =>
      result += elems(i).abs ** p
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
    if (isEmpty) throw new UnsupportedOperationException()
    var result: A = elems(0)
    val limit = len
    cfor(1)(_ < limit, _ + 1) { i =>
      result = result min elems(i)
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
    if (isEmpty) throw new UnsupportedOperationException()
    var result: A = elems(0)
    val limit = len
    cfor(1)(_ < limit, _ + 1) { i =>
      result = result max elems(i)
    }
    result
  }

  /**
   * Find the mean (average) value of this buffer's contents.
   */
  def mean(implicit ev: Field[A]): A = {
    if (isEmpty) throw new UnsupportedOperationException()
    var result: A = ev.zero
    val limit = len
    cfor(0)(_ < limit, _ + 1) { i =>
      result = (result * i / (i + 1)) + (elems(i) / (i + 1))
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
  def sort(implicit o: Order[A]): Unit =
    QuickSort.qsort(elems, 0, len - 1)

  /**
   * Create an array out of the elements in the buffer.
   * 
   * This is an O(n) operation, where n is the length of the buffer.
   */
  def toArray: Array[A] =
    Util.alloc(elems, 0, len)

  /**
   * Wrap this buffer in an Iterable[A] instance.
   * 
   * This method exists as a cheap way to get compatibility with Scala
   * collections without copying/conversion. Note that since Scala
   * collections are not specialized, using this iterable will box
   * values as they are accessed (although the underlying array will
   * still be unboxed).
   * 
   * Like iterator, this method directly wraps the buffer. Thus, you
   * should not mutate the buffer while using the resulting iterable,
   * or risk corruption and undefined behavior.
   * 
   * To get a "safe" value that is compatible with Scala collections,
   * consider using toVector, toList, or copy.toIterable.
   * 
   * Creating the Iterable[A] instance is an O(1) operation.
   */
  def toIterable: Iterable[A] =
    new Iterable[A] {
      override def size: Int = lhs.length
      def iterator: Iterator[A] = lhs.iterator
      override def foreach[U](f: A => U): Unit = lhs.foreach(a => f(a))
    }

  /**
   * Create a Vector[A] from this buffer's elements.
   * 
   * This is an O(n) operation.
   */
  def toVector: Vector[A] = {
    import scala.collection.immutable.VectorBuilder
    val b = new VectorBuilder[A]
    b.sizeHint(len)
    cfor(0)(_ < len, _ + 1) { i => b += elems(i) }
    b.result
  }

  /**
   * Create a List[A] from this buffer's elements.
   * 
   * This is an O(n) operation.
   */
  def toList: List[A] = {
    import scala.collection.mutable.ListBuffer
    val b = new ListBuffer[A]
    cfor(0)(_ < len, _ + 1) { i => b += elems(i) }
    b.toList
  }
}

object Buffer {

  /**
   * Allocate an empty Buffer.
   */
  def empty[@sp A: ClassTag]: Buffer[A] =
    ofSize[A](8)

  /**
   * Allocate an empty Buffer, capable of holding n items without
   * resizing itself.
   * 
   * This method is useful if you know you'll be adding a large number
   * of elements in advance and you want to save a few resizes.
   */
  def ofSize[@sp A: ClassTag](n: Int): Buffer[A] =
    new Buffer(new Array[A](Util.nextPowerOfTwo(n)), 0)

  /**
   * Fill a length-n Buffer with a constant value.
   * 
   * If A is a reference type, all the elements in the Buffer will
   * point to the same 'a' instance. If it is known to be a value type
   * (e.g. Int) then all the values will be primitives.
   */
  def fill[@sp A: ClassTag](n: Int)(a: A): Buffer[A] =
    unsafe(Array.fill(n)(a))

  /**
   * Wrap an array instance directly in a Buffer.
   * 
   * This method is named 'unsafe' because the underlying array could
   * potentially be modified somewhere else, changing or corrupting
   * the Buffer. You should only use this method when you know that
   * the array will not be stored or modified externally.
   */
  def unsafe[@sp A: ClassTag](arr: Array[A]): Buffer[A] =
    new Buffer(arr, arr.length)

  /**
   * Build a Buffer instance from the provided values.
   */
  def apply[A: ClassTag](args: A*): Buffer[A] =
    unsafe(args.toArray)
    
  /**
   * Build a Buffer from the provided array.
   * 
   * Unlike 'unsafe' this method clones the given array, to prevent
   * possible corruption later.
   */
  def fromArray[@sp A: ClassTag](arr: Array[A]): Buffer[A] =
    new Buffer(arr.clone, arr.length)

  /**
   * Build a Buffer from the provided iterable object.
   */
  def fromIterable[@sp A: ClassTag](items: Iterable[A]): Buffer[A] =
    unsafe(items.toArray)

  /**
   * Provide a monoid for concatenating buffers.
   * 
   * The identity value is an empty buffer, and the ++ operator is
   * used to concatenate two buffers without modifying their contents.
   */
  implicit def monoid[@sp A: ClassTag]: Monoid[Buffer[A]] =
    new Monoid[Buffer[A]] {
      def id: Buffer[A] = Buffer.empty[A]
      def op(lhs: Buffer[A], rhs: Buffer[A]): Buffer[A] = lhs ++ rhs
    }
}

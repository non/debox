package debox

import scala.annotation.{switch, tailrec}
import scala.reflect.ClassTag
import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.all._

/**
 * Set is a mutable hash set, with open addressing and double hashing.
 * 
 * Set provides constant-time membership tests, and amortized
 * constant-time addition and removal. One underlying array stores
 * items, and another tracks which buckets are used and defined.
 *
 * When the type A is known (or the caller is specialized on A),
 * Set[A] will store the values in an unboxed array.
 */
final class Set[@sp (Short, Char, Int, Float, Long, Double, AnyRef) A] protected[debox](as: Array[A], bs: Array[Byte], n: Int, u: Int)(implicit val ct: ClassTag[A]) { lhs =>

  // set machinery
  var items: Array[A] = as      // slots for items
  var buckets: Array[Byte] = bs // buckets track defined/used slots
  var len: Int = n              // number of defined slots
  var used: Int = u             // number of used slots (used >= len)

  // hashing internals
  var mask: Int = buckets.length - 1             // size-1, used for hashing
  var limit: Int = (buckets.length * 0.65).toInt // point at which we should grow

  /**
   * Check if two Sets are equal.
   * 
   * Equal means the sets have the same type (which is checked
   * using the ClassTag instances) and the same contents.
   * 
   * Comparing Sets with any of Scala's collection types will
   * return false.
   * 
   * On average this is an O(n) operation. In some cases a false
   * result can be returned more quickly.
   */
  override def equals(that: Any): Boolean = that match {
    case that: Set[_] =>
      if (size != that.size || ct != that.ct) return false
      val s = that.asInstanceOf[Set[A]]
      forall(s.apply)
    case _ =>
      false
  }

  /**
   * Hash the contents of the set to an Int value.
   * 
   * By xor'ing all the set's values together, we can be sure that
   * sets with the same contents will have the same hashCode
   * regardless of the order those elements appear.
   * 
   * This is an O(n) operation.
   */
  override def hashCode: Int = fold(0xdeadd065)(_ ^ _.##)

  /**
   * Return a string representation of the contents of the set.
   * 
   * This is an O(n) operation.
   */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Set(")
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) {
      sb.append(items(i).toString)
      i += 1
    }
    while (i < buckets.length) {
      if (buckets(i) == 3) {
        sb.append(", ")
        sb.append(items(i).toString)
      }
      i += 1
    }
    sb.append(")")
    sb.toString
  }

  /**
   * Return the size of this Set as an Int.
   * 
   * Since Sets use arrays, their size is limited to what a 32-bit
   * signed integer can represent.
   * 
   * This is an O(1) operation.
   */
  final def size: Int = len

  /**
   * Return true if the Set is empty, false otherwise.
   * 
   * This is an O(1) operation.
   */
  final def isEmpty: Boolean = len == 0

  /**
   * Return true if the Set is non-empty, false otherwise.
   * 
   * This is an O(1) operation.
   */
  final def nonEmpty: Boolean = len > 0

  /**
   * Return whether the item is found in the Set or not.
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def apply(item: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        false
      } else if (status == 3 && items(j) == item) {
        true
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = item.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Make a (shallow) copy of this set.
   * 
   * This method creates a copy of the set with the same
   * structure. However, the actual elements will not be copied.
   * 
   * This is an O(n) operation.
   */
  final def copy: Set[A] = new Set(items.clone, buckets.clone, len, used)

  /**
   * Clears the set's internal state.
   * 
   * After calling this method, the set's state is identical to that
   * obtained by calling Set.empty[A].
   * 
   * The previous arrays are not retained, and will become available
   * for garbage collection. This method returns a null of type
   * Unit1[A] to trigger specialization without allocating an actual
   * instance.
   * 
   * This is an O(1) operation, but may generate a lot of garbage if
   * the set was previously large.
   */
  final def clear: Unit1[A] = { absorb(Set.empty[A]); null }

  /**
   * Aborb the given set's contents into this set.
   * 
   * This method does not copy the other set's contents. Thus, this
   * should only be used when there are no saved references to the
   * other set. It is private, and exists primarily to simplify the
   * implementation of certain methods.
   * 
   * This is an O(1) operation, although it can potentially generate a
   * lot of garbage (if the set was previously large).
   */
  private[this] def absorb(that: Set[A]): Unit = {
    items = that.items
    buckets = that.buckets
    len = that.len
    used = that.used
    mask = that.mask
    limit = that.limit
  }

  /**
   * Synonym for +=.
   */
  final def add(item: A): Boolean = this += item

  /**
   * Add item to the set.
   * 
   * Returns whether or not the item was added. If item was already in
   * the set, this method will do nothing and return false.
   * 
   * On average, this is an amortized O(1) operation; the worst-case
   * is O(n), which will occur when the set must be resized.
   */
  def +=(item: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 3) {
        if (items(j) == item)
          false
        else
          loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      } else if (status == 2 && apply(item)) {
        false
      } else {
        items(j) = item
        buckets(j) = 3
        len += 1
        if (status == 0) {
          used += 1
          if (used > limit) grow()
        }
        true
      }
    }
    val i = item.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Synonym for ++=.
   */
  def addAll(items: Iterable[A]): Unit = this ++= items

  /**
   * Synonym for ++=.
   */
  def addAll(items: Buffer[A]): Unit = this ++= items

  /**
   * Synonym for ++=.
   */
  def addAll(items: Array[A]): Unit = this ++= items

  /**
   * Add every item in items to the set.
   * 
   * This is an O(n) operation, where n is the size of items.
   */
  def ++=(items: Iterable[A]): Unit =
    items.foreach(this += _)

  /**
   * Add every item in items to the set.
   * 
   * This is an O(n) operation, where n is the size of items.
   */
  def ++=(buf: Buffer[A]): Unit =
    cfor(0)(_ < buf.length, _ + 1) { this += buf(_) }

  /**
   * Add every item in items to the set.
   * 
   * This is an O(n) operation, where n is the size of items.
   */
  def ++=(arr: Array[A]): Unit =
    cfor(0)(_ < arr.length, _ + 1) { this += arr(_) }

  /**
   * Synonym for -=.
   */
  def remove(item: A): Boolean = this -= item

  /**
   * Remove an item from the set.
   * 
   * Returns whether the item was originally in the set or not.
   * 
   * This is an amortized O(1) operation.
   */
  final def -=(item: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && items(j) == item) {
        buckets(j) = 2
        len -= 1
        true
      } else if (status == 0) {
        false
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = item.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Set/unset the value of item in the set.
   * 
   * Like += and -= this is an amortized O(1) operation.
   */
  final def update(item: A, b: Boolean) =
    if (b) this += item else this -= item

  /**
   * Loop over the set's contents, appying f to each element.
   * 
   * There is no guaranteed order that the set's elements will be
   * traversed in, so use of foreach should not rely on a particular
   * order.
   * 
   * This is an O(n) operation, where n is the length of the buffer.
   */
  def foreach(f: A => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) f(items(i))
    }

  /**
   * Translate this Set into another Set using the given function f.
   * 
   * Note that the resulting set may be smaller than this set, if f is
   * not a one-to-one function (an injection).
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def map[@sp(Short, Char, Int, Float, Long, Double, AnyRef) B: ClassTag](f: A => B): Set[B] = {
    val out = Set.ofSize[B](len)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) out.add(f(items(i)))
    }
    if (out.size < len / 3) out.compact
    out
  }

  /**
   * Fold the set's values into a single value, using the provided
   * initial state and combining function f.
   * 
   * Like foreach, fold makes no guarantees about the order that
   * elements will be reached.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def fold[@sp(Int, Long, Double, AnyRef) B](init: B)(f: (B, A) => B): B = {
    var result = init
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) result = f(result, items(i))
    }
    result
  }

  /**
   * Grow the underlying array to best accomodate the set's size.
   * 
   * To preserve hashing access speed, the set's size should never be
   * more than 66% of the underlying array's size. When this size is
   * reached, the set needs to be updated (using this method) to have a
   * larger array.
   * 
   * The underlying array's size must always be a multiple of 2, which
   * means this method grows the array's size by 2x (or 4x if the set
   * is very small). This doubling helps amortize the cost of
   * resizing, since as the set gets larger growth will happen less
   * frequently. This method returns a null of type Unit1[A] to
   * trigger specialization without allocating an actual instance.
   * 
   * Growing is an O(n) operation, where n is the set's size.
   */
  final def grow(): Unit1[A] = {
    val next = buckets.length * (if (buckets.length < 10000) 4 else 2)
    val set = Set.ofAllocatedSize[A](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) set += items(i)
    }
    absorb(set)
    null
  }

  /**
   * Compacts the set's internal arrays to reduce memory usage.
   * 
   * This operation should be used if a set has been shrunk
   * (e.g. through --=) and is not likely to grow again.
   * 
   * This method will shrink the set to the smallest possible size
   * that allows it to be <66% full. It returns a null of type
   * Unit1[A] to trigger specialization without allocating an actual
   * instance.
   * 
   * This is an O(n) operation, where n it the set's size.
   */
  final def compact(): Unit1[A] = {
    val set = Set.ofSize[A](len)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) set += items(i)
    }
    absorb(set)
    null
  }

  // For a lot of the following methods, there are size tests to try
  // to make sure we're looping over the smaller of the two
  // sets. Some things to keep in mind:
  //
  // 1. System.arraycopy is faster than a loop.
  // 2. We want to avoid copying a large object and then shrinking it.
  // 3. & and | are symmetric (but -- is not). They all require a copy.
  // 4. &=, |=, and --= are not symmetric (they modify the lhs).
  //
  // So where possible we'd like to be looping over a smaller set,
  // doing membership tests against a larger set.

  /**
   * Union this set with the rhs set.
   * 
   * This has the effect of adding all members of rhs to lhs.
   * 
   * This is an O(n) operation, where n is rhs.size.
   */
  def |=(rhs: Set[A]): Unit =
    if (lhs.size >= rhs.size) {
      cfor(0)(_ < rhs.buckets.length, _ + 1) { i =>
        if (rhs.buckets(i) == 3) lhs += rhs.items(i)
      }
    } else {
      val out = rhs.copy
      out |= lhs
      lhs.absorb(out)
    }

  /**
   * Synonym for |.
   */
  def union(rhs: Set[A]): Set[A] = lhs | rhs

  /**
   * Return new set which is the union of lhs and rhs.
   * 
   * The new set will contain all members of lhs and rhs.
   * 
   * This is an O(m max n) operation, where m and n are the sizes of
   * the sets.
   */
  def |(rhs: Set[A]): Set[A] =
    if (lhs.size >= rhs.size) {
      val out = lhs.copy
      out |= rhs
      out
    } else {
      val out = rhs.copy
      out |= lhs
      out
    }

  /**
   * Remove any member of this which is not in rhs.
   * 
   * This is an O(m min n) operation, where m and n are the sizes of
   * the lhs and rhs sets.
   */
  def &=(rhs: Set[A]): Unit =
    if (lhs.size <= rhs.size) {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && !rhs(items(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    } else {
      val out = rhs.copy
      out &= lhs
      lhs.absorb(out)
    }

  /**
   * Synonym for &.
   */
  def intersection(rhs: Set[A]): Set[A] = this & rhs

  /**
   * Intersect this set with the rhs set.
   * 
   * This has the effect of removing any item not in rhs.
   * 
   * This is an O(m min n) operation, where m and n are the sizes of
   * the lhs and rhs sets.
   */
  def &(rhs: Set[A]): Set[A] =
    if (lhs.size <= rhs.size) {
      val out = lhs.copy
      out &= rhs
      out
    } else {
      val out = rhs.copy
      out &= lhs
      out
    }

  /**
   * Remove members of rhs from the set.
   * 
   * This operation is an O(m min n) operation, where m and n are the
   * sizes of the lhs and rhs sets.
   */
  def --=(rhs: Set[A]): Unit =
    if (lhs.size >= rhs.size) {
      cfor(0)(_ < rhs.buckets.length, _ + 1) { i =>
        if (rhs.buckets(i) == 3) lhs -= rhs.items(i)
      }
    } else {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && rhs(items(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    }

  /**
   * Remove the members of items from the set.
   * 
   * This is an O(n) operation, where n is the length of items.
   */
  def --=(items: Iterable[A]): Unit =
    items.foreach(a => this -= a)

  /**
   * Remove the members of arr from the set.
   * 
   * This is an O(n) operation, where n is the length of arr.
   */
  def --=(arr: Array[A]): Unit =
    cfor(0)(_ < arr.length, _ + 1) { i => this -= arr(i) }

  /**
   * Remove the members of buf from the set.
   * 
   * This is an O(n) operation, where n is the length of buf.
   */
  def --=(buf: Buffer[A]): Unit =
    cfor(0)(_ < buf.length, _ + 1) { i => this -= buf(i) }

  /**
   * This is a synonym for --.
   */
  def difference(rhs: Set[A]): Set[A] = lhs -- rhs

  /**
   * Create a new set with the elements of lhs that are not in rhs.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def --(rhs: Set[A]): Set[A] = {
    val out = lhs.copy
    out --= rhs
    out
  }
  
  /**
   * Count how many elements of the set satisfy the predicate p.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def count(p: A => Boolean): Int =
    fold(0)((n, a) => if (p(a)) n + 1 else n)

  /**
   * Determine if every member of the set satisifes the predicate p.
   * 
   * This is an O(n) operation, where n is the size of the
   * set. However, it will return as soon as a false result is
   * obtained.
   */
  def forall(p: A => Boolean): Boolean = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3 && !p(items(i))) return false
    }
    true
  }

  /**
   * Determine if any member of the set satisifes the predicate p.
   * 
   * This is an O(n) operation, where n is the size of the
   * set. However, it will return as soon as a true result is
   * obtained.
   */
  def exists(p: A => Boolean): Boolean = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3 && p(items(i))) return true
    }
    false
  }

  /**
   * Find a member of the set that satisfies the predicate p.
   * 
   * The method returns Some(item) if item satisfies p, and None if
   * none of set's elements satisfy p. Since Set is not ordered, if
   * multiple elements satisfy the predicate there is no guarantee
   * which one wil be found.
   * 
   * This is an O(n) operation, where n is the size of the
   * set. However, it will return as soon as a member satisfying the
   * predicate is found.
   */
  def find(p: A => Boolean): Option[A] = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val a = items(i)
        if (p(a)) return Some(a)
      }
    }
    None
  }

  /**
   * Create a new set containing all the members of this set that
   * satisfy p.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def findAll(p: A => Boolean): Set[A] = {
    val out = Set.empty[A]
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3 && p(items(i))) out += items(i)
    }
    out
  }

  /**
   * Remove any member of the set that does not satisfy p.
   * 
   * After this method, all membrers of the set will satisfy p.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def filterSelf(p: A => Boolean): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3 && !p(items(i))) {
        buckets(i) = 2
        len -= 1
      }
    }

  /**
   * Partition this set into two new sets, the first consisting of all
   * members that fail to satisfy the predicate p, and the second for
   * all those that do satisfy the predicate.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def partition(p: A => Boolean): (Set[A], Set[A]) = {
    val no = Set.ofSize[A](len / 2)
    val yes = Set.ofSize[A](len / 2)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val a = items(i)
        if (p(a)) yes += a else no += a
      }
    }
    if (no.size < len / 6) no.compact
    if (yes.size < len / 6) yes.compact
    (no, yes)
  }

  /**
   * Return an iterator over this set's contents.
   * 
   * This method does not do any copying or locking. Thus, if the set
   * is modified while the iterator is "live" the results will be
   * undefined and probably bad. Also, since sets are not ordered,
   * there is no guarantee elements will be returned in a particular
   * order.
   * 
   * Use this.copy.iterator to get a "clean" iterator if needed.
   * 
   * Creating the iterator is an O(1) operation.
   */
  def iterator: Iterator[A] = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    new Iterator[A] {
      var index = i
      def hasNext: Boolean = index < buckets.length
      def next: A = {
        val item = items(index)
        index += 1
        while (index < buckets.length && buckets(index) != 3) index += 1
        item
      }
    }
  }

  /**
   * Copy the set's elements into an array.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def toArray: Array[A] = {
    val arr = new Array[A](size)
    var j = 0
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        arr(j) = items(i)
        j += 1
      }
    }
    arr
  }

  /**
   * Copy the set's elements into a buffer.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def toBuffer: Buffer[A] = Buffer.fromArray(toArray)

  /**
   * Copy the set's elements into a sorted buffer.
   * 
   * Elements will be arranged from lowest-to-highest.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def toSortedBuffer(implicit o: Order[A]): Buffer[A] = {
    val buf = Buffer.fromArray(toArray)
    buf.sort
    buf
  }

  /**
   * Copy the sets contents into a Map. The elements of the set will
   * be keys, and each keys' value will be determined with the
   * provided function.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def toMap[@sp(Boolean, Int, Long, Double) B: ClassTag](f: A => B): spall.Map[A, B] = {
    val out = spall.Map.ofSize[A, B](len)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        val a = items(i)
        out(a) = f(a)
      }
    }
    out
  }

  /**
   * Wrap this set in an Iterable[A] instance.
   * 
   * This method exists as a cheap way to get compatibility with Scala
   * collections without copying/conversion. Note that since Scala
   * collections are not specialized, using this iterable will box
   * values as they are accessed (although the underlying set will
   * still be unboxed).
   * 
   * Like iterator, this method directly wraps the set. Thus, you
   * should not mutate the set while using the resulting iterable, or
   * risk corruption and undefined behavior.
   * 
   * To get a "safe" value that is compatible with Scala collections,
   * consider using toScalaSet.
   * 
   * Creating the Iterable[A] instance is an O(1) operation.
   */
  def toIterable: Iterable[A] =
    new Iterable[A] {
      override def size: Int = lhs.size
      def iterator: Iterator[A] = lhs.iterator
      override def foreach[U](f: A => U): Unit = lhs.foreach(a => f(a))
    }

  /**
   * Create an immutable instance of scala's Set[A].
   * 
   * This method copies the elements into a new instance which is
   * compatible with Scala's collections and Set[A] type.
   * 
   * This is an O(n) operation, where n is the size of the set.
   */
  def toScalaSet: scala.collection.immutable.Set[A] =
    iterator.toSet

}


object Set {

  /**
   * Allocate an empty Set.
   */
  def empty[@sp A: ClassTag] = new Set(new Array[A](8), new Array[Byte](8), 0, 0)

  /**
   * Allocate an empty Set, capable of holding n items without
   * resizing itself.
   * 
   * This method is useful if you know you'll be adding a large number
   * of elements in advance and you want to save a few resizes.
   */
  def ofSize[@sp A: ClassTag](n: Int) =
    ofAllocatedSize(n / 2 * 3)

  /**
   * Allocate an empty Set, with underlying storage of size n.
   * 
   * This method is useful if you know exactly how big you want the
   * underlying array to be. In most cases ofSize() is probably what
   * you want instead.
   */
  private[debox] def ofAllocatedSize[@sp A: ClassTag](n: Int) = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw DeboxOverflowError(n)
      case 0 => 8
      case n => n
    }
    new Set(new Array[A](sz), new Array[Byte](sz), 0, 0)
  }

  /**
   * Build a Set instance from the provided values.
   */
  def apply[@sp A: ClassTag](as: A*): Set[A] = fromIterable(as)

  /**
   * Build a Set from the provided array.
   * 
   * The advantage of using this method is that, unlike apply() or
   * fromIterable(), the values will not be boxed prior to the set
   * being built.
   */
  def fromArray[@sp A: ClassTag](as: Array[A]): Set[A] = {
    val n = spire.math.max(8, as.length + as.length / 2)
    val set = ofSize[A](n)
    cfor(0)(_ < as.length, _ + 1)(i => set.add(as(i)))
    set
  }

  /**
   * Build a Set from the provided iterable object.
   */
  def fromIterable[@sp A: ClassTag](as: Iterable[A]): Set[A] = {
    val set = empty[A]
    set ++= as
    set
  }

  /**
   * Provide a Eq[Set[A]] instance.
   * 
   * Since Sets are so reliant on equality, and use hash codes
   * internally, the default equality is used to compare elements.
   */
  implicit def eqv[A] =
    new Eq[Set[A]] {
      def eqv(lhs: Set[A], rhs: Set[A]): Boolean = lhs == rhs
    }

  /**
   * Provide a CMonoid[Set[A]] instance.
   * 
   * Since element order is irrelevant, union is a commutative
   * operation. The empty set is the identity element.
   */
  implicit def cmonoid[@sp A: ClassTag] =
    new CMonoid[Set[A]] {
      def id = Set.empty[A]
      def op(lhs: Set[A], rhs: Set[A]): Set[A] = lhs | rhs
    }
}

package debox
package spkey

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.{specialized => spA}
import debox.{dummysp => spB}
import scala.{specialized => spC}

import spire.algebra._
import spire.syntax.all._

/**
 * Map is a mutable hash map, with open addressing and double hashing.
 * 
 * Map provides constant-time membership tests and access, and
 * amortized constant-time addition and removal. One underlying array
 * stores keys, another stores values, and another tracks which
 * buckets are used and defined.
 * 
 * The hashing mechanism is exactly the same as that used in
 * debox.Set, which means that constructing a Set from the keys is
 * relatively fast.
 *
 * When the generic types are known (or the caller is specialized on them),
 * Map[A, B] will store the keys and values in unboxed arrays.
 * 
 * One fundamental difference between debox.Map and Scala's various
 * map types is that debox.Map does not claim to contain tuples, and
 * most of methods handle keys and values as separate parameters,
 * rather than item tuples. This means that methods like foreach() and
 * mapKey() take Function2 instances rather than Function1 instances.
 * 
 * It also means that instead of generic Tuple2 => Tuple2 functions,
 * Debox uses more specialized methods. For example:
 * 
 *   // Scala version
 *   scalaMap.map { case (k, v) => (k, v + 3) }
 * 
 *   // Debox versions
 *   deboxMap.mapValues(v => v + 3)           // best
 *   deboxMap.mapItems((k, v) => (k, v + 3))  // allocs unnecessary tuples
 *   deboxMap.
 * 
 * This means that the Debox Map API has some real differences
 * compared to Scala's API, so please check the methods you are using.
 */
final class Map[@spA(Int, Long, AnyRef) A, @spB B] protected[debox] (ks: Array[A], vs: Array[B], bs: Array[Byte], n: Int, u: Int)(implicit val cta: ClassTag[A], val ctb: ClassTag[B]) { lhs =>

  // map internals
  var keys: Array[A] = ks       // slots for keys
  var vals: Array[B] = vs       // slots for values
  var buckets: Array[Byte] = bs // buckets track defined/used slots
  var len: Int = n              // number of defined slots
  var used: Int = u             // number of used slots (used >= len)

  // hashing internals
  var mask: Int = keys.length - 1             // size - 1, used for hashing
  var limit: Int = (keys.length * 0.65).toInt // point at which we should grow

  /**
   * Check if two Maps are equal.
   * 
   * Equal means the maps have the same types (which is checked
   * using the ClassTag instances) and the same contents.
   * 
   * Comparing Maps with any of Scala's collection types will
   * return false.
   */
  override def equals(rhs: Any): Boolean = rhs match {
    case rhs: Map[_, _] =>
      if (lhs.size != rhs.size || lhs.cta != rhs.cta || lhs.ctb != rhs.ctb) return false
      val m = rhs.asInstanceOf[Map[A, B]]
      forall(m.containsItem)
    case _ =>
      false
  }

  /**
   * Hash the contents of the map to an Int value.
   * 
   * By xor'ing all the map's keys and values together, we can be sure
   * that maps with the same contents will have the same hashCode
   * regardless of the order those items appear.
   * 
   * This is an O(n) operation.
   */
  override def hashCode: Int = {
    var n: Int = 0xb0bd0bb5
    cfor(0)(_ < buckets.length, _ + 1) { i => n ^= keys(i).## ^ vals(i).## }
    n
  }

  /**
   * Return a string representation of the contents of the map.
   * 
   * This is an O(n) operation.
   */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Map(")
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    if (i < buckets.length) {
      sb.append(keys(i).toString)
      sb.append(" -> ")
      sb.append(vals(i).toString)
      i += 1
    }
    while (i < buckets.length) {
      if (buckets(i) == 3) {
        sb.append(", ")
        sb.append(keys(i).toString)
        sb.append(" -> ")
        sb.append(vals(i).toString)
      }
      i += 1
    }
    sb.append(")")
    sb.toString
  }

  /**
   * Return the size of this Map as an Int.
   * 
   * Since Maps use arrays, their size is limited to what a 32-bit
   * signed integer can represent.
   * 
   * This is an O(1) operation.
   */
  final def size: Int = len

  /**
   * Return true if the Map is empty, false otherwise.
   * 
   * This is an O(1) operation.
   */
  final def isEmpty: Boolean = len == 0

  /**
   * Return true if the Map is non-empty, false otherwise.
   * 
   * This is an O(1) operation.
   */
  final def nonEmpty: Boolean = len > 0

  /**
   * This method stores associates value with key.
   * 
   * If a previous value was associated with the key, it is
   * overwritten.
   * 
   * This method is usually invoked as map(key) = value, but can also
   * be invoked as map.update(key, value).
   * 
   * On average, this is an amortized O(1) operation; the worst-case
   * is O(n), which will happen when the map needs to be resized.
   */
  final def update(key: A, value: B): Unit = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Unit = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        keys(j) = key
        vals(j) = value
        buckets(j) = 3
        len += 1
        used += 1
        if (used > limit) grow()
      } else if (status == 2 && !contains(key)) {
        keys(j) = key
        vals(j) = value
        buckets(j) = 3
        len += 1
      } else if (keys(j) == key) {
        vals(j) = value
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * This method removes any value associated with key.
   * 
   * If there was no previous value, this method does nothing.
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def remove(key: A): Unit = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Unit = {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        buckets(j) = 2
        len -= 1
      } else if (status == 0) {
      } else {
        loop((i << 2) + i + perturbation + 1, perturbation >> 5)
      }
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Make a (shallow) copy of this map.
   * 
   * This method creates a copy of the map with the same
   * structure. However, the actual keys and values will not be
   * copied.
   * 
   * This is an O(n) operation.
   */
  final def copy: Map[A, B] = new Map(keys.clone, vals.clone, buckets.clone, len, used)

  /**
   * Clears the map's internal state.
   * 
   * After calling this method, the set's state is identical to that
   * obtained by calling Map.empty[A, B].
   * 
   * The previous arrays are not retained, and will become available
   * for garbage collection.
   * 
   * This is an O(1) operation, but may generate a lot of garbage if
   * the set was previously large.
   */
  final def clear: Unit = absorb(Map.empty[A, B])

  /**
   * Aborb the given map's contents into this map.
   * 
   * This method does not copy the other map's contents. Thus, this
   * should only be used when there are no saved references to the
   * other map. It is private, and exists primarily to simplify the
   * implementation of certain methods.
   * 
   * This is an O(1) operation, although it can potentially generate a
   * lot of garbage (if the map was previously large).
   */
  private[this] def absorb(rhs: Map[A, B]): Unit = {
    keys = rhs.keys
    vals = rhs.vals
    buckets = rhs.buckets
    len = rhs.len
    used = rhs.used
    mask = rhs.mask
    limit = rhs.limit
  }

  /**
   * Return whether the key is present in the Map or not.
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def contains(key: A): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) false
      else if (status == 3 && keys(j) == key) true
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Return whether the key is present in the Map with the given value
   * or not.
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def containsItem(key: A, value: B): Boolean = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Boolean = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) false
      else if (status == 3 && keys(j) == key) vals(j) == value
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Return the key's current value in the map, throwing an exception
   * if the key is not found.
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def apply(key: A): B = {
    @inline @tailrec def loop(i: Int, perturbation: Int): B = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) throw new KeyNotFoundException(key.toString)
      else if (status == 3 && keys(j) == key) vals(j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Return the key's current value in the map, returning the given
   * fallback value if the key is not found.
   * 
   * Unlike Scala's method, this method is eager in its second
   * parameters, so it should only be used if the default value is
   * already available (or a literal, or very cheap).
   * 
   * In cases where a lazy parameter would be desired, you should use
   * something like: myMap.get(key).getOrElse(default).
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def getOrElse(key: A, fallback: B): B = {
    @inline @tailrec def loop(i: Int, perturbation: Int): B = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) fallback
      else if (status == 3 && keys(j) == key) vals(j)
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Return the key's current value in the map as an Option, returning
   * None if the key is not found.
   * 
   * On average, this is an O(1) operation; the (unlikely) worst-case
   * is O(n).
   */
  final def get(key: A): Option[B] = {
    @inline @tailrec def loop(i: Int, perturbation: Int): Option[B] = {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) None
      else if (status == 3 && keys(j) == key) Some(vals(j))
      else loop((i << 2) + i + perturbation + 1, perturbation >> 5)
    }
    val i = key.## & 0x7fffffff
    loop(i, i)
  }

  /**
   * Given (key, value), update the map to bind value to key.
   * 
   * Unless the caller has already constrcuted the item tuple, it is
   * usually better to say 'map(key) = value' directly to avoid
   * allocating one.
   * 
   * This is an O(1) operation.
   */
  final def +=(kv: (A, B)): Unit = update(kv._1, kv._2)

  /**
   * Add the items from the given map to this map, combining values
   * using the provided Monoid[B].
   * 
   * This is an O(m) operation, where m is the size of the rhs.
   */
  final def ++=(rhs: Map[A, B])(implicit ev: Monoid[B]): Unit = {
    val z = ev.id
    rhs.foreach { (k, v) => lhs(k) = lhs.getOrElse(k, z) |+| v }
  }

  /**
   * Add the items from the given iterable to this map, combining
   * values using the provided Monoid[B].
   * 
   * This is an O(m) operation, where m is the size of the rhs.
   */
  final def ++=(rhs: Iterable[(A, B)])(implicit ev: Monoid[B]): Unit = {
    val z = ev.id
    rhs.foreach { case (k, v) => lhs(k) = lhs.getOrElse(k, z) |+| v }
  }

  /**
   * Combine the two maps into a new map, using the provided
   * CMonoid[B] to merge values for the same key.
   * 
   * This is an O(m+n) operation, where m and n are the size of the
   * maps.
   */
  final def ++(rhs: Map[A, B])(implicit ev: Monoid[B]): Map[A, B] = {
    val out = lhs.copy
    out ++= rhs
    out
  }

  /**
   * Remove the keys in the given set from this map.
   * 
   * On average, this is an O(m) operation, where m is the size of the
   * lhs or rhs, whichever is smaller.
   */
  final def --=(rhs: Set[A]): Unit =
    if (lhs.size < rhs.size) {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && rhs(keys(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    } else {
      cfor(0)(_ < rhs.buckets.length, _ + 1) { i =>
        if (rhs.buckets(i) == 3) lhs.remove(rhs.items(i))
      }
    }

  /**
   * Remove the keys in the given set from this iterable.
   * 
   * On average, this is an O(m) operation, where m is the size of the
   * rhs.
   */
  final def --=(rhs: Iterable[A]): Unit =
    rhs.foreach(a => remove(a))

  /**
   * Remove all items whose keys are not in the provided set.
   * 
   * This method modifies the existing map.
   * 
   * On average, this is an O(m) operation, where m is the size of the
   * lhs or rhs, whichever is smaller.
   */
  final def filterInPlace(rhs: Set[A]): Unit =
    if (lhs.size < rhs.size) {
      cfor(0)(_ < buckets.length, _ + 1) { i =>
        if (buckets(i) == 3 && !rhs(keys(i))) {
          buckets(i) = 2
          len -= 1
        }
      }
    } else {
      val m = Map.empty[A, B]
      cfor(0)(_ < rhs.buckets.length, _ + 1) { i =>
        if (rhs.buckets(i) == 3) {
          val k = rhs.items(i)
          m(k) = this(k)
        }
      }
      absorb(m)
    }

  /**
   * Return a new Set containing the current map's keys.
   * 
   * This method copies the underlying key array, but does not need to
   * rehash the keys, so it is relatively fast (implemented using
   * System.arraycopy). The keys themselves are not copied.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  final def keysSet: Set[A] = new Set(keys.clone, buckets.clone, len, used)

  /**
   * Return a new array containing the current map's values, in an
   * unspecified order.
   * 
   * This method is not a true function, in that map values which are
   * equivalent (have the same keys and values) may return
   * differently-ordered arrays.
   * 
   * To get an unordered set of the values, use valuesSet. To get a
   * sorted valuesArray, use sortedValuesArray.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  final def valuesArray: Array[B] = {
    val result = new Array[B](len)
    var j = 0
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) { result(j) = vals(i); j += 1 }
    }
    result
  }

  /**
   * Transform this map into an array.
   * 
   * This method uses a function to go from key and value to an
   * element to be stored in the array.
   * 
   * The order of the array is non-deterministic. Thus, maps that have
   * the same keys and values may produce different arrays. The size
   * of the array is guaranteed to be equal to the size of the map.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapToArray[@spC C: ClassTag](f: (A, B) => C): Array[C] = {
    val result = new Array[C](len)
    var j = 0
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) {
        result(j) = f(keys(i), vals(i))
        j += 1
      }
    }
    result
  }

  /**
   * Transform this map into a set.
   * 
   * This method uses a function to go from key and value to an
   * element to be added to a set.
   * 
   * Two maps with the same keys and values will always produce the
   * same set. The size of the resulting set may be smaller than the
   * map if several function invocations return the same result.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapToSet[@spC C: ClassTag](f: (A, B) => C): Set[C] = {
    val result = Set.ofSize[C](len)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) result += f(keys(i), vals(i))
    }
    result.compact
    result
  }

  /**
   * Transform this map into another map.
   * 
   * This method uses a function to go from key and value to a pair,
   * which will be added to the map.
   * 
   * If multiple invocations of the provided function return values
   * with the same key, it is non-deterministic what value the new map
   * will have for the key. In these cases, two equal maps will
   * produce different results, and the resulting map will be smaller
   * than this map.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapItemsToMapUnsafe[@spA (Int, Long, AnyRef) C: ClassTag, @spB D: ClassTag](f: (A, B) => (C, D)): Map[C, D] = {
    val result = Map.ofSize[C, D](len)
    foreach((a, b) => result += f(a, b))
    result.compact
    result
  }

  /**
   * Transform this map into another map.
   * 
   * This method uses a function to go from key and value to a pair,
   * which will be added to the map.
   * 
   * If multiple invocations of the provided function return values
   * with the same key, the given commutative monoid will be used to
   * combine the values. This commutativity ensures that maps with the
   * same keys and values will always produce the same
   * result. However, if duplicate keys are produced, the resulting
   * map will be smaller than this map.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapItemsToMap[@spA (Int, Long, AnyRef) C: ClassTag, @spB D: ClassTag](f: (A, B) => (C, D))(implicit ev: CMonoid[D]): Map[C, D] = {
    val result = Map.ofSize[C, D](len)
    val z = ev.id
    foreach { (a, b) =>
      val (c, d) = f(a, b)
      result(c) = result.getOrElse(c, z) |+| d
    }
    result.compact
    result
  }

  /**
   * Transform this map into another map.
   * 
   * This method uses a function to go from an existing key to a new
   * key, and will add the new key with the existing value to the new
   * map.
   * 
   * If multiple invocations of the provided function return the same
   * key, it is non-deterministic what value the new map will have for
   * the key. In these cases, two equal maps will produce different
   * results, and the resulting map will be smaller than this map.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapKeysUnsafe[@spA (Int, Long, AnyRef) C: ClassTag](f: A => C): Map[C, B] = {
    val result = Map.ofSize[C, B](len)
    foreach((a, b) => result(f(a)) = b)
    result.compact
    result
  }

  /**
   * Transform this map into another map.
   * 
   * This method uses a function to go from an existing key to a new
   * key, and will add the new key with the existing value to the new
   * map.
   * 
   * If multiple invocations of the provided function return values
   * with the same key, the given commutative monoid will be used to
   * combine the values. This commutativity ensures that maps with the
   * same keys and values will always produce the same
   * result. However, if duplicate keys are produced, the resulting
   * map will be smaller than this map.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapKeys[@spA (Int, Long, AnyRef) C: ClassTag](f: A => C)(implicit ev: CMonoid[B]): Map[C, B] = {
    val result = Map.ofSize[C, B](len)
    val z = ev.id
    foreach { (a, b) =>
      val c = f(a)
      result(c) = result.getOrElse(c, z) |+| b
    }
    result.compact
    result
  }

  /**
   * Transform this map into another map.
   * 
   * This method uses a function to go from an existing value to a new
   * value, and will add the existing key with the new value to the
   * new map.
   * 
   * The new map will always be the same size as the existing map, and
   * maps with equivalent keys and values will always produce
   * equivalent maps.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map.
   */
  final def mapValues[@spB C: ClassTag](f: B => C): Map[A, C] = {
    val arr = new Array[C](buckets.length)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) arr(i) = f(vals(i))
    }
    new Map[A, C](keys.clone, arr, buckets.clone, len, used)
  }

  /**
   * Run the provided function on each key and value.
   * 
   * The order the keys and values are accessed is non-deterministic.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  final def foreach(f: (A, B) => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i => if (buckets(i) == 3) f(keys(i), vals(i)) }

  /**
   * Run the provided function on each key.
   * 
   * The order the keys are accessed is non-deterministic.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  final def foreachKey(f: A => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i => if (buckets(i) == 3) f(keys(i)) }

  /**
   * Run the provided function on each value.
   * 
   * The order the values are accessed is non-deterministic.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  final def foreachValue(f: B => Unit): Unit =
    cfor(0)(_ < buckets.length, _ + 1) { i => if (buckets(i) == 3) f(vals(i)) }

  /**
   * Run the provided function on each value.
   * 
   * The order the values are accessed is non-deterministic.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  final def transformValues(f: (A, B) => B): Unit = {
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) vals(i) = f(keys(i), vals(i))
    }
  }

  /**
   * Grow the underlying array to best accomodate the map's size.
   * 
   * To preserve hashing access speed, the map's size should never be
   * more than 66% of the underlying array's size. When this size is
   * reached, the map needs to be updated (using this method) to have a
   * larger array.
   * 
   * The underlying array's size must always be a multiple of 2, which
   * means this method grows the array's size by 2x (or 4x if the map
   * is very small). This doubling helps amortize the cost of
   * resizing, since as the map gets larger growth will happen less
   * frequently. This method returns a null of type Unit1[A] to
   * trigger specialization without allocating an actual instance.
   * 
   * Growing is an O(n) operation, where n is the map's size.
   */
  final def grow(): Unit2[A, B] = {
    val next = keys.length * (if (keys.length < 10000) 4 else 2)
    val map = Map.ofSize[A, B](next)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) map(keys(i)) = vals(i)
    }
    absorb(map)
    null
  }

  /**
   * Compacts the map's internal arrays to reduce memory usage.
   * 
   * This operation should be used if a map has been shrunk
   * (e.g. through --=) and is not likely to grow again.
   * 
   * This method will shrink the map to the smallest possible size
   * that allows it to be <66% full. It returns a null of type
   * Unit2[A, B] to trigger specialization without allocating an
   * actual instance.
   * 
   * This is an O(n) operation, where n it the set's size.
   */
  final def compact(): Unit2[A, B] = {
    val map = Map.ofSize[A, B](len)
    cfor(0)(_ < buckets.length, _ + 1) { i =>
      if (buckets(i) == 3) map(keys(i)) = vals(i)
    }
    absorb(map)
    null
  }

  /**
   * Return true if every item in the map satisfies the given
   * predicate, false otherwise.
   * 
   * Note that if the map is empty, all predicates will return true.
   * 
   * On average, this is an O(n) operation, although false results may
   * be returned more quickly.
   */
  def forall(p: (A, B) => Boolean) =
    loopWhile(p) == -1

  /**
   * Return true if any item in the map satisfies the given
   * predicate, false otherwise.
   * 
   * Note that if the map is empty, all predicates will return false.
   * 
   * On average, this is an O(n) operation, although true results may
   * be returned more quickly.
   */
  def exists(p: (A, B) => Boolean) =
    loopUntil(p) != -1

  /**
   * For the given predicate, return Some(item) for an item that
   * satisfies the predicate, or None otherwise.
   * 
   * The order that items are checked is non-deterministic. Thus, maps
   * with the same keys and values may find different items first.
   * 
   * On average, this is an O(n) operation, although results may be
   * returned more quickly.
   */
  def find(p: (A, B) => Boolean): Option[(A, B)] =
    loopUntil(p) match {
      case i if i >= 0 => Some((keys(i), vals(i)))
      case _ => None
    }

  /**
   * For the given predicate, return a map of all items that satisfy
   * the predicate.
   * 
   * Maps with the same keys and values are guaranteed to return the
   * same map from this method.
   * 
   * This is an O(n) operation, where n is the size of the map.
   */
  def findAll(p: (A, B) => Boolean): Map[A, B] = {
    val out = Map.empty[A, B]
    foreach((a, b) => if (p(a, b)) out(a) = b)
    out
  }

  /**
   * Return the lowest index whose key/value do not satisfy the
   * predicate, or -1 otherwise.
   * 
   * The indices returned correspond to the map's internal
   * arrays. Care should be taken.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map, although non-negative results may be returned more quickly.
   */
  def loopWhile(p: (A, B) => Boolean): Int = {
    @inline @tailrec def loop(i: Int, limit: Int): Int = {
      if (buckets(i) == 3 && !p(keys(i), vals(i))) i
      else if (i < limit) loop(i + 1, limit)
      else -1
    }
    loop(0, buckets.length - 1)
  }

  /**
   * Return the lowest index whose key/value satisfy the predicate,
   * or -1 otherwise.
   * 
   * The indices returned correspond to the map's internal
   * arrays. Care should be taken.
   * 
   * On average, this is an O(n) operation, where n is the size of the
   * map, although non-negative results may be returned more quickly.
   */
  def loopUntil(p: (A, B) => Boolean): Int = {
    @inline @tailrec def loop(i: Int, limit: Int): Int = {
      if (buckets(i) == 3 && p(keys(i), vals(i))) i
      else if (i < limit) loop(i + 1, limit)
      else -1
    }
    loop(0, buckets.length - 1)
  }

  /**
   * Return an iterator over this map's contents.
   * 
   * This method does not do any copying or locking. Thus, if the map
   * is modified while the iterator is "live" the results will be
   * undefined and probably bad. Also, since maps are not ordered,
   * there is no guarantee elements will be returned in a particular
   * order.
   * 
   * Use this.copy.iterator to get a "clean" iterator if needed.
   * 
   * Creating the iterator is an O(1) operation.
   */
  def iterator: Iterator[(A, B)] = {
    var i = 0
    while (i < buckets.length && buckets(i) != 3) i += 1
    new Iterator[(A, B)] {
      var index = i
      def hasNext: Boolean = index < buckets.length
      def next: (A, B) = {
        val key = keys(index)
        val value = vals(index)
        index += 1
        while (index < buckets.length && buckets(index) != 3) index += 1
        (key, value)
      }
    }
  }
}

object Map {

  /**
   * Create an empty Map.
   * 
   * Example: Map.empty[Int, String]
   */
  def empty[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag]: Map[A, B] =
    new Map(new Array[A](8), new Array[B](8), new Array[Byte](8), 0, 0)

  /**
   * Create a Map that can hold n unique keys without resizing itself.
   *
   * Note that the internal representation will allocate more space
   * than requested to satisfy the requirements of internal
   * alignment. Map uses arrays whose lengths are powers of two, and
   * needs at least 33% of the map free to enable good hashing
   * performance.
   * 
   * Example: Map.ofSize[Int, String](100).
   */
  def ofSize[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag](n: Int): Map[A, B] =
    ofAllocatedSize[A, B](n / 2 * 3)

  /**
   * Allocate an empty Map, with underlying storage of size n.
   * 
   * This method is useful if you know exactly how big you want the
   * underlying array to be. In most cases ofSize() is probably what
   * you want instead.
   */
  private[debox] def ofAllocatedSize[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag](n: Int) = {
    val sz = Util.nextPowerOfTwo(n) match {
      case n if n < 0 => throw DeboxOverflowError(n)
      case 0 => 8
      case n => n
    }
    new Map(new Array[A](sz), new Array[B](sz), new Array[Byte](sz), 0, 0)
  }

  /**
   * Create a new literal map.
   * 
   * Note that this method will box the given items. Unless you
   * already have a sequence of tuples, another constructor may
   * require fewer allocations.
   * 
   * Example: Map(1 -> "cat", 2 -> "dog", 3 -> "fish")
   */
  def apply[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag](pairs: (A, B)*): Map[A, B] =
    fromIterable(pairs)

  /**
   * Create a map from an array of keys and another array of values.
   * 
   * Example: Map(Array(1,2,3), Array("cat", "dog", "fish"))
   */
  def fromArrays[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag](ks: Array[A], vs: Array[B]): Map[A, B] = {
    if (ks.length != vs.length) throw new IllegalArgumentException("ks.length != vs.length")
    val map = ofSize[A, B](ks.length)
    cfor(0)(_ < ks.length, _ + 1) { i => map(ks(i)) = vs(i) }
    map
  }

  /**
   * Create a map from an iterable of tuples.
   * 
   * Note that this method will box the given items. Unless you
   * already have a sequence of tuples, another constructor may
   * require fewer allocations.
   * 
   * Example: Map.fromIterable(List((1, "cat"), (2, "dog"), (3, "fish")))
   */
  def fromIterable[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag](pairs: Iterable[(A, B)]): Map[A, B] = {
    val result = empty[A, B]
    // work around compiler bug with foreach here
    val it = pairs.iterator
    while (it.hasNext) { result += it.next }
    result
  }

  /**
   * Provide an Eq[Map[A, B]] instance.
   * 
   * Since Maps are so reliant on equality, and use hash codes
   * internally, the default equality is used to compare elements.
   */
  implicit def eqv[A, B] =
    new Eq[Map[A, B]] {
      def eqv(lhs: Map[A, B], rhs: Map[A, B]): Boolean = lhs == rhs
    }

  /**
   * Provide a Monoid[Map[A, B]].
   * 
   * The maps are combined key-by-key, using |+| to merge values if
   * necessary.
   */
  implicit def monoid[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag: Monoid] =
    new Monoid[Map[A, B]] {
      def id: Map[A, B] = Map.empty[A, B]
      def op(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] = lhs ++ rhs
    }

  /**
   * Provide an AdditiveMonoid[Map[A, B]].
   * 
   * The maps are combined key-by-key, using + to merge values if
   * necessary.
   */
  implicit def additiveMonoid[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag: AdditiveMonoid] =
    new AdditiveMonoid[Map[A, B]] {
      implicit val m: Monoid[B] = implicitly[AdditiveMonoid[B]].additive
      def zero: Map[A, B] = Map.empty[A, B]
      def plus(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] = lhs ++ rhs
    }

  /**
   * Provide an MultiplicativeMonoid[Map[A, B]].
   * 
   * The maps are combined key-by-key, using * to merge values if
   * necessary.
   */
  implicit def multiplicativeMonoid[@spA(Int, Long, AnyRef) A: ClassTag, @spB B: ClassTag: MultiplicativeMonoid] =
    new MultiplicativeMonoid[Map[A, B]] {
      implicit val m: Monoid[B] = implicitly[MultiplicativeMonoid[B]].multiplicative
      def one: Map[A, B] = Map.empty[A, B]
      def times(lhs: Map[A, B], rhs: Map[A, B]): Map[A, B] = lhs ++ rhs
    }
}

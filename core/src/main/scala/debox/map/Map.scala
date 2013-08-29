package debox.map

import debox.SpUnit2

import spire.algebra._
import spire.syntax.monoid._

import scala.{specialized => sp}

import scala.collection.IterableLike
import scala.collection.mutable.{Builder}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

class MapBuilder[@sp A: ClassTag, @sp B: ClassTag]() extends Builder[(A, B), Map[A, B]] {
  private var elems: Map[A, B] = Map.empty[A, B]

  override def sizeHint(size: Int): Unit = elems.sizeHint(size)

  def +=(t: (A, B)): this.type = { elems(t._1) = t._2; this }

  def clear(): Unit = elems.clear()

  def result: Map[A, B] = elems
}

class MapCbf[@sp A: ClassTag, @sp B: ClassTag]() extends CanBuildFrom[Map[A, B], (A, B), Map[A, B]] {
  def apply(from: Map[A, B]) = new MapBuilder[A, B]
  def apply() = new MapBuilder[A, B]
}

object Map {
  def empty[@sp A: ClassTag, @sp B: ClassTag]() = new Map[A, B](new Array[A](8), new Array[B](8), new Array[Byte](8), 0, 0)

  def apply[@sp A: ClassTag, @sp B: ClassTag](ts: (A, B)*): Map[A, B] = {
    val map = empty[A, B]
    ts.foreach { case (a, b) => map(a) = b }
    map
  }

  def fromScala[@sp A: ClassTag, @sp B: ClassTag](pairs: Iterable[(A, B)]): Map[A, B] = {
    val map = empty[A, B]
    pairs.foreach { case (a, b) => map(a) = b }
    map
  }

  def newBuilder[@sp A: ClassTag, @sp B: ClassTag] =
    new MapBuilder[A, B]

  implicit def canBuildFrom[@sp A: ClassTag, @sp B: ClassTag] =
    new MapCbf[A, B]
}

final class Map[@sp A, @sp B] private[debox](as: Array[A], bs: Array[B], cs: Array[Byte], n: Int, u: Int)(implicit eva: ClassTag[A], evb: ClassTag[B])
    extends Iterable[(A, B)] with IterableLike[(A, B), Map[A, B]] { self =>

  private[debox] var cta: Any = eva
  private[debox] var ctb: Any = evb

  private[debox] var keys: Array[A] = as
  private[debox] var vals: Array[B] = bs
  private[debox] var buckets: Array[Byte] = cs
  private[debox] var len: Int = n
  private[debox] var used: Int = u

  // hashing internals
  private[debox] var mask = keys.length - 1 // size-1, used for hashing
  private[debox] var limit = (keys.length * 0.66).toInt // when we should resize

  override def size: Int = len

  override def hashCode(): Int =
    scala.util.hashing.MurmurHash3.unorderedHash(this, "Map".##)

  def eqv(rhs: Map[A, B]): Boolean = {
    if (size != rhs.size) return false
    var i = 0
    var j = 0
    val limit = len
    while (i < buckets.length && j < limit) {
      if (buckets(i) == 3) {
        if (!rhs.test(keys(i), vals(i))) return false
        j += 1
      }
      i += 1
    }
    true
  }

  override def equals(rhs: Any) = rhs match {
    case m: Map[_, _] =>
      cta == m.cta && ctb == m.ctb && eqv(m.asInstanceOf[Map[A, B]])
    case _ =>
      false
  }

  private[debox] def absorb(m: Map[A, B]): Unit = {
    keys = m.keys
    vals = m.vals
    buckets = m.buckets
    len = m.len
    used = m.used
    mask = m.mask
    limit = m.limit
  }

  def clear(): SpUnit2[A, B] = {
    absorb(Map.empty[A, B])
    null
  }

  def sizeHint(n: Int): SpUnit2[A, B] = {
    val limit = (n * 1.6).toInt
    var i = 8
    while (i < n && i <= 8192) i *= 4
    while (i < n && i > 0) i *= 2
    if (i < 0) sys.error("argh")
    resize(i)
  }

  def compact(): SpUnit2[A, B] = {
    val shrunk = Map.empty[A, B]
    loop((k, v) => shrunk(k) = v)
    absorb(shrunk)
    null
  }

  def copy: Map[A, B] = new Map[A, B](keys.clone, vals.clone, buckets.clone, len, used)

  def apply(key: A): B = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        return vals(j)
      } else if (status == 0) {
        throw new java.util.NoSuchElementException(key.toString)
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    null.asInstanceOf[B] // impossible
  }

  def get(key: A): Option[B] = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        return Some(vals(j))
      } else if (status == 0) {
        return None
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    None // impossible
  }

  def getOrElse(key: A, default: B): B = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        return vals(j)
      } else if (status == 0) {
        return default
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    default // impossible
  }

  def test(key: A, value: B): Boolean = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        return vals(j) == value
      } else if (status == 0) {
        return false
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  def contains(key: A): Boolean = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        return true
      } else if (status == 0) {
        return false
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  def +(item: (A, B)): Map[A, B] = {
    val map = this.copy
    map(item._1) = item._2
    map
  }

  def +=(item: (A, B)): Unit = update(item._1, item._2)

  // statuses:
  //   0 is empty
  //   2 is deleted
  //   3 is taken

  def update(key: A, value: B): Unit = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    var provisional = -1
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3) { // bucket is taken
        if (keys(j) == key) return vals(j) = value
      } else if (status == 0) { // bucket is empty
        if (provisional < 0) {
          keys(j) = key
          vals(j) = value
          buckets(j) = 3
        } else {
          keys(provisional) = key
          vals(provisional) = value
          buckets(provisional) = 3
        }
        len += 1
        if (status == 0) {
          used += 1
          if (used > limit) {
            val size = keys.length
            val factor = if (size <= 8192) 4 else 2
            resize(size * factor)
          }
        }
        return ()
      } else { // bucket is empty but was taken previously
        if (provisional < 0) provisional = j
      }
      i = (i << 2) + i + perturbation + 1
      perturbation = perturbation >> 5
    }
    () // impossible
  }

  def ++=(rhs: Traversable[(A, B)]): Unit = rhs.foreach(this += _)

  def ++=(rhs: IndexedSeq[(A, B)]): Unit = {
    sizeHint(size + rhs.length)
    rhs.foreach(this += _)
  }

  def -(key: A): Map[A, B] = {
    val map = this.copy
    map -= key
    map
  }

  def -=(key: A): Unit = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && keys(j) == key) {
        buckets(j) = 2
        len -= 1
        if ((len >> 2) < used) compact()
        return ()
      } else if (status == 0) {
        return ()
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    () // impossible
  }

  def --(rhs: Traversable[A]): Map[A, B] = {
    val map = this.copy
    rhs.foreach(map -= _)
    map
  }

  def --=(rhs: Traversable[A]): Unit = rhs.foreach(this -= _)

  def |(rhs: Map[A, B])(implicit ev: Monoid[B]): Map[A, B] = {
    val (big, small) = if (size < rhs.size) (rhs, this) else (this, rhs)
    val result = big.copy
    result |= small
    result
  }

  def |=(rhs: Map[A, B])(implicit ev: Monoid[B]): Unit = {
    rhs.loop { (a, b) =>
      this(a) = this.getOrElse(a, ev.id) |+| b
    }
  }

  def &(rhs: Map[A, B])(implicit ev: Semigroup[B]): Map[A, B] = {
    val result = Map.empty[A, B]
    val (big, small) = if (size < rhs.size) (rhs, this) else (this, rhs)
    small.loop { (a, b) =>
      big.get(a).foreach { b2 => result(a) = b |+| b2 }
    }
    result
  }

  def &=(rhs: Map[A, B])(implicit ev: Semigroup[B]): Unit = {
    val result = this & rhs
    absorb(result)
  }

  def ^(rhs: Map[A, B]): Map[A, B] = {
    val (big, small) = if (size < rhs.size) (rhs, this) else (this, rhs)
    val result = big.copy
    result ^= small
    result
  }

  def ^=(rhs: Map[A, B]): Unit =
    rhs.loop { (a, b) =>
      if (this.contains(a)) this -= a else this(a) = b
    }

  def purge(f: (A, B) => Boolean): Unit = {
    var i = 0
    var j = 0
    val limit = len
    while (i < buckets.length && j < limit) {
      if (buckets(i) == 3) {
        j += 1
        if (f(keys(i), vals(i))) buckets(i) = 2
      }
      i += 1
    }
  }

  def purgeKeys(f: A => Boolean): Unit = purge((a, _) => f(a))

  def purgeValues(f: B => Boolean): Unit = purge((_, b) => f(b))

  def keySet: debox.set.Set[A] =
    new debox.set.Set(keys.clone, buckets.clone, len, used)

  private def hash(key: A, mask0: Int, keys0: Array[A], buckets0: Array[Byte]): Int = {
    var i = key.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask0
      if (buckets0(j) == 3 && keys0(j) != key) {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      } else {
        return j
      }
    }
    -1 // impossible
  }

  private def resize(nextsize: Int): SpUnit2[A, B] = {
    val nextmask = nextsize - 1
    val nextkeys = new Array[A](nextsize)
    val nextvals = new Array[B](nextsize)
    val nextbs = new Array[Byte](nextsize)

    var i = 0
    var k = 0
    val lim = len
    while (i < buckets.length && k < lim) {
      if (buckets(i) == 3) {
        val key = keys(i)
        val j = hash(key, nextmask, nextkeys, nextbs)
        nextkeys(j) = key
        nextvals(j) = vals(i)
        nextbs(j) = 3
        k += 1
      }
      i += 1
    }

    keys = nextkeys
    vals = nextvals
    buckets = nextbs
    mask = nextmask
    limit = (nextsize * 0.66).toInt
    null
  }

  def loop(f: Function2[A, B, Unit]): Unit = {
    var i = 0
    var j = 0
    val limit = len
    while (i < buckets.length && j < limit) {
      if (buckets(i) == 3) {
        f(keys(i), vals(i))
        j += 1
      }
      i += 1
    }
  }

  def foreach[U](f: (A, B) => U): Unit = {
    var i = 0
    var j = 0
    val limit = len
    while (i < buckets.length && j < limit) {
      if (buckets(i) == 3) {
        f(keys(i), vals(i))
        j += 1
      }
      i += 1
    }
  }

  def iterator: Iterator[(A, B)] = new Iterator[(A, B)] {
    var epos = 0
    var xpos = 0
    val elen = self.len

    def hasNext: Boolean = epos < elen
  
    def next: (A, B) = {
      var i = xpos
      while (i < buckets.length) {
        if (buckets(i) == 3) {
          xpos = i + 1
          epos += 1
          return (keys(i), vals(i))
        }
        i += 1
      }
      throw new NoSuchElementException
    }
  }

  override def newBuilder = Map.newBuilder[A, B]

  import scala.collection.immutable
  def toScala: immutable.Map[A, B] =
    foldLeft(immutable.Map.empty[A, B]) { case (m, (k, v)) =>
      m.updated(k ,v)
    }
}

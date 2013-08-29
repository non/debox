package debox.set

import debox.SpUnit

import scala.{specialized => sp}

import scala.collection.IterableLike
import scala.collection.mutable.{Builder}
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

class SetBuilder[@sp A: ClassTag]() extends Builder[A, Set[A]] {
  private var elems: Set[A] = Set.empty[A]

  override def sizeHint(size: Int): Unit = elems.sizeHint(size)

  def +=(n: A): this.type = { elems += n; this }

  def clear(): Unit = elems.clear()

  def result: Set[A] = elems
}

class SetCbf[@sp A: ClassTag]() extends CanBuildFrom[Set[A], A, Set[A]] {
  def apply(from: Set[A]) = new SetBuilder[A]
  def apply() = new SetBuilder[A]
}


object Set {
  def empty[@sp A: ClassTag]() = new Set[A](new Array[A](8), new Array[Byte](8), 0, 0)

  def apply[@sp A: ClassTag](ns: A*) = {
    val set = empty[A]
    ns.foreach(set += _)
    set
  }
  
  def newBuilder[@sp A: ClassTag] =
    new SetBuilder[A]

  implicit def canBuildFrom[@sp A: ClassTag] =
    new SetCbf[A]
}

final class Set[@sp A] private[debox](as: Array[A], bs: Array[Byte], n: Int, u: Int)(implicit ev: ClassTag[A])
  extends Function1[A, Boolean] with Iterable[A] with IterableLike[A, Set[A]] { self =>

  private[debox] var ct: Any = ev

  private[debox] var items: Array[A] = as
  private[debox] var buckets: Array[Byte] = bs
  private[debox] var len: Int = n
  private[debox] var used: Int = u

  // hashing internals
  private[debox] var mask = items.length - 1 // size-1, used for hashing
  private[debox] var limit = (items.length * 0.66).toInt // when we should resize

  override def size: Int = len

  override def hashCode(): Int =
    scala.util.hashing.MurmurHash3.unorderedHash(this, "Set".##)

  def clear(): SpUnit[A] = {
    items = new Array[A](8)
    buckets = new Array[Byte](8)
    len = 0
    used = 0
    mask = 7
    limit = 5
    null
  }

  def sizeHint(n: Int): SpUnit[A] = {
    val limit = (n * 1.6).toInt
    var i = 8
    while (i < n && i <= 8192) i *= 4
    while (i < n && i > 0) i *= 2
    if (i < 0) sys.error("argh")
    resize(i)
  }

  def compact(): SpUnit[A] = {
    val shrunk = Set.empty[A]
    shrunk |= this
    items = shrunk.items
    buckets = shrunk.buckets
    len = shrunk.len
    used = shrunk.used
    mask = shrunk.mask
    limit = shrunk.limit
    null
  }

  def +(item: A): Set[A] = {
    val set = this.copy
    set += item
    set
  }

  def +=(item: A): Boolean = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3) {
        if (items(j) == item) {
          return false
        } else {
          i = (i << 2) + i + perturbation + 1
          perturbation = perturbation >> 5
        }
      } else {
        if (status == 2 && apply(item)) return false
        items(j) = item
        buckets(j) = 3
        len += 1
        if (status == 0) {
          used += 1
          if (used > limit) {
            val size = items.length
            val factor = if (size <= 8192) 4 else 2
            resize(size * factor)
          }
        }
        return true
      }
    }
    false // impossible
  }

  def ++=(rhs: Traversable[A]): Unit = rhs.foreach(this += _)

  def ++=(rhs: IndexedSeq[A]): Unit = {
    sizeHint(size + rhs.length)
    rhs.foreach(this += _)
  }

  def -(item: A): Set[A] = {
    val set = this.copy
    set -= item
    set
  }

  def -=(item: A): Boolean = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3 && items(j) == item) {
        buckets(j) = 2
        len -= 1
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

  def --(rhs: Traversable[A]): Set[A] = {
    val set = this.copy
    rhs.foreach(set -= _)
    set
  }

  def --=(rhs: Traversable[A]): Unit = rhs.foreach(this -= _)

  def |(rhs: Set[A]): Set[A] =
    if (size >= rhs.size) {
      val set = this.copy
      set |= rhs
      set
    } else {
      val set = rhs.copy
      set |= this
      set
    }

  def |=(rhs: Set[A]): Unit = {
    sizeHint(size + rhs.size)
    rhs.foreach(this += _)
  }

  def &(rhs: Set[A]): Set[A] = {
    val set = Set.empty[A]
    if (size <= rhs.size)
      this.foreach(n => if (rhs(n)) set += n)
    else
      rhs.foreach(n => if (this(n)) set += n)
    set
  }

  def &=(rhs: Set[A]): Unit = {
    val set = this & rhs
    items = set.items
    buckets = set.buckets
    len = set.len
    used = set.used
    mask = set.mask
    limit = set.limit
  }

  def copy: Set[A] = new Set[A](items.clone, buckets.clone, len, used)

  def apply(item: A): Boolean = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        return false
      } else if (status == 3 && items(j) == item) {
        return true
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  private def hash(item: A, mask0: Int, items0: Array[A], buckets0: Array[Byte]): Int = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask0
      if (buckets0(j) == 3 && items0(j) != item) {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      } else {
        return j
      }
    }
    -1 // impossible
  }

  private def resize(nextsize: Int): SpUnit[A] = { 
    val nextmask = nextsize - 1
    val nextitems = new Array[A](nextsize)
    val nextbs = new Array[Byte](nextsize)

    var i = 0
    var k = 0
    val lim = len
    while (i < buckets.length && k < lim) {
      if (buckets(i) == 3) {
        val item = items(i)
        val j = hash(item, nextmask, nextitems, nextbs)
        nextitems(j) = item
        nextbs(j) = 3
        k += 1
      }
      i += 1
    }

    items = nextitems
    buckets = nextbs
    mask = nextmask
    limit = (nextsize * 0.66).toInt
    null
  }

  override def foreach[U](f: A => U) {
    var i = 0
    var j = 0
    val limit = len
    while (i < buckets.length && j < limit) {
      if (buckets(i) == 3) {
        f(items(i))
        j += 1
      }
      i += 1
    }
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var epos = 0
    var xpos = 0
    val elen = self.len

    def hasNext: Boolean = epos < elen
  
    def next: A = {
      var i = xpos
      while (i < buckets.length) {
        if (buckets(i) == 3) {
          xpos = i + 1
          epos += 1
          return items(i)
        }
        i += 1
      }
      throw new NoSuchElementException
    }
  }

  override def newBuilder = Set.newBuilder[A]

  def eqv(rhs: Set[A]): Boolean = size == rhs.size && forall(rhs)

  override def equals(rhs: Any): Boolean = rhs match {
    case set: Set[_] => ct == set.ct && eqv(set.asInstanceOf[Set[A]])
    case _ => false
  }
}

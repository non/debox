package debox.map

import scala.reflect.ClassTag

import debox._
import debox.buffer.Buffer
import debox.set.Set

import scala.{specialized => spec}

object Map {
  def empty[@spec(Int, Long, Double, AnyRef) A:ClassTag:Unset:Hash, @spec(Int, Long, Double, AnyRef) B:ClassTag] = {
    new Map(Buckets.ofDim[A](8), new Array[B](8), 0, 8)
  }

  def apply[@spec(Int, Long, Double, AnyRef) A:ClassTag:Unset:Hash, @spec(Int, Long, Double, AnyRef) B:ClassTag]() = empty[A, B]

  def apply[@spec(Int, Long, Double, AnyRef) A:ClassTag:Unset:Hash, @spec(Int, Long, Double, AnyRef) B:ClassTag](ks:Array[A], vs:Array[B]) = {
    val len = ks.length
    val sz = size(len)
    val map = new Map(Buckets.ofDim[A](sz), new Array[B](sz), 0, sz)
    var i = 0
    while (i < len) { map(ks(i)) = vs(i); i += 1 }
    map
  }

  def apply[@spec(Int, Long, Double, AnyRef) A:ClassTag:Unset:Hash, @spec(Int, Long, Double, AnyRef) B:ClassTag](items:(A, B)*) = {
    val len = items.length
    val sz = size(len)
    val map = new Map(Buckets.ofDim[A](sz), new Array[B](sz), 0, sz)
    var i = 0
    while (i < len) {
      val (k, v) = items(i)
      map(k) = v
      i += 1
    }
    map
  }

  private def size(n:Int) = {
    var sz = 8
    var limit = 5
    while (n > limit) {
      println("%s < %s (%s)" format (n, limit, sz))
      if (sz < 10000) sz <<= 2 else sz <<= 1
      limit = (sz * 0.65).toInt
      if (sz <= 0) sys.error("overflow")
    }
    sz
  }
}

final class Map[@spec(Int, Long, Double, AnyRef) A:ClassTag:Hash, @spec(Int, Long, Double, AnyRef) B:ClassTag] protected[debox] (ks:Buckets[A], vs:Array[B], n:Int, s:Int) extends Function1[A, B] {

  // set internals
  var keys:Buckets[A] = ks // keys track set/unset
  var vals:Array[B] = vs // values mirror keys
  var len:Int = n // number of items in map

  // hashing internals
  var size = s // number of buckets allocated, must be a power of 2
  var mask = s - 1 // size-1, used for hashing
  var limit = (s * 0.65).toInt // point at which we should resize

  final def length:Int = len

  final def update(key:A, value:B):Unit = {
    val i = hash(key, mask, keys)
    val wasUnset = keys.notItemAt(i, key)
    keys(i) = key
    vals(i) = value
    if (wasUnset) {
      len += 1
      if (len > limit) resize()
    }
  }

  final def remove(key:A):Unit = {
    val i = hash(key, mask, keys)
    if (keys.notItemAt(i, key)) return ()
    keys.unset(i)
    len -= 1
  }

  final def copy:Map[A, B] = new Map(keys.copy, vals.clone, len, size)

  final def mapToArray[@spec(Int, Long, Double, AnyRef) C:ClassTag](f:(A, B) => C):Array[C] = {
    var j = 0
    val arr = new Array[C](len)
    keys.foreachIndex {
      i => arr(j) = f(keys(i), vals(i))
      j += 1
    }
    arr
  }

  final def mapToSet[@spec(Int, Long, Double, AnyRef) C:ClassTag:Unset:Hash](f:(A, B) => C):Set[C] = {
    val s = Set.empty[C]
    keys.foreachIndex(i => s.add(f(keys(i), vals(i))))
    s
  }

  final def mapToMap[@spec(Int, Long, Double, AnyRef) C:ClassTag:Hash, @spec(Int, Long, Double, AnyRef) D:ClassTag](k:A => C, v:B => D):Map[C, D] = {
    val m = Map.empty[C, D]
    keys.foreachIndex(i => m(k(keys(i))) = v(vals(i)))
    m
  }

  final def foreach(f:(A, B) => Unit) { keys.foreachIndex(i => f(keys(i), vals(i))) }

  final def foreachKey(f:A => Unit) { keys.foreach(f) }

  final def foreachVal(f:B => Unit) { keys.foreachIndex(i => f(vals(i))) }

  override def toString:String = if (len == 0) {
    "Map()"
  } else {
    val sb = new StringBuilder
    sb.append("Map(")
    foreach((k, v) => sb.append("%s -> %s, " format (k, v)))
    sb.dropRight(2).append(")").mkString
  }

  final protected[this] def hash(key:A, mask:Int, bs:Buckets[A]):Int = {
    var i = Hash[A].hash(key) & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask
  
      // if this index is empty, or equal to our value already
      if (bs(j) == key || bs.isUnset(j, bs(j))) return j
  
      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    -1
  }

  final def contains(key:A):Boolean = {
    var i = Hash[A].hash(key) & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask
    
      // if this index is empty, or equal to our value already
      val slot = keys(j)
      if (keys.isUnset(j, slot)) return false
      if (slot == key) return true
    
      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    false
  }

  final def apply(key:A):B = {
    var i = Hash[A].hash(key) & 0x7fffffff // positive hash code for the item
    var perturbation = i // perturbation helps avoid collisions
    
    // while there are collisions, we have to keep modifying our index to find
    // new addresses. the open addressing scheme here is inspired by python's.
    while (true) {
      // j is the index we're going to try
      val j = i & mask
    
      // if this index is empty, or equal to our value already
      val slot = keys(j)
      if (keys.isUnset(j, slot)) sys.error("key %s not found" format key)
      if (slot == key) return vals(j)
    
      // otherwise, find a new index to try
      i = (i << 2) + i + perturbation + 1
      perturbation >>= 5
    }
    
    // should never happen
    null.asInstanceOf[B]
  }

  final def get(key:A):Option[B] = {
    if (contains(key)) Some(apply(key)) else None
  }

  final protected[this] def resize():A = {
    val factor = if (size < 10000) 4 else 2
    
    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextkeys = Buckets.ofDim[A](nextsize)
    val nextvals = new Array[B](nextsize)
    
    var i = 0
    var count = 0
    while (count < len) {
      val key = keys(i)
      if (keys.isSet(i, key)) {
        val j = hash(key, nextmask, nextkeys)
        nextkeys(j) = key
        nextvals(j) = vals(i)
        count += 1
      }
      i += 1
    }
    
    size = nextsize
    mask = nextmask
    keys = nextkeys
    vals = nextvals
    limit = (size * 0.65).toInt

    Hash[A].zero
  }
}

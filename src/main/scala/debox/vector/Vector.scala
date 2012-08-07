package debox.vector

import debox._
import debox.buffer.Buffer

import scala.reflect.ClassTag

import scala.{specialized => spec}

object Vector {
  @inline final def size1 = 32
  @inline final def size2 = 1024
  @inline final def size3 = 32768
  @inline final def size4 = 1048576
  @inline final def size5 = 33554432
  @inline final def size6 = 1073741824

  @inline final def mask1 = 31
  @inline final def mask2 = 1023
  @inline final def mask3 = 32767
  @inline final def mask4 = 1048575
  @inline final def mask5 = 33554431
  @inline final def mask6 = 1073741823

  @inline final def shift1 = 0
  @inline final def shift2 = 5
  @inline final def shift3 = 10
  @inline final def shift4 = 15
  @inline final def shift5 = 20
  @inline final def shift6 = 25

  type Array2[A] = Array[Array[A]]
  type Array3[A] = Array[Array[Array[A]]]
  type Array4[A] = Array[Array[Array[Array[A]]]]
  type Array5[A] = Array[Array[Array[Array[Array[A]]]]]
  type Array6[A] = Array[Array[Array[Array[Array[Array[A]]]]]]

  final def apply[@spec A:ClassTag]():Vector[A] = new Vector1(0, alloc0[A])
  final def empty[@spec A:ClassTag]():Vector[A] = new Vector1(0, alloc0[A])

  final def copymod1[@spec A:ClassTag](as:Array[A], z:Int, a:A) = {
    val as2 = as.clone
    as2(z) = a
    as2
  }

  final def copymod2[@spec A:ClassTag](bs:Array2[A], y:Int, z:Int, a:A) = {
    val bs2 = bs.clone
    val as2 = if (bs2(y) == null) alloc0[A] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    bs2
  }

  final def copymod3[@spec A:ClassTag](cs:Array3[A], x:Int, y:Int, z:Int, a:A) = {
    val cs2 = cs.clone
    val bs2 = if (cs2(x) == null) alloc0[Array[A]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[A] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    cs2
  }

  final def copymod4[@spec A:ClassTag](ds:Array4[A], w:Int, x:Int, y:Int, z:Int, a:A) = {
    val ds2 = ds.clone
    val cs2 = if (ds2(w) == null) alloc0[Array2[A]] else ds2(w).clone
    val bs2 = if (cs2(x) == null) alloc0[Array[A]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[A] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    ds2(w) = cs2
    ds2
  }

  final def copymod5[@spec A:ClassTag](es:Array5[A], v:Int, w:Int, x:Int, y:Int, z:Int, a:A) = {
    val es2 = es.clone
    val ds2 = if (es2(v) == null) alloc0[Array3[A]] else es2(v).clone
    val cs2 = if (ds2(w) == null) alloc0[Array2[A]] else ds2(w).clone
    val bs2 = if (cs2(x) == null) alloc0[Array[A]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[A] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    ds2(w) = cs2
    es2(v) = ds2
    es2
  }

  final def copymod6[@spec A:ClassTag](fs:Array6[A], u:Int, v:Int, w:Int, x:Int, y:Int, z:Int, a:A) = {
    val fs2 = fs.clone
    val es2 = if (fs2(u) == null) alloc0[Array4[A]] else fs2(u).clone
    val ds2 = if (es2(v) == null) alloc0[Array3[A]] else es2(v).clone
    val cs2 = if (ds2(w) == null) alloc0[Array2[A]] else ds2(w).clone
    val bs2 = if (cs2(x) == null) alloc0[Array[A]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[A] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    ds2(w) = cs2
    es2(v) = ds2
    fs2(u) = es2
    fs2
  }

  @inline final def alloc0[@spec A:ClassTag] = new Array[A](32)

  final def alloc1[@spec A:ClassTag](a:A) = {
    val arr = new Array[A](32)
    arr(0) = a
    arr
  }

  final def alloc2[@spec A:ClassTag](a0:A, a1:A) = {
    val arr = new Array[A](32)
    arr(0) = a0
    arr(1) = a1
    arr
  }

  final def iter1[@spec A](f:A => Unit, len:Int, as:Array[A]) {
    var i = 0
    while (i < len) { f(as(i)); i += 1 }
  }

  final def iter2[@spec A](f:A => Unit, len:Int, bs:Array2[A]) {
    var i = 0
    val q = len >> shift1
    val r = len & mask1
    while (i < q) { iter1(f, size1, bs(i)); i += 1 }
    if (r != 0) iter1(f, r, bs(q))
  }

  final def iter3[@spec A](f:A => Unit, len:Int, cs:Array3[A]) {
    var i = 0
    val q = len >> shift2
    val r = len & mask2
    while (i < q) { iter2(f, size2, cs(i)); i += 1 }
    if (r != 0) iter2(f, r, cs(q))
  }

  final def iter4[@spec A](f:A => Unit, len:Int, ds:Array4[A]) {
    var i = 0
    val q = len >> shift3
    val r = len & mask3
    while (i < q) { iter3(f, size3, ds(i)); i += 1 }
    if (r != 0) iter3(f, r, ds(q))
  }

  final def iter5[@spec A](f:A => Unit, len:Int, es:Array5[A]) {
    var i = 0
    val q = len >> shift4
    val r = len & mask4
    while (i < q) { iter4(f, size4, es(i)); i += 1 }
    if (r != 0) iter4(f, r, es(q))
  }

  final def iter6[@spec A](f:A => Unit, len:Int, fs:Array6[A]) {
    var i = 0
    val q = len >> shift5
    val r = len & mask5
    while (i < q) { iter5(f, size4, fs(i)); i += 1 }
    if (r != 0) iter5(f, r, fs(q))
  }

  final def mapp1[@spec A, @spec Z:ClassTag](f:A => Z, len:Int, as:Array[A]) = {
    val zs = alloc0[Z]
    var i = 0
    while (i < len) { zs(i) = f(as(i)); i += 1 }
    zs
  }

  final def mapp2[@spec A, @spec Z:ClassTag](f:A => Z, len:Int, bs:Array2[A]) = {
    val zs = alloc0[Array[Z]]
    var i = 0
    val q = len >> shift1
    val r = len & mask1
    while (i < q) { zs(i) = mapp1(f, size1, bs(i)); i += 1 }
    if (r != 0) zs(q) = mapp1(f, r, bs(q))
    zs
  }

  final def mapp3[@spec A, @spec Z:ClassTag](f:A => Z, len:Int, cs:Array3[A]) = {
    val zs = alloc0[Array2[Z]]
    var i = 0
    val q = len >> shift2
    val r = len & mask2
    while (i < q) { zs(i) = mapp2(f, size2, cs(i)); i += 1 }
    if (r != 0) zs(q) = mapp2(f, r, cs(q))
    zs
  }

  final def mapp4[@spec A, @spec Z:ClassTag](f:A => Z, len:Int, ds:Array4[A]) = {
    val zs = alloc0[Array3[Z]]
    var i = 0
    val q = len >> shift3
    val r = len & mask3
    while (i < q) { zs(i) = mapp3(f, size3, ds(i)); i += 1 }
    if (r != 0) zs(q) = mapp3(f, r, ds(q))
    zs
  }

  final def mapp5[@spec A, @spec Z:ClassTag](f:A => Z, len:Int, es:Array5[A]) = {
    val zs = alloc0[Array4[Z]]
    var i = 0
    val q = len >> shift4
    val r = len & mask4
    while (i < q) { zs(i) = mapp4(f, size4, es(i)); i += 1 }
    if (r != 0) zs(q) = mapp4(f, r, es(q))
    zs
  }

  final def mapp6[@spec A, @spec Z:ClassTag](f:A => Z, len:Int, fs:Array6[A]) = {
    val zs = alloc0[Array5[Z]]
    var i = 0
    val q = len >> shift5
    val r = len & mask5
    while (i < q) { zs(i) = mapp5(f, size4, fs(i)); i += 1 }
    if (r != 0) zs(q) = mapp5(f, r, fs(q))
    zs
  }
}

trait Vector[@spec A] {
  override def toString = {
    val sb = new StringBuffer("Vector(")
    if (length > 0) {
      sb.append(apply(0).toString)
      for (i <- 1 until length) {
        sb.append(", ")
        sb.append(apply(i).toString)
      }
    }
    sb.append(")")
    sb.toString
  }

  override def equals(other:Any):Boolean = other match {
    case v:Vector[A] => if (length == v.length) {
      for (i <- 0 until length) {
        if (apply(i) != v(i)) return false
      }
      true
    } else {
      false
    }
    case _ => false
  }
    

  def apply(i:Int):A
  def updated(i:Int, a:A):Vector[A]
  def append(a:A):Vector[A]
  def length:Int

  def foreach(f:A => Unit):Unit
  def map[@spec B:ClassTag](f:A => B):Vector[B]
}

import Vector._

class Vector1[@spec A:ClassTag] (n:Int, as:Array[A]) extends Vector[A] {
  def length = n

  def apply(i:Int):A = as(i)

  def updated(i:Int, a:A) = new Vector1(n, copymod1(as, i, a))

  def append(a:A) = if (n < size1) {
    new Vector1(n + 1, copymod1(as, n, a))
  } else {
    new Vector2(n + 1, alloc2(as, alloc1(a)))
  }

  def foreach(f:A => Unit) = iter1(f, n, as)

  def map[@spec B:ClassTag](f:A => B):Vector[B] = new Vector1(n, mapp1(f, n, as))
}

class Vector2[@spec A:ClassTag] (n:Int, bs:Array2[A]) extends Vector[A] {
  def length = n

  @inline final def y(i:Int) = i >> shift2
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):A = bs(y(i))(z(i))

  def updated(i:Int, a:A) = {
    new Vector2(n, copymod2(bs, y(i), z(i), a))
  }

  def append(a:A) = if (n < size2) {
    new Vector2(n + 1, copymod2(bs, y(n), z(n), a))
  } else {
    new Vector3(n + 1, alloc2(bs, alloc1(alloc1(a))))
  }

  def foreach(f:A => Unit) = iter2(f, n, bs)

  def map[@spec B:ClassTag](f:A => B):Vector[B] = new Vector2(n, mapp2(f, n, bs))
}

class Vector3[@spec A:ClassTag] (n:Int, cs:Array3[A]) extends Vector[A] {
  def length = n

  @inline final def x(i:Int) = i >> shift3
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):A = cs(x(i))(y(i))(z(i))

  def updated(i:Int, a:A) = {
    new Vector3(n, copymod3(cs, x(i), y(i), z(i), a))
  }

  def append(a:A) = if (n < size3) {
    new Vector3(n + 1, copymod3(cs, x(n), y(n), z(n), a))
  } else {
    new Vector4(n + 1, alloc2(cs, alloc1(alloc1(alloc1(a)))))
  }

  def foreach(f:A => Unit) = iter3(f, n, cs)

  def map[@spec B:ClassTag](f:A => B):Vector[B] = new Vector3(n, mapp3(f, n, cs))
}

class Vector4[@spec A:ClassTag] (n:Int, ds:Array4[A]) extends Vector[A] {
  def length = n

  @inline final def w(i:Int) = i >> shift4
  @inline final def x(i:Int) = (i >> shift3) & 31
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):A = ds(w(i))(x(i))(y(i))(z(i))

  def updated(i:Int, a:A) = {
    new Vector4(n, copymod4(ds, w(i), x(i), y(i), z(i), a))
  }

  def append(a:A) = if (n < size4) {
    new Vector4(n + 1, copymod4(ds, w(n), x(n), y(n), z(n), a))
  } else {
    new Vector5(n + 1, alloc2(ds, alloc1(alloc1(alloc1(alloc1(a))))))
  }

  def foreach(f:A => Unit) = iter4(f, n, ds)

  def map[@spec B:ClassTag](f:A => B):Vector[B] = new Vector4(n, mapp4(f, n, ds))
}

class Vector5[@spec A:ClassTag] (n:Int, es:Array5[A]) extends Vector[A] {
  def length = n

  @inline final def v(i:Int) = i >> shift5
  @inline final def w(i:Int) = (i >> shift4) & 31
  @inline final def x(i:Int) = (i >> shift3) & 31
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):A = es(v(i))(w(i))(x(i))(y(i))(z(i))

  def updated(i:Int, a:A) = {
    new Vector5(n, copymod5(es, v(i), w(i), x(i), y(i), z(i), a))
  }

  def append(a:A) = if (n < size5) {
    new Vector5(n + 1, copymod5(es, v(n), w(n), x(n), y(n), z(n), a))
  } else {
    new Vector6(n + 1, alloc2(es, alloc1(alloc1(alloc1(alloc1(alloc1(a)))))))
  }

  def foreach(f:A => Unit) = iter5(f, n, es)

  def map[@spec B:ClassTag](f:A => B):Vector[B] = new Vector5(n, mapp5(f, n, es))
}

class Vector6[@spec A:ClassTag] (n:Int, fs:Array6[A]) extends Vector[A] {
  def length = n

  @inline final def u(i:Int) = i >> shift6
  @inline final def v(i:Int) = (i >> shift5) & 31
  @inline final def w(i:Int) = (i >> shift4) & 31
  @inline final def x(i:Int) = (i >> shift3) & 31
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):A = fs(u(i))(v(i))(w(i))(x(i))(y(i))(z(i))

  def updated(i:Int, a:A) = {
    new Vector6(n, copymod6(fs, u(i), v(i), w(i), x(i), y(i), z(i), a))
  }

  def append(a:A) = if (n < size6) {
    new Vector6(n + 1, copymod6(fs, u(n), v(n), w(n), x(n), y(n), z(n), a))
  } else {
    sys.error("vector overflow: max size exceeded")
  }

  def foreach(f:A => Unit) = iter6(f, n, fs)

  def map[@spec B:ClassTag](f:A => B):Vector[B] = new Vector6(n, mapp6(f, n, fs))
}

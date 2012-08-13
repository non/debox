package debox.vector

import debox._
import debox.buffer.Buffer

import scala.reflect.ClassTag

import scala.{specialized => spec}

object IntVector {
  @inline final def size1 = 32
  @inline final def size2 = 1024
  @inline final def size3 = 32768
  @inline final def size4 = 1048576
  @inline final def size5 = 33554432
  @inline final def size6 = 1073741824

  @inline final def mask2 = 31
  @inline final def mask3 = 1023
  @inline final def mask4 = 32767
  @inline final def mask5 = 1048575
  @inline final def mask6 = 33554431

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

  final def apply():IntVector = new IntVector1(0, alloc0[Int])
  final def empty():IntVector = new IntVector1(0, alloc0[Int])

  final def copymod1(as:Array[Int], z:Int, a:Int) = {
    val as2 = as.clone
    as2(z) = a
    as2
  }

  final def copymod2(bs:Array2[Int], y:Int, z:Int, a:Int) = {
    val bs2 = bs.clone
    val as2 = if (bs2(y) == null) alloc0[Int] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    bs2
  }

  final def copymod3(cs:Array3[Int], x:Int, y:Int, z:Int, a:Int) = {
    val cs2:Array3[Int] = cs.clone
    val bs2:Array2[Int] = if (cs2(x) == null) alloc0[Array[Int]] else cs2(x).clone
    val as2:Array[Int] = if (bs2(y) == null) alloc0[Int] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    cs2
  }

  final def copymod4(ds:Array4[Int], w:Int, x:Int, y:Int, z:Int, a:Int) = {
    val ds2 = ds.clone
    val cs2 = if (ds2(w) == null) alloc0[Array2[Int]] else ds2(w).clone
    val bs2 = if (cs2(x) == null) alloc0[Array[Int]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[Int] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    ds2(w) = cs2
    ds2
  }

  final def copymod5(es:Array5[Int], v:Int, w:Int, x:Int, y:Int, z:Int, a:Int) = {
    val es2 = es.clone
    val ds2 = if (es2(v) == null) alloc0[Array3[Int]] else es2(v).clone
    val cs2 = if (ds2(w) == null) alloc0[Array2[Int]] else ds2(w).clone
    val bs2 = if (cs2(x) == null) alloc0[Array[Int]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[Int] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    ds2(w) = cs2
    es2(v) = ds2
    es2
  }

  final def copymod6(fs:Array6[Int], u:Int, v:Int, w:Int, x:Int, y:Int, z:Int, a:Int) = {
    val fs2 = fs.clone
    val es2 = if (fs2(u) == null) alloc0[Array4[Int]] else fs2(u).clone
    val ds2 = if (es2(v) == null) alloc0[Array3[Int]] else es2(v).clone
    val cs2 = if (ds2(w) == null) alloc0[Array2[Int]] else ds2(w).clone
    val bs2 = if (cs2(x) == null) alloc0[Array[Int]] else cs2(x).clone
    val as2 = if (bs2(y) == null) alloc0[Int] else bs2(y).clone
    as2(z) = a
    bs2(y) = as2
    cs2(x) = bs2
    ds2(w) = cs2
    es2(v) = ds2
    fs2(u) = es2
    fs2
  }

  @inline final def alloc0[@spec A:ClassTag] = new Array[A](32)

  @inline final def alloc1[@spec A:ClassTag](a:A) = {
    val arr = new Array[A](32)
    arr(0) = a
    arr
  }

  @inline final def alloc2[@spec A:ClassTag](a0:A, a1:A) = {
    val arr = new Array[A](32)
    arr(0) = a0
    arr(1) = a1
    arr
  }

  final def iter1(f:Int => Unit, len:Int, as:Array[Int]) {
    var i = 0
    while (i < len) { f(as(i)); i += 1 }
  }

  final def iter2(f:Int => Unit, len:Int, bs:Array2[Int]) {
    var i = 0
    val q = len >> shift2
    val r = len & mask2
    while (i < q) { iter1(f, size1, bs(i)); i += 1 }
    if (r != 0) iter1(f, r, bs(q))
  }

  final def iter3(f:Int => Unit, len:Int, cs:Array3[Int]) {
    var i = 0
    val q = len >> shift3
    val r = len & mask3
    while (i < q) { iter2(f, size2, cs(i)); i += 1 }
    if (r != 0) iter2(f, r, cs(q))
  }

  final def iter4(f:Int => Unit, len:Int, ds:Array4[Int]) {
    var i = 0
    val q = len >> shift4
    val r = len & mask4
    while (i < q) { iter3(f, size3, ds(i)); i += 1 }
    if (r != 0) iter3(f, r, ds(q))
  }

  final def iter5(f:Int => Unit, len:Int, es:Array5[Int]) {
    var i = 0
    val q = len >> shift5
    val r = len & mask5
    while (i < q) { iter4(f, size4, es(i)); i += 1 }
    if (r != 0) iter4(f, r, es(q))
  }

  final def iter6(f:Int => Unit, len:Int, fs:Array6[Int]) {
    var i = 0
    val q = len >> shift6
    val r = len & mask6
    while (i < q) { iter5(f, size4, fs(i)); i += 1 }
    if (r != 0) iter5(f, r, fs(q))
  }

  final def mapp1[@spec Z:ClassTag](f:Int => Z, len:Int, as:Array[Int]) = {
    val zs = alloc0[Z]
    var i = 0
    while (i < len) { zs(i) = f(as(i)); i += 1 }
    zs
  }

  final def mapp2[@spec Z:ClassTag](f:Int => Z, len:Int, bs:Array2[Int]) = {
    val zs = alloc0[Array[Z]]
    var i = 0
    val q = len >> shift2
    val r = len & mask2
    while (i < q) { zs(i) = mapp1(f, size1, bs(i)); i += 1 }
    if (r != 0) zs(q) = mapp1(f, r, bs(q))
    zs
  }

  final def mapp3[@spec Z:ClassTag](f:Int => Z, len:Int, cs:Array3[Int]) = {
    val zs = alloc0[Array2[Z]]
    var i = 0
    val q = len >> shift3
    val r = len & mask3
    while (i < q) { zs(i) = mapp2(f, size2, cs(i)); i += 1 }
    if (r != 0) zs(q) = mapp2(f, r, cs(q))
    zs
  }

  final def mapp4[@spec Z:ClassTag](f:Int => Z, len:Int, ds:Array4[Int]) = {
    val zs = alloc0[Array3[Z]]
    var i = 0
    val q = len >> shift4
    val r = len & mask4
    while (i < q) { zs(i) = mapp3(f, size3, ds(i)); i += 1 }
    if (r != 0) zs(q) = mapp3(f, r, ds(q))
    zs
  }

  final def mapp5[@spec Z:ClassTag](f:Int => Z, len:Int, es:Array5[Int]) = {
    val zs = alloc0[Array4[Z]]
    var i = 0
    val q = len >> shift5
    val r = len & mask5
    while (i < q) { zs(i) = mapp4(f, size4, es(i)); i += 1 }
    if (r != 0) zs(q) = mapp4(f, r, es(q))
    zs
  }

  final def mapp6[@spec Z:ClassTag](f:Int => Z, len:Int, fs:Array6[Int]) = {
    val zs = alloc0[Array5[Z]]
    var i = 0
    val q = len >> shift6
    val r = len & mask6
    while (i < q) { zs(i) = mapp5(f, size4, fs(i)); i += 1 }
    if (r != 0) zs(q) = mapp5(f, r, fs(q))
    zs
  }
}

trait IntVector {
  override def toString = {
    val sb = new StringBuffer("IntVector(")
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
    case v:IntVector => if (length == v.length) {
      for (i <- 0 until length) {
        if (apply(i) != v(i)) return false
      }
      true
    } else {
      false
    }
    case _ => false
  }
    

  def apply(i:Int):Int
  def updated(i:Int, a:Int):IntVector
  def append(a:Int):IntVector
  def length:Int

  def foreach(f:Int => Unit):Unit
  def map[@spec Z:ClassTag](f:Int => Z):Vector[Z]
}

import IntVector._

class IntVector1(n:Int, as:Array[Int]) extends IntVector {
  def length = n

  def apply(i:Int):Int = as(i)

  def updated(i:Int, a:Int) = new IntVector1(n, copymod1(as, i, a))

  def append(a:Int) = if (n < size1) {
    new IntVector1(n + 1, copymod1(as, n, a))
  } else {
    new IntVector2(n + 1, alloc2(as, alloc1(a)))
  }

  def foreach(f:Int => Unit) = iter1(f, n, as)

  def map[@spec Z:ClassTag](f:Int => Z) = new Vector1(n, mapp1(f, n, as))
}

class IntVector2(n:Int, bs:Array2[Int]) extends IntVector {
  def length = n

  @inline final def y(i:Int) = i >> shift2
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):Int = bs(y(i))(z(i))

  def updated(i:Int, a:Int) = {
    new IntVector2(n, copymod2(bs, y(i), z(i), a))
  }

  def append(a:Int) = if (n < size2) {
    new IntVector2(n + 1, copymod2(bs, y(n), z(n), a))
  } else {
    new IntVector3(n + 1, alloc2(bs, alloc1(alloc1(a))))
  }

  def foreach(f:Int => Unit) = iter2(f, n, bs)

  def map[@spec Z:ClassTag](f:Int => Z) = new Vector2(n, mapp2(f, n, bs))
}

class IntVector3(n:Int, cs:Array3[Int]) extends IntVector {
  def length = n

  @inline final def x(i:Int) = i >> shift3
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):Int = cs(x(i))(y(i))(z(i))

  def updated(i:Int, a:Int) = {
    new IntVector3(n, copymod3(cs, x(i), y(i), z(i), a))
  }

  def append(a:Int) = if (n < size3) {
    new IntVector3(n + 1, copymod3(cs, x(n), y(n), z(n), a))
  } else {
    new IntVector4(n + 1, alloc2(cs, alloc1(alloc1(alloc1(a)))))
  }

  def foreach(f:Int => Unit) = iter3(f, n, cs)

  def map[@spec Z:ClassTag](f:Int => Z) = new Vector3(n, mapp3(f, n, cs))
}

class IntVector4(n:Int, ds:Array4[Int]) extends IntVector {
  def length = n

  @inline final def w(i:Int) = i >> shift4
  @inline final def x(i:Int) = (i >> shift3) & 31
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):Int = ds(w(i))(x(i))(y(i))(z(i))

  def updated(i:Int, a:Int) = {
    new IntVector4(n, copymod4(ds, w(i), x(i), y(i), z(i), a))
  }

  def append(a:Int) = if (n < size4) {
    new IntVector4(n + 1, copymod4(ds, w(n), x(n), y(n), z(n), a))
  } else {
    new IntVector5(n + 1, alloc2(ds, alloc1(alloc1(alloc1(alloc1(a))))))
  }

  def foreach(f:Int => Unit) = iter4(f, n, ds)

  def map[@spec Z:ClassTag](f:Int => Z) = new Vector4(n, mapp4(f, n, ds))
}

class IntVector5(n:Int, es:Array5[Int]) extends IntVector {
  def length = n

  @inline final def v(i:Int) = i >> shift5
  @inline final def w(i:Int) = (i >> shift4) & 31
  @inline final def x(i:Int) = (i >> shift3) & 31
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):Int = es(v(i))(w(i))(x(i))(y(i))(z(i))

  def updated(i:Int, a:Int) = {
    new IntVector5(n, copymod5(es, v(i), w(i), x(i), y(i), z(i), a))
  }

  def append(a:Int) = if (n < size5) {
    new IntVector5(n + 1, copymod5(es, v(n), w(n), x(n), y(n), z(n), a))
  } else {
    new IntVector6(n + 1, alloc2(es, alloc1(alloc1(alloc1(alloc1(alloc1(a)))))))
  }

  def foreach(f:Int => Unit) = iter5(f, n, es)

  def map[@spec Z:ClassTag](f:Int => Z) = new Vector5(n, mapp5(f, n, es))
}

class IntVector6(n:Int, fs:Array6[Int]) extends IntVector {
  def length = n

  @inline final def u(i:Int) = i >> shift6
  @inline final def v(i:Int) = (i >> shift5) & 31
  @inline final def w(i:Int) = (i >> shift4) & 31
  @inline final def x(i:Int) = (i >> shift3) & 31
  @inline final def y(i:Int) = (i >> shift2) & 31
  @inline final def z(i:Int) = i & 31

  def apply(i:Int):Int = fs(u(i))(v(i))(w(i))(x(i))(y(i))(z(i))

  def updated(i:Int, a:Int) = {
    new IntVector6(n, copymod6(fs, u(i), v(i), w(i), x(i), y(i), z(i), a))
  }

  def append(a:Int) = if (n < size6) {
    new IntVector6(n + 1, copymod6(fs, u(n), v(n), w(n), x(n), y(n), z(n), a))
  } else {
    sys.error("vector overflow: max size exceeded")
  }

  def foreach(f:Int => Unit) = iter6(f, n, fs)

  def map[@spec Z:ClassTag](f:Int => Z) = new Vector6(n, mapp6(f, n, fs))
}

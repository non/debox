package debox

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.{specialized => spec}

import debox.map.Map
import debox.set.Set

import language.implicitConversions
import language.experimental.macros

// making the Ops classes extend AnyVal seems to crash the compiler.
// it would be nice to provide macro implementations for all of them, either
// in terms of the basic foreach_ macro or independently. for now this is
// probably good enough. 

class MapMacroOps[A, B](m: Map[A, B]) {
  def foreach_(f: (A, B) => Unit): Unit = macro Macros.mapForeach[A, B]
}

class MapOps[A:ClassTag, B:ClassTag](m: Map[A, B]) {//extends AnyVal {
  def unionLeft(that: Map[A, B]): Map[A, B] = {
    val out = that.copy
    Ext.mapMacroOps(m).foreach_((k, v) => out(k) = v)
    out
  }
  
  def unionRight(that: Map[A, B]): Map[A, B] = new MapOps(that).unionLeft(m)
  
  def unionMerge(that: Map[A, B])(f: (B, B) => B): Map[A, B] = {
    val out = that.copy
    Ext.mapMacroOps(m).foreach_((k, v) => out(k) = if (out.contains(k)) f(v, out(k)) else v)
    out
  }
}

class SetMacroOps[A: ClassTag](m: Set[A]) {
  def foreach_(f: A => Unit): Unit = macro Macros.setForeach[A]
  def fold[T](init: T)(f: (T, A) => T): T = macro Macros.setFold[A, T]
  def loopWhile(p: A => Boolean): Boolean = macro Macros.setLoopWhile[A]
  def loopUntil(p: A => Boolean): Boolean = macro Macros.setLoopUntil[A]
}

class SetOps[@spec(Int, Long, Double, AnyRef) A](m: Set[A]) {
  def union(that: Set[A])(implicit ct: ClassTag[A]): Set[A] = {
    if (m.length > that.length) return Ext.setOps(that).union(m)
    val out = that.copy
    Ext.setMacroOps(m).foreach_(a => out.add(a))
    out
  }
  
  def intersection(that: Set[A])(implicit ct: ClassTag[A]): Set[A] = {
    if (m.length < that.length) return Ext.setOps(that).intersection(m)
    val out = Set.empty[A]
    Ext.setMacroOps(m).foreach_(a => if (that(a)) out.add(a))
    out
  }
  
  def difference(that: Set[A])(implicit ct: ClassTag[A]): Set[A] = {
    val out = Set.empty[A]
    Ext.setMacroOps(m).foreach_(a => if (!that(a)) out.add(a))
    out
  }

  def extend(that: Set[A])(implicit ct:ClassTag[A]): Unit =
    Ext.setMacroOps(that).foreach_(a => m.add(a))

  def isSubsetOf(that: Set[A])(implicit ct:ClassTag[A]): Boolean = {
    if (m.length > that.length) return false
    Ext.setMacroOps(m).loopWhile(that)
  }

  def isSupersetOf(that: Set[A])(implicit ct:ClassTag[A]): Boolean = new SetOps(that).isSubsetOf(m)

  def exists(p: A => Boolean)(implicit ct:ClassTag[A]): Boolean = Ext.setMacroOps(m).loopUntil(p)
  def forall(p: A => Boolean)(implicit ct:ClassTag[A]): Boolean = Ext.setMacroOps(m).loopWhile(p)

  def count(p: A => Boolean)(implicit ct:ClassTag[A]): Int = Ext.setMacroOps(m).fold(0) {
    (n, a) => if (p(a)) n + 1 else n
  }

  def partition(p: A => Boolean)(implicit ct: ClassTag[A]): (Set[A], Set[A]) = {
    val n = m.items.length
    val no = Set.empty[A]
    val yes = Set.empty[A]
    Ext.setMacroOps(m).foreach_(a => if (p(a)) yes.add(a) else no.add(a))
    (no, yes)
  }
}

object Ext {
  implicit def mapOps[A:ClassTag, B:ClassTag](m: Map[A, B]) = new MapOps(m)
  implicit def mapMacroOps[A:ClassTag, B:ClassTag](m: Map[A, B]) = new MapMacroOps(m)
  implicit def setOps[A:ClassTag](m: Set[A]) = new SetOps(m)
  implicit def setMacroOps[A:ClassTag](m: Set[A]) = new SetMacroOps(m)
}

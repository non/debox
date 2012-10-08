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

class MapOps[A:ClassTag:Hash, B:ClassTag](m: Map[A, B]) {//extends AnyVal {
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

class SetMacroOps[A](m: Set[A]) {
  def foreach_(f: A => Unit): Unit = macro Macros.setForeach[A]
  def fold[T](init: T)(f: (T, A) => T): T = macro Macros.setFold[A, T]
  def loopWhile(p: A => Boolean): Boolean = macro Macros.setLoopWhile[A]
  def loopUntil(p: A => Boolean): Boolean = macro Macros.setLoopUntil[A]
}

class SetOps[@spec A:ClassTag:Hash](m: Set[A]) {
  def map[@spec B:ClassTag:Hash](f: A => B): Set[B] = {
    //val out = Set.empty[B]
    val out = Set.ofDim[B](m.items.length / 2)
    Ext.setMacroOps(m).foreach_(a => out.add(f(a)))
    out
  }

  def union(that: Set[A]): Set[A] = {
    if (m.length > that.length) return Ext.setOps(that).union(m)
    val out = that.copy
    Ext.setMacroOps(m).foreach_(a => out.add(a))
    out
  }
  
  def intersection(that: Set[A]): Set[A] = {
    if (m.length < that.length) return Ext.setOps(that).intersection(m)
    val out = Set.empty[A]
    Ext.setMacroOps(m).foreach_(a => if (that(a)) out.add(a))
    out
  }
  
  def difference(that: Set[A]): Set[A] = {
    val out = Set.empty[A]
    Ext.setMacroOps(m).foreach_(a => if (!that(a)) out.add(a))
    out
  }

  def extend(that: Set[A]): Unit = Ext.setMacroOps(that).foreach_(a => m.add(a))

  def isSubsetOf(that: Set[A]): Boolean = {
    if (m.length > that.length) return false
    Ext.setMacroOps(m).loopWhile(that)
  }

  def isSupersetOf(that: Set[A]): Boolean = new SetOps(that).isSubsetOf(m)

  def exists(p: A => Boolean): Boolean = Ext.setMacroOps(m).loopUntil(p)
  def forall(p: A => Boolean): Boolean = Ext.setMacroOps(m).loopWhile(p)

  def count(p: A => Boolean): Int = Ext.setMacroOps(m).fold(0) {
    (n, a) => if (p(a)) n + 1 else n
  }

  def partition(p: A => Boolean): (Set[A], Set[A]) = {
    val n = m.items.length
    val no = Set.empty[A]
    val yes = Set.empty[A]
    Ext.setMacroOps(m).foreach_(a => if (p(a)) yes.add(a) else no.add(a))
    (no, yes)
  }
}

object Ext {
  implicit def mapOps[A:ClassTag:Hash, B:ClassTag](m: Map[A, B]) = new MapOps(m)
  implicit def mapMacroOps[A:ClassTag:Hash, B:ClassTag](m: Map[A, B]) = new MapMacroOps(m)
  implicit def setOps[A:ClassTag:Hash](m: Set[A]) = new SetOps(m)
  implicit def setMacroOps[A:ClassTag:Hash](m: Set[A]) = new SetMacroOps(m)
}

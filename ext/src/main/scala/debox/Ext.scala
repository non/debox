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
  def foreach_(f: A => Unit): Unit = macro set.Macros.foreach[A]
}

object Ext {
  implicit def mapOps[A:ClassTag, B:ClassTag](m: Map[A, B]) = new MapOps(m)
  implicit def mapMacroOps[A:ClassTag, B:ClassTag](m: Map[A, B]) = new MapMacroOps(m)

  implicit def setMacroOps[A:ClassTag](m: Set[A]) = new SetMacroOps(m)
}

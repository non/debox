package debox

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import spire.algebra.Order
import spire.std.any._

import scala.collection.mutable
import scala.reflect._

abstract class SetCheck[A: Arbitrary: ClassTag: Order]
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import debox.{Set => DSet, Map => DMap}

  def hybridEq[A](d: DSet[A], s: mutable.Set[A]): Boolean =
    d.size == s.size && d.forall(s.contains)

  property("fromArray") {
    forAll { xs: Array[A] =>
      val set = DSet.fromArray(xs)
      val control = mutable.Set(xs.toSeq: _*)
      hybridEq(set, control) shouldBe true
    }
  }

  property("fromIterable, apply") {
    forAll { xs: List[A] =>
      val set1 = DSet.fromIterable(xs)
      val set2 = DSet(xs: _*)
      val control = mutable.Set(xs: _*)
      hybridEq(set1, control) shouldBe true
      hybridEq(set2, control) shouldBe true
    }
  }

  property("equals (==), hashCode (##)") {
    forAll { xs: List[A] =>
      val a = DSet.fromIterable(xs)
      val b = DSet.fromIterable(xs.reverse)
      a shouldBe b
      a.## shouldBe b.##
    }
  }

  property("copy") {
    forAll { xs: List[A] =>
      val a = DSet.fromIterable(xs)
      val b = a.copy
      a shouldBe b
      xs.foreach { x =>
        a -= x
        a(x) shouldBe false
        b(x) shouldBe true
        a should not be b
      }
    }
  }

  property("clear") {
    forAll { xs: List[A] =>
      val a = DSet.fromIterable(xs)
      a.clear
      a shouldBe DSet.empty[A]
    }
  }

  property("adding elements (+=)") {
    forAll { xs: Set[A] =>
      val set = DSet.empty[A]
      val control = mutable.Set.empty[A]
      xs.foreach { x =>
        set += x
        control += x
        set(x) shouldBe true
        hybridEq(set, control) shouldBe true
      }
    }
  }

  property("removing elements (-=)") {
    forAll { xs: Set[A] =>
      val set = DSet.fromIterable(xs)
      val control = mutable.Set(xs.toSeq: _*)
      xs.foreach { x =>
        set -= x
        control -= x
        set(x) shouldBe false
        hybridEq(set, control) shouldBe true
      }
    }
  }

  property("random += and -=") {
    forAll { (tpls: List[(A, Boolean)]) =>
      val set = DSet.empty[A]
      val control = mutable.Set.empty[A]
      tpls.foreach {
        case (x, true) => set += x; control += x
        case (x, false) => set -= x; control -= x
      }
      hybridEq(set, control) shouldBe true
    }
  }

  property("bulk add (++=)") {
    forAll { (xs: List[A], ys: List[A]) =>
      val set = DSet.empty[A]
      val control = mutable.Set.empty[A]

      set ++= xs
      control ++= xs
      hybridEq(set, control) shouldBe true

      set ++= ys
      control ++= ys
      hybridEq(set, control) shouldBe true
    }
  }

  property("union (|)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = DSet.fromIterable(xs)
      val bs = DSet.fromIterable(ys)
      val cs = DSet.fromIterable(xs | ys)
      (as | bs) shouldBe cs
    }
  }

  property("intersection (&)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = DSet.fromIterable(xs)
      val bs = DSet.fromIterable(ys)
      val cs = DSet.fromIterable(xs & ys)
      (as & bs) shouldBe cs
    }
  }
  
  property("difference (--)") {
    forAll { (xs: Set[A], ys: Set[A]) =>
      val as = DSet.fromIterable(xs)
      val bs = DSet.fromIterable(ys)
      val cs = DSet.fromIterable(xs -- ys)
      (as -- bs) shouldBe cs
    }
  }

  property("foreach") {
    forAll { (xs: Set[A]) =>
      val a = DSet.fromIterable(xs)
      val b = DSet.empty[A]
      a.foreach(b += _)
      a shouldBe b
    }
  }

  property("map") {
    forAll { (xs: Set[A], f: A => A) =>
      val a = DSet.fromIterable(xs)
      a.map(x => x) shouldBe a
      a.map(f) shouldBe DSet.fromIterable(xs.map(f))
    }
  }

  property("map composition") {
    forAll { (xs: Set[A], f: A => A, g: A => A) =>
      val set = DSet.fromIterable(xs)
      set.map(a => g(f(a))) shouldBe set.map(f).map(g)
    }
  }

  property("partition") {
    forAll { (xs: Set[A], f: A => Boolean) =>
      val a = DSet.fromIterable(xs)
      val (b, c) = a.partition(f)
      b.foreach { x => a(x) shouldBe true }
      c.foreach { x => a(x) shouldBe true }
      a.size shouldBe (b.size + c.size)
      b.exists(f) shouldBe false
      c.forall(f) shouldBe true
    }
  }

  property("find / exists") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = DSet.fromIterable(xs)
      a.find(p).isDefined shouldBe a.exists(p)
    }
  }

  property("findAll / count") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = DSet.fromIterable(xs)
      a.findAll(p).size shouldBe a.count(p)
    }
  }

  property("findAll / filterSelf") {
    forAll { (xs: Set[A], p: A => Boolean) =>
      val a = DSet.fromIterable(xs)
      val b = DSet.fromIterable(xs)
      a.filterSelf(p)
      a shouldBe b.findAll(p)
    }
  }

  property("toBuffer") {
    forAll { (xs: Array[A]) =>
      val set1 = DSet.fromArray(xs)
      val buf1 = set1.toBuffer
      val buf2 = Buffer.fromArray(xs)
      buf1.sort
      buf2.sort
      //buf1 shouldBe buf2
    }
  }

  property("toMap") {
    forAll { (xs: List[A]) =>
      val set1 = DSet.fromIterable(xs)
      val map1 = set1.toMap(a => a)
      val map2 = DMap.fromIterable(xs.map(a => (a, a)))
      map1 shouldBe map2
    }
  }
}

class BooleanSetCheck extends SetCheck[Boolean]
class IntSetCheck extends SetCheck[Int]
class DoubleSetCheck extends SetCheck[Double]
class StringSetCheck extends SetCheck[String]

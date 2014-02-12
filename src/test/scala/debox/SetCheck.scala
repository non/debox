package debox

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import scala.collection.mutable
import scala.reflect._

abstract class SetCheck[A: Arbitrary: ClassTag]
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import debox.{Set => DSet}

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
}

class BooleanSetCheck extends SetCheck[Boolean]
class IntSetCheck extends SetCheck[Int]
class DoubleSetCheck extends SetCheck[Double]
class StringSetCheck extends SetCheck[String]

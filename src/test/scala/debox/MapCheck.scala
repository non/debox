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
import scala.{specialized => sp}

abstract class MapCheck[A: Arbitrary: ClassTag, B: Arbitrary: ClassTag]
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import scala.collection.immutable.Map
  import debox.{Map => DMap}

  def hybridEq[A](d: DMap[A, B], s: mutable.Map[A, B]): Boolean =
    d.size == s.size && s.forall { case (k, v) => d.get(k) == Some(v) }

  property("fromArrays") {
    forAll { (pairs: List[(A, B)]) =>
      val (ks, vs) = pairs.unzip
      val map = DMap.fromArrays(ks.toArray, vs.toArray)
      val control = mutable.Map(pairs: _*)
      hybridEq(map, control) shouldBe true
    }
  }

  property("fromIterable, apply") {
    forAll { pairs: List[(A, B)] =>
      val map1 = DMap.fromIterable(pairs)
      val map2 = DMap(pairs: _*)
      val control = mutable.Map(pairs: _*)
      hybridEq(map1, control) shouldBe true
      hybridEq(map2, control) shouldBe true
    }
  }

  property("equals (==), hashCode (##)") {
    forAll { (xs0: Map[A, B], ys: Map[A, B]) =>
      val xs = xs0.toList
      val a = DMap.fromIterable(xs)
      val b = DMap.fromIterable(xs.reverse)
      a shouldBe b
      a.## shouldBe b.##

      val c = DMap.fromIterable(ys)
      if (xs0 == ys) {
        a shouldBe c
        a.## shouldBe c.##
      } else {
        a should not be c
      }
    }
  }

  property("copy") {
    forAll { kvs: List[(A, B)] =>
      val a = DMap.fromIterable(kvs)
      val b = a.copy
      a shouldBe b
      kvs.foreach { case (k, _) =>
        a.remove(k)
        a.contains(k) shouldBe false
        b.contains(k) shouldBe true
        a should not be b
      }
    }
  }

  property("clear") {
    forAll { kvs: List[(A, B)] =>
      val a = DMap.fromIterable(kvs)
      a.clear
      a shouldBe DMap.empty[A, B]
    }
  }

  property("adding elements (update)") {
    forAll { kvs: Map[A, B] =>
      val map = DMap.empty[A, B]
      val control = mutable.Map.empty[A, B]
      kvs.foreach { case (k, v) =>
        map(k) = v
        control(k) = v
        map.contains(k) shouldBe true
        hybridEq(map, control) shouldBe true
      }
    }
  }

  property("removing elements (remove)") {
    forAll { kvs: Map[A, B] =>
      val map = DMap.fromIterable(kvs)
      val control = mutable.Map(kvs.toSeq: _*)
      kvs.foreach { case (k, v) =>
        map.remove(k)
        control -= k
        map.contains(k) shouldBe false
        hybridEq(map, control) shouldBe true
      }
    }
  }

  property("random += and -=") {
    forAll { (pairs: List[(A, Option[B])]) =>
      val map = DMap.empty[A, B]
      val control = mutable.Map.empty[A, B]
      pairs.foreach {
        case (a, Some(b)) => map(a) = b; control(a) = b
        case (a, None) => map.remove(a); control -= a
      }
      hybridEq(map, control) shouldBe true
    }
  }

  // property("bulk add (++=)") {
  //   forAll { (xs: List[A], ys: List[A]) =>
  //     val map = DMap.empty[A]
  //     val control = mutable.Map.empty[A]
  // 
  //     map ++= xs
  //     control ++= xs
  //     hybridEq(map, control) shouldBe true
  // 
  //     map ++= ys
  //     control ++= ys
  //     hybridEq(map, control) shouldBe true
  //   }
  // }

  // property("union (|)") {
  //   forAll { (xs: Map[A], ys: Map[A]) =>
  //     val as = DMap.fromIterable(xs)
  //     val bs = DMap.fromIterable(ys)
  //     val cs = DMap.fromIterable(xs | ys)
  //     (as | bs) shouldBe cs
  //   }
  // }
  // 
  // property("intersection (&)") {
  //   forAll { (xs: Map[A], ys: Map[A]) =>
  //     val as = DMap.fromIterable(xs)
  //     val bs = DMap.fromIterable(ys)
  //     val cs = DMap.fromIterable(xs & ys)
  //     (as & bs) shouldBe cs
  //   }
  // }
  // 
  // property("difference (--)") {
  //   forAll { (xs: Map[A], ys: Map[A]) =>
  //     val as = DMap.fromIterable(xs)
  //     val bs = DMap.fromIterable(ys)
  //     val cs = DMap.fromIterable(xs -- ys)
  //     (as -- bs) shouldBe cs
  //   }
  // }
}

class IntIntMapCheck extends MapCheck[Int, Int]
class IntBooleanMapCheck extends MapCheck[Int, Boolean]
class IntStringMapCheck extends MapCheck[Int, String]
class StringIntMapCheck extends MapCheck[String, Int]
class StringBooleanMapCheck extends MapCheck[String, Boolean]
class StringStringMapCheck extends MapCheck[String, String]

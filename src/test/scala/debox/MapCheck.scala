package debox

import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck._
import org.scalatest.propspec._

import scala.collection.mutable
import scala.reflect._
import spire.algebra.{CMonoid, Ring}
import spire.std.any._
import spire.syntax.monoid._

abstract class MapCheck[A: Arbitrary: ClassTag: Cogen, B: Arbitrary: ClassTag: Cogen: CMonoid]
    extends AnyPropSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  import scala.collection.immutable.Set
  import scala.collection.immutable.Map
  import debox.{Map => DMap, Set => DSet}

  def hybridEq(d: DMap[A, B], s: mutable.Map[A, B]): Boolean =
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
      val b = a.copy()
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
      a.clear()
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
      kvs.foreach { case (k, _) =>
        map.remove(k)
        control -= k
        map.contains(k) shouldBe false
        hybridEq(map, control) shouldBe true
      }
    }
  }

  property("random += and -=") {
    forAll { (pairs: List[(A, B, Boolean)]) =>
      val map = DMap.empty[A, B]
      val control = mutable.Map.empty[A, B]
      pairs.foreach {
        case (a, b, true) => map(a) = b; control(a) = b
        case (a, _, false) => map.remove(a); control -= a
      }
      hybridEq(map, control) shouldBe true
    }
  }

  property("getOrElse/getOrElseUpdate") {
    forAll { (keys: Set[A], values: (B, B, B)) =>
      val map = DMap.empty[A, B]
      val control = mutable.Map.empty[A, B]
      val (a, b, c) = values
      keys.foreach {
        case k =>
          map.getOrElse(k, c) shouldBe c
          map.getOrElseUpdate(k, a) shouldBe a
          map.getOrElse(k, c) shouldBe a
          map.getOrElseUpdate(k, b) shouldBe a
          map.getOrElse(k, c) shouldBe a

          control.getOrElseUpdate(k, a)
          control.getOrElseUpdate(k, b)
      }
      hybridEq(map, control) shouldBe true
    }
  }

  property("foreach") {
    forAll { (kvs: Map[A, B]) =>
      val map1 = DMap.fromIterable(kvs)
      val map2 = DMap.empty[A, B]
      map1.foreach { (k, v) =>
        map2.contains(k) shouldBe false
        map2(k) = v
      }
      map1 shouldBe map2
    }
  }

  property("iterator") {
    forAll { (kvs: Map[A, B]) =>
      val map1 = DMap.fromIterable(kvs)
      val map2 = DMap.empty[A, B]
      map1.iterator().foreach { case (k, v) =>
        map2.contains(k) shouldBe false
        map2(k) = v
      }
      map1 shouldBe map2
    }
  }

  property("mapToSet") {
    forAll { (kvs: Map[A, B], f: (A, B) => B) =>
      val m = DMap.fromIterable(kvs)
      m.mapToSet((_, b) => b) shouldBe DSet.fromArray(m.valuesArray)

      val s2 = kvs.foldLeft(Set.empty[B]) { case (s, (a, b)) =>
        s + f(a, b)
      }
      
      m.mapToSet(f) shouldBe DSet.fromIterable(s2)
    }
  }

  property("mapItemsToMap") {

    // import Arbitrary.arbitrary
    // implicit val arbf2: Arbitrary[(A, B) => (A, B)] =
    //   Arbitrary(for {
    //     f <- arbitrary[(A, B) => A]
    //     g <- arbitrary[(A, B) => B]
    //   } yield (a, b) => (f(a, b), g(a, b)))

    forAll { (kvs: Map[A, B], f: (A, B) => (A, B)) =>
      val m = DMap.fromIterable(kvs)
      m.mapToSet((_, b) => b) shouldBe DSet.fromArray(m.valuesArray)

      val kvs2 = kvs.foldLeft(Map.empty[A, B]) { case (m, (a, b)) =>
        val (aa, bb1) = f(a, b)
        val bb2 = m.getOrElse(aa, CMonoid[B].empty)
        m.updated(aa, bb1 |+| bb2)
      }
      
      m.mapItemsToMap(f) shouldBe DMap.fromIterable(kvs2)
    }
  }

  property("mapKeys") {
    forAll { (kvs: Map[A, B], f: A => A) =>
      val m = DMap.fromIterable(kvs)
      m.mapKeys(a => a) shouldBe m

      val kvs2 = kvs.foldLeft(Map.empty[A, B]) { case (m, (a, b)) =>
        val aa = f(a)
        val bb = m.getOrElse(aa, CMonoid[B].empty)
        m.updated(aa, bb |+| b)
      }

      m.mapKeys(f) shouldBe DMap.fromIterable(kvs2)
    }
  }

  property("mapValues") {
    forAll { (kvs: Map[A, B], f: B => B) =>
      val m = DMap.fromIterable(kvs)
      m.mapValues(b => b) shouldBe m

      m.mapValues(f) shouldBe DMap.fromIterable(kvs.map {
        case (k, v) => (k, f(v))
      })
    }
  }

  property("forall / exists / findAll") {
    forAll { (kvs: Map[A, B], f: (A, B) => Boolean) =>
      val m = DMap.fromIterable(kvs)
      m.forall(f) shouldBe kvs.forall { case (a, b) => f(a, b) }
      m.exists(f) shouldBe kvs.exists { case (a, b) => f(a, b) }

      val kvs2 = kvs.filter { case (a, b) => f(a, b) }
      m.findAll(f) shouldBe DMap.fromIterable(kvs2)
    }
  }
}

object Impl {
  implicit val cmint: CMonoid[Int] = Ring[Int].additive

  // argh, why? (i guess CRig doesn't exist)
  implicit val cmboolean: CMonoid[Boolean] = new CMonoid[Boolean] {
    def empty: Boolean = false
    def combine(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
  }

  // junky but law-abiding
  implicit val cmstring: CMonoid[String] = new CMonoid[String] {
    def empty: String = ""
    def combine(lhs: String, rhs: String): String = if (lhs > rhs) lhs else rhs
  }
}

import Impl._

class IntIntMapCheck extends MapCheck[Int, Int]
class IntBooleanMapCheck extends MapCheck[Int, Boolean]
class IntStringMapCheck extends MapCheck[Int, String]
class StringIntMapCheck extends MapCheck[String, Int]
class StringBooleanMapCheck extends MapCheck[String, Boolean]
class StringStringMapCheck extends MapCheck[String, String]

package debox.syntax

import scala.reflect.ClassTag

import org.scalatest._
import prop._
import org.scalacheck.{Arbitrary, Cogen}

import debox.syntax.all._

import spire.algebra._
import spire.compat._
import spire.implicits._

abstract class SyntaxCheck[
  A: Arbitrary: ClassTag: Cogen: Order,
  B: Arbitrary: ClassTag: AdditiveCMonoid: Cogen: Order
] extends PropSpec
    with Matchers
    with PropertyChecks {

  implicit val cmonoid: CMonoid[B] =
    CMonoid.additive[B]

  property("set.toBuffer") {
    forAll { (xs: Array[A]) =>
      val x = debox.Set.fromArray(xs)
      val y = debox.Set.fromArray(x.toBuffer.toArray)
      x shouldBe y
    }
  }

  property("buffer.sort") {
    forAll { (xs: List[A]) =>
      val buf = debox.Buffer.fromIterable(xs)
      buf.sort
      buf shouldBe debox.Buffer.fromIterable(xs.sorted)
    }
  }

  property("map.mapItemsToMap") {
    forAll { (kvs: Map[A, B], f: (A, B) => (A, B)) =>
      val m = debox.Map.fromIterable(kvs)
      m.mapToSet((a, b) => b) shouldBe debox.Set.fromArray(m.valuesArray)

      val kvs2 = kvs.foldLeft(Map.empty[A, B]) { case (m, (a, b)) =>
        val (aa, bb1) = f(a, b)
        val bb2 = m.getOrElse(aa, CMonoid[B].id)
        m.updated(aa, bb1 |+| bb2)
      }

      m.mapItemsToMap(f) shouldBe debox.Map.fromIterable(kvs2)
    }
  }

  property("map.mapKeys") {
    forAll { (kvs: Map[A, B], f: A => A) =>
      val m = debox.Map.fromIterable(kvs)
      m.mapKeys(a => a) shouldBe m

      val kvs2 = kvs.foldLeft(Map.empty[A, B]) { case (m, (a, b)) =>
        val aa = f(a)
        val bb = m.getOrElse(aa, CMonoid[B].id)
        m.updated(aa, bb |+| b)
      }

      m.mapKeys(f) shouldBe debox.Map.fromIterable(kvs2)
    }
  }
}

class BooleanSyntaxCheck extends SyntaxCheck[Boolean, Int]
class IntSyntaxCheck extends SyntaxCheck[Int, Int]
class DoubleSyntaxCheck extends SyntaxCheck[Double, Double]
class StringSyntaxCheck extends SyntaxCheck[String, Double]

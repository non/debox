package debox.instances

import scala.reflect.ClassTag

import spire.algebra._
import spire.implicits._
import spire.laws._
import spire.laws.arb._

import org.typelevel.discipline.scalatest.Discipline

import debox.implicits._

import org.scalatest.FunSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

class LawTests extends FunSuite with Discipline {

  implicit def arbBuffer[A: Arbitrary: ClassTag]: Arbitrary[debox.Buffer[A]] =
    Arbitrary(arbitrary[Array[A]].map(xs => debox.Buffer.fromArray(xs)))

  checkAll("Order[Buffer[Int]]", OrderLaws[debox.Buffer[Int]].order)
  checkAll("Order[Buffer[String]]", OrderLaws[debox.Buffer[String]].order)

  checkAll("Monoid[Buffer[Int]]", GroupLaws[debox.Buffer[Int]].monoid)
  checkAll("Monoid[Buffer[String]]", GroupLaws[debox.Buffer[String]].monoid)

  checkAll("AdditiveMonoid[Buffer[Int]]", GroupLaws[debox.Buffer[Int]].additiveMonoid)
  checkAll("AdditiveMonoid[Buffer[BigInt]]", GroupLaws[debox.Buffer[BigInt]].additiveMonoid)

  checkAll("MultiplicativeMonoid[Buffer[Int]]", RingLaws[debox.Buffer[Int]].multiplicativeSemigroup)
  checkAll("MultiplicativeMonoid[Buffer[BigInt]]", RingLaws[debox.Buffer[BigInt]].multiplicativeSemigroup)
}

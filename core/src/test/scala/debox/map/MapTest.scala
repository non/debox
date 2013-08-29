package debox.map

import scala.{specialized => sp}
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

abstract class GenericMapSpec[@sp A: ClassTag: Arbitrary, @sp B: ClassTag: Arbitrary](t: String)
    extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  property(s"$t: fromMap/toMap") {
    forAll { (control: immutable.Map[A, B]) =>
      Map.fromScala(control).toScala should be === control
    }
  }

  property(s"$t: update (=)") {
    forAll { (pairs: List[(A, B)]) =>
      val (map, control) = (Map.empty[A, B], mutable.Map.empty[A, B])
      pairs.foreach { case (a, b) =>
        map(a) = b
        control(a) = b
      }
      map.toScala should be === control.toMap
    }
  }

  property(s"$t: add (+=)") {
    forAll { (pairs: List[(A, B)]) =>
      val (map, control) = (Map.empty[A, B], mutable.Map.empty[A, B])
      pairs.foreach { pair =>
        map += pair
        control += pair
      }
      map.toScala should be === control.toMap
    }
  }

  property(s"$t: bulk add (++=)") {
    forAll { (pairs: List[(A, B)]) =>
      val (map, control) = (Map.empty[A, B], mutable.Map.empty[A, B])
      map ++= pairs
      control ++= pairs
      map.toScala should be === control.toMap
    }
  }

  property(s"$t: remove (-=)") {
    forAll { pairs: List[(A, B)] =>
      val map = Map(pairs: _*)
      val control = mutable.Map(pairs: _*)
      pairs.foreach { case (a, _) =>
        map -= a
        control -= a
        map.toScala should be === control.toMap
      }
      map.isEmpty should be === true
      control.isEmpty should be === true
    }
  }

  property(s"$t: bulk remove (--=)") {
    forAll { pairs: List[(A, B)] =>
      val map = Map(pairs: _*)
      val control = mutable.Map(pairs: _*)
      map --= pairs.map(_._1)
      control --= pairs.map(_._1)
      map.isEmpty should be === true
      control.isEmpty should be === true
    }
  }

  property(s"$t: random += and -=") {
    forAll { (tpls: List[(A, B, Boolean)]) =>
      val map = Map.empty[A, B]
      val control = mutable.Map.empty[A, B]

      tpls.foreach {
        case (a, b, true) => 
          map(a) = b
          control(a) = b
        case (a, _, false) =>
          map -= a
          control -= a
      }

      map.toScala should be === control.toMap
    }
  }

  property(s"$t: get") {
    forAll { (control: immutable.Map[A, B], as: List[A]) =>
      val map = Map.fromScala(control)
      as.foreach { a =>
        map.get(a) should be === control.get(a)
      }
    }
  }

  property(s"$t: size") {
    forAll { (xs: List[(A, B)], ys: List[(A, B)]) =>
      val map = Map((xs ++ ys): _*)
      val control = (xs ++ ys).toMap
      map.size should be === control.size
    }
  }
}

class IntIntMapSpec extends GenericMapSpec[Int, Int]("int/int")
class StringIntMapSpec extends GenericMapSpec[String, Int]("string/int")
class DoubleTupleMapSpec extends GenericMapSpec[Double, (String, Int)]("double/string,int")

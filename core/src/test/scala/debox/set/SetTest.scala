package debox.set

import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

abstract class GenericSetSpec[A: ClassTag: Arbitrary](t: String)
    extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  property(s"$t: add a single element (+=)") {
    forAll { (x: A, y: A) =>

      val same = x == y
      val set = Set.empty[A]

      set.size should be === 0
      set(x) should be === false
      set(y) should be === false
      
      set += x
      set.size should be === 1
      set(x) should be === true
      set(y) should be === same

      set += x
      set(x) should be === true
      set.size should be === 1
      set(y) should be === same
      
      set -= x
      set(x) should be === false
      set.size should be === 0
      set(y) should be === false

      set -= x
      set(x) should be === false
      set.size should be === 0
      set(y) should be === false
    }
  }

  property(s"$t: adding elements (+=)") {
    forAll { xs: immutable.Set[A] =>
      val set = Set.empty[A]

      var n = 0
      xs.foreach { x =>
        set.size should be === n
        set(x) should be === false
        set += x
        set(x) should be === true
        n += 1
      }

      set.size should be === xs.size

      xs.foreach { x =>
        set += x
        set.size should be === xs.size
      }

      xs.foreach { x =>
        set(x) should be === true
      }
    }
  }

  property(s"$t: removing elements (-=)") {
    forAll { xs: immutable.Set[A] =>
      val set = Set.empty[A]

      xs.foreach(set += _)

      set.size should be === xs.size

      var n = xs.size
      xs.foreach { x =>
        set(x) should be === true
        set -= x
        n -= 1
        set(x) should be === false
        set.size should be === n
      }
    }
  }

  property(s"$t: random += and -=") {
    forAll { (tpls: List[(A, Boolean)]) =>
      val set = Set.empty[A]
      val control = mutable.Set.empty[A]

      tpls.foreach {
        case (x, true) => 
          set += x
          control += x
        case (x, false) =>
          set -= x
          control -= x
      }

      if (set.size != control.size) {
        println("tpls=%s" format tpls)
        println("set=%s" format set)
        println("control=%s" format control)
        println("")
        sys.error("death to videodrome")
      }
      set.size should be === control.size
      control.foreach { x =>
        set(x) should be === true
      }
    }
  }

  property(s"$t: bulk add (++=)") {
    forAll { (xs: List[A], ys: List[A]) =>

      val a = Set.empty[A]
      val b = Set.empty[A]
      val c = Set.empty[A]

      a ++= xs
      xs.foreach(b += _)
      a.toList should be === b.toList

      a ++= ys
      c ++= (xs ++ ys)
      a.toList should be === c.toList
    }
  }

  property(s"$t: union (|)") {
    forAll { (xs: immutable.Set[A], ys: immutable.Set[A]) =>
      val zs = xs | ys

      val as = Set.empty[A]
      xs.foreach(as += _)

      val bs = Set.empty[A]
      ys.foreach(bs += _)

      val cs = Set.empty[A]
      zs.foreach(cs += _)

      (as | bs) should be === cs
    }
  }

  property(s"$t: intersection (&)") {
    forAll { (xs: immutable.Set[A], ys: immutable.Set[A]) =>
      val zs = xs & ys

      val as = Set.empty[A]
      xs.foreach(as += _)

      val bs = Set.empty[A]
      ys.foreach(bs += _)

      val cs = Set.empty[A]
      zs.foreach(cs += _)

      (as & bs) should be === cs
    }
  }

  property(s"$t: difference (--)") {
    forAll { (xs: immutable.Set[A], ys: immutable.Set[A]) =>
      val zs = xs -- ys

      val as = Set.empty[A]
      xs.foreach(as += _)

      val bs = Set.empty[A]
      ys.foreach(bs += _)

      val cs = Set.empty[A]
      zs.foreach(cs += _)

      (as -- bs) should be === cs
    }
  }

  property(s"$t: size") {
    forAll { (xs: List[A], ys: List[A]) =>

      val as = Set.empty[A] ++ (xs ++ ys)
      val control = (xs ++ ys).toSet
      as.size should be === control.size
    }
  }
}

class IntSetSpec extends GenericSetSpec[Int]("int")
class DoubleSetSpec extends GenericSetSpec[Double]("double")
class StringSetSpec extends GenericSetSpec[String]("string")

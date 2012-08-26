package debox.map

import debox._

import scala.math.round

import org.scalatest.FunSuite

class MapTest extends FunSuite {

  test("apply") {
    val m = Map.empty[Int, Int]
    assert(m.contains(2) === false)
    assert(m.get(2) === None)
    assert(m.get(5) === None)
    assert(m.length === 0)

    m(2) = 3
    assert(m.contains(2) === true)
    assert(m(2) === 3)
    assert(m.get(2) === Some(3))
    assert(m.get(5) === None)
    assert(m.length === 1)

    m(2) = 4
    assert(m.contains(2) === true)
    assert(m(2) === 4)
    assert(m.get(2) === Some(4))
    assert(m.get(5) === None)
    assert(m.length === 1)

    m(5) = 6
    assert(m.contains(5) === true)
    assert(m(5) === 6)
    assert(m.get(2) === Some(4))
    assert(m.get(5) === Some(6))
    assert(m.length === 2)

    m.remove(5)
    assert(m.contains(5) === false)
    assert(m.get(2) === Some(4))
    assert(m.get(5) === None)
    assert(m.length === 1)
  }

  test("mapToMap") {
    val m = Map.empty[Int, Int]
    m(1) = 1
    m(2) = 2
    val mm = m.mapToMap(_ * 10, _.toString * 3)
    assert(mm.length === 2)
    assert(mm(10) === "111")
    assert(mm(20) === "222")
  }

  test("mapToSet") {
    val m = Map.empty[Int, Double]
    m(1) = 2.0
    m(2) = 2.4
    m(3) = 2.5
    m(4) = 2.9
    val s = m.mapToSet((k,v) => round(v))
    assert(s.length === 2)
    assert(s(2))
    assert(s(3))
  }

  test("mapToArray") {
    val m = Map.empty[Int, Int]
    m(1) = 1
    m(2) = 2
    m(3) = 2
    m(4) = 2
    m(5) = 2
    val arr = m.mapToArray((k,v) => k % 3)
    assert(arr.contains(0))
    assert(arr.contains(1))
    assert(arr.contains(2))
  }

  test("toString") {
    assert(Map.empty[Int, Int].toString === "Map()")
    assert(Map(1 -> 2.0).toString === "Map(1 -> 2.0)")
  }
}

package debox.map

import debox._

import org.scalatest.FunSuite

class MapTest extends FunSuite {

  test("apply") {
    val m = Map.empty[Int, Int]
    assert(m.contains(2) === false)
    assert(m.length === 0)

    m(2) = 3
    assert(m.contains(2) === true)
    assert(m(2) === 3)
    assert(m.length === 1)

    m(2) = 4
    assert(m.contains(2) === true)
    assert(m(2) === 4)
    assert(m.length === 1)

    m(5) = 6
    assert(m.contains(5) === true)
    assert(m(5) === 6)
    assert(m.length === 2)
  }

  test("toString") {
    assert(Map.empty[Int, Int].toString === "Map()")
    assert(Map(1 -> 2.0, 3 -> 4.0).toString === "Map(1 -> 2.0, 3 -> 4.0)")
  }

}

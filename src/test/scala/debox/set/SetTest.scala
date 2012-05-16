package debox.set

import org.scalatest.FunSuite

class SetTest extends FunSuite {
  test("basic operations on Set[Double]") {
    val set = new Set(Array.ofDim[Double](8), Array(0), 0, 8)

    assert(set.length === 0)
    assert(set(13.0) === false)
    assert(set(22.0) === false)

    assert(set.add(22.0) === true)
    assert(set.length === 1)
    assert(set(13.0) === false)
    assert(set(22.0) === true)

    assert(set.add(22.0) === false)
    assert(set.length === 1)
    assert(set(13.0) === false)
    assert(set(22.0) === true)

    assert(set.remove(13.0) === false)
    assert(set.length === 1)
    assert(set(13.0) === false)
    assert(set(22.0) === true)

    assert(set.remove(22.0) === true)
    assert(set.length === 0)
    assert(set(13.0) === false)
    assert(set(22.0) === false)
  }

  test("Set[String] should also work") {
    val set = new Set(Array.ofDim[String](8), Array(0), 0, 8)
    val data = "iwejgiewjgtwu7tuhvudsvudshvusy8wqr83ur3hrtewhgijgsji"
    data.map(_.toString).foreach(s => set.add(s))
    assert(set("i") === true)
    assert(set("3") === true)
    assert(set("Z") === false)
    assert(set("Q") === false)
  }

  test("Set[Int] should grow") {
    val set = new Set(Array.ofDim[Int](8), Array(0), 0, 8)

    for(i <- 1 to 200) {
      assert(set.add(i) === true)
      assert(set(i) === true)
      assert(set.length == i)
    }

    assert(set.toArray === (1 to 200).toArray)

    for(i <- 1 to 200) {
      assert(set.add(i) === false)
      assert(set(i) === true)
      assert(set.length == 200)
    }
  }
}

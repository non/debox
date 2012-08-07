package debox.vector

import debox._

import org.scalatest.FunSuite

class VectorTest extends FunSuite {
  test("empty vector") {
    val a = Vector.empty[Int]
    val b = Vector.empty[Int]
    assert(a === b)
  }

  test("growing vector") {
    val a = Vector.empty[Int]
    val b = a.append(1)
    val c = b.append(2)
    val d = a.append(1).append(2)
    assert(c === d)
  }

  test("large vectors") {
    val a = Vector.empty[Int]
    var z = a
    val n = 1000
    for (i <- 0 until n) z = z.append(i)
    assert(z.length === n)
    assert(z(0) === 0)
    assert(z(n - 1) === n - 1)

    assert(a.length === 0)
  }

  test("foreach") {
    var ns = Vector.empty[Int]
    for (i <- 0 until 15) ns = ns.append(i)
    var lst = List.empty[Int]
    ns.foreach { i => lst = i :: lst }
    assert(lst === (0 until 15).reverse.toList)
  }

  test("map") {
    var x = Vector.empty[Int]
    for (i <- 0 until 30) x = x.append(i)
    x = x.map(_ + 1000)

    var y = Vector.empty[Int]
    for (i <- 1000 until 1030) y = y.append(i)

    assert(x === y)
  }
}

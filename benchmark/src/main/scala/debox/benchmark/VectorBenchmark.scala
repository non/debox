package debox.benchmark

import scala.{specialized => spec}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random._

import com.google.caliper.Param

object VectorBenchmarks extends MyRunner(classOf[VectorBenchmarks])

class VectorBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17", "20"))
  var pow:Int = 0

  var data:Array[Int] = null
  var sv:scala.collection.immutable.Vector[Int] = null
  var dv:debox.vector.Vector[Int] = null

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextInt)

    var i = 0
    sv = scala.collection.immutable.Vector.empty[Int]
    dv = debox.vector.Vector.empty[Int]
    while (i < n) {
      val z = data(i)
      sv = sv :+ z
      dv = dv.append(z)
      i += 1
    }
  }

  def timeScalaAppend(reps:Int) = run(reps) {
    var i = 0
    val n = data.length
    var v = scala.collection.immutable.Vector.empty[Int]
    while (i < n) {
      v = v :+ data(i)
      i += 1
    }
    v.length
  }

  def timeDeboxAppend(reps:Int) = run(reps) {
    var i = 0
    val n = data.length
    var v = debox.vector.Vector.empty[Int]
    while (i < n) {
      v = v.append(data(i))
      i += 1
    }
    v.length
  }

  def timeScalaForeach(reps:Int) = run(reps) {
    var total = 0
    sv.foreach { total += _ }
    total
  }

  def timeDeboxForeach(reps:Int) = run(reps) {
    var total = 0
    dv.foreach { total += _ }
    total
  }

  def timeScalaMap(reps:Int) = run(reps) {
    val v = sv.map(_ * -1.0)
    v.length
  }

  def timeDeboxMap(reps:Int) = run(reps) {
    val v = dv.map(_ * -1.0)
    v.length
  }
}

package debox.benchmark

import scala.collection.mutable
import scala.util.Random._

import spire.syntax.cfor._

import com.google.caliper.Param

object MapBenchmarks extends MyRunner(classOf[MapBenchmarks])

class MapBenchmarks extends MyBenchmark {
  @Param(Array("4", "6", "8", "11", "14", "17", "20"))
  var pow: Int = 0

  var keys: Array[Int] = null
  var vals: Array[Double] = null
  var keys2: Array[Int] = null

  var scalaMap: mutable.Map[Int, Double] = null
  var javaMap: java.util.HashMap[Int, Double] = null
  var deboxMap: debox.Map[Int, Double] = null

  override protected def setUp(): Unit = {
    val n = scala.math.pow(2, pow.toDouble).toInt

    keys = init(n)(nextInt)
    vals = init(n)(nextDouble)
    keys2 = init(n)(nextInt)

    scalaMap = mutable.Map.empty[Int, Double]
    javaMap = new java.util.HashMap[Int, Double]()
    deboxMap = debox.Map.empty[Int, Double]

    cfor(0)(_ < keys.length, _ + 1) { i =>
      scalaMap(keys(i)) = vals(i)
      javaMap.put(keys(i), vals(i))
      deboxMap(keys(i)) = vals(i)
    }
  }

  // building benchmark
  def timeBuildScalaMap(reps: Int) = run(reps) {
    val m = mutable.Map.empty[Int, Double]
    cfor(0)(_ < keys.length, _ + 1) { i => m(keys(i)) = vals(i) }
    m.size
  }

  def timeBuildJavaMap(reps: Int) = run(reps) {
    val m = new java.util.HashMap[Int, Double]
    cfor(0)(_ < keys.length, _ + 1) { i => m.put(keys(i), vals(i)) }
    m.size
  }

  def timeBuildDeboxMap(reps: Int) = run(reps) {
    val m = debox.Map.empty[Int, Double]
    cfor(0)(_ < keys.length, _ + 1) { i => m(keys(i)) = vals(i) }
    m.size
  }
  
  // foreach benchmark
  def timeForeachScalaMap(reps: Int) = run(reps) {
    var ks = 0
    var vs = 0.0
    scalaMap.foreach{ case (k, _) => ks += k * 3 }
    scalaMap.foreach{ case (_, v) => vs += v * 3 }
    scalaMap.foreach{ case (k, v) => { ks -= k; vs -= 2 * v } }
    scalaMap.foreach{ case (k, v) => { ks -= 2 * k; vs -= v } }
    (ks, vs)
  }

  def timeForeachJavaMap(reps: Int) = run(reps) {
    val es = javaMap.entrySet
    var ks = 0
    var vs = 0.0
    val it1 = es.iterator; while (it1.hasNext) { ks += it1.next().getKey * 3 }
    val it2 = es.iterator; while (it2.hasNext) { vs += it2.next().getValue * 3 }
    val it3 = es.iterator; while (it3.hasNext) {
      val e = it3.next(); ks -= e.getKey; vs -= 2 * e.getValue
    }
    val it4 = es.iterator; while (it4.hasNext) {
      val e = it4.next(); ks -= 2 * e.getKey; vs -= e.getValue
    }
    (ks, vs)
  }

  def timeForeachDeboxMap(reps: Int) = run(reps) {
    var ks = 0
    var vs = 0.0
    deboxMap.foreach((k,v) => ks += k * 3)
    deboxMap.foreach((k,v) => vs += v * 3)
    deboxMap.foreach((k,v) => { ks -= k; vs -= 2 * v })
    deboxMap.foreach((k,v) => { ks -= 2 * k; vs -= v })
    (ks, vs)
  }
  
  // contains benchmark
  def timeContainsScalaMap(reps: Int) = run(reps) {
    var t = 0
    cfor(0)(_ < keys.length, _ + 1) { i => if (scalaMap.contains(keys(i))) t += 1 }
    cfor(0)(_ < keys2.length, _ + 1) { i => if (scalaMap.contains(keys2(i))) t += 1 }
    t
  }

  def timeContainsJavaMap(reps: Int) = run(reps) {
    var t = 0
    cfor(0)(_ < keys.length, _ + 1) { i => if (javaMap.containsKey(keys(i))) t += 1 }
    cfor(0)(_ < keys2.length, _ + 1) { i => if (javaMap.containsKey(keys2(i))) t += 1 }
    t
  }

  def timeContainsDeboxMap(reps: Int) = run(reps) {
    var t = 0
    cfor(0)(_ < keys.length, _ + 1) { i => if (deboxMap.contains(keys(i))) t += 1 }
    cfor(0)(_ < keys2.length, _ + 1) { i => if (deboxMap.contains(keys2(i))) t += 1 }
    t
  }
}

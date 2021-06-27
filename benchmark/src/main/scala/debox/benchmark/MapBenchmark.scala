package debox.benchmark

import scala.collection.mutable
import scala.util.Random._

import spire.syntax.cfor._

import org.openjdk.jmh.annotations._

object MapBenchmark {

  @State(Scope.Benchmark)
  class BenchmarkState {

    @Param(Array("4", "6", "8", "11", "14", "17", "20"))
    var pow: Int = 0

    var keys: Array[Int] = null
    var vals: Array[Double] = null
    var keys2: Array[Int] = null

    var scalaMap: mutable.Map[Int, Double] = null
    var javaMap: java.util.HashMap[Int, Double] = null
    var deboxMap: debox.Map[Int, Double] = null

    @Setup(Level.Trial)
    def setup(): Unit = {
      val n = scala.math.pow(2, pow.toDouble).toInt

      keys = init(n)(nextInt())
      vals = init(n)(nextDouble())
      keys2 = init(n)(nextInt())

      scalaMap = mutable.Map.empty[Int, Double]
      javaMap = new java.util.HashMap[Int, Double]()
      deboxMap = debox.Map.empty[Int, Double]

      cfor(0)(_ < keys.length, _ + 1) { i =>
        scalaMap(keys(i)) = vals(i)
        javaMap.put(keys(i), vals(i))
        deboxMap(keys(i)) = vals(i)
      }
    }
  }
}

class MapBenchmark {

  import MapBenchmark.BenchmarkState

  @Benchmark
  def buildScalaMap(st: BenchmarkState): mutable.Map[Int, Double] = {
    val keys = st.keys
    val vals = st.vals
    val m = mutable.Map.empty[Int, Double]
    cfor(0)(_ < keys.length, _ + 1) { i => m(keys(i)) = vals(i) }
    m
  }

  @Benchmark
  def buildJavaMap(st: BenchmarkState): java.util.HashMap[Int, Double] = {
    val keys = st.keys
    val vals = st.vals
    val m = new java.util.HashMap[Int, Double]
    cfor(0)(_ < keys.length, _ + 1) { i => m.put(keys(i), vals(i)) }
    m
  }

  @Benchmark
  def buildDeboxMap(st: BenchmarkState): debox.Map[Int, Double] = {
    val keys = st.keys
    val vals = st.vals
    val m = debox.Map.empty[Int, Double]
    cfor(0)(_ < keys.length, _ + 1) { i => m(keys(i)) = vals(i) }
    m
  }

  @Benchmark
  def foreachScalaMap(st: BenchmarkState): (Int, Double) = {
    val scalaMap = st.scalaMap
    var ks = 0
    var vs = 0.0
    scalaMap.foreach { case (k, _) => ks += k * 3 }
    scalaMap.foreach { case (_, v) => vs += v * 3 }
    scalaMap.foreach { case (k, v) => { ks -= k; vs -= 2 * v } }
    scalaMap.foreach { case (k, v) => { ks -= 2 * k; vs -= v } }
    (ks, vs)
  }

  @Benchmark
  def foreachJavaMap(st: BenchmarkState): (Int, Double) = {
    val javaMap = st.javaMap
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

  @Benchmark
  def foreachDeboxMap(st: BenchmarkState): (Int, Double) = {
    val deboxMap = st.deboxMap
    var ks = 0
    var vs = 0.0
    deboxMap.foreach((k,_) => ks += k * 3)
    deboxMap.foreach((_,v) => vs += v * 3)
    deboxMap.foreach((k,v) => { ks -= k; vs -= 2 * v })
    deboxMap.foreach((k,v) => { ks -= 2 * k; vs -= v })
    (ks, vs)
  }

  @Benchmark
  def containsScalaMap(st: BenchmarkState): Int = {
    val keys = st.keys
    val keys2 = st.keys2
    val scalaMap = st.scalaMap
    var t = 0
    cfor(0)(_ < keys.length, _ + 1) { i => if (scalaMap.contains(keys(i))) t += 1 }
    cfor(0)(_ < keys2.length, _ + 1) { i => if (scalaMap.contains(keys2(i))) t += 1 }
    t
  }

  @Benchmark
  def containsJavaMap(st: BenchmarkState): Int = {
    val keys = st.keys
    val keys2 = st.keys2
    val javaMap = st.javaMap
    var t = 0
    cfor(0)(_ < keys.length, _ + 1) { i => if (javaMap.containsKey(keys(i))) t += 1 }
    cfor(0)(_ < keys2.length, _ + 1) { i => if (javaMap.containsKey(keys2(i))) t += 1 }
    t
  }

  @Benchmark
  def containsDeboxMap(st: BenchmarkState): Int = {
    val keys = st.keys
    val keys2 = st.keys2
    val deboxMap = st.deboxMap
    var t = 0
    cfor(0)(_ < keys.length, _ + 1) { i => if (deboxMap.contains(keys(i))) t += 1 }
    cfor(0)(_ < keys2.length, _ + 1) { i => if (deboxMap.contains(keys2(i))) t += 1 }
    t
  }
}

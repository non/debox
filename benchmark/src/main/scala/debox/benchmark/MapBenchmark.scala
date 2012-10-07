package debox.benchmark

import scala.reflect.ClassTag
import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import debox.Ext._

import com.google.caliper.Param

import debox._
import debox.set

object MapBenchmarks extends MyRunner(classOf[MapBenchmarks])

class MapBenchmarks extends MyBenchmark {
  @Param(Array("4", "6", "8", "11", "14", "17", "20"))
  var pow:Int = 0

  var keys:Array[Int] = null
  var vals:Array[Double] = null
  var keys2:Array[Int] = null

  var scalaMap:mutable.Map[Int, Double] = null
  var javaMap:java.util.HashMap[Int, Double] = null
  var deboxMap:map.Map[Int, Double] = null

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt

    keys = init(n)(nextInt)
    vals = init(n)(nextDouble)
    keys2 = init(n)(nextInt)

    scalaMap = mutable.Map.empty[Int, Double]
    javaMap = new java.util.HashMap[Int, Double]()
    deboxMap = map.Map.empty[Int, Double]

    var i = 0
    while (i < n) {
      scalaMap(keys(i)) = vals(i)
      javaMap.put(keys(i), vals(i))
      deboxMap(keys(i)) = vals(i)
      i += 1
    }
  }

  // building benchmark
  def timeBuildScalaMap(reps:Int) = run(reps)(buildScalaMap)
  def timeBuildJavaMap(reps:Int) = run(reps)(buildScalaMap)
  def timeBuildDeboxMap(reps:Int) = run(reps)(buildDeboxMap)
  
  // foreach benchmark
  def timeForeachScalaMap(reps:Int) = run(reps)(foreachScalaMap)
  def timeForeachJavaMap(reps:Int) = run(reps)(foreachJavaMap)
  def timeForeachDeboxMap(reps:Int) = run(reps)(foreachDeboxMap)
  def timeForeachMacroDeboxMap(reps:Int) = run(reps)(foreachMacroDeboxMap)
  
  // contains benchmark
  def timeContainsScalaMap(reps:Int) = run(reps)(containsScalaMap)
  def timeContainsJavaMap(reps:Int) = run(reps)(containsJavaMap)
  def timeContainsDeboxMap(reps:Int) = run(reps)(containsDeboxMap)

  def buildScalaMap:Int = {
    val m = mutable.Map.empty[Int, Double]
    var i = 0
    val len = keys.length
    while (i < len) { m(keys(i)) = vals(i); i += 1 }
    m.size
  }

  def buildJavaMap:Int = {
    val m = new java.util.HashMap[Int, Double]
    var i = 0
    val len = keys.length
    while (i < len) { m.put(keys(i), vals(i)); i += 1 }
    m.size
  }

  def buildDeboxMap:Int = {
    val m = map.Map.empty[Int, Double]
    var i = 0
    val len = keys.length
    while (i < len) { m(keys(i)) = vals(i); i += 1 }
    m.length
  }

  // foreach benchmark
  def foreachScalaMap = {
    var ks = 0
    var vs = 0.0
    scalaMap.foreach{case (k,v) => ks += k * 3}
    scalaMap.foreach{case (k,v) => vs += v * 3}
    scalaMap.foreach{case (k,v) => { ks -= k; vs -= 2 * v }}
    scalaMap.foreach{case (k,v) => { ks -= 2 * k; vs -= v }}

    (ks, vs)
  }

  // foreach benchmark
  def foreachJavaMap = {
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

  def foreachDeboxMap = {
    var ks = 0
    var vs = 0.0
    deboxMap.foreach((k,v) => ks += k * 3)
    deboxMap.foreach((k,v) => vs += v * 3)
    deboxMap.foreach((k,v) => { ks -= k; vs -= 2 * v })
    deboxMap.foreach((k,v) => { ks -= 2 * k; vs -= v })
    (ks, vs)
  }

  def foreachMacroDeboxMap = {
    import debox.Ext._
    var ks = 0
    var vs = 0.0
    deboxMap.foreach_((k, v) => ks += k * 3)
    deboxMap.foreach_((k, v) => vs += v * 3)
    deboxMap.foreach_((k, v) => { ks -= k; vs -= 2 * v })
    deboxMap.foreach_((k, v) => { ks -= 2 * k; vs -= v })
    (ks, vs)
  }

  // contains benchmark
  def containsScalaMap:Long = {
    var i = 0
    var len = keys.length
    var t = 0
    while (i < len) { if (scalaMap.contains(keys(i))) t += 1; i += 1 }
    i = 0
    len = keys2.length
    while (i < len) { if (scalaMap.contains(keys2(i))) t += 1; i += 1 }
    t
  }

  def containsJavaMap:Long = {
    var i = 0
    var len = keys.length
    var t = 0
    while (i < len) { if (javaMap.containsKey(keys(i))) t += 1; i += 1 }
    i = 0
    len = keys2.length
    while (i < len) { if (javaMap.containsKey(keys2(i))) t += 1; i += 1 }
    t
  }

  def containsDeboxMap:Long = {
    var i = 0
    var len = keys.length
    var t = 0
    while (i < len) { if (deboxMap.contains(keys(i))) t += 1; i += 1 }
    i = 0
    len = keys2.length
    while (i < len) { if (deboxMap.contains(keys2(i))) t += 1; i += 1 }
    t
  }
}

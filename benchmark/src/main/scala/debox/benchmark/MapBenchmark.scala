package debox.benchmark

import scala.reflect.ClassTag
import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import com.google.caliper.Param

import debox._
import debox.set

object MapBenchmarks extends MyRunner(classOf[MapBenchmarks])

class MapBenchmarks extends MyBenchmark {
  //@Param(Array("8", "11", "14", "17", "20"))
  @Param(Array("8", "11", "14", "17"))
  var pow:Int = 0

  var keys:Array[Int] = null
  var vals:Array[Double] = null
  var keys2:Array[Int] = null

  var scalaMap:mutable.Map[Int, Double] = null
  var javaMap:java.util.HashMap[Int, Double] = null
  var markedMap:map.Map[Int, Double] = null
  var bitmaskMap:map.Map[Int, Double] = null

  def mf[A](implicit ev:ClassTag[A]) = ev
  val noUnset = NoUnset
  def marked[A](a:A) = MarkedUnset(a)
  val hashInt = Hash.IntHash

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt

    keys = init(n)(nextInt)
    vals = init(n)(nextDouble)
    keys2 = init(n)(nextInt)

    scalaMap = mutable.Map.empty[Int, Double]
    javaMap = new java.util.HashMap[Int, Double]()
    markedMap = map.Map.empty[Int, Double](mf[Int], marked(0), hashInt, mf[Double])
    bitmaskMap = map.Map.empty[Int, Double](mf[Int], noUnset, hashInt, mf[Double])

    var i = 0
    while (i < n) {
      scalaMap(keys(i)) = vals(i)
      javaMap.put(keys(i), vals(i))
      markedMap(keys(i)) = vals(i)
      bitmaskMap(keys(i)) = vals(i)
      i += 1
    }
  }

  // building benchmark
  def timeBuildScalaMap(reps:Int) = run(reps)(buildScalaMap)
  def timeBuildJavaMap(reps:Int) = run(reps)(buildScalaMap)
  def timeBuildMarkedMap(reps:Int) = run(reps)(buildMarkedMap)
  def timeBuildBitmaskMap(reps:Int) = run(reps)(buildBitmaskMap)

  // foreach benchmark
  def timeForeachScalaMap(reps:Int) = run(reps)(foreachScalaMap)
  def timeForeachMarkedMap(reps:Int) = run(reps)(foreachMarkedMap)
  def timeForeachBitmaskMap(reps:Int) = run(reps)(foreachBitmaskMap)
  
  // contains benchmark
  def timeContainsScalaMap(reps:Int) = run(reps)(containsScalaMap)
  def timeContainsJavaMap(reps:Int) = run(reps)(containsJavaMap)
  def timeContainsMarkedMap(reps:Int) = run(reps)(containsMarkedMap)
  def timeContainsBitmaskMap(reps:Int) = run(reps)(containsBitmaskMap)

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

  def buildMarkedMap:Int = {
    val m = map.Map.empty[Int, Double](mf[Int], marked(0), Hash.IntHash, mf[Double])
    var i = 0
    val len = keys.length
    while (i < len) { m(keys(i)) = vals(i); i += 1 }
    m.length
  }

  def buildBitmaskMap:Int = {
    val m = map.Map.empty[Int, Double](mf[Int], NoUnset, Hash.IntHash, mf[Double])
    var i = 0
    val len = keys.length
    while (i < len) { m(keys(i)) = vals(i); i += 1 }
    m.length
  }


  // foreach benchmark
  def foreachScalaMap = {
    var ks = 0
    var vs = 0.0
    scalaMap.foreach{case (k,v) => { ks += k; vs += v }}
    (ks, vs)
  }

  def foreachMarkedMap = {
    var ks = 0
    var vs = 0.0
    markedMap.foreach((k,v) => { ks += k; vs += v })
    (ks, vs)
  }

  def foreachBitmaskMap = {
    var ks = 0
    var vs = 0.0
    bitmaskMap.foreach((k,v) => { ks += k; vs += v })
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

  def containsMarkedMap:Long = {
    var i = 0
    var len = keys.length
    var t = 0
    while (i < len) { if (markedMap.contains(keys(i))) t += 1; i += 1 }
    i = 0
    len = keys2.length
    while (i < len) { if (markedMap.contains(keys2(i))) t += 1; i += 1 }
    t
  }

  def containsBitmaskMap:Long = {
    var i = 0
    var len = keys.length
    var t = 0
    while (i < len) { if (bitmaskMap.contains(keys(i))) t += 1; i += 1 }
    i = 0
    len = keys2.length
    while (i < len) { if (bitmaskMap.contains(keys2(i))) t += 1; i += 1 }
    t
  }
}

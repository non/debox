package debox.benchmark

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import com.google.caliper.Param

import debox._
import debox.set

object MapBenchmarks extends MyRunner(classOf[MapBenchmarks])

class MapBenchmarks extends MyBenchmark {
  //@Param(Array("8", "11", "14", "17", "20"))
  @Param(Array("17"))
  var pow:Int = 0

  var keys:Array[Int] = null
  var vals:Array[Double] = null

  //var scalaMap:mutable.Map[Int, Double] = null
  //var markedMap:set.Map[Int, Double] = null
  //var bitmaskMap:set.Map[Int, Double] = null

  def mf[A](implicit ev:Manifest[A]) = ev
  def marked[A](a:A) = MarkedUnset(a)

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt

    keys = init(n)(nextInt)
    vals = init(n)(nextDouble)
  }

  // building benchmark
  def timeBuildScalaMap(reps:Int) = run(reps)(buildScalaMap)
  def timeBuildMarkedMap(reps:Int) = run(reps)(buildMarkedMap)
  def timeBuildBitmaskMap(reps:Int) = run(reps)(buildBitmaskMap)
  
  // building benchmark
  def buildScalaMap:Int = {
    val m = mutable.Map.empty[Int, Double]
    var i = 0
    val len = keys.length
    while (i < len) { m(keys(i)) = vals(i); i += 1 }
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
}

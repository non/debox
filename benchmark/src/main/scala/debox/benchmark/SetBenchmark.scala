package debox.benchmark

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import com.google.caliper.Param

import debox._
import debox.set

object SetBenchmarks extends MyRunner(classOf[SetBenchmarks])

class SetBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17", "20"))
  var pow:Int = 0

  var data:Array[Long] = null
  var data2:Array[Long] = null
  var scalaSet:mutable.Set[Long] = null
  var markedSet:set.Set[Long] = null
  var bitmaskSet:set.Set[Long] = null

  val manifest = implicitly[Manifest[Long]]
  val noUnset = NoUnset[Long]()
  val markZero = MarkedUnset(0L)

  var j = 1

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong).map(n => if(n == 0L) n + 1 else n)
    data2 = init(n / 10)(nextLong).map(n => if(n == 0L) n + 1 else n)

    scalaSet = mutable.Set(data:_*)
    bitmaskSet = set.Set(data)(manifest, noUnset)
    markedSet = set.Set(data)(manifest, markZero)
  }

  // building benchmark
  def timeBuildScalaSet(reps:Int) = run(reps)(buildScalaSet)
  def timeBuildMarkedSet(reps:Int) = run(reps)(buildMarkedSet)
  def timeBuildBitmaskSet(reps:Int) = run(reps)(buildMarkedSet)

  def buildScalaSet:Int = {
    val s = mutable.Set.empty[Long]
    var i = 0
    val len = data.length
    while (i < len) {
      s.add(data(i))
      i += 1
    }
    s.size
  }

  def buildMarkedSet:Int = {
    val s = set.Set.empty[Long](manifest, markZero)
    var i = 0
    val len = data.length
    while (i < len) {
      s.add(data(i))
      i += 1
    }
    s.length
  }

  def buildBitmaskSet:Int = {
    val s = set.Set.empty[Long](manifest, noUnset)
    var i = 0
    val len = data.length
    while (i < len) {
      s.add(data(i))
      i += 1
    }
    s.length
  }

  // foreach benchmark
  def timeForeachScalaSet(reps:Int) = run(reps)(foreachScalaSet)
  def timeForeachMarkedSet(reps:Int) = run(reps)(foreachMarkedSet)
  def timeForeachBitmaskSet(reps:Int) = run(reps)(foreachBitmaskSet)

  def foreachScalaSet:Long = {
    var t = 0L
    scalaSet.foreach(n => t += n)
    t
  }

  def foreachMarkedSet:Long = {
    var t = 0L
    markedSet.foreach(n => t += n)
    t
  }

  def foreachBitmaskSet:Long = {
    var t = 0L
    bitmaskSet.foreach(n => t += n)
    t
  }

  // contains benchmark
  def timeContainsScalaSet(reps:Int) = run(reps)(foreachScalaSet)
  def timeContainsMarkedSet(reps:Int) = run(reps)(foreachMarkedSet)
  def timeContainsBitmaskSet(reps:Int) = run(reps)(foreachBitmaskSet)

  def containsScalaSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) {
      if (scalaSet.contains(data(i))) t += 1
      i += 1
    }
    i = 0
    len = data2.length
    while (i < len) {
      if (scalaSet.contains(data2(i))) t += 1
      i += 1
    }
    t
  }

  def containsMarkedSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) {
      if (markedSet.contains(data(i))) t += 1
      i += 1
    }
    i = 0
    len = data2.length
    while (i < len) {
      if (markedSet.contains(data2(i))) t += 1
      i += 1
    }
    t
  }

  def containsBitmaskSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) {
      if (bitmaskSet.contains(data(i))) t += 1
      i += 1
    }
    i = 0
    len = data2.length
    while (i < len) {
      if (bitmaskSet.contains(data2(i))) t += 1
      i += 1
    }
    t
  }
}

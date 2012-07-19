package debox.benchmark

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import com.google.caliper.Param

import debox._
import debox.set

object SetBenchmarks extends MyRunner(classOf[SetBenchmarks])

class SetBenchmarks extends MyBenchmark {
  //@Param(Array("8", "11", "14", "17", "20"))
  @Param(Array("14"))
  var pow:Int = 0

  var data:Array[Long] = null
  var data2:Array[Long] = null
  var scalaSet:mutable.Set[Long] = null
  var markedSet:set.Set[Long] = null
  var bitmaskSet:set.Set[Long] = null
  var markedBucketSet:set.Set[Long] = null
  var bitmaskBucketSet:set.Set[Long] = null

  val manifest = implicitly[Manifest[Long]]
  val noUnset = NoUnset
  val markZero = MarkedUnset(0L)

  var j = 1

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong).map(n => if(n == 0L) n + 1 else n)
    data2 = init(n / 10)(nextLong).map(n => if(n == 0L) n + 1 else n)

    scalaSet = mutable.Set(data:_*)
    bitmaskSet = set.Set(data)(manifest, noUnset)
    markedSet = set.Set(data)(manifest, markZero)
    markedBucketSet = set.Set2(data)(manifest, markZero)
    bitmaskBucketSet = set.Set2(data)(manifest, noUnset)
  }

  //// building benchmark
  //def timeBuildScalaSet(reps:Int) = run(reps)(buildScalaSet)
  //def timeBuildMarkedSet(reps:Int) = run(reps)(buildMarkedSet)
  //def timeBuildBitmaskSet(reps:Int) = run(reps)(buildMarkedSet)
  //def timeBuildMarkedBucketSet(reps:Int) = run(reps)(buildMarkedBucketSet)
  //def timeBuildBitmaskBucketSet(reps:Int) = run(reps)(buildBitmaskBucketSet)
  //
  //// foreach benchmark
  //def timeForeachScalaSet(reps:Int) = run(reps)(foreachScalaSet)
  //def timeForeachMarkedSet(reps:Int) = run(reps)(foreachMarkedSet)
  //def timeForeachBitmaskSet(reps:Int) = run(reps)(foreachBitmaskSet)
  //def timeForeachMarkedBucketSet(reps:Int) = run(reps)(foreachMarkedBucketSet)
  //def timeForeachBitmaskBucketSet(reps:Int) = run(reps)(foreachBitmaskBucketSet)
  //
  //// contains benchmark
  //def timeContainsScalaSet(reps:Int) = run(reps)(containsScalaSet)
  //def timeContainsMarkedSet(reps:Int) = run(reps)(containsMarkedSet)
  //def timeContainsBitmaskSet(reps:Int) = run(reps)(containsBitmaskSet)
  //def timeContainsMarkedBucketSet(reps:Int) = run(reps)(containsMarkedBucketSet)
  //def timeContainsBitmaskBucketSet(reps:Int) = run(reps)(containsBitmaskBucketSet)

  // map benchmark
  def timeMapScalaSet(reps:Int) = run(reps)(mapScalaSet)
  //def timeMapMarkedSet2Marked(reps:Int) = run(reps)(mapMarkedSet2Marked)
  //def timeMapMarkedSet2Bitmask(reps:Int) = run(reps)(mapMarkedSet2Bitmask)
  //def timeMapBitmaskSet2Marked(reps:Int) = run(reps)(mapBitmaskSet2Marked)
  //def timeMapBitmaskSet2Bitmask(reps:Int) = run(reps)(mapBitmaskSet2Bitmask)
  def timeMapMarkedBucketSet(reps:Int) = run(reps)(mapMarkedBucketSet)
  def timeMapBitmaskBucketSet(reps:Int) = run(reps)(mapBitmaskBucketSet)

  // building benchmark
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

  def buildMarkedBucketSet:Int = {
    val s = set.Set2.empty[Long](manifest, markZero)
    var i = 0
    val len = data.length
    while (i < len) {
      s.add(data(i))
      i += 1
    }
    s.length
  }

  def buildBitmaskBucketSet:Int = {
    val s = set.Set2.empty[Long](manifest, noUnset)
    var i = 0
    val len = data.length
    while (i < len) {
      s.add(data(i))
      i += 1
    }
    s.length
  }

  // foreach benchmark
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

  def foreachMarkedBucketSet:Long = {
    var t = 0L
    markedBucketSet.foreach(n => t += n)
    t
  }

  def foreachBitmaskBucketSet:Long = {
    var t = 0L
    bitmaskBucketSet.foreach(n => t += n)
    t
  }


  // contains benchmark
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

  def containsMarkedBucketSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) {
      if (markedBucketSet.contains(data(i))) t += 1
      i += 1
    }
    i = 0
    len = data2.length
    while (i < len) {
      if (markedBucketSet.contains(data2(i))) t += 1
      i += 1
    }
    t
  }

  def containsBitmaskBucketSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) {
      if (bitmaskBucketSet.contains(data(i))) t += 1
      i += 1
    }
    i = 0
    len = data2.length
    while (i < len) {
      if (bitmaskBucketSet.contains(data2(i))) t += 1
      i += 1
    }
    t
  }

  // map
  def mapScalaSet = scalaSet.map(_.toInt + 3)

  val ms = implicitly[Manifest[Int]]

  def mapMarkedSet2Marked = markedSet.map(_.toInt + 3)(ms, MarkedUnset(0))
  def mapMarkedSet2Bitmask = markedSet.map(_.toInt + 3)(ms, NoUnset)

  def mapBitmaskSet2Marked = bitmaskSet.map(_.toInt + 3)(ms, MarkedUnset(0))
  def mapBitmaskSet2Bitmask = bitmaskSet.map(_.toInt + 3)(ms, NoUnset)

  def mapMarkedBucketSet = markedBucketSet.map(_.toInt + 3)
  def mapBitmaskBucketSet = bitmaskBucketSet.map(_.toInt + 3)
}

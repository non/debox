package debox.benchmark

import scala.reflect.ClassTag

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import com.google.caliper.Param

import debox._
import debox.set
import debox.Ext._

object SetBenchmarks extends MyRunner(classOf[SetBenchmarks])

class SetBenchmarks extends MyBenchmark {
  @Param(Array("6", "8", "11", "14", "17", "20"))
  var pow:Int = 0

  var data:Array[Long] = null
  var data2:Array[Long] = null

  var scalaSet:mutable.Set[Long] = null
  var javaSet:java.util.HashSet[Long] = null
  var deboxSet:set.Set[Long] = null

  var j = 1

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong).map(n => if(n == 0L) n + 1 else n)
    data2 = init(n / 10)(nextLong).map(n => if(n == 0L) n + 1 else n)

    scalaSet = mutable.Set.empty[Long]
    javaSet = new java.util.HashSet[Long]()
    deboxSet = set.Set.empty[Long]

    var i = 0
    while (i < n) {
      val item = data(i)
      scalaSet.add(item)
      javaSet.add(item)
      deboxSet.add(item)
      i += 1
    }
  }

  // building benchmark
  def timeBuildScalaSet(reps:Int) = run(reps)(buildScalaSet)
  def timeBuildJavaSet(reps:Int) = run(reps)(buildJavaSet)
  def timeBuildDeboxSet(reps:Int) = run(reps)(buildDeboxSet)
  
  // foreach benchmark
  def timeForeachScalaSet(reps:Int) = run(reps)(foreachScalaSet)
  def timeForeachJavaSet(reps:Int) = run(reps)(foreachScalaSet)
  def timeForeachDeboxSet(reps:Int) = run(reps)(foreachDeboxSet)
  def timeForeachMacroDeboxSet(reps:Int) = run(reps)(foreachMacroDeboxSet)
  
  // contains benchmark
  def timeContainsScalaSet(reps:Int) = run(reps)(containsScalaSet)
  def timeContainsJavaSet(reps:Int) = run(reps)(containsJavaSet)
  def timeContainsDeboxSet(reps:Int) = run(reps)(containsDeboxSet)
  
  // map benchmark
  def timeMapScalaSet(reps:Int) = run(reps)(mapScalaSet)
  def timeMapDeboxSet(reps:Int) = run(reps)(mapDeboxSet)
  //def timeMapMacroDeboxSet(reps:Int) = run(reps)(mapMacroDeboxSet)

  def timeFoldScalaSet(reps:Int) = run(reps)(foldScalaSet)
  def timeFoldDeboxSet(reps:Int) = run(reps)(foldDeboxSet)

  // building benchmark
  def buildScalaSet:Int = {
    val s = mutable.Set.empty[Long]
    var i = 0
    val len = data.length
    while (i < len) { s.add(data(i)); i += 1 }
    s.size
  }

  def buildJavaSet:Int = {
    val s = new java.util.HashSet[Long]
    var i = 0
    val len = data.length
    while (i < len) { s.add(data(i)); i += 1 }
    s.size
  }

  def buildDeboxSet:Int = {
    val s = set.Set.empty[Long]
    var i = 0
    val len = data.length
    while (i < len) { s.add(data(i)); i += 1 }
    s.length
  }

  // foreach benchmark
  def foreachScalaSet:Long = {
    var t = 0L
    scalaSet.foreach(n => t += 4 * n)
    scalaSet.foreach(n => t -= 2 * n)
    scalaSet.foreach(n => t += n)
    scalaSet.foreach(n => t -= 2 * n)
    t
  }

  def foreachJavaSet:Long = {
    var t = 0L
    val it1 = javaSet.iterator; while (it1.hasNext) { t += 4 * it1.next() }
    val it2 = javaSet.iterator; while (it2.hasNext) { t -= 2 * it2.next() }
    val it3 = javaSet.iterator; while (it3.hasNext) { t += it3.next() }
    val it4 = javaSet.iterator; while (it4.hasNext) { t -= 2 * it4.next() }
    t
  }

  def foreachDeboxSet:Long = {
    var t = 0L
    deboxSet.foreach(n => t += 4 * n)
    deboxSet.foreach(n => t -= 2 * n)
    deboxSet.foreach(n => t += n)
    deboxSet.foreach(n => t -= 2 * n)
    t
  }

  def foreachMacroDeboxSet:Long = {
    var t = 0L
    deboxSet.foreach_(n => t += 4 * n)
    deboxSet.foreach_(n => t -= 2 * n)
    deboxSet.foreach_(n => t += n)
    deboxSet.foreach_(n => t -= 2 * n)
    t
  }

  // contains benchmark
  def containsScalaSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (scalaSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (scalaSet(data2(i))) t += 1; i += 1 }
    t
  }

  def containsJavaSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (javaSet.contains(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (javaSet.contains(data2(i))) t += 1; i += 1 }
    t
  }

  def containsDeboxSet:Long = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (deboxSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (deboxSet(data2(i))) t += 1; i += 1 }
    t
  }

  // map
  val ms = implicitly[ClassTag[Int]]

  def mapScalaSet = {
    val a = scalaSet.map(_.toInt + 3)
    val b = scalaSet.map(_.toString)
    val c = scalaSet.map(_ & 0xffff)
    (a, b, c)
  }
    
  def mapDeboxSet = {
    val a = deboxSet.map(_.toInt + 3)
    val b = deboxSet.map(_.toString)
    val c = deboxSet.map(_ & 0xffff)
    (a, b, c)
  }

  //def mapMacroDeboxSet = {
  //  val a = deboxSet.map_[Int](_.toInt + 3)(implicitly[ClassTag[Int]])
  //  val b = deboxSet.map_(_.toString)(implicitly[ClassTag[String]])
  //  val c = deboxSet.map_(_ & 0xffff)(implicitly[ClassTag[Long]])
  //  (a, b, c)
  //}

  def foldScalaSet = {
    val zmin = scalaSet.foldLeft(Long.MaxValue)((x, y) => if (y < x) y else x)
    val zmax = scalaSet.foldLeft(Long.MinValue)((x, y) => if (y > x) y else x)
    val t = scalaSet.foldLeft(0.0)((t:Double, x:Long) => t + x)
    (zmin, zmax, t)
  }

  def foldDeboxSet = {
    val zmin = deboxSet.fold(Long.MaxValue)((x, y) => if (y < x) y else x)
    val zmax = deboxSet.fold(Long.MinValue)((x, y) => if (y > x) y else x)
    val t = deboxSet.fold(0.0)((x, y) => x + y)
    (zmin, zmax, t)
  }
}

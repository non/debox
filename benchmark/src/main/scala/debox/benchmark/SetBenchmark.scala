package debox.benchmark

import scala.reflect.ClassTag

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import com.google.caliper.Param
import java.lang.Math

import debox._
import debox.set

object SetBenchmarks extends MyRunner(classOf[SetBenchmarks])

class SetBenchmarks extends MyBenchmark {
  //@Param(Array("6", "8", "11", "14", "17"))
  @Param(Array("10"))
  var pow: Int = 0

  var data: Array[Long] = null
  var data2: Array[Long] = null

  var scalaSet: mutable.Set[Long] = null
  var javaSet: java.util.HashSet[Long] = null
  var deboxSet: set.Set[Long] = null

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
      deboxSet += item
      i += 1
    }
  }

  def timeBuildScalaSet(reps: Int) = run(reps) {
    val s = mutable.Set.empty[Long]
    var i = 0
    val len = data.length
    while (i < len) { s.add(data(i)); i += 1 }
    s.size
  }

  def timeBuildJavaSet(reps: Int) = run(reps) {
    val s = new java.util.HashSet[Long]
    var i = 0
    val len = data.length
    while (i < len) { s.add(data(i)); i += 1 }
    s.size
  }

  def timeBuildDeboxSet(reps: Int) = run(reps) {
    val s = set.Set.empty[Long]
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildDeboxSetHinted(reps: Int) = run(reps) {
    val s = set.Set.empty[Long]
    s.sizeHint(data.length)
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBulkBuildScalaSet(reps: Int) = run(reps) {
    val s = mutable.Set.empty[Long]
    s ++= data
    s.size
  }

  def timeBulkBuildDeboxSet(reps: Int) = run(reps) {
    val s = set.Set.empty[Long]
    s ++= data
    s.size
  }
  
  def timeForeachScalaSet(reps: Int) = run(reps) {
    var t = 0L
    scalaSet.foreach(n => t += 4 * n)
    scalaSet.foreach(n => t -= 2 * n)
    scalaSet.foreach(n => t += n)
    scalaSet.foreach(n => t -= 2 * n)
    t
  }

  def timeForeachDeboxSet(reps: Int) = run(reps) {
    var t = 0L
    deboxSet.foreach(n => t += 4 * n)
    deboxSet.foreach(n => t -= 2 * n)
    deboxSet.foreach(n => t += n)
    deboxSet.foreach(n => t -= 2 * n)
    t
  }

  def timeScalaSetIterator(reps: Int) = run(reps) {
    var t = 0L
    val it1 = scalaSet.iterator
    while (it1.hasNext) { t += 4 * it1.next() }
    t
  }

  def timeJavaSetIterator(reps: Int) = run(reps) {
    var t = 0L
    val it1 = javaSet.iterator
    while (it1.hasNext) { t += 4 * it1.next() }
    t
  }

  def timeDeboxSetIterator(reps: Int) = run(reps) {
    var t = 0L
    val it1 = deboxSet.iterator
    while (it1.hasNext) { t += 4 * it1.next() }
    t
  }

  def timeContainsScalaSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (scalaSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (scalaSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsJavaSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (javaSet.contains(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (javaSet.contains(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsDeboxSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (deboxSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (deboxSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeMapScalaSet(reps: Int) = run(reps) {
    val a = scalaSet.map(_.toInt + 3)
    val b = scalaSet.map(_.toString)
    val c = scalaSet.map(_ & 0xffff)
    (a, b, c)
  }

  def timeMapDeboxSet(reps: Int) = run(reps) {
    val a = deboxSet.map(_.toInt + 3)
    val b = deboxSet.map(_.toString)
    val c = deboxSet.map(_ & 0xffff)
    (a, b, c)
  }

  def timeFoldScalaSet(reps: Int) = run(reps) {
    val zmin = scalaSet.foldLeft(Long.MaxValue)(Math.min)
    val zmax = scalaSet.foldLeft(Long.MinValue)(Math.max)
    val t = scalaSet.foldLeft(0L)(_ + _)
    (zmin, zmax, t)
  }

  def timeFoldDeboxSet(reps: Int) = run(reps) {
    val zmin = deboxSet.fold(Long.MaxValue)(Math.min)
    val zmax = deboxSet.fold(Long.MinValue)(Math.max)
    val t = deboxSet.fold(0L)(_ + _)
    (zmin, zmax, t)
  }

  def timePartitionScalaSet(reps: Int) = run(reps) {
    val a = scalaSet.partition(_ % 2 == 0)
    val b = scalaSet.partition(_ % 3 == 0)
    val c = scalaSet.partition(_ % 5 == 0)
    (a, b, c)
  }
  
  def timePartitionDeboxSet(reps: Int) = run(reps) {
    val a = deboxSet.partition(_ % 2 == 0)
    val b = deboxSet.partition(_ % 3 == 0)
    val c = deboxSet.partition(_ % 5 == 0)
    (a, b, c)
  }
}

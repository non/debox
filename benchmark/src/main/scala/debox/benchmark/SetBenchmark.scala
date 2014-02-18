package debox.benchmark

import scala.reflect.ClassTag

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

import spire.syntax.cfor._

import com.google.caliper.Param

object SetBenchmarks extends MyRunner(classOf[SetBenchmarks])

class SetBenchmarks extends MyBenchmark {
  @Param(Array("6", "8", "11", "14", "17", "20"))
  var pow: Int = 0

  var data: Array[Long] = null
  var data2: Array[Long] = null

  var scalaSet: mutable.Set[Long] = null
  var javaSet: java.util.HashSet[Long] = null
  var deboxSet: debox.Set[Long] = null

  var scalaSet2: mutable.Set[Long] = null
  var javaSet2: java.util.HashSet[Long] = null
  var deboxSet2: debox.Set[Long] = null

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong).map(x => if (x == 0L) 1L else x)
    data2 = init(n)(nextLong).map(x => if (x == 0L) 1L else x)

    scalaSet = mutable.Set.empty[Long]
    javaSet = new java.util.HashSet[Long]
    deboxSet = debox.Set.empty[Long]

    cfor(0)(_ < data.length, _ + 1) { i =>
      scalaSet.add(data(i))
      javaSet.add(data(i))
      deboxSet.add(data(i))
    }

    scalaSet2 = mutable.Set.empty[Long]
    javaSet2 = new java.util.HashSet[Long]
    deboxSet2 = debox.Set.empty[Long]

    cfor(0)(_ < data2.length, _ + 1) { i =>
      scalaSet2.add(data2(i))
      javaSet2.add(data2(i))
      deboxSet2.add(data2(i))
    }
  }

  // map
  val ms = implicitly[ClassTag[Int]]

  // build benchmark
  def timeBuildScalaSet(reps: Int) = run(reps) {
    val s = mutable.Set.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => s.add(data(i)) }
    s.size
  }
  def timeBuildJavaSet(reps: Int) = run(reps) {
    val s = new java.util.HashSet[Long]
    cfor(0)(_ < data.length, _ + 1) { i => s.add(data(i)) }
    s.size
  }
  def timeBuildDeboxSet(reps: Int) = run(reps) {
    val s = debox.Set.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => s += data(i) }
    s.size
  }

  // unbuild benchmark
  def timeUnbuildScalaSet(reps: Int) = run(reps) {
    val s = scalaSet.clone
    cfor(0)(_ < data.length, _ + 1) { i => s.remove(data(i)) }
    s.size
  }
  def timeUnbuildJavaSet(reps: Int) = run(reps) {
    val s = javaSet.clone.asInstanceOf[java.util.HashSet[Long]]
    cfor(0)(_ < data.length, _ + 1) { i => s.remove(data(i)) }
    s.size
  }
  def timeUnbuildDeboxSet(reps: Int) = run(reps) {
    val s = deboxSet.copy
    cfor(0)(_ < data.length, _ + 1) { i => s -= data(i) }
    s.size
  }
  
  // foreach benchmark
  def timeForeachScalaSet(reps: Int) = run(reps) {
    var t = 0L
    scalaSet.foreach(t += 4 * _)
    // scalaSet.foreach(t -= 2 * _)
    // scalaSet.foreach(t += _)
    // scalaSet.foreach(t -= 2 * _)
    t
  }
  def timeForeachJavaSet(reps: Int) = run(reps) {
    var t = 0L
    val it1 = javaSet.iterator; while (it1.hasNext) { t += 4 * it1.next }
    // val it2 = javaSet.iterator; while (it2.hasNext) { t -= 2 * it2.next }
    // val it3 = javaSet.iterator; while (it3.hasNext) { t += it3.next }
    // val it4 = javaSet.iterator; while (it4.hasNext) { t -= 2 * it4.next }
    t
  }
  def timeForeachDeboxSet(reps: Int) = run(reps) {
    var t = 0L
    deboxSet.foreach(t += 4 * _)
    // deboxSet.foreach(t -= 2 * _)
    // deboxSet.foreach(t += _)
    // deboxSet.foreach(t -= 2 * _)
    t
  }
  
  // contains benchmark
  def timeContainsScalaSet(reps: Int) = run(reps) {
    var t = 0
    cfor(0)(_ < data.length, _ + 1) { i => if (scalaSet(data(i))) t += 1 }
    cfor(0)(_ < data2.length, _ + 1) { i => if (scalaSet(data2(i))) t += 1 }
    t
  }
  def timeContainsJavaSet(reps: Int) = run(reps) {
    var t = 0
    cfor(0)(_ < data.length, _ + 1) { i => if (javaSet.contains(data(i))) t += 1 }
    cfor(0)(_ < data2.length, _ + 1) { i => if (javaSet.contains(data2(i))) t += 1 }
    t
  }
  def timeContainsDeboxSet(reps: Int) = run(reps) {
    var t = 0
    cfor(0)(_ < data.length, _ + 1) { i => if (deboxSet(data(i))) t += 1 }
    cfor(0)(_ < data2.length, _ + 1) { i => if (deboxSet(data2(i))) t += 1 }
    t
  }
  
  // map benchmark
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

  // fold benchmark
  def timeFoldScalaSet(reps: Int) = run(reps) {
    val zmin = scalaSet.foldLeft(Long.MaxValue)((x, y) => if (y < x) y else x)
    val zmax = scalaSet.foldLeft(Long.MinValue)((x, y) => if (y > x) y else x)
    val t = scalaSet.foldLeft(0L)((t: Long, x: Long) => t + x)
    (zmin, zmax, t)
  }
  def timeFoldDeboxSet(reps: Int) = run(reps) {
    val zmin = deboxSet.fold(Long.MaxValue)((x, y) => if (y < x) y else x)
    val zmax = deboxSet.fold(Long.MinValue)((x, y) => if (y > x) y else x)
    val t = deboxSet.fold(0L)((x, y) => x + y)
    (zmin, zmax, t)
  }

  // partition benchmark
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

package debox.benchmark

import scala.collection.mutable
import scala.util.Random.nextLong
import spire.syntax.cfor._
import org.openjdk.jmh.annotations._

object SetBenchmark {

  @State(Scope.Benchmark)
  class BenchmarkState {

    @Param(Array("8", "11", "14", "17", "20"))
    var pow: Int = 0

    var data: Array[Long] = _
    var data2: Array[Long] = _
    var scalaSet: mutable.Set[Long] = _
    var scalaSet2: mutable.Set[Long] = _
    var javaSet: java.util.HashSet[Long] = _
    var javaSet2: java.util.HashSet[Long] = _
    var deboxSet: debox.Set[Long] = _
    var deboxSet2: debox.Set[Long] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      val n = scala.math.pow(2, pow.toDouble).toInt
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
  }
}

class SetBenchmark {

  import SetBenchmark.BenchmarkState

  @Benchmark
  def timeBuildScalaSet(st: BenchmarkState) = {
    val data = st.data
    val s = mutable.Set.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => s.add(data(i)) }
    s.size
  }

  @Benchmark
  def timeBuildJavaSet(st: BenchmarkState) = {
    val data = st.data
    val s = new java.util.HashSet[Long]
    cfor(0)(_ < data.length, _ + 1) { i => s.add(data(i)) }
    s.size
  }

  @Benchmark
  def timeBuildDeboxSet(st: BenchmarkState) = {
    val data = st.data
    val s = debox.Set.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => s += data(i) }
    s.size
  }

  @Benchmark
  def timeUnbuildScalaSet(st: BenchmarkState) = {
    val data = st.data
    val scalaSet = st.scalaSet
    val s = scalaSet.clone
    cfor(0)(_ < data.length, _ + 1) { i => s.remove(data(i)) }
    s.size
  }

  @Benchmark
  def timeUnbuildJavaSet(st: BenchmarkState) = {
    val data = st.data
    val s = st.javaSet.clone.asInstanceOf[java.util.HashSet[Long]]
    cfor(0)(_ < data.length, _ + 1) { i => s.remove(data(i)) }
    s.size
  }

  @Benchmark
  def timeUnbuildDeboxSet(st: BenchmarkState) = {
    val data = st.data
    val s = st.deboxSet.copy
    cfor(0)(_ < data.length, _ + 1) { i => s -= data(i) }
    s.size
  }
  
  @Benchmark
  def timeForeachScalaSet(st: BenchmarkState) = {
    val scalaSet = st.scalaSet
    var t = 0L
    scalaSet.foreach(t += 4 * _)
    // scalaSet.foreach(t -= 2 * _)
    // scalaSet.foreach(t += _)
    // scalaSet.foreach(t -= 2 * _)
    t
  }

  @Benchmark
  def timeForeachJavaSet(st: BenchmarkState) = {
    val javaSet = st.javaSet
    var t = 0L
    val it1 = javaSet.iterator; while (it1.hasNext) { t += 4 * it1.next }
    // val it2 = javaSet.iterator; while (it2.hasNext) { t -= 2 * it2.next }
    // val it3 = javaSet.iterator; while (it3.hasNext) { t += it3.next }
    // val it4 = javaSet.iterator; while (it4.hasNext) { t -= 2 * it4.next }
    t
  }

  @Benchmark
  def timeForeachDeboxSet(st: BenchmarkState) = {
    val deboxSet = st.deboxSet
    var t = 0L
    deboxSet.foreach(t += 4 * _)
    // deboxSet.foreach(t -= 2 * _)
    // deboxSet.foreach(t += _)
    // deboxSet.foreach(t -= 2 * _)
    t
  }
  
  // contains benchmark
  @Benchmark
  def timeContainsScalaSet(st: BenchmarkState) = {
    val data = st.data
    val data2 = st.data2
    val scalaSet = st.scalaSet
    var t = 0
    cfor(0)(_ < data.length, _ + 1) { i => if (scalaSet(data(i))) t += 1 }
    cfor(0)(_ < data2.length, _ + 1) { i => if (scalaSet(data2(i))) t += 1 }
    t
  }

  @Benchmark
  def timeContainsJavaSet(st: BenchmarkState) = {
    val data = st.data
    val data2 = st.data2
    val javaSet = st.javaSet
    var t = 0
    cfor(0)(_ < data.length, _ + 1) { i => if (javaSet.contains(data(i))) t += 1 }
    cfor(0)(_ < data2.length, _ + 1) { i => if (javaSet.contains(data2(i))) t += 1 }
    t
  }

  @Benchmark
  def timeContainsDeboxSet(st: BenchmarkState) = {
    val data = st.data
    val data2 = st.data2
    val deboxSet = st.deboxSet
    var t = 0
    cfor(0)(_ < data.length, _ + 1) { i => if (deboxSet(data(i))) t += 1 }
    cfor(0)(_ < data2.length, _ + 1) { i => if (deboxSet(data2(i))) t += 1 }
    t
  }
  
  // map benchmark
  @Benchmark
  def timeMapScalaSet(st: BenchmarkState) = {
    val scalaSet = st.scalaSet
    val a = scalaSet.map(_.toInt + 3)
    val b = scalaSet.map(_.toString)
    val c = scalaSet.map(_ & 0xffff)
    (a, b, c)
  }

  @Benchmark
  def timeMapDeboxSet(st: BenchmarkState) = {
    val deboxSet = st.deboxSet
    val a = deboxSet.map(_.toInt + 3)
    val b = deboxSet.map(_.toString)
    val c = deboxSet.map(_ & 0xffff)
    (a, b, c)
  }

  // fold benchmark
  @Benchmark
  def timeFoldScalaSet(st: BenchmarkState) = {
    val scalaSet = st.scalaSet
    val zmin = scalaSet.foldLeft(Long.MaxValue)((x, y) => if (y < x) y else x)
    val zmax = scalaSet.foldLeft(Long.MinValue)((x, y) => if (y > x) y else x)
    val t = scalaSet.foldLeft(0L)((t: Long, x: Long) => t + x)
    (zmin, zmax, t)
  }

  @Benchmark
  def timeFoldDeboxSet(st: BenchmarkState) = {
    val deboxSet = st.deboxSet
    val zmin = deboxSet.fold(Long.MaxValue)((x, y) => if (y < x) y else x)
    val zmax = deboxSet.fold(Long.MinValue)((x, y) => if (y > x) y else x)
    val t = deboxSet.fold(0L)((x, y) => x + y)
    (zmin, zmax, t)
  }

  // partition benchmark
  @Benchmark
  def timePartitionScalaSet(st: BenchmarkState) = {
    val scalaSet = st.scalaSet
    val a = scalaSet.partition(_ % 2 == 0)
    val b = scalaSet.partition(_ % 3 == 0)
    val c = scalaSet.partition(_ % 5 == 0)
    (a, b, c)
  }

  @Benchmark
  def timePartitionDeboxSet(st: BenchmarkState) = {
    val deboxSet = st.deboxSet
    val a = deboxSet.partition(_ % 2 == 0)
    val b = deboxSet.partition(_ % 3 == 0)
    val c = deboxSet.partition(_ % 5 == 0)
    (a, b, c)
  }
}

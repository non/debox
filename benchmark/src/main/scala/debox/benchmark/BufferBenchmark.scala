package debox.benchmark

import scala.collection.mutable.ArrayBuffer
import scala.util.Random.nextLong

import spire.syntax.cfor._
import debox._

import org.openjdk.jmh.annotations._

object BufferBenchmark {

  @State(Scope.Benchmark)
  class BenchmarkState {

    @Param(Array("8", "11", "14", "17", "20"))
    var pow: Int = 0

    var data: Array[Long] = _
    var abuf: ArrayBuffer[Long] = _
    var sbuf: Buffer[Long] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      val n = scala.math.pow(2, pow.toDouble).toInt
      data = init(n)(nextLong())
      // abuf = ArrayBuffer(data: _*)
      // The above was valid in 2.12, but 2.13 has breaking changes (https://docs.scala-lang.org/overviews/core/collections-migration-213.html)
      // In 2.13 we have to pass an immutable Sequence, but the ArrayBuffer is still mutable, so the benchmarks are still valid
      abuf = ArrayBuffer(data.toSeq: _*)
      sbuf = Buffer.fromArray(data)
    }
  }
}

class BufferBenchmark {

  import BufferBenchmark.BenchmarkState

  @Benchmark
  def appendArrayBuffer(st: BenchmarkState) = {
    val data = st.data
    val bf = scala.collection.mutable.ArrayBuffer.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => bf.append(data(i)) }
    bf.length
  }

  @Benchmark
  def appendDeboxBuffer(st: BenchmarkState) = {
    val data = st.data
    val bf = Buffer.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => bf.append(data(i)) }
    bf.length
  }

  @Benchmark
  def removeArrayBuffer(st: BenchmarkState) = {
    val abuf = st.abuf
    val bf = abuf.clone
    while (bf.nonEmpty) bf.remove(bf.length - 1)
    bf.length
  }

  @Benchmark
  def removeDeboxBuffer(st: BenchmarkState) = {
    val sbuf = st.sbuf
    val bf = sbuf.copy()
    while (bf.nonEmpty) bf.remove(bf.length - 1)
    bf.length
  }

  @Benchmark
  def foreachArrayBuffer(st: BenchmarkState) = {
    val abuf = st.abuf
    var t = 0L; abuf.foreach(t += _); t
  }

  @Benchmark
  def foreachDeboxBuffer(st: BenchmarkState) = {
    val sbuf = st.sbuf
    var t = 0L; sbuf.foreach(t += _); t
  }

  @Benchmark
  def foreachArray(st: BenchmarkState) = {
    val data = st.data
    var t = 0L; data.foreach(t += _); t
  }

  @Benchmark
  def whileArrayBuffer(st: BenchmarkState) = {
    val abuf = st.abuf
    var i = 0
    val len = abuf.length
    var total = 0L
    while (i < len) {
      total += abuf(i)
      i += 1
    }
    total
  }

  @Benchmark
  def whileDeboxBuffer(st: BenchmarkState) = {
    val sbuf = st.sbuf
    var i = 0
    val len = sbuf.length
    var total = 0L
    while (i < len) {
      total += sbuf(i)
      i += 1
    }
    total
  }

  @Benchmark
  def whileArray(st: BenchmarkState) = {
    val data = st.data
    var i = 0
    val len = data.length
    var total = 0L
    while (i < len) {
      total += data(i)
      i += 1
    }
    total
  }
}

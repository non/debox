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
      data = init(n)(nextLong)
      abuf = ArrayBuffer(data: _*)
      sbuf = Buffer.fromArray(data)
    }
  }
}

class BufferBenchmark {

  import BufferBenchmark.BenchmarkState

  @Benchmark
  def timeAppendArrayBuffer(st: BenchmarkState) = {
    val data = st.data
    val bf = scala.collection.mutable.ArrayBuffer.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => bf.append(data(i)) }
    bf.length
  }

  def timeAppendDeboxBuffer(st: BenchmarkState) = {
    val data = st.data
    val bf = Buffer.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => bf.append(data(i)) }
    bf.length
  }

  def timeRemoveArrayBuffer(st: BenchmarkState) = {
    val abuf = st.abuf
    val bf = abuf.clone
    while (bf.nonEmpty) bf.remove(bf.length - 1)
    bf.length
  }

  def timeRemoveDeboxBuffer(st: BenchmarkState) = {
    val sbuf = st.sbuf
    val bf = sbuf.copy
    while (bf.nonEmpty) bf.remove(bf.length - 1)
    bf.length
  }

  def timeForeachArrayBuffer(st: BenchmarkState) = {
    val abuf = st.abuf
    var t = 0L; abuf.foreach(t += _); t
  }

  def timeForeachDeboxBuffer(st: BenchmarkState) = {
    val sbuf = st.sbuf
    var t = 0L; sbuf.foreach(t += _); t
  }

  def timeForeachArray(st: BenchmarkState) = {
    val data = st.data
    var t = 0L; data.foreach(t += _); t
  }

  def timeWhileArrayBuffer(st: BenchmarkState) = {
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

  def timeWhileDeboxBuffer(st: BenchmarkState) = {
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

  def timeWhileArray(st: BenchmarkState) = {
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

package debox
package benchmark

import scala.{specialized => spec}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random._

import com.google.caliper.Param

object BufferBenchmarks extends MyRunner(classOf[BufferBenchmarks])

class BufferBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17", "20"))
  var pow: Int = 0

  var data: Array[Long] = null
  var abuf: ArrayBuffer[Long] = null
  var sbuf: Buffer[Long] = null

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong)
    abuf = ArrayBuffer(data: _*)
    sbuf = Buffer.fromArray(data)
  }

  def timeAppendArrayBuffer(reps: Int) = run(reps) {
    val bf = scala.collection.mutable.ArrayBuffer.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => bf.append(data(i)) }
    bf.length
  }

  def timeAppendDeboxBuffer(reps: Int) = run(reps) {
    val bf = Buffer.empty[Long]
    cfor(0)(_ < data.length, _ + 1) { i => bf.append(data(i)) }
    bf.length
  }

  def timeRemoveArrayBuffer(reps: Int) = run(reps) {
    val bf = abuf.clone
    while (bf.nonEmpty) bf.remove(bf.length - 1)
    bf.length
  }

  def timeRemoveDeboxBuffer(reps: Int) = run(reps) {
    val bf = sbuf.copy
    while (bf.nonEmpty) bf.remove(bf.length - 1)
    bf.length
  }

  def timeForeachArrayBuffer(reps: Int) = run(reps) {
    var t = 0L; abuf.foreach(t += _); t
  }

  def timeForeachDeboxBuffer(reps: Int) = run(reps) {
    var t = 0L; sbuf.foreach(t += _); t
  }

  def timeForeachArray(reps: Int) = run(reps) {
    var t = 0L; data.foreach(t += _); t
  }

  def timeWhileArrayBuffer(reps: Int) = run(reps)(whileArrayBuffer)
  def whileArrayBuffer: Long = {
    var i = 0
    val len = abuf.length
    var total = 0L
    while (i < len) {
      total += abuf(i)
      i += 1
    }
    total
  }

  def timeWhileDeboxBuffer(reps: Int) = run(reps)(whileDeboxBuffer)
  def whileDeboxBuffer: Long = {
    var i = 0
    val len = sbuf.length
    var total = 0L
    while (i < len) {
      total += sbuf(i)
      i += 1
    }
    total
  }

  def timeWhileArray(reps: Int) = run(reps)(whileArray)
  def whileArray: Long = {
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

package debox.benchmark

import scala.{specialized => spec}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random._

import com.google.caliper.Param

import debox._
import debox.buffer._

object BufferBenchmarks extends MyRunner(classOf[BufferBenchmarks])

class BufferBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17", "20"))
  var pow:Int = 0

  var data:Array[Long] = null
  var abuf:ArrayBuffer[Long] = null
  var sbuf:Buffer[Long] = null

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextLong)
    abuf = ArrayBuffer(data:_*)
    sbuf = Mutable.safe(data)
  }

  def appendArrayBuffer:Long = {
    val len = data.length
    val bf = scala.collection.mutable.ArrayBuffer.empty[Long]
    var i = 0
    while (i < len) {
      bf.append(data(i))
      i += 1
    }
    var total = 0L
    i = 0
    while (i < len) {
      total += bf(i)
      i += 1
    }
    total
  }

  def appendDeboxBuffer:Long = {
    val len = data.length
    val bf = debox.buffer.Mutable.empty[Long]
    var i = 0
    while (i < len) {
      bf.append(data(i))
      i += 1
    }
    var total = 0L
    i = 0
    while (i < len) {
      total += bf(i)
      i += 1
    }
    total
  }

  def foreachArrayBuffer:Long = {
    var i = 0
    var total = 0L
    abuf.foreach(total += _)
    total
  }

  def foreachDeboxBuffer:Long = {
    var i = 0
    var total = 0L
    sbuf.foreach(total += _)
    total
  }

  def foreachArray:Long = {
    var i = 0
    var total = 0L
    data.foreach(total += _)
    total
  }

  def whileArrayBuffer:Long = {
    var i = 0
    val len = abuf.length
    var total = 0L
    while (i < len) {
      total += abuf(i)
      i += 1
    }
    total
  }

  def whileDeboxBuffer:Long = {
    var i = 0
    val len = sbuf.length
    var total = 0L
    while (i < len) {
      total += sbuf(i)
      i += 1
    }
    total
  }

  def whileArray:Long = {
    var i = 0
    val len = data.length
    var total = 0L
    while (i < len) {
      total += data(i)
      i += 1
    }
    total
  }

  def timeAppendArrayBuffer(reps:Int) = run(reps)(appendArrayBuffer)
  def timeAppendDeboxBuffer(reps:Int) = run(reps)(appendDeboxBuffer)

  def timeForeachArrayBuffer(reps:Int) = run(reps)(foreachArrayBuffer)
  def timeForeachDeboxBuffer(reps:Int) = run(reps)(foreachDeboxBuffer)
  def timeForeachArray(reps:Int) = run(reps)(foreachArray)

  def timeWhileArrayBuffer(reps:Int) = run(reps)(whileArrayBuffer)
  def timeWhileDeboxBuffer(reps:Int) = run(reps)(whileDeboxBuffer)
  def timeWhileArray(reps:Int) = run(reps)(whileArray)
}

package org.openjdk.jmh.samples

import org.openjdk.jmh.annotations._
import scala.util.Random.nextInt

object VectorBenchmark {

  @State(Scope.Benchmark)
  class BenchmarkState {

    @Param(Array("10", "100", "1000", "10000"))
    var size: Int = 0

    var list: List[Int] = _
    var vector: Vector[Int] = _
    var array: Array[Int] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      list = (1 to size).map(_ => nextInt).toList
      vector = list.toVector
      array = vector.toArray
    }
  }
}

class VectorBenchmark {

  import VectorBenchmark.BenchmarkState

  @Benchmark
  def prependList(st: BenchmarkState): List[Int] =
    999 :: st.list

  @Benchmark
  def prependVector(st: BenchmarkState): Vector[Int] =
    999 +: st.vector

  @Benchmark
  def appendVector(st: BenchmarkState): Vector[Int] =
    st.vector :+ 999

  // @Benchmark
  // def prependArray(st: BenchmarkState): Array[Int] = {
  //   val arr = new Array[Int](st.size + 1)
  //   System.arraycopy(st.array, 0, arr, 1, st.array.length)
  //   arr(0) = 999
  //   arr
  // }
}

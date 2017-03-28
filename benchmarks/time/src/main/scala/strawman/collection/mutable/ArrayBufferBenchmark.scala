package strawman.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import scala.{Any, AnyRef, Int, Unit}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class ArrayBufferBenchmark {

  @Param(scala.Array("8", "64", "512", "4096", "32768", "262144"/*, "2097152"*/))
  var size: Int = _

  var xs: ArrayBuffer[AnyRef] = _
  var obj: Any = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = ArrayBuffer.fill(size)("")
    obj = ""
  }

  @Benchmark
  def cons(): Any = {
    var ys = ArrayBuffer.empty[Any]
    var i = 0
    while (i < size) {
      ys += obj
      i += 1
    }
    ys
  }

  @Benchmark
  def uncons(): Any = xs.tail

  @Benchmark
  def concat(): Any = xs ++ xs

  @Benchmark
  def foreach(): Any = {
    var n = 0
    xs.foreach(x => if (x eq null) n += 1)
    n
  }

  @Benchmark
  def iterator(): Any = {
    var n = 0
    val it = xs.iterator()
    while (it.hasNext) if (it.next() eq null) n += 1
    n
  }

  @Benchmark
  def lookup(): Any = xs(size - 1)

  @Benchmark
  def map(): Any = xs.map(x => if (x eq null) "foo" else "bar")

  @Benchmark
  def reverse(): Any = xs.reverse
}

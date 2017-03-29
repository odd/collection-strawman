package strawman.collection.mutable

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.{Any, AnyRef, Int, Unit}
import scala.Predef._

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class ScalaArrayBenchmark {

  @Param(scala.Array("8", "64", "512", "4096", "32768", "262144"/*, "2097152"*/))
  var size: Int = _

  var xs: scala.Array[AnyRef] = _
  var obj: Any = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = scala.Array.fill(size)("")
    obj = ""
  }

  @Benchmark
  def cons(): Any = {
    var ys = scala.Array.empty[Any]
    var i = 0
    while (i < size) {
      ys = obj +: ys
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
  def foreach_while(): Any = {
    var n = 0
    var ys = xs
    while (ys.nonEmpty) {
      if (ys.head eq null) n += 1
      ys = ys.tail
    }
    n
  }

  @Benchmark
  def iterator(): Any = {
    var n = 0
    val it = xs.iterator
    while (it.hasNext) if (it.next() eq null) n += 1
    n
  }

  @Benchmark
  def lookup(): Any = xs(size - 1)

  @Benchmark
  def map(): Any = xs.map(x => if (x eq null) "foo" else "bar")

  @Benchmark
  def reverse(): Any = xs.reverse

  @Benchmark
  def foldLeft(): Any = xs.foldLeft(0) {
    case (acc, x) =>
      if (x eq null) acc + 2
      else acc - 1
  }

  @Benchmark
  def foldRight(): Any = xs.foldRight(0) {
    case (x, acc) =>
      if (x eq null) acc - 2
      else acc + 1
  }
}
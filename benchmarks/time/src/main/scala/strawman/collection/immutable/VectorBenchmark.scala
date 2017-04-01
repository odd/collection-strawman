package strawman.collection.immutable

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.{Any, AnyRef, Int, Unit}
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Threads(Threads.MAX)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class VectorBenchmark {

  @Param(scala.Array("1", "7", "17", "282", "7890", "32911", "73121", "262144"))
  var size: Int = _

  var xs: scala.Vector[AnyRef] = _
  var obj: Any = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = scala.Vector.fill(size)("")
    obj = ""
  }

  @Benchmark
  def cons(bh: Blackhole): Any = {
    var ys = scala.Vector.empty[Any]
    var i = 0
    while (i < size) {
      ys = obj +: ys
      i += 1
    }
    bh.consume(ys)
    bh.consume(i)
  }

  @Benchmark
  def uncons(bh: Blackhole): Any = bh.consume(xs.tail)

  @Benchmark
  def concat(bh: Blackhole): Any = bh.consume(xs ++ xs)

  @Benchmark
  def foreach(bh: Blackhole): Any = {
    var n = 0
    xs.foreach { x =>
      bh.consume(x)
      n += 1
    }
    bh.consume(n)
  }

  @Benchmark
  def foreach_while(bh: Blackhole): Any = {
    var n = 0
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      n += 1
      ys = ys.tail
    }
    bh.consume(n)
  }

  @Benchmark
  def iterator(bh: Blackhole): Any = {
    var n = 0
    val it = xs.iterator
    while (it.hasNext) {
      bh.consume(it.next())
      n += 1
    }
    bh.consume(n)
  }

  @Benchmark
  def lookup(bh: Blackhole): Any = if (xs.nonEmpty) bh.consume(xs(size - 1))

  @Benchmark
  def map(bh: Blackhole): Any = xs.map { x =>
    bh.consume(x)
    if (x eq null) "foo" else "bar"
  }

  @Benchmark
  def reverse(bh: Blackhole): Any = bh.consume(xs.reverse)

  @Benchmark
  def foldLeft(bh: Blackhole): Any = bh.consume(xs.foldLeft(0) {
    case (acc, n) =>
      bh.consume(n)
      acc + 1
  })

  @Benchmark
  def foldRight(bh: Blackhole): Any = bh.consume(xs.foldRight(0) {
    case (n, acc) =>
      bh.consume(n)
      acc - 1
  })
}

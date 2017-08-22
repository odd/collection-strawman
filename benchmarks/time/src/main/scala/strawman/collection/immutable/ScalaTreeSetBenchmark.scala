package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Int, Long, Unit, math}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class ScalaTreeSetBenchmark {
  @Param(scala.Array(/*"0", */"1", "2", "3", "4", "7", "8", "15", "16", "17", "39", "282", "73121", "7312102"))
  var size: Int = _

  var xs: scala.collection.immutable.TreeSet[Long] = _
  var zipped: scala.collection.immutable.TreeSet[(Long, Long)] = _
  var randomIndices: scala.Array[Int] = _
  def fresh(n: Int) = scala.collection.immutable.TreeSet((1 to n).map(_.toLong): _*)

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = fresh(size)
    zipped = xs.map(x => (x, x))
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def incl(bh: Blackhole): Unit = {
    var ys = fresh(size)
    var i = 0L
    while (i < 1000) {
      ys += i
      i += 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def concat(bh: Blackhole): Unit = {
    val ys = fresh(size)
    bh.consume(ys ++ ys)
  }

  @Benchmark
  def tail(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def init(bh: Blackhole): Unit = bh.consume(xs.init)

  @Benchmark
  @OperationsPerInvocation(100)
  def slice_front(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(0, size / (i + 1)))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def slice_rear(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(size - size / (i + 1), size))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def slice_middle(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      bh.consume(xs.slice(size / 2 - size / (2 * (i + 1)), size / 2 + size / (2 * (i + 1))))
      i += 1
    }
  }

  @Benchmark
  def loop_foreach(bh: Blackhole): Unit = xs.foreach(x => bh.consume(x))

  @Benchmark
  def loop_headTail(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      ys = ys.tail
    }
  }

  @Benchmark
  def loop_initLast(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.last)
      ys = ys.init
    }
  }

  @Benchmark
  def loop_iterator(bh: Blackhole): Unit = {
    val it = xs.iterator
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def contains(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.contains(i))
      i += 1
    }
  }

  @Benchmark
  def map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  @OperationsPerInvocation(100)
  def span(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val (xs1, xs2) = xs.span(x => x < randomIndices(i))
      bh.consume(xs1)
      bh.consume(xs2)
      i += 1
    }
  }

  @Benchmark
  def unzip(bh: Blackhole): Unit = bh.consume(zipped.unzip(t => (t._1, t._2)))

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

  @Benchmark
  def groupBy(bh: Blackhole): Unit = {
    val result = xs.groupBy(_ % 5)
    bh.consume(result)
  }
}
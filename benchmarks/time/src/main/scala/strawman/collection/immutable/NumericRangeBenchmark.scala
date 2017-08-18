package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Int, Unit, math}

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class NumericRangeBenchmark {

  @Param(scala.Array(/*"0", */"1"/*, "2", "3", "4", "7"*/, "8"/*, "15", "16"*/, "17"/*, "39"*/, "282", "4096", "31980", "73121", "140000"))
  var size: Int = _

  var xs: NumericRange[Int] = _
  var randomIndices: scala.Array[Int] = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    xs = NumericRange.inclusive(1, size, 1)
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
    }
  }

  @Benchmark
  def prependAll(bh: Blackhole): Unit = bh.consume(xs ++: xs)

  @Benchmark
  def appendAll(bh: Blackhole): Unit = bh.consume(xs :++ xs)

  @Benchmark
  def tail(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def init(bh: Blackhole): Unit = bh.consume(xs.init)

  @Benchmark
  def slice_front(bh: Blackhole): Unit = {
    var i = 0
    while (i < size) {
      bh.consume(xs.slice(0, i))
      i += math.max(size / 100, 1)
    }
  }

  @Benchmark
  def slice_rear(bh: Blackhole): Unit = {
    var i = size - 1
    while (i >= 0) {
      bh.consume(xs.slice(i, size))
      i -= math.max(size / 100, 1)
    }
  }

  @Benchmark
  def slice_middle(bh: Blackhole): Unit = {
    var i = size / 2
    while (i >= 0) {
      bh.consume(xs.slice(i, size - i))
      i -= math.max(size / 100, 1)
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
    val it = xs.iterator()
    while (it.hasNext) {
      bh.consume(it.next())
    }
  }

  @Benchmark
  def distinct(bh: Blackhole): Unit = bh.consume(xs.distinct)

  @Benchmark
  @OperationsPerInvocation(1000)
  def lookup_last(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs(size - 1))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def lookup_random(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs(randomIndices(i)))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def updated_last(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.updated(size - 1, i))
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def updated_random(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs.updated(randomIndices(i), i))
      i += 1
    }
  }

  @Benchmark
  def map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

  @Benchmark
  @OperationsPerInvocation(100)
  def patch(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      val from = randomIndices(i)
      val replaced = randomIndices(if (i > 0) i - 1 else math.min(i + 1, size))
      val length = randomIndices(if (i > 1) i - 2 else math.min(i + 2, size))
      bh.consume(xs.patch(from, xs.take(length), replaced))
      i += 1
    }
  }

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
  def padTo(bh: Blackhole): Unit = bh.consume(xs.padTo(size * 2, 42))

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

  @Benchmark
  def groupBy(bh: Blackhole): Unit = {
    val result = xs.groupBy(_ % 5)
    bh.consume(result)
  }
}
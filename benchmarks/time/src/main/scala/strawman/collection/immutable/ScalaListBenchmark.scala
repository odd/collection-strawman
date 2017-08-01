package strawman.collection.immutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.{Any, AnyRef, Int, Long, Unit}
import scala.Predef.intWrapper

@BenchmarkMode(scala.Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Fork(1)
@Warmup(iterations = 12)
@Measurement(iterations = 12)
@State(Scope.Benchmark)
class ScalaListBenchmark {

  @Param(scala.Array("0"/*, "1", "2", "3", "4", "7"*/, "8"/*, "15", "16"*/, "17"/*, "39"*/, "282", "4096", "31980", "73121", "120000"))
  var size: Int = _

  var xs: scala.List[Long] = _
  var xss: scala.Array[scala.List[Long]] = _
  var randomIndices: scala.Array[Int] = _

  @Setup(Level.Trial)
  def initData(): Unit = {
    def freshCollection() = scala.List((1 to size).map(_.toLong): _*)
    xs = freshCollection()
    xss = scala.Array.fill(1000)(freshCollection())
    if (size > 0) {
      randomIndices = scala.Array.fill(1000)(scala.util.Random.nextInt(size))
    }
  }

  @Benchmark
  //@OperationsPerInvocation(size)
  def cons(bh: Blackhole): Unit = {
    var ys = scala.List.empty[Long]
    var i = 0L
    while (i < size) {
      ys = i +: ys // Note: In the case of TreeSet, always inserting elements that are already ordered creates a bias
      i = i + 1
    }
    bh.consume(ys)
  }

  @Benchmark
  //@OperationsPerInvocation(size)
  def snoc(bh: Blackhole): Unit = {
    var ys = scala.List.empty[Long]
    var i = 0L
    while (i < size) {
      ys = ys :+ i // Note: In the case of TreeSet, always inserting elements that are already ordered creates a bias
      i = i + 1
    }
    bh.consume(ys)
  }

  @Benchmark
  def uncons(bh: Blackhole): Unit = bh.consume(xs.tail)

  @Benchmark
  def unsnoc(bh: Blackhole): Unit = bh.consume(xs.init)

  @Benchmark
  def concat(bh: Blackhole): Unit = bh.consume(xs ++ xs)

  @Benchmark
  def foreach(bh: Blackhole): Unit = xs.foreach(x => bh.consume(x))

  @Benchmark
  //  @OperationsPerInvocation(size)
  def foreach_while(bh: Blackhole): Unit = {
    var ys = xs
    while (ys.nonEmpty) {
      bh.consume(ys.head)
      ys = ys.tail
    }
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
  @OperationsPerInvocation(1000)
  def lookupLast(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xss(i)(size - 1))
      i = i + 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(1000)
  def randomLookup(bh: Blackhole): Unit = {
    var i = 0
    while (i < 1000) {
      bh.consume(xs(randomIndices(i)))
      i = i + 1
    }
  }

  @Benchmark
  def map(bh: Blackhole): Unit = bh.consume(xs.map(x => x + 1))

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

  @Benchmark
  //@OperationsPerInvocation(size)
  def consSnoc(bh: Blackhole): Unit = {
    var ys = scala.List.empty[Long]
    var i = 0L
    while (i < size) {
      if ((i & 1) == 1) ys = ys :+ i
      else ys = i +: ys
      i = i + 1
    }
    bh.consume(ys)
  }
}

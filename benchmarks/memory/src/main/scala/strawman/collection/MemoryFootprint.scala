package bench

import strawman.collection.immutable.{LazyList, List, Spandex}

import scala.{Any, AnyRef, App, Int, Long, Seq, StringContext}
import scala.Predef.{ArrowAssoc, println}
import scala.compat.Platform
import java.lang.Runtime
import java.nio.file.{Files, Paths}

import strawman.collection.mutable.{ArrayBuffer, ListBuffer}

object MemoryFootprint extends App {

  val reportPath = Paths.get(args(0))

  val sizes = scala.List(8, 64, 512, 4096, 32768, 262144, 2097152)

  val runtime = Runtime.getRuntime
  val obj: AnyRef = null
  var placeholder: Any = _

  def benchmark[A](gen: Int => A): scala.List[(Int, Long)] = (
    // We run 5 iterations and pick the last result only
    for (_ <- scala.Range(0, 5)) yield {
      for (size <- sizes) yield {
        placeholder = null
        Platform.collectGarbage()
        val memBefore = runtime.totalMemory() - runtime.freeMemory()
        placeholder = gen(size)
        Platform.collectGarbage()
        val memAfter = runtime.totalMemory() - runtime.freeMemory()
        size -> (memAfter - memBefore)
      }
    }
  ).last

  val memories =
    scala.Predef.Map(
      "scala.List"  -> benchmark(scala.List.fill(_)(obj)),
      "List"        -> benchmark(List.fill(_)(obj)),
      "LazyList"    -> benchmark(LazyList.fill(_)(obj)),
      "ArrayBuffer" -> benchmark(ArrayBuffer.fill(_)(obj)),
      "ListBuffer"  -> benchmark(ListBuffer.fill(_)(obj)),
      "Spandex"     -> benchmark(Spandex.fill(_)(obj))
    )

  // We use a format similar to the one used by JMH so that
  // our charts can be generated in the same way
  import jawn.ast._
  val report =
    JArray.fromSeq(
      memories.flatMap { case (name, values) =>
        values.map { case (size, value) =>
          JObject.fromSeq(Seq(
            "benchmark" -> JString(s"$name.memory-footprint"),
            "params" -> JObject.fromSeq(Seq(
              "size" -> JString(size.toString)
            )),
            "primaryMetric" -> JObject.fromSeq(Seq(
              "score" -> JNum(value),
              "scoreConfidence" -> JArray.fromSeq(Seq(JNum(value), JNum(value)))
            ))
          ))
        }
      }.to[Seq]
    )
  Files.write(reportPath, FastRenderer.render(report).getBytes)

}

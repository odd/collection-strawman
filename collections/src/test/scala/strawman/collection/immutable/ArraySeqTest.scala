package strawman
package collection
package immutable

import org.junit.Assert._
import org.junit.Test

import scala.{Any, Exception, Int, NoSuchElementException, IndexOutOfBoundsException, Nothing, Unit}
import scala.Predef.{identity, intWrapper, ArrowAssoc}
import scala.runtime.RichInt
import scala.util.Try
import scala.util.control.NonFatal
import strawman.collection.immutable.ArraySeq.AutoTrimming

class ArraySeqTest {
  val O = null

  @Test
  def testEmpty(): Unit = {
    assertEquals("apply with no arguments is equal to empty", ArraySeq.empty, ArraySeq())
    assertEquals("apply with no arguments has size 0", 0, ArraySeq().size)
    assertEquals("apply with no arguments has empty iterator", false, ArraySeq.empty.iterator().hasNext)
  }

  @Test
  def testApply(): Unit = {
    assertEquals("apply with 00 arguments has correct elements array", List(), ArraySeq().array.to(List))
    assertEquals("apply with 01 argument  has correct elements array", List(1, O, O, O, O, O, O, O), ArraySeq(1).array.to(List))
    assertEquals("apply with 02 arguments has correct elements array", List(1, 2, O, O, O, O, O, O), ArraySeq(1, 2).array.to(List))
    assertEquals("apply with 03 arguments has correct elements array", List(1, 2, 3, O, O, O, O, O), ArraySeq(1, 2, 3).array.to(List))
    assertEquals("apply with 04 arguments has correct elements array", List(1, 2, 3, 4, O, O, O, O), ArraySeq(1, 2, 3, 4).array.to(List))
    assertEquals("apply with 05 arguments has correct elements array", List(1, 2, 3, 4, 5, O, O, O), ArraySeq(1, 2, 3, 4, 5).array.to(List))
    assertEquals("apply with 06 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, O, O), ArraySeq(1, 2, 3, 4, 5, 6).array.to(List))
    assertEquals("apply with 07 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, O), ArraySeq(1, 2, 3, 4, 5, 6, 7).array.to(List))
    assertEquals("apply with 08 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8).array.to(List))
    assertEquals("apply with 09 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).array.to(List))
    assertEquals("apply with 10 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, O, O, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).array.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, O, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).array.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).array.to(List))
  }

  @Test
  def testBuild(): Unit = {
    assertEquals("empty", ArraySeq.empty, ArraySeq.newBuilder().result())
    assertEquals("single", ArraySeq(2), ArraySeq.newBuilder().add(2).result())
    assertEquals("multiple one by one", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ArraySeq.newBuilder().add(1).add(2).add(3).add(4).add(5).add(6).add(7).add(8).add(9).add(10).add(11).add(12).result())
    assertEquals("multiple all at once", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ArraySeq.newBuilder().addAll(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)).result())
  }

  @Test
  def testExpandFront(): Unit = {
    val O = null
    val a = 1 +: 2 +: ArraySeq(3, 4, 5, 6) :+ 7
    assertEquals("no unnecessary expansion", List(3, 4, 5, 6, 7, 0, 1, 2), (0 +: a).array.to(List))
    val b = 1 +: 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 1", List(3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O, 0, 1, 2), (0 +: b).array.to(List))
    val c = 1 +: 2 +: ArraySeq(3, 4, 5, 6, 7, 8)
    assertEquals("expands correctly 2", List(3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O, 0, 1, 2), (0 +: c).array.to(List))
    val d = 1 +: 2 +: ArraySeq(3, 4, 5, 6, 7, 8, 9, 10)
    assertEquals("expands correctly 3", List(3, 4, 5, 6, 7, 8, 9, 10, O, O, O, O, O, 0, 1, 2), (0 +: d).array.to(List))
    val e = 0 +: 1 +: 2 +: ArraySeq(3, 4, 5, 6, 7, 8, 9)
    assertEquals("expands correctly 4", List(3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, -1, 0, 1, 2), (-1 +: e).array.to(List))
    val f = 0 +: 1 +: 2 +: ArraySeq(3)
    assertEquals("expands correctly 5", List(3, O, O, O, -1, 0, 1, 2), (-1 +: f).array.to(List))
    val g = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.dropRight(7)
    assertEquals("expands correctly 6", List(1, O, O, O, O, O, O, 0), (0 +: h).array.to(List))
    val i = g.dropRight(8)
    assertEquals("expands correctly 7", List(O, O, O, O, O, O, O, 0), (0 +: i).array.to(List))
  }

  @Test
  def testExpandEnd(): Unit = {
    val a = 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("no unnecessary expansion", List(3, 4, 5, 6, 7, 8, 9, 2), (a :+ 9).array.to(List))
    val b = 1 +: 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 1", List(3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, O, O, 1, 2), (b :+ 9).array.to(List))
    val c = ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 2", List(3, 4, 5, 6, 7 ,8, 9, O), (c :+ 9).array.to(List))
    val d = ArraySeq(0, 1, 2, 3, 4) :+ 5 :+ 6 :+ 7
    assertEquals("expands correctly 3", List(0, 1, 2, 3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O), (d :+ 8).array.to(List))
    val g = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.drop(7)
    assertEquals("expands correctly 4", List(8, 9, O, O, O, O, O, O), (h :+ 9).array.to(List))
    val i = g.drop(8)
    assertEquals("expands correctly 5", List(9, O, O, O, O, O, O, O), (i :+ 9).array.to(List))
  }

  @Test
  def testSingleElement(): Unit = {
    assertEquals("apply with single argument has length 1", 1, ArraySeq(1).length)
    val it = ArraySeq(1).iterator()
    assertTrue("apply with single argument has single element iterator", it.hasNext)
    assertEquals("apply with single argument has single element iterator with correct element", 1, it.next())
    assertFalse("apply with single argument has only single element iterator", it.hasNext)
    val rit = ArraySeq(1).reverse.iterator()
    assertTrue("apply with reversed single argument has single element iterator with correct element",
      rit.hasNext && rit.next() == 1)
    assertFalse("apply with reversed single argument has only single element iterator", rit.hasNext)
    assertEquals("apply with single argument has correct element at index 0", 1, ArraySeq(1)(0))
    assertEquals("apply with reversed single argument has correct element at index 0", 1, ArraySeq(1).reverse.apply(0))
    assertEquals("apply with single argument has correct element as head", 1, ArraySeq(1).head)
    assertEquals("apply with reversed single argument has correct element as head", 1, ArraySeq(1).reverse.head)
    assertEquals("apply with single argument has empty tail", ArraySeq.empty, ArraySeq(1).tail)
    assertEquals("apply with reversed single argument has empty tail", ArraySeq.empty, ArraySeq(1).reverse.tail)
  }

  @Test
  def testReverse(): Unit = {
    assertEquals("reverse empty is empty", ArraySeq.empty, ArraySeq.empty.reverse)
    assertEquals("reverse effectively empty is empty", ArraySeq.empty, ArraySeq(1, 2, 3).filter(_ == 4).reverse)
    assertEquals("reverse single is same", ArraySeq(1), ArraySeq(1).reverse)
    assertEquals("reverse effectively single is same", ArraySeq(1), ArraySeq(1, 2).take(1).reverse)
    assertEquals("reverse double is correct", ArraySeq(2, 1), ArraySeq(1, 2).reverse)
    assertEquals("reverse effectively double is correct", ArraySeq(2, 1), ArraySeq(1, 2, 3).take(2).reverse)
    assertEquals("reverse large is correct", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).reverse)
    val reverse = ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).reverse
    val reverse2 = reverse.reverse
    assertEquals("reverse reverse large is correct", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), reverse2)
  }

  @Test
  def testHeadsAndTails(): Unit = {
    assertTrue("empty tail fails", Try(ArraySeq.empty.tail).isFailure)
    assertTrue("empty reverse tail fails", Try(ArraySeq.empty.reverse.tail).isFailure)
    assertEquals("tail", ArraySeq(2, 3, 4), ArraySeq(1, 2, 3, 4).tail)
    val r = ArraySeq(1, 2, 3, 4).reverse
    assertEquals("reverse tail", ArraySeq(3, 2, 1), r.tail)
    assertEquals("tail tail", ArraySeq(3, 4), ArraySeq(1, 2, 3, 4).tail.tail)
    assertEquals("tail reverse tail", ArraySeq(3, 2), ArraySeq(1, 2, 3, 4).tail.reverse.tail)
    assertEquals("tail tail tail", ArraySeq(4), ArraySeq(1, 2, 3, 4).tail.tail.tail)
    assertEquals("tail tail reverse tail", ArraySeq(3), ArraySeq(1, 2, 3, 4).tail.tail.reverse.tail)
    assertEquals("tail tail tail tail", ArraySeq.empty, ArraySeq(1, 2, 3, 4).tail.tail.tail.tail)
    assertEquals("tail tail tail reverse tail", ArraySeq.empty, ArraySeq(1, 2, 3, 4).tail.tail.tail.reverse.tail)
    assertTrue("empty head fails", Try(ArraySeq.empty.head).isFailure)
    assertEquals("head", 1, ArraySeq(1, 2, 3, 4).head)
    assertEquals("reverse head", 4, ArraySeq(1, 2, 3, 4).reverse.head)
    assertEquals("reverse tail head", 3, ArraySeq(1, 2, 3, 4).reverse.tail.head)
    assertEquals("tail tail head", 3, ArraySeq(1, 2, 3, 4).tail.tail.head)
    assertEquals("reverse tail tail reverse head", 1, ArraySeq(1, 2, 3, 4).reverse.tail.tail.reverse.head)
    assertEquals("tail tail tail head", 4, ArraySeq(1, 2, 3, 4).tail.tail.tail.head)
    assertEquals("tail reverse tail reverse tail reverse head", 3, ArraySeq(1, 2, 3, 4).tail.reverse.tail.reverse.tail.reverse.head)
  }

  @Test
  def testTakeAndDrop(): Unit = {
    assertEquals("take 0", ArraySeq.empty, ArraySeq(1, 2, 3, 4).take(0))
    assertEquals("reverse take 0", ArraySeq.empty, ArraySeq(1, 2, 3, 4).reverse.take(0))
    assertEquals("take 1", ArraySeq(1), ArraySeq(1, 2, 3, 4).take(1))
    assertEquals("reverse take 1", ArraySeq(4), ArraySeq(1, 2, 3, 4).reverse.take(1))
    assertEquals("take 2", ArraySeq(1, 2), ArraySeq(1, 2, 3, 4).take(2))
    assertEquals("reverse take 2", ArraySeq(4, 3), ArraySeq(1, 2, 3, 4).reverse.take(2))
    assertEquals("drop 0", ArraySeq(1, 2, 3, 4), ArraySeq(1, 2, 3, 4).drop(0))
    assertEquals("reverse drop 0", ArraySeq(4, 3, 2, 1), ArraySeq(1, 2, 3, 4).reverse.drop(0))
    assertEquals("drop 1", ArraySeq(2, 3, 4), ArraySeq(1, 2, 3, 4).drop(1))
    assertEquals("reverse drop 1", ArraySeq(3, 2, 1), ArraySeq(1, 2, 3, 4).reverse.drop(1))
    assertEquals("drop 2", ArraySeq(3, 4), ArraySeq(1, 2, 3, 4).drop(2))
    assertEquals("reverse drop 2", ArraySeq(2, 1), ArraySeq(1, 2, 3, 4).reverse.drop(2))
    assertEquals("drop 1 take 1", ArraySeq(2), ArraySeq(1, 2, 3, 4).drop(1).take(1))
    assertEquals("reverse drop 1 take 1", ArraySeq(3), ArraySeq(1, 2, 3, 4).reverse.drop(1).take(1))
    assertEquals("drop 2 take 1", ArraySeq(3), ArraySeq(1, 2, 3, 4).drop(2).take(1))
    assertEquals("reverse drop 2 take 1", ArraySeq(2), ArraySeq(1, 2, 3, 4).reverse.drop(2).take(1))
    assertEquals("drop 2 take 2", ArraySeq(3, 4), ArraySeq(1, 2, 3, 4).drop(2).take(2))
    assertEquals("reverse drop 2 take 2", ArraySeq(2, 1), ArraySeq(1, 2, 3, 4).reverse.drop(2).take(2))
    assertEquals("drop 2 take 0", ArraySeq.empty, ArraySeq(1, 2, 3, 4).drop(2).take(0))
    assertEquals("reverse drop 2 take 0", ArraySeq.empty, ArraySeq(1, 2, 3, 4).reverse.drop(2).take(0))
    assertEquals("drop 2 take 2 drop 1 take 1", ArraySeq(4), ArraySeq(1, 2, 3, 4).drop(2).take(2).drop(1).take(1))
    assertEquals("reverse drop 2 take 2 drop 1 take 1", ArraySeq(1), ArraySeq(1, 2, 3, 4).reverse.drop(2).take(2).drop(1).take(1))
  }

  @Test
  def testFilter(): Unit = {
    assertEquals("filter empty", ArraySeq.empty, ArraySeq().filter(_ != null))
    assertEquals("filter single inclusive", ArraySeq(2), ArraySeq(2).filter(_ > 1))
    assertEquals("filter single exclusive", ArraySeq(), ArraySeq(1).filter(_ > 1))
    assertEquals("filter multiple even", ArraySeq(2, 4), ArraySeq(1, 2, 3, 4).filter(_ % 2 == 0))
    assertEquals("reverse filter multiple even", ArraySeq(4, 2), ArraySeq(1, 2, 3, 4).reverse.filter(_ % 2 == 0))
    assertEquals("filter multiple odd", ArraySeq(1, 3), ArraySeq(1, 2, 3, 4).filter(_ % 2 == 1))
    assertEquals("reverse filter multiple odd", ArraySeq(3, 1), ArraySeq(1, 2, 3, 4).reverse.filter(_ % 2 == 1))
  }

  @Test
  def testMap(): Unit = {
    assertEquals("map empty", ArraySeq.empty, ArraySeq().map(identity))
    assertEquals("reverse map empty", ArraySeq.empty, ArraySeq().reverse.map(identity))
    assertEquals("map single", ArraySeq(4), ArraySeq(2).map(n => n * n))
    assertEquals("reverse map single", ArraySeq(4), ArraySeq(2).reverse.map(n => n * n))
    assertEquals("map multiple", ArraySeq(1, 4, 9, 16), ArraySeq(1, 2, 3, 4).map(n => n * n))
    assertEquals("reverse map multiple", ArraySeq(16, 9, 4, 1), ArraySeq(1, 2, 3, 4).reverse.map(n => n * n))
  }

  @Test
  def testCollect(): Unit = {
    assertEquals("empty", ArraySeq.empty, ArraySeq().collect { case n => n })
    assertEquals("reverse empty", ArraySeq.empty, ArraySeq().reverse.collect { case n => n })
    assertEquals("single", ArraySeq(4), ArraySeq(2).collect { case n => n * n })
    assertEquals("single miss", ArraySeq(2), ArraySeq(2).collect {
      case n if n != 2 => n * n
      case n => n
    } )
    assertEquals("reverse single", ArraySeq(4), ArraySeq(2).reverse.collect { case n => n * n})
    assertEquals("reverse single miss", ArraySeq(2), ArraySeq(2).reverse.collect {
      case n if n != 2 => n * n
      case n => n
    })
    assertEquals("multiple mixed", ArraySeq(1, 2, 9, 16), ArraySeq(1, 2, 3, 4).collect {
      case n if n != 2 => n * n
      case n => n
    })
    assertEquals("reverse multiple mixed", ArraySeq(16, 9, 2, 1), ArraySeq(1, 2, 3, 4).reverse.collect {
      case n if n != 2 => n * n
      case n => n
    })
  }

  @Test
  def testFlatMap(): Unit = {
    assertEquals("flatmap empty", ArraySeq.empty, ArraySeq().flatMap((x: Any) => ArraySeq(x)))
    assertEquals("flatmap single", ArraySeq(2), ArraySeq(2).flatMap(n => ArraySeq(n)))
    assertEquals("flatmap multiple", ArraySeq(2, 4, 4, 16), ArraySeq(1, 2, 3, 4).flatMap {
      case n if n % 2 == 0 => ArraySeq(n, n * n)
      case _ => ArraySeq.empty
    })
    assertEquals("reverse flatmap multiple", ArraySeq(4, 16, 2, 4), ArraySeq(1, 2, 3, 4).reverse.flatMap {
      case n if n % 2 == 0 => ArraySeq(n, n * n)
      case _ => ArraySeq.empty
    })
  }

  @Test
  def testFill(): Unit = {
    assertEquals("fill empty", ArraySeq.empty, ArraySeq.fill(0)(9))
    assertEquals("fill single", ArraySeq(9), ArraySeq.fill(1)(9))
    assertEquals("fill multiple", ArraySeq(9, 9, 9), ArraySeq.fill(3)(9))
    assertEquals("fill multiple reverse", ArraySeq(9, 9, 9), ArraySeq.fill(3)(9).reverse)
  }

  @Test
  def testPrepend(): Unit = {
    assertEquals("prepend on empty", ArraySeq(1), 1 +: ArraySeq.empty)
    assertEquals("prepend on empty reversed", ArraySeq(1), 1 +: ArraySeq.empty.reverse)
    assertEquals("prepend single", ArraySeq(1, 2, 3, 4), 1 +: ArraySeq(2, 3, 4))
    assertEquals("prepend single on reversed", ArraySeq(1, 4, 3, 2), 1 +: ArraySeq(2, 3, 4).reverse)
    assertEquals("prepend multiple", ArraySeq(0, 1, 2, 3, 4), 0 +: 1 +: ArraySeq(2, 3, 4))
    assertEquals("prepend multiple on reversed", ArraySeq(0, 1, 4, 3, 2), 0 +: 1 +: ArraySeq(2, 3, 4).reverse)
    assertEquals(
      "prepend multiple over beginning of array",
      ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: ArraySeq(1, 2, 3, 4, 5, 6))
    val x = -1 +: 0 +: ArraySeq(1, 2, 3, 4, 5, 6).reverse
    assertEquals(
      "prepend multiple over beginning of array on reversed",
      ArraySeq(-2, -1, 0, 6, 5, 4, 3, 2, 1),
      -2 +: x)
  }

  @Test
  def testAppend(): Unit = {
    assertEquals("append on empty", ArraySeq(1), ArraySeq.empty :+ 1)
    assertEquals("append on empty reversed", ArraySeq(1), ArraySeq.empty.reverse :+ 1)
    assertEquals("append single", ArraySeq(1, 2, 3, 4), ArraySeq(1, 2, 3) :+ 4)
    assertEquals("append single on reversed", ArraySeq(3, 2, 1, 4), ArraySeq(1, 2, 3).reverse :+ 4)
    assertEquals("append multiple", ArraySeq(1, 2, 3, 4, 5), ArraySeq(1, 2, 3) :+ 4 :+ 5)
    assertEquals("append multiple on reversed", ArraySeq(3, 2, 1, 4, 5), ArraySeq(1, 2, 3).reverse :+ 4 :+ 5)
    assertEquals(
      "append multiple over ending of array",
      ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      ArraySeq(1, 2, 3, 4, 5, 6) :+ 7 :+ 8 :+ 9 :+ 10)
    assertEquals(
      "append multiple over ending of array reversed",
      ArraySeq(6, 5, 4, 3, 2, 1, 7, 8, 9, 10),
      ArraySeq(1, 2, 3, 4, 5, 6).reverse :+ 7 :+ 8 :+ 9 :+ 10)
  }

  @Test
  def testAppendAll(): Unit = {
    assertEquals("on empty", ArraySeq(1, 2, 3), ArraySeq.empty :++ List(1, 2, 3))
    assertEquals("on reversed empty", ArraySeq(1, 2, 3), ArraySeq.empty.reverse :++ List(1, 2, 3))
    assertEquals("on single", ArraySeq(1, 2, 3, 4), ArraySeq(1) :++ List(2, 3, 4))
    assertEquals("on reversed single", ArraySeq(1, 2, 3, 4), ArraySeq(1).reverse :++ List(2, 3, 4))
    assertEquals("on multiple", ArraySeq(1, 2, 3, 4, 5, 6), ArraySeq(1, 2, 3) :++ List(4, 5, 6))
    assertEquals("on reversed multiple", ArraySeq(3, 2, 1, 4, 5, 6), ArraySeq(1, 2, 3).reverse :++ List(4, 5, 6))
    assertEquals("on multiple over rear", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), ArraySeq(1, 2, 3, 4, 5, 6, 7) :++ List(8, 9, 10, 11))
    assertEquals("on reversed multiple over rear", ArraySeq(7, 6, 5, 4, 3, 2, 1, 8, 9, 10, 11), ArraySeq(1, 2, 3, 4, 5, 6, 7).reverse :++ List(8, 9, 10, 11))
  }

  @Test
  def testPrependAll(): Unit = {
    assertEquals("on empty", ArraySeq(1, 2, 3), List(1, 2, 3) ++: ArraySeq.empty)
    assertEquals("on reversed empty", ArraySeq(1, 2, 3), List(1, 2, 3) ++: ArraySeq.empty.reverse)
    assertEquals("on single", ArraySeq(1, 2, 3, 4), List(1, 2, 3) ++: ArraySeq(4))
    assertEquals("on reversed single", ArraySeq(1, 2, 3, 4), List(1, 2, 3) ++: ArraySeq(4).reverse)
    assertEquals("on multiple", ArraySeq(1, 2, 3, 4, 5, 6), List(1, 2, 3) ++: ArraySeq(4, 5, 6))
    assertEquals("on reversed multiple", ArraySeq(1, 2, 3, 6, 5, 4), List(1, 2, 3) ++: ArraySeq(4, 5, 6).reverse)
    assertEquals("on multiple over front", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), List(1, 2, 3, 4) ++: ArraySeq(5, 6, 7, 8, 9, 10, 11))
    assertEquals("on reversed multiple over front", ArraySeq(1, 2, 3, 4, 11, 10, 9, 8, 7, 6, 5), List(1, 2, 3, 4) ++: ArraySeq(5, 6, 7, 8, 9, 10, 11).reverse)
    assertEquals("with list on empty", ArraySeq(1, 2, 3), List(1, 2, 3) ++: ArraySeq.empty)
    assertEquals("with multiple lists", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 4), List(-3, -2) ++: List(-1, 0) ++: 1 +: ArraySeq(2, 3, 4))
    assertEquals("with multiple lists reversed", ArraySeq(-3, -2, 4, 3, 2, 1, 0, -1), List(-3, -2) ++: (List(-1, 0) ++: 1 +: ArraySeq(2, 3, 4)).reverse)
  }

  @Test
  def testPrependAndAppendEqual(): Unit = {
    assertEquals("prepend equal on empty", ArraySeq(1), 1 +: (1 +: ArraySeq.empty).tail)
    assertEquals("append equal on empty", ArraySeq(1), (ArraySeq.empty :+ 1).take(0) :+ 1)
    assertEquals("prepend equal on single", ArraySeq(0, 1), 0 +: (0 +: ArraySeq(1)).tail)
    assertEquals("append equal on single", ArraySeq(1, 2), (ArraySeq(1) :+ 2).take(1) :+ 2)
    val a = ArraySeq(1, 2, 3)
    val b = 0 +: a
    val c = b.tail
    val d = 0 +: c
    val e = -1 +: b
    val f = -2 +: d
    assertEquals("prepend equal", ArraySeq(1, 2, 3), a)
    assertEquals("prepend equal", ArraySeq(0, 1, 2, 3), b)
    assertEquals("prepend equal", ArraySeq(1, 2, 3), c)
    assertEquals("prepend equal", ArraySeq(0, 1, 2, 3), d)
    assertTrue("prepend eq arrays", b.array eq d.array)
    assertEquals("prepend equal", ArraySeq(-1, 0, 1, 2, 3), e)
    assertEquals("prepend equal", ArraySeq(-2, 0, 1, 2, 3), f)
  }

  @Test
  def testConcat(): Unit = {
    val eight = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8)
    assertEquals("concat eight with self", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8), eight ++ eight)
    assertEquals("concat empty with empty", ArraySeq.empty, ArraySeq.empty[Nothing] ++ ArraySeq.empty) // Explicit type annotation needed to avoid dotty bug.
    assertEquals("concat empty with single", ArraySeq(1), ArraySeq.empty ++ ArraySeq(1))
    assertEquals("concat single with empty", ArraySeq(1), ArraySeq(1) ++ ArraySeq.empty)
    assertEquals("concat empty with multiple", ArraySeq(1, 2, 3), ArraySeq.empty ++ ArraySeq(1, 2, 3))
    assertEquals("concat multiple with empty", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3) ++ ArraySeq.empty)
    assertEquals("concat single with single", ArraySeq(1, 2), ArraySeq(1) ++ ArraySeq(2))
    assertEquals("concat single with multiple", ArraySeq(1, 2, 3, 4), ArraySeq(1) ++ (ArraySeq(2) :+ 3 :+ 4))
    assertEquals("concat multiple with single", ArraySeq(1, 2, 3, 4), (ArraySeq(1) :+ 2 :+ 3) ++ ArraySeq(4))
    assertEquals("concat multiple with mix", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 4), ArraySeq(-3, -2) ++ ArraySeq(-1, 0) ++ (1 +: ArraySeq(2, 3, 4)))
    assertEquals("concat multiple with mix reversed", ArraySeq(-3, -2, 0, -1, 1, 4, 3, 2), ArraySeq(-3, -2) ++ ArraySeq(-1, 0).reverse ++ (1 +: ArraySeq(2, 3, 4).reverse))
    val full = 1 +: 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("concat empty with full", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8), ArraySeq.empty ++ full)
    assertEquals("concat full with empty", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8), full ++ ArraySeq.empty)
    assertEquals("concat single with full", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8), ArraySeq(0) ++ full)
    assertEquals("concat full with single", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9), full ++ ArraySeq(9))
    assertEquals("concat full with full", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8), full ++ full)
    assertEquals("concat full with full reversed", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1), full ++ full.reverse)
    assertEquals("concat full reversed with full", ArraySeq(8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse ++ full)
    assertEquals("concat full with full mapped reversed", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 64, 49, 36, 25, 16, 9, 4, 1), full ++ full.reverse.map(x => x * x))
    assertEquals("concat full mapped reversed with full", ArraySeq(64, 49, 36, 25, 16, 9, 4, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse.map(x => x * x) ++ full)
    val large = -1 +: 0 +: full :+ 9 :+ 10 :+ 11
    assertEquals(
      "concat large with multiple",
      ArraySeq(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
      large ++ ArraySeq(12, 13, 14))
    assertEquals(
      "concat large with itself reversed",
      ArraySeq(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1),
      large ++ large.reverse)
  }

  @Test
  def testImmutablity(): Unit = {
    val a = ArraySeq()
    assertEquals("immutablity of empty", ArraySeq.empty, a)
    val b = 1 +: a
    assertEquals("immutablity of empty", ArraySeq(1), b)
    val c = a :+ 2
    assertEquals("immutablity of empty", ArraySeq(2), c)
    val d = ArraySeq(2, 3)
    assertEquals("immutablity of multiple", ArraySeq(2, 3), d)
    val e = 0 +: 1 +: d
    assertEquals("immutablity of multiple", ArraySeq(0, 1, 2, 3), e)
    val f = d :+ 4 :+ 5
    assertEquals("immutablity of multiple", ArraySeq(2, 3, 4, 5), f)
    val g = 0 +: 1 +: f
    assertEquals("immutablity of multiple", ArraySeq(0, 1, 2, 3, 4, 5), g)

    assertEquals("immutablity of empty still", ArraySeq.empty, a)
    assertEquals("immutablity of empty still", ArraySeq(1), b)
    assertEquals("immutablity of empty still", ArraySeq(2), c)
    assertEquals("immutablity of multiple still", ArraySeq(2, 3), d)
    assertEquals("immutablity of multiple still", ArraySeq(0, 1, 2, 3), e)
    assertEquals("immutablity of multiple still", ArraySeq(2, 3, 4, 5), f)
    assertEquals("immutablity of multiple still", ArraySeq(0, 1, 2, 3, 4, 5), g)
  }

  @Test
  def testLarge(): Unit = {
    val a = ArraySeq.tabulate(10)(identity)
    val ar = a.reverse
    assertEquals("large a", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse", a, ar.reverse)
    val b = -2 +: -1 +: a
    val br = ar :+ -1 :+ -2
    assertEquals("large b", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse", b, br.reverse)
    val b2 = -4 +: -3 +: -2 +: -1 +: a
    val b2r = ar :+ -1 :+ -2 :+ -3 :+ -4
    assertEquals("large b2", ArraySeq(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse", b2, b2r.reverse)
    val c = b :+ 10
    val cr = 10 +: br
    assertEquals("large c", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed", ArraySeq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse", c, cr.reverse)
    val d2 = b2 :+ 10 :+ 11 :+ 12
    val d2r = 12 +: 11 +: 10 +: b2r
    assertEquals("large d2", ArraySeq(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse", d2, d2r.reverse)
    val d = b :+ 10 :+ 11
    val dr = 11 +: 10 +: br
    assertEquals("large d", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed", ArraySeq(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse", d, dr.reverse)
    val e = c :+ 11
    val er = 11 +: cr
    assertEquals("large e", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed", ArraySeq(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse", e, er.reverse)
    val f = -3 +: c
    val fr = cr :+ -3
    assertEquals("large f", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed", ArraySeq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse", f, fr.reverse)
    val g = e.tail.tail.tail
    val gr = er.tail.tail.tail
    assertEquals("large g", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed", ArraySeq(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
    val h = (g.tail ++ g.tail.tail.tail).tail
    val h2 = (gr.tail.reverse ++ g.tail.reverse).tail.tail
    assertEquals("large h", ArraySeq(3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5, 6, 7, 8, 9, 10, 11), h)
    assertEquals("large h2", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2), h2)

    assertEquals("large a still", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed still", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse still", a, ar.reverse)
    assertEquals("large b still", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed still", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse still", b, br.reverse)
    assertEquals("large b2 still", ArraySeq(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed still", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse still", b2, b2r.reverse)
    assertEquals("large c still", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed still", ArraySeq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse still", c, cr.reverse)
    assertEquals("large d2 still", ArraySeq(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed still", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse still", d2, d2r.reverse)
    assertEquals("large d still", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed still", ArraySeq(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse still", d, dr.reverse)
    assertEquals("large e still", ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed still", ArraySeq(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse still", e, er.reverse)
    assertEquals("large f still", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed still", ArraySeq(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse still", f, fr.reverse)
    assertEquals("large g still", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed still", ArraySeq(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
    assertEquals("large h still", ArraySeq(3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5, 6, 7, 8, 9, 10, 11), h)
    assertEquals("large h reversed still", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2), h2)
    assertEquals("large h equals its reversed reverse", h, h.reverse.reverse)
    assertEquals("large h2 equals its reversed reverse", h2, h2.reverse.reverse)
  }

  @Test
  def testIterator(): Unit = {
    var it = ArraySeq.empty[Int].iterator()
    assertFalse("empty has empty iterator", it.hasNext)

    it = ArraySeq.empty[Int].reverse.iterator()
    assertFalse("empty reverse has empty iterator", it.hasNext)

    it = ArraySeq(1).iterator()
    assertTrue("single has non empty iterator", it.hasNext)
    assertEquals("single iterator has correct element", 1, it.next())
    assertFalse("single iterator has only one element", it.hasNext)

    it = (1 +: ArraySeq.empty).iterator()
    assertTrue("single prepended has non empty iterator", it.hasNext)
    assertEquals("single prepended iterator has correct element", 1, it.next())
    assertFalse("single prepended iterator has only one element", it.hasNext)

    it = (ArraySeq.empty :+ 1).iterator()
    assertTrue("single appended has non empty iterator", it.hasNext)
    assertEquals("single appended iterator has correct element", 1, it.next())
    assertFalse("single appended iterator has only one element", it.hasNext)

    it = ArraySeq(1).reverse.iterator()
    assertTrue("single reverse has non empty iterator", it.hasNext)
    assertEquals("single reverse iterator has correct element", 1, it.next())
    assertFalse("single reverse iterator has only one element", it.hasNext)

    it = ArraySeq(1, 2).iterator()
    assertTrue("double has non empty iterator", it.hasNext)
    assertEquals("double iterator has correct first element", 1, it.next())
    assertEquals("double iterator has correct second element", 2, it.next())
    assertFalse("double iterator has only correct elements", it.hasNext)
    val reversed = ArraySeq(1, 2).reverse
    it = reversed.iterator()
    assertTrue("double reverse has non empty iterator", it.hasNext)
    assertEquals("double reverse iterator has correct first element", 2, it.next())
    assertEquals("double reverse iterator has correct second element", 1, it.next())
    assertFalse("double reverse iterator has only correct elements", it.hasNext)

    it = (0 +: 1 +: ArraySeq(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).iterator()
    assertTrue("large has non empty iterator", it.hasNext)
    assertTrue(
      "large iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    assertFalse("large iterator iterator has only correct elements", it.hasNext)

    it = (0 +: 1 +: ArraySeq(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).reverse.iterator()
    assertTrue("large reverse has non empty iterator", it.hasNext)
    assertTrue(
      "large reverse iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).reverse))
    assertFalse("large reverse iterator iterator has only correct elements", it.hasNext)

    class A()
    class B() extends A()
    val a1 = new A()
    val a2 = new A()
    val b = new B()
    val it3= (ArraySeq(a1, a2) :+ b).iterator()
    assertTrue("subs has non empty iterator", it3.hasNext)
    assertTrue(
      "subs iterator has correct elements",
      it3.sameElements(List(a1, a2, b)))

    // Test iterator index bounds checking
    it = ArraySeq.empty[Int].iterator()
    try {
      it.next()
      fail("should be out of bounds")
    } catch {
      case _: NoSuchElementException => // expected
      case NonFatal(e) =>
        fail("wrong exception: " + e)
        e.printStackTrace()
    }

    it = ArraySeq(1, 2, 3, 4).dropRight(1).iterator()
    try {
      it.next()
      it.next()
      it.next()
      it.next() // should be out of bounds
      fail("should be out of bounds")
    } catch {
      case _: NoSuchElementException => // expected
      case _: IndexOutOfBoundsException => // expected
      case NonFatal(e) =>
        fail("wrong exception: " + e)
        e.printStackTrace()
    }

    val a3 = ArraySeq.range(1, 63)
    val b3 = a3 :++ ArraySeq(63, 64, 65, 66)
    val c3 = a3 :+ 0
    val d3 = c3 :++ ArraySeq(64, 65, 66)
    val e3 = c3 :++ ArraySeq(64, -1, 66)
    val bl3 = List.newBuilder().++=(b3.iterator()).result()
    val cl3 = List.newBuilder().++=(c3.iterator()).result()
    val dl3 = List.newBuilder().++=(d3.iterator()).result()
    val el3 = List.newBuilder().++=(e3.iterator()).result()
    assertEquals("after expansion with append all", List.range(1, 67), bl3)
    assertEquals("after expansion with append all then overlay to original", List.range(1, 63) :+ 0, cl3)
    assertEquals("after expansion with append all then overlay to original and append all with only identical", List.range(1, 63) :++ List(0, 64, 65, 66), dl3)
    assertEquals("after expansion with append all then overlay to original and append all with mixed", List.range(1, 63) :++ List(0, 64, -1, 66), el3)

    val a3r = ArraySeq.range(1, 63).reverse
    val b3r = ArraySeq(63, 64, 65, 66).reverse ++: a3r
    val c3r = 0 +: a3r
    val d3r = ArraySeq(64, 65, 66).reverse ++: c3r
    val e3r = ArraySeq(64, -1, 66).reverse ++: c3r
    val bl3r = List.newBuilder().++=(b3r.iterator()).result()
    val cl3r = List.newBuilder().++=(c3r.iterator()).result()
    val dl3r = List.newBuilder().++=(d3r.iterator()).result()
    val el3r = List.newBuilder().++=(e3r.iterator()).result()
    assertEquals("after expansion with append all", List.range(1, 67).reverse, bl3r)
    assertEquals("after expansion with append all then overlay to original", (List.range(1, 63) :+ 0).reverse, cl3r)
    assertEquals("after expansion with append all then overlay to original and append all with only identical", (List.range(1, 63) :++ List(0, 64, 65, 66)).reverse, dl3r)
    assertEquals("after expansion with append all then overlay to original and append all with mixed", (List.range(1, 63) :++ List(0, 64, -1, 66)).reverse, el3r)
  }

  @Test
  def testReverseIterator(): Unit = {
    var it = ArraySeq.empty[Int].reverseIterator()
    assertFalse("empty has empty reversed iterator", it.hasNext)

    it = ArraySeq.empty[Int].reverse.reverseIterator()
    assertFalse("empty reverse has empty reversed iterator", it.hasNext)

    it = ArraySeq(1).reverseIterator()
    assertTrue("single has non empty reversed iterator", it.hasNext)
    assertEquals("single reversed iterator has correct element", 1, it.next())
    assertFalse("single reversed iterator has only one element", it.hasNext)

    it = (1 +: ArraySeq.empty).reverseIterator()
    assertTrue("single prepended has non empty reversed iterator", it.hasNext)
    assertEquals("single prepended reversed iterator has correct element", 1, it.next())
    assertFalse("single prepended reversed iterator has only one element", it.hasNext)

    it = (ArraySeq.empty :+ 1).reverseIterator()
    assertTrue("single appended has non empty reversed iterator", it.hasNext)
    assertEquals("single appended reversed iterator has correct element", 1, it.next())
    assertFalse("single appended reversed iterator has only one element", it.hasNext)

    it = ArraySeq(1).reverse.reverseIterator()
    assertTrue("single reverse has non empty reversed iterator", it.hasNext)
    assertEquals("single reverse reversed iterator has correct element", 1, it.next())
    assertFalse("single reverse reversed iterator has only one element", it.hasNext)

    it = ArraySeq(1, 2).reverseIterator()
    assertTrue("double has non empty reversed iterator", it.hasNext)
    assertEquals("double reversed iterator has correct first element", 2, it.next())
    assertEquals("double reversed iterator has correct second element", 1, it.next())
    assertFalse("double reversed iterator has only correct elements", it.hasNext)
    val reversed = ArraySeq(1, 2).reverse
    it = reversed.reverseIterator()
    assertTrue("double reverse has non empty reversed iterator", it.hasNext)
    assertEquals("double reverse reversed iterator has correct first element", 1, it.next())
    assertEquals("double reverse reversed iterator has correct second element", 2, it.next())
    assertFalse("double reverse reversed iterator has only correct elements", it.hasNext)

    it = (0 +: 1 +: ArraySeq(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).reverseIterator()
    assertTrue("large has non empty reversed iterator", it.hasNext)
    assertTrue(
      "large reversed iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).reverse))
    assertFalse("large reversed iterator has only correct elements", it.hasNext)

    it = (0 +: 1 +: ArraySeq(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).reverse.reverseIterator()
    assertTrue("large reverse has non empty reversed iterator", it.hasNext)
    assertTrue(
      "large reverse reversed iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    assertFalse("large reverse reversed iterator iterator has only correct elements", it.hasNext)

    class A()
    class B() extends A()
    val a1 = new A()
    val a2 = new A()
    val b = new B()
    val it3= (ArraySeq(a1, a2) :+ b).reverseIterator()
    assertTrue("subs has non empty reversed iterator", it3.hasNext)
    assertTrue(
      "subs reversed iterator has correct elements",
      it3.sameElements(List(a1, a2, b).reverse))

    // Test iterator index bounds checking
    it = ArraySeq.empty[Int].reverseIterator()
    try {
      it.next()
      fail("should be out of bounds")
    } catch {
      case _: NoSuchElementException => // expected
      case NonFatal(e) =>
        fail("wrong exception: " + e)
        e.printStackTrace()
    }

    it = ArraySeq(1, 2, 3, 4).dropRight(1).reverseIterator()
    try {
      it.next()
      it.next()
      it.next()
      it.next() // should be out of bounds
      fail("should be out of bounds")
    } catch {
      case _: NoSuchElementException => // expected
      case _: IndexOutOfBoundsException => // expected
      case NonFatal(e) =>
        fail("wrong exception: " + e)
        e.printStackTrace()
    }

    val a3 = ArraySeq.range(1, 63).reverse
    val b3 = ArraySeq(63, 64, 65, 66).reverse :++ a3
    val c3 = 0 +: a3
    val d3 = ArraySeq(64, 65, 66).reverse :++ c3
    val e3 = ArraySeq(64, -1, 66).reverse :++ c3
    val bl3 = List.newBuilder().++=(b3.reverseIterator()).result()
    val cl3 = List.newBuilder().++=(c3.reverseIterator()).result()
    val dl3 = List.newBuilder().++=(d3.reverseIterator()).result()
    val el3 = List.newBuilder().++=(e3.reverseIterator()).result()
    assertEquals("after expansion with append all", List.range(1, 67), bl3)
    assertEquals("after expansion with append all then overlay to original", List.range(1, 63) :+ 0, cl3)
    assertEquals("after expansion with append all then overlay to original and append all with only identical", List.range(1, 63) :++ List(0, 64, 65, 66), dl3)
    assertEquals("after expansion with append all then overlay to original and append all with mixed", List.range(1, 63) :++ List(0, 64, -1, 66), el3)

    val a3r = ArraySeq.range(1, 63).reverse
    val b3r = ArraySeq(63, 64, 65, 66).reverse ++: a3r
    val c3r = 0 +: a3r
    val d3r = ArraySeq(64, 65, 66).reverse ++: c3r
    val e3r = ArraySeq(64, -1, 66).reverse ++: c3r
    val bl3r = List.newBuilder().++=(b3r.reverseIterator()).result()
    val cl3r = List.newBuilder().++=(c3r.reverseIterator()).result()
    val dl3r = List.newBuilder().++=(d3r.reverseIterator()).result()
    val el3r = List.newBuilder().++=(e3r.reverseIterator()).result()
    assertEquals("after expansion with append all", List.range(1, 67), bl3r)
    assertEquals("after expansion with append all then overlay to original", List.range(1, 63) :+ 0, cl3r)
    assertEquals("after expansion with append all then overlay to original and append all with only identical", List.range(1, 63) :++ List(0, 64, 65, 66), dl3r)
    assertEquals("after expansion with append all then overlay to original and append all with mixed", List.range(1, 63) :++ List(0, 64, -1, 66), el3r)
  }

  @Test
  def testForeach(): Unit = {
    var n = 0
    ArraySeq.empty[Int].foreach(_ => n += 1)
    assertEquals("empty foreach", 0, n)
    ArraySeq(1).foreach(x => n += x)
    assertEquals("single foreach", 1, n)
    ArraySeq(2, 3, 4, 5, 6).foreach(x => n += x)
    assertEquals("multiple foreach", 21, n)
    var m = 6
    ArraySeq(1, 2, 3).reverse.foreach(x => m -= x)
    assertEquals("multiple foreach reversed", 0, m)
  }

  @Test
  def testIndexWhere(): Unit = {
    assertEquals("empty index where", -1, ArraySeq.empty[Int].indexWhere(_ => true))
    assertEquals("empty reversed index where", -1, ArraySeq.empty[Int].reverse.indexWhere(_ => true))
    assertEquals("single index where exists", 0, ArraySeq(1).indexWhere(x => x == 1))
    assertEquals("single index reversed where exists", 0, ArraySeq(1).reverse.indexWhere(x => x == 1))
    assertEquals("single index where not exists", -1, ArraySeq(1).indexWhere(x => x == 2))
    assertEquals("single index reversed where not exists", -1, ArraySeq(1).reverse.indexWhere(x => x == 2))
    assertEquals("multiple index where exists", 1, ArraySeq(1, 2, 3, 4).indexWhere(x => x % 2 == 0))
    assertEquals("multiple index reversed where exists", 0, ArraySeq(1, 2, 3, 4).reverse.indexWhere(x => x % 2 == 0))
    assertEquals("multiple index where not exists", -1, ArraySeq(1, 2, 3, 4).indexWhere(x => x == 5))
    assertEquals("multiple index reversed where not exists", -1, ArraySeq(1, 2, 3, 4).reverse.indexWhere(x => x == 5))
    assertEquals("multiple index where exists with from", 3, ArraySeq(1, 2, 3, 4).indexWhere(x => x % 2 == 0, 2))
    assertEquals("multiple index reversed where exists with from", 2, ArraySeq(1, 2, 3, 4).reverse.indexWhere(x => x % 2 == 0, 2))
    assertEquals("multiple index where not exists with from", -1, ArraySeq(1, 2, 3, 4).indexWhere(x => x == 5, 2))
    assertEquals("multiple index reversed where not exists with from", -1, ArraySeq(1, 2, 3, 4).reverse.indexWhere(x => x == 5, 2))
  }

  @Test
  def testLastIndexWhere(): Unit = {
    assertEquals("empty last index where", -1, ArraySeq.empty[Int].lastIndexWhere(_ => true))
    assertEquals("single last index where exists", 0, ArraySeq(1).lastIndexWhere(x => x == 1))
    assertEquals("single last index where not exists", -1, ArraySeq(1).lastIndexWhere(x => x == 2))
    assertEquals("multiple last index where exists", 3, ArraySeq(1, 2, 3, 4).lastIndexWhere(x => x % 2 == 0))
    assertEquals("multiple last index where not exists", -1, ArraySeq(1, 2, 3, 4).lastIndexWhere(x => x == 5))
    assertEquals("multiple last index where exists with end", 1, ArraySeq(1, 2, 3, 4).lastIndexWhere(x => x % 2 == 0, 2))
    assertEquals("multiple last index where not exists with end", -1, ArraySeq(1, 2, 3, 4).lastIndexWhere(x => x == 5, 2))
  }

  @Test
  def testFoldLeft(): Unit = {
    assertEquals("empty fold left", 0, ArraySeq.empty[Int].foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("single fold left", -1, ArraySeq(1).foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("multiple fold left", -10, ArraySeq(1, 2, 3, 4).foldLeft(0) {
      case (acc, x) => acc - x
    })
  }

  @Test
  def testFoldRight(): Unit = {
    assertEquals("empty fold right", 0, ArraySeq.empty[Int].foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("single fold right", 1, ArraySeq(1).foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("multiple fold right", -2, ArraySeq(1, 2, 3, 4).foldRight(0) {
      case (x, acc) => x - acc
    })
  }

  @Test
  def testFrom(): Unit = {
    assertEquals("empty list is empty", ArraySeq.empty, ArraySeq.from(List.empty))
    assertEquals("empty scala list is empty", ArraySeq.empty, ArraySeq.from(Nil))
    assertEquals("empty lazy list is empty", ArraySeq.empty, ArraySeq.from[Int](LazyList.empty))
  }

  @Test
  def testTrim(): Unit = {
    assertEquals("empty trimmed is empty", ArraySeq.empty, ArraySeq.from(List.empty).trim())
    assertEquals("single trimmed has elements array of correct length", 1, ArraySeq(1).trim().array.length)
    assertEquals("single trimmed has elements array with correct element", 1, ArraySeq(1).trim().array(0))
    assertEquals("multiple trimmed has elements array of correct length", 3, ArraySeq(1, 2, 3).trim().array.length)
    assertEquals("multiple trimmed has elements array with correct element", 1, ArraySeq(1, 2, 3).trim().array(0))
    assertEquals("multiple trimmed has elements array with correct element", 2, ArraySeq(1, 2, 3).trim().array(1))
    assertEquals("multiple trimmed has elements array with correct element", 3, ArraySeq(1, 2, 3).trim().array(2))
  }

  @Test
  def testPrependEqElement(): Unit = {
    val a = ArraySeq(1, 2, 3)
    val b = 1 +: a.tail
    val c = 0 +: a.tail
    val d = -1 +: a.tail
    val e = -1 +: d.tail
    val f = -1 +: a.tail
    assertSame("prepend equal single on tail equals original", a.array, b.array)
    assertNotSame("prepend non equal single on tail does not equal original", a, c)
    assertSame("prepend equal single on tail equals original", d.array, e.array)
    assertNotSame("prepend non equal single on tail does not equal original", d, f)
    val ar = a.reverse
    val br = 1 +: ar.tail
    val cr = 0 +: ar.tail
    val dr = -1 +: ar.tail
    val er = -1 +: dr.tail
    val fr = -1 +: ar.tail
    assertSame("prepend equal single on tail of reversed equals original", ar.array, br.array)
    assertNotSame("prepend non equal single on tail of reversed does not equal original", ar, cr)
    assertSame("prepend equal single on tail of reversed equals original", dr.array, er.array)
    assertNotSame("prepend non equal single on tail of reversed does not equal original", dr, fr)
  }

  @Test
  def testAppendEqElement(): Unit = {
    val a = ArraySeq(1, 2, 3)
    val b = a.take(2) :+ 3
    val c = a.take(2) :+ 4
    val d = a.take(2) :+ 5
    val e = d.take(2) :+ 5
    val f = a.take(2) :+ 5
    assertSame("append equal single on init equals original", a.array, b.array)
    assertNotSame("append non equal single on init does not equal original", a, c)
    assertSame("append equal single on init equals original", d.array, e.array)
    assertNotSame("append non equal single on init does not equal original", d, f)
    val ar = a.reverse
    val br = ar.take(2) :+ 3
    val cr = ar.take(2) :+ 4
    val dr = ar.take(2) :+ 5
    val er = dr.take(2) :+ 5
    val fr = ar.take(2) :+ 5
    assertSame("append equal single on init of reversed equals original", ar.array, br.array)
    assertNotSame("append non equal single on init of reversed does not equal original", ar, cr)
    assertSame("append equal single on init of reversed equals original", dr.array, er.array)
    assertNotSame("append non equal single on init of reversed does not equal original", dr, fr)
  }

  @Test
  def testLast(): Unit = {
    assertTrue("empty last fails", Try(ArraySeq.empty.last).isFailure)
    assertEquals("single last", 1, ArraySeq(1).last)
    assertEquals("single reversed last", 1, ArraySeq(1).reverse.last)
    assertEquals("multiple last", 3, ArraySeq(1, 2, 3).last)
    assertEquals("multiple reversed last", 1, ArraySeq(1, 2, 3).reverse.last)
  }

  @Test
  def testInit(): Unit = {
    assertTrue("empty init fails", Try(ArraySeq.empty.init).isFailure)
    assertEquals("single init", ArraySeq.empty, ArraySeq(1).init)
    assertEquals("single reversed init", ArraySeq.empty, ArraySeq(1).reverse.init)
    assertEquals("multiple init", ArraySeq(1, 2), ArraySeq(1, 2, 3).init)
    val r = ArraySeq(1, 2, 3).reverse
    val in = r.init
    assertEquals("multiple reverse init", ArraySeq(3, 2), in)
  }

  @Test
  def testSlice(): Unit = {
    assertEquals("empty slice of single is empty", ArraySeq.empty, ArraySeq(1).slice(0, 0))
    assertEquals("empty slice of single reversed is empty", ArraySeq.empty, ArraySeq(1).reverse.slice(0, 0))
    assertEquals("empty slice of multiple is empty", ArraySeq.empty, ArraySeq(1, 2, 3).slice(1, 1))
    assertEquals("empty slice of multiple reversed is empty", ArraySeq.empty, ArraySeq(1, 2, 3).reverse.slice(1, 1))
    assertEquals("single slice of single", ArraySeq(1), ArraySeq(1).slice(0, 1))
    assertEquals("single slice of single reversed", ArraySeq(1), ArraySeq(1).slice(0, 1))
    assertEquals("single slice of multiple", ArraySeq(3), ArraySeq(1, 2, 3).slice(2, 3))
    assertEquals("single slice of multiple reversed", ArraySeq(1), ArraySeq(1, 2, 3).reverse.slice(2, 3))
    assertEquals("multiple slice of multiple", ArraySeq(2, 3), ArraySeq(1, 2, 3).slice(1, 3))
    assertEquals("multiple slice of multiple reversed", ArraySeq(2, 1), ArraySeq(1, 2, 3).reverse.slice(1, 3))
    var sx = 4 +: ArraySeq(1, 2, 3).reverse
    assertEquals("multiple slice of multiple reversed with prepended", ArraySeq(3, 2), sx.slice(1, 3))
    sx = ArraySeq(1, 2, 3).reverse :+ 0
    assertEquals("multiple slice of multiple reversed with append", ArraySeq(2, 1), sx.slice(1, 3))
    assertEquals("multiple slice of full", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).slice(0, 3))
    assertEquals("multiple slice of full reversed", ArraySeq(3, 2, 1), ArraySeq(1, 2, 3).reverse.slice(0, 3))
    assertEquals("multiple slice of full with overlay", ArraySeq(4, 2, 3), ArraySeq(1, 2, 3).updated(0, 4).slice(0, 3))
    assertEquals("multiple slice of full with overlay reversed", ArraySeq(3, 2, 4), ArraySeq(1, 2, 3).updated(0, 4).reverse.slice(0, 3))
    assertEquals("multiple slice of full with reversed overlay", ArraySeq(4, 2, 1), ArraySeq(1, 2, 3).reverse.updated(0, 4).slice(0, 3))
  }

  @Test
  def testTakeRightAndDropRight(): Unit = {
    assertEquals("take right 0", ArraySeq.empty, ArraySeq(1, 2, 3, 4).takeRight(0))
    assertEquals("take right 1", ArraySeq(4), ArraySeq(1, 2, 3, 4).takeRight(1))
    assertEquals("take right 2", ArraySeq(3, 4), ArraySeq(1, 2, 3, 4).takeRight(2))
    assertEquals("drop right 0", ArraySeq(1, 2, 3, 4), ArraySeq(1, 2, 3, 4).dropRight(0))
    assertEquals("drop right 1", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3, 4).dropRight(1))
    assertEquals("drop right 2", ArraySeq(1, 2), ArraySeq(1, 2, 3, 4).dropRight(2))
    assertEquals("drop right 1 take right 1", ArraySeq(3), ArraySeq(1, 2, 3, 4).dropRight(1).takeRight(1))
    assertEquals("drop right 2 take right 1", ArraySeq(2), ArraySeq(1, 2, 3, 4).dropRight(2).takeRight(1))
    assertEquals("drop right 2 take right 2", ArraySeq(1, 2), ArraySeq(1, 2, 3, 4).dropRight(2).takeRight(2))
    assertEquals("drop right 2 take right 0", ArraySeq.empty, ArraySeq(1, 2, 3, 4).dropRight(2).takeRight(0))
    assertEquals("drop right 2 take right 2 drop right 1 take right 1", ArraySeq(1), ArraySeq(1, 2, 3, 4).dropRight(2).takeRight(2).dropRight(1).takeRight(1))
  }

  @Test
  def testPatchToFront(): Unit = {
    assertEquals("Empty with empty at front", ArraySeq.empty, ArraySeq.empty.patch(0, ArraySeq.empty, 0))
    assertEquals("Empty reversed with empty at front", ArraySeq.empty, ArraySeq.empty.reverse.patch(0, ArraySeq.empty, 0))
    assertEquals("Empty with empty reversed at front", ArraySeq.empty, ArraySeq.empty.patch(0, ArraySeq.empty.reverse, 0))
    assertEquals("Empty reversed with empty reversed at front", ArraySeq.empty, ArraySeq.empty.reverse.patch(0, ArraySeq.empty.reverse, 0))
    assertEquals("Empty with single at front", ArraySeq(1), ArraySeq.empty.patch(0, ArraySeq(1), 0))
    assertEquals("Empty reversed with single at front", ArraySeq(1), ArraySeq.empty.reverse.patch(0, ArraySeq(1), 0))
    assertEquals("Empty with single reversed at front", ArraySeq(1), ArraySeq.empty.patch(0, ArraySeq(1).reverse, 0))
    assertEquals("Empty reversed with single reversed at front", ArraySeq(1), ArraySeq.empty.reverse.patch(0, ArraySeq(1).reverse, 0))
    assertEquals("Single with empty at front replacing 0", ArraySeq(1), ArraySeq(1).patch(0, ArraySeq.empty, 0))
    assertEquals("Single with empty at front replacing 0", ArraySeq(1), ArraySeq(1).patch(0, ArraySeq.empty, 0))
    assertEquals("Single with empty at front replacing 1", ArraySeq.empty, ArraySeq(1).patch(0, ArraySeq.empty, 1))
    assertEquals("Single with single at front replacing 0", ArraySeq(1, 2), ArraySeq(2).patch(0, ArraySeq(1), 0))
    assertEquals("Single reversed with single at front replacing 0", ArraySeq(1, 2), ArraySeq(2).reverse.patch(0, ArraySeq(1), 0))
    assertEquals("Single with single at front replacing 1", ArraySeq(1), ArraySeq(2).patch(0, ArraySeq(1), 1))
    assertEquals("Single reversed with single at front replacing 1", ArraySeq(1), ArraySeq(2).reverse.patch(0, ArraySeq(1), 1))
    assertEquals("Single with single at front replacing 2", ArraySeq(1), ArraySeq(2).patch(0, ArraySeq(1), 2))
    assertEquals("Single with single at front replacing max", ArraySeq(1), ArraySeq(2).patch(0, ArraySeq(1), 245))
    assertEquals("Single with multiple at front replacing 0", ArraySeq(1, 2, 3, 4), ArraySeq(4).patch(0, ArraySeq(1, 2, 3), 0))
    assertEquals("Single with multiple at front replacing 1", ArraySeq(1, 2, 3), ArraySeq(4).patch(0, ArraySeq(1, 2, 3), 1))
    assertEquals("Single with multiple at front replacing 2", ArraySeq(1, 2, 3), ArraySeq(4).patch(0, ArraySeq(1, 2, 3), 2))
    assertEquals("Single with multiple at front replacing max", ArraySeq(1, 2, 3), ArraySeq(2).patch(0, ArraySeq(1, 2, 3), 245))
    assertEquals("Multiple with empty at front replacing 0", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).patch(0, ArraySeq.empty, 0))
    assertEquals("Multiple with empty at front replacing 1", ArraySeq(2, 3), ArraySeq(1, 2, 3).patch(0, ArraySeq.empty, 1))
    assertEquals("Multiple with single at front replacing 0", ArraySeq(4, 1, 2, 3), ArraySeq(1, 2, 3).patch(0, ArraySeq(4), 0))
    assertEquals("Multiple with single at front replacing 1", ArraySeq(4, 2, 3), ArraySeq(1, 2, 3).patch(0, ArraySeq(4), 1))
    assertEquals("Multiple with single at front replacing 2", ArraySeq(4, 3), ArraySeq(1, 2, 3).patch(0, ArraySeq(4), 2))
    assertEquals("Multiple with single at front replacing max", ArraySeq(4), ArraySeq(1, 2, 3).patch(0, ArraySeq(4), 245))
  }
  @Test
  def testPatchToRear(): Unit = {
    assertEquals("Empty with empty at rear", ArraySeq.empty, ArraySeq.empty.patch(0, ArraySeq.empty, 0))
    assertEquals("Empty reversed with empty at rear", ArraySeq.empty, ArraySeq.empty.reverse.patch(0, ArraySeq.empty, 0))
    assertEquals("Empty with single at rear", ArraySeq(1), ArraySeq.empty.patch(0, ArraySeq(1), 0))
    assertEquals("Empty reversed with single at rear", ArraySeq(1), ArraySeq.empty.reverse.patch(0, ArraySeq(1), 0))
    assertEquals("Single with empty at rear", ArraySeq(1), ArraySeq(1).patch(1, ArraySeq.empty, 0))
    assertEquals("Single reversed with empty at rear", ArraySeq(1), ArraySeq(1).reverse.patch(1, ArraySeq.empty, 0))
    assertEquals("Single with single at rear replacing 0", ArraySeq(2, 1), ArraySeq(2).patch(1, ArraySeq(1), 0))
    assertEquals("Single reversed with single at rear replacing 0", ArraySeq(2, 1), ArraySeq(2).reverse.patch(1, ArraySeq(1), 0))
    assertEquals("Single with multiple at rear", ArraySeq(4, 1, 2, 3), ArraySeq(4).patch(1, ArraySeq(1, 2, 3), 0))
    assertEquals("Single reversed with multiple at rear", ArraySeq(4, 1, 2, 3), ArraySeq(4).reverse.patch(1, ArraySeq(1, 2, 3), 0))
    assertEquals("Multiple with empty at rear", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).patch(3, ArraySeq.empty, 0))
    assertEquals("Multiple reversed with empty at rear", ArraySeq(3, 2, 1), ArraySeq(1, 2, 3).reverse.patch(3, ArraySeq.empty, 0))
    assertEquals("Multiple with single at rear", ArraySeq(1, 2, 3, 4), ArraySeq(1, 2, 3).patch(3, ArraySeq(4), 0))
    assertEquals("Multiple reversed with single at rear", ArraySeq(3, 2, 1, 4), ArraySeq(1, 2, 3).reverse.patch(3, ArraySeq(4), 0))
    assertEquals("Multiple with multiple at rear", ArraySeq(1, 2, 3, 4, 5, 6), ArraySeq(1, 2, 3).patch(3, ArraySeq(4, 5, 6), 0))
    assertEquals("Multiple reversed with multiple at rear", ArraySeq(3, 2, 1, 4, 5, 6), ArraySeq(1, 2, 3).reverse.patch(3, ArraySeq(4, 5, 6), 0))
  }
  @Test
  def testPatchInMiddle(): Unit = {
    assertEquals("Multiple with empty in middle replacing 0", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq.empty, 0))
    assertEquals("Multiple reversed with empty in middle replacing 0", ArraySeq(3, 2, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq.empty, 0))
    var xs = ArraySeq(1, 2, 3)
    var ys = xs.patch(1, ArraySeq.empty, 1)
    assertEquals("Multiple with empty in middle replacing 1", ArraySeq(1, 3), ys)
    xs = ArraySeq(1, 2, 3, 4, 5, 6).reverse
    ys = xs.patch(1, ArraySeq.empty, 1)
    assertEquals("Multiple reversed with empty in middle replacing 1", ArraySeq(6, 4, 3, 2, 1), ys)
    xs = ArraySeq(1, 2, 3).reverse
    ys = xs.patch(1, ArraySeq.empty, 1)
    assertEquals("Multiple reversed with empty in middle replacing 1", ArraySeq(3, 1), ys)
    assertEquals("Multiple with empty in middle replacing max", ArraySeq(1), ArraySeq(1, 2, 3).patch(1, ArraySeq.empty, 245))
    assertEquals("Multiple reversed with empty in middle replacing max", ArraySeq(3), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq.empty, 245))
    assertEquals("Multiple with single in middle replacing 0", ArraySeq(1, 4, 2, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4), 0))
    xs = ArraySeq(1, 2, 3).reverse
    ys = xs.patch(1, ArraySeq(4), 0)
    assertEquals("Multiple reversed with single in middle replacing 0", ArraySeq(3, 4, 2, 1), ys)
    assertEquals("Multiple with single in middle replacing 1", ArraySeq(1, 4, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4), 1))
    assertEquals("Multiple reversed with single in middle replacing 1", ArraySeq(3, 4, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4), 1))
    assertEquals("Multiple with single in middle replacing max", ArraySeq(1, 4), ArraySeq(1, 2, 3).patch(1, ArraySeq(4), 245))
    assertEquals("Multiple reversed with single in middle replacing max", ArraySeq(3, 4), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4), 245))
    assertEquals("Multiple with multiple in middle replacing 0", ArraySeq(1, 4, 5, 6, 2, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4, 5, 6), 0))
    assertEquals("Multiple reversed with multiple in middle replacing 0", ArraySeq(3, 4, 5, 6, 2, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4, 5, 6), 0))
    assertEquals("Multiple with multiple in middle replacing 1", ArraySeq(1, 4, 5, 6, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4, 5, 6), 1))
    assertEquals("Multiple reversed with multiple in middle replacing 1", ArraySeq(3, 4, 5, 6, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4, 5, 6), 1))
    assertEquals("Multiple with multiple in middle replacing max", ArraySeq(1, 4, 5, 6), ArraySeq(1, 2, 3).patch(1, ArraySeq(4, 5, 6), 245))
    assertEquals("Multiple reversed with multiple in middle replacing max", ArraySeq(3, 4, 5, 6), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4, 5, 6), 245))
  }
  @Test
  def testAutoTrimming(): Unit = {
    val autoTrimming = AutoTrimming(minUsagePercentage = 25)
    val s6 = ArraySeq(1, 2, 3, 4, 5, 6).withAutoTrimming(autoTrimming)
    assertEquals("1 out of 8 is not trimmed", 8, s6.slice(0, 1).array.length)
    assertEquals("2 out of 8 is not trimmed", 8, s6.slice(0, 2).array.length)
    assertEquals("3 out of 8 is not trimmed", 8, s6.slice(0, 3).array.length)
    val s32 = ArraySeq.tabulate(32)(identity).withAutoTrimming(autoTrimming)
    assertEquals("2 out of 32 is trimmed to 8", 8, s32.slice(0, 2).array.length)
    assertEquals("8 out of 32 is trimmed to 8", 8, s32.slice(0, 8).array.length)
    assertEquals("9 out of 32 is not trimmed", 32, s32.slice(0, 9).array.length)
    var s = ArraySeq.empty[Int].withAutoTrimming(autoTrimming)
    for (i <- 0 until 512) {
      if (i % 2 == 0) s = s :+ i
      else s = i +: s
    }
    assertEquals("512 is correct capacity", 512, s.array.length)
    for (i <- 0 until 31) {
      s = s.tail
    }
    assertEquals("after removing 31 out of 512 capacity is still 512", 512, s.array.length)
    s = s.tail
    assertEquals("after removing 32 out of 512 capacity is still 512", 512, s.array.length)
    s = s.tail
    assertEquals("after removing 33 out of 512 capacity is still 512", 512, s.array.length)
    for (i <- 33 until 255) {
      s = s.tail
    }
    assertEquals("after removing 255 out of 512 capacity is still 512", 512, s.array.length)
    s = s.tail
    assertEquals("after removing 256 out of 512 capacity is still 512", 512, s.array.length)
    s = s.tail
    assertEquals("after removing 257 out of 512 capacity is still 512", 512, s.array.length)
    for (i <- 257 until 383) {
      s = s.tail
    }
    assertEquals("after removing 383 out of 512 capacity is still 512", 512, s.array.length)
    s = s.tail
    assertEquals("after removing 384 out of 512 capacity is 128", 128, s.array.length)
    s = s.tail
    assertEquals("after removing 385 out of 512 capacity is 128", 128, s.array.length)
    s = s.slice(0, 2)
    assertEquals("after keeping 2 out of 128 capacity is 8", 8, s.array.length)
    val s2 = ArraySeq.range(0, 32).withoutAutoTrimming
    assertEquals("2 out of 32 is not trimmed", 32, s2.slice(0, 2).array.length)
    assertEquals("8 out of 32 is not trimmed", 32, s2.slice(0, 8).array.length)
    assertEquals("9 out of 32 is not trimmed", 32, s2.slice(0, 9).array.length)
    assertEquals("24 out of 32 is not trimmed", 32, s2.slice(0, 24).array.length)
    val s3 = s2.withAutoTrimming(AutoTrimming(minUsagePercentage = 75))
    assertEquals("2 out of 32 is trimmed to 8", 8, s3.slice(0, 2).array.length)
    assertEquals("8 out of 32 is trimmed to 8", 8, s3.slice(0, 8).array.length)
    assertEquals("9 out of 32 is trimmed to 16", 16, s3.slice(0, 9).array.length)

    assertEquals("2 out of 32 with function is trimmed to 8", 8, s3.withoutAutoTrimming { s3 =>
      var s = s3.slice(0, 2)
      assertEquals("2 out of 32 in function is not trimmed", 32, s.array.length)
      s
    }.array.length)
  }
  @Test
  def testPadTo(): Unit = {
    assertEquals("empty to 0 is empty", ArraySeq.empty, ArraySeq.empty.padTo(0, 0))
    assertEquals("empty to 3", ArraySeq(0, 0, 0), ArraySeq.empty.padTo(3, 0))
    assertEquals("single to 0 is same", ArraySeq(2), ArraySeq(2).padTo(0, 0))
    assertEquals("single to 1 is same", ArraySeq(2), ArraySeq(2).padTo(1, 0))
    assertEquals("single to 2", ArraySeq(2, 0), ArraySeq(2).padTo(2, 0))
    assertEquals("single to 3", ArraySeq(2, 0, 0), ArraySeq(2).padTo(3, 0))
    assertEquals("multiple to 0 is same", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(0, 0))
    assertEquals("reversed multiple to 0 is same", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(0, 0))
    assertEquals("multiple to 1 is same", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(1, 0))
    assertEquals("reversed multiple to 1 is same", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(1, 0))
    assertEquals("multiple to 12 is same", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(12, 0))
    assertEquals("reversed multiple to 12 is same", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(12, 0))
    assertEquals("multiple to 24", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(24, 0))
    assertEquals("reversed multiple to 24", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(24, 0))
  }

  @Test
  def testDistinct(): Unit = {
    assertEquals("empty is empty", ArraySeq.empty, ArraySeq.empty.distinct)
    assertEquals("single is same", ArraySeq(2), ArraySeq(2).distinct)
    assertEquals("multiple without duplicates is same", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).distinct)
    assertEquals("reversed multiple without duplicates is same", ArraySeq(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.distinct)
    assertEquals("multiple with duplicates", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8), ArraySeq(1, 2, 3, 4, 5, 6, 1, 2, 7, 6, 8).distinct)
    assertEquals("reversed multiple with duplicates", ArraySeq(8, 6, 7, 2, 1, 5, 4, 3), ArraySeq(1, 2, 3, 4, 5, 6, 1, 2, 7, 6, 8).reverse.distinct)
  }

  @Test
  def testReset(): Unit = {
    var a = ArraySeq(1, 2, 3)
    assertEquals("a", ArraySeq(1, 2, 3), a)
    assertEquals("a.array", List(1, 2, 3, O, O, O, O, O), a.array.to(List))
    a = a.reset()
    assertEquals("a after resetting", ArraySeq(1, 2, 3), a)
    assertEquals("a.array after resetting", List(1, 2, 3, O, O, O, O, O), a.array.to(List))
    val b = a :+ 4
    assertEquals("a after appending", ArraySeq(1, 2, 3), a)
    assertEquals("b after appending", ArraySeq(1, 2, 3, 4), b)
    assertEquals("a.array after appending", List(1, 2, 3, 4, O, O, O, O), a.array.to(List))
    a = a.reset()
    assertEquals("a after appending and resetting", ArraySeq(1, 2, 3), a)
    assertEquals("b after appending and resetting", ArraySeq(1, 2, 3, 4), b)
    assertEquals("a.array after appending and resetting", List(1, 2, 3, 4, O, O, O, O), a.array.to(List))
    val c = a :+ 5
    assertEquals("a after appending and resetting and appending", ArraySeq(1, 2, 3), a)
    assertEquals("b after appending and resetting and appending", ArraySeq(1, 2, 3, 5), b)
    assertEquals("c after appending and resetting and appending", ArraySeq(1, 2, 3, 5), c)
    assertEquals("a.array after appending and resetting and appending", List(1, 2, 3, 5, O, O, O, O), a.array.to(List))
  }

  @Test
  def testOverlayPrepend(): Unit = {
    val a = ArraySeq(1, 2, 3)
    val b = 0 +: a
    val c = 8 +: a
    val s = c.toString
    val d = -1 +: b
    val e = -1 +: c
    assertEquals("b has correct elements", ArraySeq(0, 1, 2, 3), b)
    assertEquals("c has correct elements", ArraySeq(8, 1, 2, 3), c)
    assertEquals("d has correct elements", ArraySeq(-1, 0, 1, 2, 3), d)
    assertEquals("e has correct elements", ArraySeq(-1, 8, 1, 2, 3), e)
    assertSame("d and e has same array", d.array, e.array)
    val ar = a.reverse
    val br = 0 +: ar
    val cr = 8 +: ar
    val dr = -1 +: br
    val er = -1 +: cr
    assertEquals("br has correct elements", ArraySeq(0, 3, 2, 1), br)
    assertEquals("cr has correct elements", ArraySeq(8, 3, 2, 1), cr)
    assertEquals("dr has correct elements", ArraySeq(-1, 0, 3, 2, 1), dr)
    assertEquals("er has correct elements", ArraySeq(-1, 8, 3, 2, 1), er)
    assertSame("dr and er has same array", dr.array, er.array)
  }

  @Test
  def testOverlayPrependExpand(): Unit = {
    val a = ArraySeq(1, 2, 3, 4, 5, 6, 7)
    val b = 0 +: a
    val c = 8 +: a
    val d = -1 +: b
    val e = -1 +: c
    assertEquals("b has correct elements", ArraySeq(0, 1, 2, 3, 4, 5, 6, 7), b)
    assertEquals("c has correct elements", ArraySeq(8, 1, 2, 3, 4, 5, 6, 7), c)
    assertEquals("d has correct elements", ArraySeq(-1, 0, 1, 2, 3, 4, 5, 6, 7), d)
    assertEquals("e has correct elements", ArraySeq(-1, 8, 1, 2, 3, 4, 5, 6, 7), e)
    val ar = a.reverse
    val br = 0 +: ar
    val cr = 8 +: ar
    val dr = -1 +: br
    val er = -1 +: cr
    assertEquals("br has correct elements", ArraySeq(0, 7, 6, 5, 4, 3, 2, 1), br)
    assertEquals("cr has correct elements", ArraySeq(8, 7, 6, 5, 4, 3, 2, 1), cr)
    assertEquals("dr has correct elements", ArraySeq(-1, 0, 7, 6, 5, 4, 3, 2, 1), dr)
    assertEquals("er has correct elements", ArraySeq(-1, 8, 7, 6, 5, 4, 3, 2, 1), er)
  }

  @Test
  def testOverlayAppend(): Unit = {
    val a = ArraySeq(1, 2, 3)
    val b = a :+ 4
    val c = a :+ 8
    val d = b :+ 5
    val e = c :+ 5
    assertEquals("b has correct elements", ArraySeq(1, 2, 3, 4), b)
    assertEquals("c has correct elements", ArraySeq(1, 2, 3, 8), c)
    assertEquals("d has correct elements", ArraySeq(1, 2, 3, 4, 5), d)
    assertEquals("e has correct elements", ArraySeq(1, 2, 3, 8, 5), e)
    assertSame("d and e has same array", d.array, e.array)
    val ar = a.reverse
    val br = ar :+ 4
    val cr = ar :+ 8
    val dr = br :+ 5
    val er = cr :+ 5
    assertEquals("br has correct elements", ArraySeq(3, 2, 1, 4), br)
    assertEquals("cr has correct elements", ArraySeq(3, 2, 1, 8), cr)
    assertEquals("dr has correct elements", ArraySeq(3, 2, 1, 4, 5), dr)
    assertEquals("er has correct elements", ArraySeq(3, 2, 1, 8, 5), er)
    assertSame("dr and er has same array", dr.array, er.array)
  }

  @Test
  def testOverlayAppendExpand(): Unit = {
    val a = ArraySeq(-3, -2, -1, 0, 1, 2, 3)
    val b = a :+ 4
    val c = a :+ 8
    val d = b :+ 5
    val e = c :+ 5
    assertEquals("b has correct elements", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 4), b)
    assertEquals("c has correct elements", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 8), c)
    assertEquals("d has correct elements", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 4, 5), d)
    assertEquals("e has correct elements", ArraySeq(-3, -2, -1, 0, 1, 2, 3, 8, 5), e)
    val ar = a.reverse
    val br = ar :+ 4
    val cr = ar :+ 8
    val dr = br :+ 5
    val x = dr.foreach(-_)
    val er = cr :+ 5
    assertEquals("br has correct elements", ArraySeq(3, 2, 1, 0, -1, -2, -3, 4), br)
    assertEquals("cr has correct elements", ArraySeq(3, 2, 1, 0, -1, -2, -3, 8), cr)
    assertEquals("dr has correct elements", ArraySeq(3, 2, 1, 0, -1, -2, -3, 4, 5), dr)
    assertEquals("er has correct elements", ArraySeq(3, 2, 1, 0, -1, -2, -3, 8, 5), er)
  }

  @Test
  def testUpdated(): Unit = {
    assertEquals("single", ArraySeq(0), ArraySeq(1).updated(0, 0))
    assertEquals("single reversed", ArraySeq(0), ArraySeq(1).reverse.updated(0, 0))
    assertEquals("small first", ArraySeq(0, 2, 3), ArraySeq(1, 2, 3).updated(0, 0))
    assertEquals("small first reversed", ArraySeq(0, 2, 1), ArraySeq(1, 2, 3).reverse.updated(0, 0))
    assertEquals("small middle", ArraySeq(1, 0, 3), ArraySeq(1, 2, 3).updated(1, 0))
    assertEquals("small middle reversed", ArraySeq(3, 0, 1), ArraySeq(1, 2, 3).reverse.updated(1, 0))
    assertEquals("small last", ArraySeq(1, 2, 0), ArraySeq(1, 2, 3).updated(2, 0))
    assertEquals("small last reversed", ArraySeq(3, 2, 0), ArraySeq(1, 2, 3).reverse.updated(2, 0))
    assertEquals("large first", ArraySeq(0, 2, 3, 4, 5, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).updated(0, 0))
    assertEquals("large first reversed", ArraySeq(0, 8, 7, 6, 5, 4, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).reverse.updated(0, 0))
    assertEquals("large middle", ArraySeq(1, 2, 3, 4, 0, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).updated(4, 0))
    assertEquals("large middle reversed", ArraySeq(9, 8, 7, 6, 0, 4, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).reverse.updated(4, 0))
    assertEquals("large last", ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 0), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).updated(8, 0))
    assertEquals("large last reversed", ArraySeq(9, 8, 7, 6, 5, 4, 3, 2, 0), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).reverse.updated(8, 0))
    assertEquals("large many", ArraySeq(0, 1, 2, 3, 4, 5, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).updated(0, 0).updated(1, 1).updated(2, 2).updated(3, 3).updated(4, 4).updated(5, 5))
    assertEquals("large many reversed", ArraySeq(0, 1, 2, 3, 4, 5, 3, 2, 1), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).reverse.updated(0, 0).updated(1, 1).updated(2, 2).updated(3, 3).updated(4, 4).updated(5, 5))
    val xs = ArraySeq.range(0, 100)
    val ys = ArraySeq.range(0, 100)
    assertEquals("large last repeated", ys.updated(99, 4), xs.updated(99, 0).updated(99, 1).updated(99, 2).updated(99, 3).updated(99, 4))
  }

  @Test
  def testMaxOverlayPercentage(): Unit = {
    val s = ArraySeq.range(1, 65)
    var t = s
    var u = s.take(8).map(-_) ++: s.drop(8)
    for (i <- 1 to 8) t = t.updated(i - 1, -i)
    assertEquals("will produce correct results when updating 8 / 64", u, t)
    var xs = u.to(List)
    var ys = t.array.to(List)
    t = s
    u = s.take(32).map(-_) ++: s.drop(32)
    for (i <- 1 to 31) t = t.updated(i - 1, -i)
    t = t.updated(31, -32)
    assertEquals("will produce correct results when updating 32 / 64", u, t)
    xs = u.to(List)
    ys = t.array.to(List)
    assertEquals("will merge overlay when updating 32 / 64", xs, ys)
    t = s
    u = s.take(64).map(-_)
    for (i <- 1 to 63) t = t.updated(i - 1, -i)
    t = t.updated(63, -64)
    assertEquals("will produce correct results when updating 48 / 64", u, t)
    xs = u.to(List)
    ys = t.array.to(List)
    assertEquals("will merge overlay when updating 48 / 64", xs, ys)
  }

  @Test
  def testMkString(): Unit = {
    assertEquals("empty", "[]", ArraySeq.empty.mkString("[", ", ", "]"))
    assertEquals("single", "[1]", ArraySeq(1).mkString("[", ", ", "]"))
    val xs = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    assertEquals("multiple", "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]", xs.mkString("[", ", ", "]"))
    val xsr = xs.reverse
    assertEquals("multiple reversed", "[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]", xsr.mkString("[", ", ", "]"))
    val ysr = ArraySeq(1, 2, 3).reverse :+ 4
    assertEquals("append single on reversed", "[3, 2, 1, 4]", ysr.mkString("[", ", ", "]"))
  }

  @Test
  def testEquals(): Unit = {
    assertEquals("empty with empty", ArraySeq.empty, ArraySeq.empty)
    assertEquals("empty with empty reversed", ArraySeq.empty, ArraySeq.empty.reverse)
    assertEquals("empty with empty with auto triming", ArraySeq.empty, ArraySeq.empty.withAutoTrimming(AutoTrimming.Always))
    assertEquals("empty with empty with auto triming reversed", ArraySeq.empty, ArraySeq.empty.withAutoTrimming(AutoTrimming.Always).reverse)
    assertEquals("empty with empty without auto triming", ArraySeq.empty, ArraySeq.empty.withoutAutoTrimming)
    assertEquals("empty with empty without auto triming reversed", ArraySeq.empty, ArraySeq.empty.withoutAutoTrimming.reverse)
    assertEquals("empty with empty apply", ArraySeq.empty, ArraySeq())
    assertEquals("empty with empty apply reversed", ArraySeq.empty, ArraySeq().reverse)
    assertEquals("empty with empty slice", ArraySeq.empty, ArraySeq(1, 2, 3).slice(0, 0))
    assertEquals("empty with empty reversed slice", ArraySeq.empty, ArraySeq(1, 2, 3).reverse.slice(0, 0))
    assertEquals("single with single", ArraySeq(1), ArraySeq(1))
    assertEquals("single with single reversed", ArraySeq(1), ArraySeq(1).reverse)
    assertEquals("single with single without auto triming", ArraySeq(1), ArraySeq(1).withoutAutoTrimming)
    assertEquals("single with single without auto triming reversed", ArraySeq(1), ArraySeq(1).withoutAutoTrimming.reverse)
    assertEquals("single with single slice", ArraySeq(1), ArraySeq(1, 2, 3).slice(0, 1))
    assertEquals("single with single reversed slice", ArraySeq(1), ArraySeq(3, 2, 1).reverse.slice(0, 1))
    assertEquals("multiple with multiple", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3))
    assertEquals("multiple with multiple reversed", ArraySeq(1, 2, 3), ArraySeq(3, 2, 1).reverse)
    assertEquals("multiple with multiple slice", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3, 4, 5, 6).slice(0, 3))
    assertEquals("multiple with multiple reversed slice", ArraySeq(1, 2, 3), ArraySeq(6, 5, 4, 3, 2, 1).reverse.slice(0, 3))
  }

  @Test
  def testTakeWhile(): Unit = {
    assertEquals("empty with true", ArraySeq.empty, ArraySeq.empty[Int].takeWhile(_ => true))
    assertEquals("empty with false", ArraySeq.empty, ArraySeq.empty[Int].takeWhile(_ => false))
    assertEquals("single with true", ArraySeq(1), ArraySeq(1).takeWhile(_ => true))
    assertEquals("single with false", ArraySeq.empty, ArraySeq(1).takeWhile(_ => false))
    assertEquals("multiple with true", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).takeWhile(_ => true))
    assertEquals("multiple with false", ArraySeq.empty, ArraySeq(1).takeWhile(_ => false))
    assertEquals("multiple with predicate", ArraySeq(1, 2), ArraySeq(1, 2, 3, 4, 5, 6).takeWhile(_ < 3))
  }

  @Test
  def testZip(): Unit = {
    assertEquals("empty with empty", ArraySeq.empty, ArraySeq.empty.zip(ArraySeq.empty))
    assertEquals("empty with single", ArraySeq.empty, ArraySeq.empty.zip(ArraySeq(1)))
    assertEquals("empty with multiple", ArraySeq.empty, ArraySeq.empty.zip(ArraySeq(1, 2, 3)))
    assertEquals("single with empty", ArraySeq.empty, ArraySeq(1).zip(ArraySeq.empty))
    assertEquals("single with single", ArraySeq(1 -> 2), ArraySeq(1).zip(ArraySeq(2)))
    assertEquals("single with multiple", ArraySeq(1 -> 2), ArraySeq(1).zip(ArraySeq(2, 3, 4)))
    assertEquals("multiple with empty", ArraySeq.empty, ArraySeq(1, 2, 3).zip(ArraySeq.empty))
    assertEquals("multiple with single", ArraySeq(1 -> 2), ArraySeq(1, 2, 3).zip(ArraySeq(2)))
    assertEquals("multiple with multiple", ArraySeq(1 -> 2, 2 -> 3, 3 -> 4), ArraySeq(1, 2, 3).zip(ArraySeq(2, 3, 4)))
    assertEquals("multiple reversed with empty", ArraySeq.empty, ArraySeq(1, 2, 3).reverse.zip(ArraySeq.empty))
    assertEquals("multiple reversed with single", ArraySeq(3 -> 2), ArraySeq(1, 2, 3).reverse.zip(ArraySeq(2)))
    assertEquals("multiple reversed with multiple", ArraySeq(3 -> 2, 2 -> 3, 1 -> 4), ArraySeq(1, 2, 3).reverse.zip(ArraySeq(2, 3, 4)))
  }

  @Test
  def testUnzip(): Unit = {
    assertEquals("empty", ArraySeq.empty -> ArraySeq.empty, ArraySeq.empty.unzip)
    assertEquals("single", ArraySeq(1) -> ArraySeq(2), ArraySeq(1 -> 2).unzip)
    assertEquals("multiple", ArraySeq(1, 2, 3) -> ArraySeq(2, 3, 4), ArraySeq(1 -> 2, 2 -> 3, 3 -> 4).unzip)
    assertEquals("multiple reversed", ArraySeq(3, 2, 1) -> ArraySeq(4, 3, 2), ArraySeq(1 -> 2, 2 -> 3, 3 -> 4).reverse.unzip)
  }

  @Test
  def testAutoTrimmingAlways(): Unit = {
    assertEquals("empty", List.empty, ArraySeq.empty.withAutoTrimming(AutoTrimming.Always).array.to(List))
    assertEquals("single", List(1), ArraySeq(1).withAutoTrimming(AutoTrimming.Always).array.to(List))
    assertEquals("multiple", List(1, 2, 3), ArraySeq(1, 2, 3).withAutoTrimming(AutoTrimming.Always).array.to(List))
    assertEquals("multiple over edge", List.range(1, 11), (ArraySeq(1, 2, 3, 4, 5, 6, 7).withAutoTrimming(AutoTrimming.Always) :+ 8 :+ 9 :+ 10).array.to(List))
  }
}

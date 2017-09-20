package strawman
package collection
package immutable

import org.junit.Assert._
import org.junit.Test

import scala.{Any, Exception, Int, NoSuchElementException, Nothing, Unit}
import scala.Predef.{identity, intWrapper}
import scala.runtime.RichInt
import scala.util.Try
import scala.util.control.NonFatal

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
    assertEquals("apply with 00 arguments has correct elements array", List(), ArraySeq().primary.elements.to(List))
    assertEquals("apply with 01 argument  has correct elements array", List(1, O, O, O, O, O, O, O), ArraySeq(1).primary.elements.to(List))
    assertEquals("apply with 02 arguments has correct elements array", List(1, 2, O, O, O, O, O, O), ArraySeq(1, 2).primary.elements.to(List))
    assertEquals("apply with 03 arguments has correct elements array", List(1, 2, 3, O, O, O, O, O), ArraySeq(1, 2, 3).primary.elements.to(List))
    assertEquals("apply with 04 arguments has correct elements array", List(1, 2, 3, 4, O, O, O, O), ArraySeq(1, 2, 3, 4).primary.elements.to(List))
    assertEquals("apply with 05 arguments has correct elements array", List(1, 2, 3, 4, 5, O, O, O), ArraySeq(1, 2, 3, 4, 5).primary.elements.to(List))
    assertEquals("apply with 06 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, O, O), ArraySeq(1, 2, 3, 4, 5, 6).primary.elements.to(List))
    assertEquals("apply with 07 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, O), ArraySeq(1, 2, 3, 4, 5, 6, 7).primary.elements.to(List))
    assertEquals("apply with 08 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8).primary.elements.to(List))
    assertEquals("apply with 09 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9).primary.elements.to(List))
    assertEquals("apply with 10 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, O, O, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).primary.elements.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, O, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).primary.elements.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, O, O, O, O), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).primary.elements.to(List))
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
    assertEquals("no unnecessary expansion", List(3, 4, 5, 6, 7, 0, 1, 2), (0 +: a).primary.elements.to(List))
    val b = 1 +: 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 1", List(3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O, 0, 1, 2), (0 +: b).primary.elements.to(List))
    val c = 1 +: 2 +: ArraySeq(3, 4, 5, 6, 7, 8)
    assertEquals("expands correctly 2", List(3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O, 0, 1, 2), (0 +: c).primary.elements.to(List))
    val d = 1 +: 2 +: ArraySeq(3, 4, 5, 6, 7, 8, 9, 10)
    assertEquals("expands correctly 3", List(3, 4, 5, 6, 7, 8, 9, 10, O, O, O, O, O, 0, 1, 2), (0 +: d).primary.elements.to(List))
    val e = 0 +: 1 +: 2 +: ArraySeq(3, 4, 5, 6, 7, 8, 9)
    assertEquals("expands correctly 4", List(3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, -1, 0, 1, 2), (-1 +: e).primary.elements.to(List))
    val f = 0 +: 1 +: 2 +: ArraySeq(3)
    assertEquals("expands correctly 5", List(3, O, O, O, -1, 0, 1, 2), (-1 +: f).primary.elements.to(List))
    val g = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.dropRight(7)
    assertEquals("expands correctly 6", List(1, O, O, O, O, O, O, 0), (0 +: h).primary.elements.to(List))
    val i = g.dropRight(8)
    assertEquals("expands correctly 7", List(O, O, O, O, O, O, O, 0), (0 +: i).primary.elements.to(List))
  }

  @Test
  def testExpandEnd(): Unit = {
    val O = null
    val a = 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("no unnecessary expansion", List(3, 4, 5, 6, 7, 8, 9, 2), (a :+ 9).primary.elements.to(List))
    val b = 1 +: 2 +: ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 1", List(3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, O, O, 1, 2), (b :+ 9).primary.elements.to(List))
    val c = ArraySeq(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 2", List(3, 4, 5, 6, 7 ,8, 9, O), (c :+ 9).primary.elements.to(List))
    val d = ArraySeq(0, 1, 2, 3, 4) :+ 5 :+ 6 :+ 7
    assertEquals("expands correctly 3", List(0, 1, 2, 3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O), (d :+ 8).primary.elements.to(List))
    val g = ArraySeq(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.drop(7)
    assertEquals("expands correctly 4", List(8, 9, O, O, O, O, O, O), (h :+ 9).primary.elements.to(List))
    val i = g.drop(8)
    assertEquals("expands correctly 5", List(9, O, O, O, O, O, O, O), (i :+ 9).primary.elements.to(List))
  }

  @Test
  def testSingleElement(): Unit = {
    assertEquals("apply with single argument has length 1", 1, ArraySeq(1).length)
    val it = ArraySeq(1).iterator()
    assertTrue("apply with single argument has single element iterator with correct element", it.hasNext && it.next() == 1)
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
    assertEquals("prepend single", ArraySeq(1, 2, 3, 4), 1 +: ArraySeq(2, 3, 4))
    assertEquals("prepend multiple", ArraySeq(0, 1, 2, 3, 4), 0 +: 1 +: ArraySeq(2, 3, 4))
    assertEquals(
      "prepend multiple over beginning of array",
      ArraySeq(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: ArraySeq(1, 2, 3, 4, 5, 6))
  }

  @Test
  def testAppend(): Unit = {
    assertEquals("append on empty", ArraySeq(1), ArraySeq.empty :+ 1)
    assertEquals("append single", ArraySeq(1, 2, 3, 4), ArraySeq(1, 2, 3) :+ 4)
    assertEquals("append multiple", ArraySeq(1, 2, 3, 4, 5), ArraySeq(1, 2, 3) :+ 4 :+ 5)
    assertEquals(
      "append multiple over ending of array",
      ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      ArraySeq(1, 2, 3, 4, 5, 6) :+ 7 :+ 8 :+ 9 :+ 10)
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
    assertTrue("prepend eq primaries", b.primary eq d.primary)
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
      case NonFatal(_) => fail("wrong exception")
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
      case NonFatal(_) => fail("wrong exception")
    }
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
    assertEquals("single index where exists", 0, ArraySeq(1).indexWhere(x => x == 1))
    assertEquals("single index where not exists", -1, ArraySeq(1).indexWhere(x => x == 2))
    assertEquals("multiple index where exists", 1, ArraySeq(1, 2, 3, 4).indexWhere(x => x % 2 == 0))
    assertEquals("multiple index where not exists", -1, ArraySeq(1, 2, 3, 4).indexWhere(x => x == 5))
    assertEquals("multiple index where exists with from", 3, ArraySeq(1, 2, 3, 4).indexWhere(x => x % 2 == 0, 2))
    assertEquals("multiple index where not exists with from", -1, ArraySeq(1, 2, 3, 4).indexWhere(x => x == 5, 2))
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
  def testFromIterable(): Unit = {
    assertEquals("empty list is empty", ArraySeq.empty, ArraySeq.fromIterable(List.empty))
    assertEquals("empty scala list is empty", ArraySeq.empty, ArraySeq.fromIterable(Nil))
    assertEquals("empty lazy list is empty", ArraySeq.empty, ArraySeq.fromIterable[Int](LazyList.empty))
  }

  @Test
  def testTrim(): Unit = {
    assertEquals("empty trimmed is empty", ArraySeq.empty, ArraySeq.fromIterable(List.empty).trim())
    assertEquals("single trimmed has elements array of correct length", 1, ArraySeq(1).trim().primary.elements.length)
    assertEquals("single trimmed has elements array with correct element", 1, ArraySeq(1).trim().primary.elements(0))
    assertEquals("multiple trimmed has elements array of correct length", 3, ArraySeq(1, 2, 3).trim().primary.elements.length)
    assertEquals("multiple trimmed has elements array with correct element", 1, ArraySeq(1, 2, 3).trim().primary.elements(0))
    assertEquals("multiple trimmed has elements array with correct element", 2, ArraySeq(1, 2, 3).trim().primary.elements(1))
    assertEquals("multiple trimmed has elements array with correct element", 3, ArraySeq(1, 2, 3).trim().primary.elements(2))
  }

  @Test
  def testPrependEqElement(): Unit = {
    val a = ArraySeq(1, 2, 3)
    val b = 1 +: a.tail
    val c = 0 +: a.tail
    val d = -1 +: a.tail
    val e = -1 +: d.tail
    val f = -1 +: a.tail
    assertSame("prepend equal single on tail equals original", a.primary.elements, b.primary.elements)
    assertNotSame("prepend non equal single on tail does not equal original", a.primary.elements, c.primary.elements)
    assertSame("prepend equal single on tail equals original", d.primary.elements, e.primary.elements)
    assertNotSame("prepend non equal single on tail does not equal original", d.primary.elements, f.primary.elements)
  }

  @Test
  def testAppendEqElement(): Unit = {
    val a = ArraySeq(1, 2, 3)
    val b = a.take(2) :+ 3
    val c = a.take(2) :+ 4
    val d = a.take(2) :+ 5
    val e = d.take(2) :+ 5
    val f = a.take(2) :+ 5
    assertSame("append equal single on init equals original", a.primary.elements, b.primary.elements)
    assertNotSame("append non equal single on init does not equal original", a.primary.elements, c.primary.elements)
    assertSame("append equal single on init equals original", d.primary.elements, e.primary.elements)
    assertNotSame("append non equal single on init does not equal original", d.primary.elements, f.primary.elements)
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
    assertEquals("Empty with empty at front", ArraySeq.empty, ArraySeq.empty.patch(0, ArraySeq.empty))
    assertEquals("Empty reversed with empty at front", ArraySeq.empty, ArraySeq.empty.reverse.patch(0, ArraySeq.empty))
    assertEquals("Empty with empty reversed at front", ArraySeq.empty, ArraySeq.empty.patch(0, ArraySeq.empty.reverse))
    assertEquals("Empty reversed with empty reversed at front", ArraySeq.empty, ArraySeq.empty.reverse.patch(0, ArraySeq.empty.reverse))
    assertEquals("Empty with single at front", ArraySeq(1), ArraySeq.empty.patch(0, ArraySeq(1)))
    assertEquals("Empty reversed with single at front", ArraySeq(1), ArraySeq.empty.reverse.patch(0, ArraySeq(1)))
    assertEquals("Empty with single reversed at front", ArraySeq(1), ArraySeq.empty.patch(0, ArraySeq(1).reverse))
    assertEquals("Empty reversed with single reversed at front", ArraySeq(1), ArraySeq.empty.reverse.patch(0, ArraySeq(1).reverse))
    assertEquals("Single with empty at front replacing 0", ArraySeq(1), ArraySeq(1).patch(0, ArraySeq.empty, 0))
    assertEquals("Single with empty at front replacing 0", ArraySeq(1), ArraySeq(1).patch(0, ArraySeq.empty, 0))
    assertEquals("Single with empty at front replacing 1", ArraySeq.empty, ArraySeq(1).patch(0, ArraySeq.empty, 1))
    assertEquals("Single with single at front replacing 0", ArraySeq(1, 2), ArraySeq(2).patch(0, ArraySeq(1), 0))
    assertEquals("Single reversed with single at front replacing 0", ArraySeq(1, 2), ArraySeq(2).reverse.patch(0, ArraySeq(1)))
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
    assertEquals("Empty with empty at rear", ArraySeq.empty, ArraySeq.empty.patch(0, ArraySeq.empty))
    assertEquals("Empty reversed with empty at rear", ArraySeq.empty, ArraySeq.empty.reverse.patch(0, ArraySeq.empty))
    assertEquals("Empty with single at rear", ArraySeq(1), ArraySeq.empty.patch(0, ArraySeq(1)))
    assertEquals("Empty reversed with single at rear", ArraySeq(1), ArraySeq.empty.reverse.patch(0, ArraySeq(1)))
    assertEquals("Single with empty at rear", ArraySeq(1), ArraySeq(1).patch(1, ArraySeq.empty))
    assertEquals("Single reversed with empty at rear", ArraySeq(1), ArraySeq(1).reverse.patch(1, ArraySeq.empty))
    assertEquals("Single with single at rear replacing 0", ArraySeq(2, 1), ArraySeq(2).patch(1, ArraySeq(1)))
    assertEquals("Single reversed with single at rear replacing 0", ArraySeq(2, 1), ArraySeq(2).reverse.patch(1, ArraySeq(1)))
    assertEquals("Single with multiple at rear", ArraySeq(4, 1, 2, 3), ArraySeq(4).patch(1, ArraySeq(1, 2, 3)))
    assertEquals("Single reversed with multiple at rear", ArraySeq(4, 1, 2, 3), ArraySeq(4).reverse.patch(1, ArraySeq(1, 2, 3)))
    assertEquals("Multiple with empty at rear", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).patch(3, ArraySeq.empty))
    assertEquals("Multiple reversed with empty at rear", ArraySeq(3, 2, 1), ArraySeq(1, 2, 3).reverse.patch(3, ArraySeq.empty))
    assertEquals("Multiple with single at rear", ArraySeq(1, 2, 3, 4), ArraySeq(1, 2, 3).patch(3, ArraySeq(4)))
    assertEquals("Multiple reversed with single at rear", ArraySeq(3, 2, 1, 4), ArraySeq(1, 2, 3).reverse.patch(3, ArraySeq(4)))
    assertEquals("Multiple with multiple at rear", ArraySeq(1, 2, 3, 4, 5, 6), ArraySeq(1, 2, 3).patch(3, ArraySeq(4, 5, 6)))
    assertEquals("Multiple reversed with multiple at rear", ArraySeq(3, 2, 1, 4, 5, 6), ArraySeq(1, 2, 3).reverse.patch(3, ArraySeq(4, 5, 6)))
  }
  @Test
  def testPatchInMiddle(): Unit = {
    assertEquals("Multiple with empty in middle replacing 0", ArraySeq(1, 2, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq.empty))
    assertEquals("Multiple reversed with empty in middle replacing 0", ArraySeq(3, 2, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq.empty))
    assertEquals("Multiple with empty in middle replacing 1", ArraySeq(1, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq.empty, 1))
    assertEquals("Multiple reversed with empty in middle replacing 1", ArraySeq(3, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq.empty, 1))
    assertEquals("Multiple with empty in middle replacing max", ArraySeq(1), ArraySeq(1, 2, 3).patch(1, ArraySeq.empty, 245))
    assertEquals("Multiple reversed with empty in middle replacing max", ArraySeq(3), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq.empty, 245))
    assertEquals("Multiple with single in middle replacing 0", ArraySeq(1, 4, 2, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4)))
    assertEquals("Multiple reversed with single in middle replacing 0", ArraySeq(3, 4, 2, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4)))
    assertEquals("Multiple with single in middle replacing 1", ArraySeq(1, 4, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4), 1))
    assertEquals("Multiple reversed with single in middle replacing 1", ArraySeq(3, 4, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4), 1))
    assertEquals("Multiple with single in middle replacing max", ArraySeq(1, 4), ArraySeq(1, 2, 3).patch(1, ArraySeq(4), 245))
    assertEquals("Multiple reversed with single in middle replacing max", ArraySeq(3, 4), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4), 245))
    assertEquals("Multiple with multiple in middle replacing 0", ArraySeq(1, 4, 5, 6, 2, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4, 5, 6)))
    assertEquals("Multiple reversed with multiple in middle replacing 0", ArraySeq(3, 4, 5, 6, 2, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4, 5, 6)))
    assertEquals("Multiple with multiple in middle replacing 1", ArraySeq(1, 4, 5, 6, 3), ArraySeq(1, 2, 3).patch(1, ArraySeq(4, 5, 6), 1))
    assertEquals("Multiple reversed with multiple in middle replacing 1", ArraySeq(3, 4, 5, 6, 1), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4, 5, 6), 1))
    assertEquals("Multiple with multiple in middle replacing max", ArraySeq(1, 4, 5, 6), ArraySeq(1, 2, 3).patch(1, ArraySeq(4, 5, 6), 245))
    assertEquals("Multiple reversed with multiple in middle replacing max", ArraySeq(3, 4, 5, 6), ArraySeq(1, 2, 3).reverse.patch(1, ArraySeq(4, 5, 6), 245))
  }
  @Test
  def testAutoTrimming(): Unit = {
    // Default shrinkThreshold is 25%
    assertEquals("1 out of 8 is not shrunk", 8, ArraySeq(1, 2, 3, 4, 5, 6).slice(0, 1).primary.elements.length)
    assertEquals("2 out of 8 is not shrunk", 8, ArraySeq(1, 2, 3, 4, 5, 6).slice(0, 2).primary.elements.length)
    assertEquals("3 out of 8 is not shrunk", 8, ArraySeq(1, 2, 3, 4, 5, 6).slice(0, 3).primary.elements.length)
    val s32 = ArraySeq.tabulate(32)(identity)
    assertEquals("2 out of 32 is shrunk to 8", 8, s32.slice(0, 2).primary.elements.length)
    assertEquals("8 out of 32 is shrunk to 8", 8, s32.slice(0, 8).primary.elements.length)
    assertEquals("9 out of 32 is not shrunk", 32, s32.slice(0, 9).primary.elements.length)
    var s = ArraySeq.empty[Int]
    for (i <- 0 until 512) {
      if (i % 2 == 0) s = s :+ i
      else s = i +: s
    }
    assertEquals("512 is correct capacity", 512, s.primary.elements.length)
    for (i <- 0 until 31) {
      s = s.tail
    }
    assertEquals("after removing 31 out of 512 capacity is still 512", 512, s.primary.elements.length)
    s = s.tail
    assertEquals("after removing 32 out of 512 capacity is still 512", 512, s.primary.elements.length)
    s = s.tail
    assertEquals("after removing 33 out of 512 capacity is still 512", 512, s.primary.elements.length)
    for (i <- 33 until 255) {
      s = s.tail
    }
    assertEquals("after removing 255 out of 512 capacity is still 512", 512, s.primary.elements.length)
    s = s.tail
    assertEquals("after removing 256 out of 512 capacity is still 512", 512, s.primary.elements.length)
    s = s.tail
    assertEquals("after removing 257 out of 512 capacity is still 512", 512, s.primary.elements.length)
    for (i <- 257 until 383) {
      s = s.tail
    }
    assertEquals("after removing 383 out of 512 capacity is still 512", 512, s.primary.elements.length)
    s = s.tail
    assertEquals("after removing 384 out of 512 capacity is 128", 128, s.primary.elements.length)
    s = s.tail
    assertEquals("after removing 385 out of 512 capacity is 128", 128, s.primary.elements.length)
    s = s.slice(0, 2)
    assertEquals("after keeping 2 out of 128 capacity is 8", 8, s.primary.elements.length)
    val s2 = ArraySeq.range(0, 32).withoutAutoShrinking
    assertEquals("2 out of 32 is not shrunk", 32, s2.slice(0, 2).primary.elements.length)
    assertEquals("8 out of 32 is not shrunk", 32, s2.slice(0, 8).primary.elements.length)
    assertEquals("9 out of 32 is not shrunk", 32, s2.slice(0, 9).primary.elements.length)
    assertEquals("24 out of 32 is not shrunk", 32, s2.slice(0, 24).primary.elements.length)
    val s3 = s2.withAutoShrinking(75)
    assertEquals("2 out of 32 is shrunk to 8", 8, s3.slice(0, 2).primary.elements.length)
    assertEquals("8 out of 32 is shrunk to 8", 8, s3.slice(0, 8).primary.elements.length)
    assertEquals("9 out of 32 is shrunk to 16", 16, s3.slice(0, 9).primary.elements.length)
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
    val a = ArraySeq(1, 2, 3)
    assertEquals("a", ArraySeq(1, 2, 3), a)
    assertEquals("a.primary", List(1, 2, 3, O, O, O, O, O), a.primary.elements.to(List))
    a.primary.reset()
    assertEquals("a after resetting", ArraySeq(1, 2, 3), a)
    assertEquals("a.primary after resetting", List(1, 2, 3, O, O, O, O, O), a.primary.elements.to(List))
    val b = a :+ 4
    assertEquals("a after appending", ArraySeq(1, 2, 3), a)
    assertEquals("b after appending", ArraySeq(1, 2, 3, 4), b)
    assertEquals("a.primary after appending", List(1, 2, 3, 4, O, O, O, O), a.primary.elements.to(List))
    a.primary.reset()
    assertEquals("a after appending and resetting", ArraySeq(1, 2, 3), a)
    assertEquals("b after appending and resetting", ArraySeq(1, 2, 3, 4), b)
    assertEquals("a.primary after appending and resetting", List(1, 2, 3, 4, O, O, O, O), a.primary.elements.to(List))
    val c = a :+ 5
    assertEquals("a after appending and resetting and appending", ArraySeq(1, 2, 3), a)
    assertEquals("b after appending and resetting and appending", ArraySeq(1, 2, 3, 5), b)
    assertEquals("c after appending and resetting and appending", ArraySeq(1, 2, 3, 5), c)
    assertEquals("a.primary after appending and resetting and appending", List(1, 2, 3, 5, O, O, O, O), a.primary.elements.to(List))
  }
}

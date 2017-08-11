package strawman
package collection
package immutable
package test

import org.junit.Assert._
import org.junit.Test

import scala.{Any, Exception, Int, NoSuchElementException, Nothing, Unit}
import scala.Predef.{identity, intWrapper}
import scala.runtime.RichInt
import scala.util.Try
import scala.util.control.NonFatal

class SpandexTest {
  @Test
  def testEmpty(): Unit = {
    assertEquals("apply with no arguments is equal to empty", Spandex.empty, Spandex())
    assertEquals("apply with no arguments has size 0", 0, Spandex().size)
    assertEquals("apply with no arguments has empty iterator", false, Spandex.empty.iterator().hasNext)
  }

  @Test
  def testApply(): Unit = {
    val O = null
    assertEquals("apply with 00 arguments has correct elements array", List(), Spandex().primary.elements.to(List))
    assertEquals("apply with 01 argument  has correct elements array", List(1, O, O, O, O, O, O, O), Spandex(1).primary.elements.to(List))
    assertEquals("apply with 02 arguments has correct elements array", List(1, 2, O, O, O, O, O, O), Spandex(1, 2).primary.elements.to(List))
    assertEquals("apply with 03 arguments has correct elements array", List(1, 2, 3, O, O, O, O, O), Spandex(1, 2, 3).primary.elements.to(List))
    assertEquals("apply with 04 arguments has correct elements array", List(1, 2, 3, 4, O, O, O, O), Spandex(1, 2, 3, 4).primary.elements.to(List))
    assertEquals("apply with 05 arguments has correct elements array", List(1, 2, 3, 4, 5, O, O, O), Spandex(1, 2, 3, 4, 5).primary.elements.to(List))
    assertEquals("apply with 06 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, O, O), Spandex(1, 2, 3, 4, 5, 6).primary.elements.to(List))
    assertEquals("apply with 07 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, O), Spandex(1, 2, 3, 4, 5, 6, 7).primary.elements.to(List))
    assertEquals("apply with 08 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8), Spandex(1, 2, 3, 4, 5, 6, 7, 8).primary.elements.to(List))
    assertEquals("apply with 09 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, O, O), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9).primary.elements.to(List))
    assertEquals("apply with 10 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, O, O, O, O, O, O), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).primary.elements.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, O, O, O, O, O), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).primary.elements.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, O, O, O, O), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).primary.elements.to(List))
  }

  @Test
  def testExpandFront(): Unit = {
    val O = null
    val a = 1 +: 2 +: Spandex(3, 4, 5, 6) :+ 7
    assertEquals("no unnecessary expansion", List(3, 4, 5, 6, 7, 0, 1, 2), (0 +: a).primary.elements.to(List))
    val b = 1 +: 2 +: Spandex(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 1", List(3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O, 0, 1, 2), (0 +: b).primary.elements.to(List))
    val c = 1 +: 2 +: Spandex(3, 4, 5, 6, 7, 8)
    assertEquals("expands correctly 2", List(3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O, 0, 1, 2), (0 +: c).primary.elements.to(List))
    val d = 1 +: 2 +: Spandex(3, 4, 5, 6, 7, 8, 9, 10)
    assertEquals("expands correctly 3", List(3, 4, 5, 6, 7, 8, 9, 10, O, O, O, O, O, 0, 1, 2), (0 +: d).primary.elements.to(List))
    val e = 0 +: 1 +: 2 +: Spandex(3, 4, 5, 6, 7, 8, 9)
    assertEquals("expands correctly 4", List(3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, -1, 0, 1, 2), (-1 +: e).primary.elements.to(List))
    val f = 0 +: 1 +: 2 +: Spandex(3)
    assertEquals("expands correctly 5", List(3, O, O, O, -1, 0, 1, 2), (-1 +: f).primary.elements.to(List))
    val g = Spandex(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.dropRight(7)
    assertEquals("expands correctly 6", List(1, O, O, O, O, O, O, 0), (0 +: h).primary.elements.to(List))
    val i = g.dropRight(8)
    assertEquals("expands correctly 7", List(0, O, O, O, O, O, O, O), (0 +: i).primary.elements.to(List))
  }

  @Test
  def testExpandEnd(): Unit = {
    val O = null
    val a = 2 +: Spandex(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("no unnecessary expansion", List(3, 4, 5, 6, 7, 8, 9, 2), (a :+ 9).primary.elements.to(List))
    val b = 1 +: 2 +: Spandex(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 1", List(3, 4, 5, 6, 7, 8, 9, O, O, O, O, O, O, O, 1, 2), (b :+ 9).primary.elements.to(List))
    val c = Spandex(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("expands correctly 2", List(3, 4, 5, 6, 7 ,8, 9, O), (c :+ 9).primary.elements.to(List))
    val d = Spandex(0, 1, 2, 3, 4) :+ 5 :+ 6 :+ 7
    assertEquals("expands correctly 3", List(0, 1, 2, 3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O), (d :+ 8).primary.elements.to(List))
    val g = Spandex(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.drop(7)
    assertEquals("expands correctly 4", List(8, 9, O, O, O, O, O, O), (h :+ 9).primary.elements.to(List))
    val i = g.drop(8)
    assertEquals("expands correctly 5", List(9, O, O, O, O, O, O, O), (i :+ 9).primary.elements.to(List))
  }

  @Test
  def testSingleElement(): Unit = {
    assertEquals("apply with single argument has length 1", 1, Spandex(1).length)
    val it = Spandex(1).iterator()
    assertTrue("apply with single argument has single element iterator with correct element", it.hasNext && it.next() == 1)
    assertFalse("apply with single argument has only single element iterator", it.hasNext)
    val rit = Spandex(1).reverse.iterator()
    assertTrue("apply with reversed single argument has single element iterator with correct element",
      rit.hasNext && rit.next() == 1)
    assertFalse("apply with reversed single argument has only single element iterator", rit.hasNext)
    assertEquals("apply with single argument has correct element at index 0", 1, Spandex(1)(0))
    assertEquals("apply with reversed single argument has correct element at index 0", 1, Spandex(1).reverse.apply(0))
    assertEquals("apply with single argument has correct element as head", 1, Spandex(1).head)
    assertEquals("apply with reversed single argument has correct element as head", 1, Spandex(1).reverse.head)
    assertEquals("apply with single argument has empty tail", Spandex.empty, Spandex(1).tail)
    assertEquals("apply with reversed single argument has empty tail", Spandex.empty, Spandex(1).reverse.tail)
  }

  @Test
  def testReverse(): Unit = {
    assertEquals("reverse empty is empty", Spandex.empty, Spandex.empty.reverse)
    assertEquals("reverse effectively empty is empty", Spandex.empty, Spandex(1, 2, 3).filter(_ == 4).reverse)
    assertEquals("reverse single is same", Spandex(1), Spandex(1).reverse)
    assertEquals("reverse effectively single is same", Spandex(1), Spandex(1, 2).take(1).reverse)
    assertEquals("reverse double is correct", Spandex(2, 1), Spandex(1, 2).reverse)
    assertEquals("reverse effectively double is correct", Spandex(2, 1), Spandex(1, 2, 3).take(2).reverse)
    assertEquals("reverse large is correct", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), Spandex(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).reverse)
    val reverse = Spandex(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).reverse
    val reverse2 = reverse.reverse
    assertEquals("reverse reverse large is correct", Spandex(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), reverse2)
  }

  @Test
  def testHeadsAndTails(): Unit = {
    assertTrue("empty tail fails", Try(Spandex.empty.tail).isFailure)
    assertTrue("empty reverse tail fails", Try(Spandex.empty.reverse.tail).isFailure)
    assertEquals("tail", Spandex(2, 3, 4), Spandex(1, 2, 3, 4).tail)
    val r = Spandex(1, 2, 3, 4).reverse
    assertEquals("reverse tail", Spandex(3, 2, 1), r.tail)
    assertEquals("tail tail", Spandex(3, 4), Spandex(1, 2, 3, 4).tail.tail)
    assertEquals("tail reverse tail", Spandex(3, 2), Spandex(1, 2, 3, 4).tail.reverse.tail)
    assertEquals("tail tail tail", Spandex(4), Spandex(1, 2, 3, 4).tail.tail.tail)
    assertEquals("tail tail reverse tail", Spandex(3), Spandex(1, 2, 3, 4).tail.tail.reverse.tail)
    assertEquals("tail tail tail tail", Spandex.empty, Spandex(1, 2, 3, 4).tail.tail.tail.tail)
    assertEquals("tail tail tail reverse tail", Spandex.empty, Spandex(1, 2, 3, 4).tail.tail.tail.reverse.tail)
    assertTrue("empty head fails", Try(Spandex.empty.head).isFailure)
    assertEquals("head", 1, Spandex(1, 2, 3, 4).head)
    assertEquals("reverse head", 4, Spandex(1, 2, 3, 4).reverse.head)
    assertEquals("reverse tail head", 3, Spandex(1, 2, 3, 4).reverse.tail.head)
    assertEquals("tail tail head", 3, Spandex(1, 2, 3, 4).tail.tail.head)
    assertEquals("reverse tail tail reverse head", 1, Spandex(1, 2, 3, 4).reverse.tail.tail.reverse.head)
    assertEquals("tail tail tail head", 4, Spandex(1, 2, 3, 4).tail.tail.tail.head)
    assertEquals("tail reverse tail reverse tail reverse head", 3, Spandex(1, 2, 3, 4).tail.reverse.tail.reverse.tail.reverse.head)
  }

  @Test
  def testTakeAndDrop(): Unit = {
    assertEquals("take 0", Spandex.empty, Spandex(1, 2, 3, 4).take(0))
    assertEquals("reverse take 0", Spandex.empty, Spandex(1, 2, 3, 4).reverse.take(0))
    assertEquals("take 1", Spandex(1), Spandex(1, 2, 3, 4).take(1))
    assertEquals("reverse take 1", Spandex(4), Spandex(1, 2, 3, 4).reverse.take(1))
    assertEquals("take 2", Spandex(1, 2), Spandex(1, 2, 3, 4).take(2))
    assertEquals("reverse take 2", Spandex(4, 3), Spandex(1, 2, 3, 4).reverse.take(2))
    assertEquals("drop 0", Spandex(1, 2, 3, 4), Spandex(1, 2, 3, 4).drop(0))
    assertEquals("reverse drop 0", Spandex(4, 3, 2, 1), Spandex(1, 2, 3, 4).reverse.drop(0))
    assertEquals("drop 1", Spandex(2, 3, 4), Spandex(1, 2, 3, 4).drop(1))
    assertEquals("reverse drop 1", Spandex(3, 2, 1), Spandex(1, 2, 3, 4).reverse.drop(1))
    assertEquals("drop 2", Spandex(3, 4), Spandex(1, 2, 3, 4).drop(2))
    assertEquals("reverse drop 2", Spandex(2, 1), Spandex(1, 2, 3, 4).reverse.drop(2))
    assertEquals("drop 1 take 1", Spandex(2), Spandex(1, 2, 3, 4).drop(1).take(1))
    assertEquals("reverse drop 1 take 1", Spandex(3), Spandex(1, 2, 3, 4).reverse.drop(1).take(1))
    assertEquals("drop 2 take 1", Spandex(3), Spandex(1, 2, 3, 4).drop(2).take(1))
    assertEquals("reverse drop 2 take 1", Spandex(2), Spandex(1, 2, 3, 4).reverse.drop(2).take(1))
    assertEquals("drop 2 take 2", Spandex(3, 4), Spandex(1, 2, 3, 4).drop(2).take(2))
    assertEquals("reverse drop 2 take 2", Spandex(2, 1), Spandex(1, 2, 3, 4).reverse.drop(2).take(2))
    assertEquals("drop 2 take 0", Spandex.empty, Spandex(1, 2, 3, 4).drop(2).take(0))
    assertEquals("reverse drop 2 take 0", Spandex.empty, Spandex(1, 2, 3, 4).reverse.drop(2).take(0))
    assertEquals("drop 2 take 2 drop 1 take 1", Spandex(4), Spandex(1, 2, 3, 4).drop(2).take(2).drop(1).take(1))
    assertEquals("reverse drop 2 take 2 drop 1 take 1", Spandex(1), Spandex(1, 2, 3, 4).reverse.drop(2).take(2).drop(1).take(1))
  }

  @Test
  def testFilter(): Unit = {
    assertEquals("filter empty", Spandex.empty, Spandex().filter(_ != null))
    assertEquals("filter single inclusive", Spandex(2), Spandex(2).filter(_ > 1))
    assertEquals("filter single exclusive", Spandex(), Spandex(1).filter(_ > 1))
    assertEquals("filter multiple even", Spandex(2, 4), Spandex(1, 2, 3, 4).filter(_ % 2 == 0))
    assertEquals("reverse filter multiple even", Spandex(4, 2), Spandex(1, 2, 3, 4).reverse.filter(_ % 2 == 0))
    assertEquals("filter multiple odd", Spandex(1, 3), Spandex(1, 2, 3, 4).filter(_ % 2 == 1))
    assertEquals("reverse filter multiple odd", Spandex(3, 1), Spandex(1, 2, 3, 4).reverse.filter(_ % 2 == 1))
  }

  @Test
  def testMap(): Unit = {
    assertEquals("map empty", Spandex.empty, Spandex().map(identity))
    assertEquals("reverse map empty", Spandex.empty, Spandex().reverse.map(identity))
    assertEquals("map single", Spandex(4), Spandex(2).map(n => n * n))
    assertEquals("reverse map single", Spandex(4), Spandex(2).reverse.map(n => n * n))
    assertEquals("map multiple", Spandex(1, 4, 9, 16), Spandex(1, 2, 3, 4).map(n => n * n))
    assertEquals("reverse map multiple", Spandex(16, 9, 4, 1), Spandex(1, 2, 3, 4).reverse.map(n => n * n))
  }

  @Test
  def testFlatMap(): Unit = {
    assertEquals("flatmap empty", Spandex.empty, Spandex().flatMap((x: Any) => Spandex(x)))
    assertEquals("flatmap single", Spandex(2), Spandex(2).flatMap(n => Spandex(n)))
    assertEquals("flatmap multiple", Spandex(2, 4, 4, 16), Spandex(1, 2, 3, 4).flatMap {
      case n if n % 2 == 0 => Spandex(n, n * n)
      case _ => Spandex.empty
    })
    assertEquals("reverse flatmap multiple", Spandex(4, 16, 2, 4), Spandex(1, 2, 3, 4).reverse.flatMap {
      case n if n % 2 == 0 => Spandex(n, n * n)
      case _ => Spandex.empty
    })
  }

  @Test
  def testFill(): Unit = {
    assertEquals("fill empty", Spandex.empty, Spandex.fill(0)(9))
    assertEquals("fill single", Spandex(9), Spandex.fill(1)(9))
    assertEquals("fill multiple", Spandex(9, 9, 9), Spandex.fill(3)(9))
    assertEquals("fill multiple reverse", Spandex(9, 9, 9), Spandex.fill(3)(9).reverse)
  }

  @Test
  def testPrepend(): Unit = {
    assertEquals("prepend on empty", Spandex(1), 1 +: Spandex.empty)
    assertEquals("prepend single", Spandex(1, 2, 3, 4), 1 +: Spandex(2, 3, 4))
    assertEquals("prepend multiple", Spandex(0, 1, 2, 3, 4), 0 +: 1 +: Spandex(2, 3, 4))
    assertEquals(
      "prepend multiple over beginning of array",
      Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: Spandex(1, 2, 3, 4, 5, 6))
  }

  @Test
  def testAppend(): Unit = {
    assertEquals("append on empty", Spandex(1), Spandex.empty :+ 1)
    assertEquals("append single", Spandex(1, 2, 3, 4), Spandex(1, 2, 3) :+ 4)
    assertEquals("append multiple", Spandex(1, 2, 3, 4, 5), Spandex(1, 2, 3) :+ 4 :+ 5)
    assertEquals(
      "append multiple over ending of array",
      Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      Spandex(1, 2, 3, 4, 5, 6) :+ 7 :+ 8 :+ 9 :+ 10)
  }

  @Test
  def testAppendAll(): Unit = {
    assertEquals("on empty", Spandex(1, 2, 3), Spandex.empty :++ List(1, 2, 3))
    assertEquals("on reversed empty", Spandex(1, 2, 3), Spandex.empty.reverse :++ List(1, 2, 3))
    assertEquals("on single", Spandex(1, 2, 3, 4), Spandex(1) :++ List(2, 3, 4))
    assertEquals("on reversed single", Spandex(1, 2, 3, 4), Spandex(1).reverse :++ List(2, 3, 4))
    assertEquals("on multiple", Spandex(1, 2, 3, 4, 5, 6), Spandex(1, 2, 3) :++ List(4, 5, 6))
    assertEquals("on reversed multiple", Spandex(3, 2, 1, 4, 5, 6), Spandex(1, 2, 3).reverse :++ List(4, 5, 6))
    assertEquals("on multiple over rear", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), Spandex(1, 2, 3, 4, 5, 6, 7) :++ List(8, 9, 10, 11))
    assertEquals("on reversed multiple over rear", Spandex(7, 6, 5, 4, 3, 2, 1, 8, 9, 10, 11), Spandex(1, 2, 3, 4, 5, 6, 7).reverse :++ List(8, 9, 10, 11))
  }

  @Test
  def testPrependAll(): Unit = {
    assertEquals("on empty", Spandex(1, 2, 3), List(1, 2, 3) ++: Spandex.empty)
    assertEquals("on reversed empty", Spandex(1, 2, 3), List(1, 2, 3) ++: Spandex.empty.reverse)
    assertEquals("on single", Spandex(1, 2, 3, 4), List(1, 2, 3) ++: Spandex(4))
    assertEquals("on reversed single", Spandex(1, 2, 3, 4), List(1, 2, 3) ++: Spandex(4).reverse)
    assertEquals("on multiple", Spandex(1, 2, 3, 4, 5, 6), List(1, 2, 3) ++: Spandex(4, 5, 6))
    assertEquals("on reversed multiple", Spandex(1, 2, 3, 6, 5, 4), List(1, 2, 3) ++: Spandex(4, 5, 6).reverse)
    assertEquals("on multiple over front", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), List(1, 2, 3, 4) ++: Spandex(5, 6, 7, 8, 9, 10, 11))
    assertEquals("on reversed multiple over front", Spandex(1, 2, 3, 4, 11, 10, 9, 8, 7, 6, 5), List(1, 2, 3, 4) ++: Spandex(5, 6, 7, 8, 9, 10, 11).reverse)
    assertEquals("with list on empty", Spandex(1, 2, 3), List(1, 2, 3) ++: Spandex.empty)
    assertEquals("with iterator", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4).iterator() ++: Spandex(5, 6, 7, 8, 9))
    assertEquals("with iterator on reversed", Spandex(1, 2, 3, 4, 9, 8, 7, 6, 5), List(1, 2, 3, 4).iterator() ++: Spandex(5, 6, 7, 8, 9).reverse)
    assertEquals("with multiple lists", Spandex(-3, -2, -1, 0, 1, 2, 3, 4), List(-3, -2) ++: List(-1, 0) ++: 1 +: Spandex(2, 3, 4))
    assertEquals("with multiple lists reversed", Spandex(-3, -2, 4, 3, 2, 1, 0, -1), List(-3, -2) ++: (List(-1, 0) ++: 1 +: Spandex(2, 3, 4)).reverse)
  }

  @Test
  def testPrependAndAppendEqual(): Unit = {
    assertEquals("prepend equal on empty", Spandex(1), 1 +: (1 +: Spandex.empty).tail)
    assertEquals("append equal on empty", Spandex(1), (Spandex.empty :+ 1).take(0) :+ 1)
    assertEquals("prepend equal on single", Spandex(0, 1), 0 +: (0 +: Spandex(1)).tail)
    assertEquals("append equal on single", Spandex(1, 2), (Spandex(1) :+ 2).take(1) :+ 2)
    val a = Spandex(1, 2, 3)
    val b = 0 +: a
    val c = b.tail
    val d = 0 +: c
    val e = -1 +: b
    val f = -2 +: d
    assertEquals("prepend equal", Spandex(1, 2, 3), a)
    assertEquals("prepend equal", Spandex(0, 1, 2, 3), b)
    assertEquals("prepend equal", Spandex(1, 2, 3), c)
    assertEquals("prepend equal", Spandex(0, 1, 2, 3), d)
    assertTrue("prepend eq primaries", b.primary eq d.primary)
    assertEquals("prepend equal", Spandex(-1, 0, 1, 2, 3), e)
    assertEquals("prepend equal", Spandex(-2, 0, 1, 2, 3), f)
  }

  @Test
  def testConcat(): Unit = {
    val eight = Spandex(1, 2, 3, 4, 5, 6, 7, 8)
    assertEquals("concat eight with self", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8), eight ++ eight)
    assertEquals("concat empty with empty", Spandex.empty, Spandex.empty[Nothing] ++ Spandex.empty) // Explicit type annotation needed to avoid dotty bug.
    assertEquals("concat empty with single", Spandex(1), Spandex.empty ++ Spandex(1))
    assertEquals("concat single with empty", Spandex(1), Spandex(1) ++ Spandex.empty)
    assertEquals("concat empty with multiple", Spandex(1, 2, 3), Spandex.empty ++ Spandex(1, 2, 3))
    assertEquals("concat multiple with empty", Spandex(1, 2, 3), Spandex(1, 2, 3) ++ Spandex.empty)
    assertEquals("concat single with single", Spandex(1, 2), Spandex(1) ++ Spandex(2))
    assertEquals("concat single with multiple", Spandex(1, 2, 3, 4), Spandex(1) ++ (Spandex(2) :+ 3 :+ 4))
    assertEquals("concat multiple with single", Spandex(1, 2, 3, 4), (Spandex(1) :+ 2 :+ 3) ++ Spandex(4))
    assertEquals("concat multiple with mix", Spandex(-3, -2, -1, 0, 1, 2, 3, 4), Spandex(-3, -2) ++ Spandex(-1, 0) ++ (1 +: Spandex(2, 3, 4)))
    assertEquals("concat multiple with mix reversed", Spandex(-3, -2, 0, -1, 1, 4, 3, 2), Spandex(-3, -2) ++ Spandex(-1, 0).reverse ++ (1 +: Spandex(2, 3, 4).reverse))
    val full = 1 +: 2 +: Spandex(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("concat empty with full", Spandex(1, 2, 3, 4, 5, 6, 7, 8), Spandex.empty ++ full)
    assertEquals("concat full with empty", Spandex(1, 2, 3, 4, 5, 6, 7, 8), full ++ Spandex.empty)
    assertEquals("concat single with full", Spandex(0, 1, 2, 3, 4, 5, 6, 7, 8), Spandex(0) ++ full)
    assertEquals("concat full with single", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9), full ++ Spandex(9))
    assertEquals("concat full with full", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8), full ++ full)
    assertEquals("concat full with full reversed", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1), full ++ full.reverse)
    assertEquals("concat full reversed with full", Spandex(8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse ++ full)
    assertEquals("concat full with full mapped reversed", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 64, 49, 36, 25, 16, 9, 4, 1), full ++ full.reverse.map(x => x * x))
    assertEquals("concat full mapped reversed with full", Spandex(64, 49, 36, 25, 16, 9, 4, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse.map(x => x * x) ++ full)
    val large = -1 +: 0 +: full :+ 9 :+ 10 :+ 11
    assertEquals(
      "concat large with multiple",
      Spandex(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
      large ++ Spandex(12, 13, 14))
    assertEquals(
      "concat large with itself reversed",
      Spandex(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1),
      large ++ large.reverse)
  }

  @Test
  def testImmutablity(): Unit = {
    val a = Spandex()
    assertEquals("immutablity of empty", Spandex.empty, a)
    val b = 1 +: a
    assertEquals("immutablity of empty", Spandex(1), b)
    val c = a :+ 2
    assertEquals("immutablity of empty", Spandex(2), c)
    val d = Spandex(2, 3)
    assertEquals("immutablity of multiple", Spandex(2, 3), d)
    val e = 0 +: 1 +: d
    assertEquals("immutablity of multiple", Spandex(0, 1, 2, 3), e)
    val f = d :+ 4 :+ 5
    assertEquals("immutablity of multiple", Spandex(2, 3, 4, 5), f)
    val g = 0 +: 1 +: f
    assertEquals("immutablity of multiple", Spandex(0, 1, 2, 3, 4, 5), g)

    assertEquals("immutablity of empty still", Spandex.empty, a)
    assertEquals("immutablity of empty still", Spandex(1), b)
    assertEquals("immutablity of empty still", Spandex(2), c)
    assertEquals("immutablity of multiple still", Spandex(2, 3), d)
    assertEquals("immutablity of multiple still", Spandex(0, 1, 2, 3), e)
    assertEquals("immutablity of multiple still", Spandex(2, 3, 4, 5), f)
    assertEquals("immutablity of multiple still", Spandex(0, 1, 2, 3, 4, 5), g)
  }

  @Test
  def testLarge(): Unit = {
    val a = Spandex.tabulate(10)(identity)
    val ar = a.reverse
    assertEquals("large a", Spandex(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse", a, ar.reverse)
    val b = -2 +: -1 +: a
    val br = ar :+ -1 :+ -2
    assertEquals("large b", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse", b, br.reverse)
    val b2 = -4 +: -3 +: -2 +: -1 +: a
    val b2r = ar :+ -1 :+ -2 :+ -3 :+ -4
    assertEquals("large b2", Spandex(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse", b2, b2r.reverse)
    val c = b :+ 10
    val cr = 10 +: br
    assertEquals("large c", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed", Spandex(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse", c, cr.reverse)
    val d2 = b2 :+ 10 :+ 11 :+ 12
    val d2r = 12 +: 11 +: 10 +: b2r
    assertEquals("large d2", Spandex(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed", Spandex(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse", d2, d2r.reverse)
    val d = b :+ 10 :+ 11
    val dr = 11 +: 10 +: br
    assertEquals("large d", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed", Spandex(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse", d, dr.reverse)
    val e = c :+ 11
    val er = 11 +: cr
    assertEquals("large e", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed", Spandex(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse", e, er.reverse)
    val f = -3 +: c
    val fr = cr :+ -3
    assertEquals("large f", Spandex(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed", Spandex(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse", f, fr.reverse)
    val g = e.tail.tail.tail
    val gr = er.tail.tail.tail
    assertEquals("large g", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed", Spandex(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
    val h = (g.tail ++ g.tail.tail.tail).tail
    val h2 = (gr.tail.reverse ++ g.tail.reverse).tail.tail
    assertEquals("large h", Spandex(3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5, 6, 7, 8, 9, 10, 11), h)
    assertEquals("large h2", Spandex(0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2), h2)

    assertEquals("large a still", Spandex(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed still", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse still", a, ar.reverse)
    assertEquals("large b still", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed still", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse still", b, br.reverse)
    assertEquals("large b2 still", Spandex(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed still", Spandex(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse still", b2, b2r.reverse)
    assertEquals("large c still", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed still", Spandex(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse still", c, cr.reverse)
    assertEquals("large d2 still", Spandex(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed still", Spandex(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse still", d2, d2r.reverse)
    assertEquals("large d still", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed still", Spandex(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse still", d, dr.reverse)
    assertEquals("large e still", Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed still", Spandex(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse still", e, er.reverse)
    assertEquals("large f still", Spandex(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed still", Spandex(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse still", f, fr.reverse)
    assertEquals("large g still", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed still", Spandex(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
    assertEquals("large h still", Spandex(3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5, 6, 7, 8, 9, 10, 11), h)
    assertEquals("large h reversed still", Spandex(0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2), h2)
    assertEquals("large h equals its reversed reverse", h, h.reverse.reverse)
    assertEquals("large h2 equals its reversed reverse", h2, h2.reverse.reverse)
  }

  @Test
  def testIterator(): Unit = {
    var it = Spandex.empty[Int].iterator()
    assertFalse("empty has empty iterator", it.hasNext)

    it = Spandex.empty[Int].reverse.iterator()
    assertFalse("empty reverse has empty iterator", it.hasNext)

    it = Spandex(1).iterator()
    assertTrue("single has non empty iterator", it.hasNext)
    assertEquals("single iterator has correct element", 1, it.next())
    assertFalse("single iterator has only one element", it.hasNext)

    it = Spandex(1).reverse.iterator()
    assertTrue("single reverse has non empty iterator", it.hasNext)
    assertEquals("single reverse iterator has correct element", 1, it.next())
    assertFalse("single reverse iterator has only one element", it.hasNext)

    it = Spandex(1, 2).iterator()
    assertTrue("double has non empty iterator", it.hasNext)
    assertEquals("double iterator has correct first element", 1, it.next())
    assertEquals("double iterator has correct second element", 2, it.next())
    assertFalse("double iterator has only correct elements", it.hasNext)
    val reversed = Spandex(1, 2).reverse
    it = reversed.iterator()
    assertTrue("double reverse has non empty iterator", it.hasNext)
    assertEquals("double reverse iterator has correct first element", 2, it.next())
    assertEquals("double reverse iterator has correct second element", 1, it.next())
    assertFalse("double reverse iterator has only correct elements", it.hasNext)

    it = (0 +: 1 +: Spandex(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).iterator()
    assertTrue("large has non empty iterator", it.hasNext)
    assertTrue(
      "large iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    assertFalse("large iterator iterator has only correct elements", it.hasNext)

    it = (0 +: 1 +: Spandex(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).reverse.iterator()
    assertTrue("large reverse has non empty iterator", it.hasNext)
    assertTrue(
      "large reverse iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).reverse))
    assertFalse("large reverse iterator iterator has only correct elements", it.hasNext)

    /*
    val it2 = (Spandex_Z(1, 2, 3) :+ "kalle").iterator()
    assertTrue("any has non empty iterator", it2.hasNext)
    assertTrue(
      "any iterator has correct elements",
      it2.sameElements(List(1, 2, 3, "kalle")))
    */

    class A()
    class B() extends A()
    val a1 = new A()
    val a2 = new A()
    val b = new B()
    val it3= (Spandex(a1, a2) :+ b).iterator()
    assertTrue("subs has non empty iterator", it3.hasNext)
    assertTrue(
      "subs iterator has correct elements",
      it3.sameElements(List(a1, a2, b)))

    // Test iterator index bounds checking
    it = Spandex.empty[Int].iterator()
    try {
      it.next()
      fail("should be out of bounds")
    } catch {
      case _: NoSuchElementException => // expected
      case NonFatal(_) => fail("wrong exception")
    }

    it = Spandex(1, 2, 3, 4).dropRight(1).iterator()
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
    Spandex.empty[Int].foreach(_ => n += 1)
    assertEquals("empty foreach", 0, n)
    Spandex(1).foreach(x => n += x)
    assertEquals("single foreach", 1, n)
    Spandex(2, 3, 4, 5, 6).foreach(x => n += x)
    assertEquals("multiple foreach", 21, n)
    var m = 6
    Spandex(1, 2, 3).reverse.foreach(x => m -= x)
    assertEquals("multiple foreach reversed", 0, m)
  }

  @Test
  def testIndexWhere(): Unit = {
    assertEquals("empty index where", -1, Spandex.empty[Int].indexWhere(_ => true))
    assertEquals("single index where exists", 0, Spandex(1).indexWhere(x => x == 1))
    assertEquals("single index where not exists", -1, Spandex(1).indexWhere(x => x == 2))
    assertEquals("multiple index where exists", 1, Spandex(1, 2, 3, 4).indexWhere(x => x % 2 == 0))
    assertEquals("multiple index where not exists", -1, Spandex(1, 2, 3, 4).indexWhere(x => x == 5))
  }

  @Test
  def testFoldLeft(): Unit = {
    assertEquals("empty fold left", 0, Spandex.empty[Int].foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("single fold left", -1, Spandex(1).foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("multiple fold left", -10, Spandex(1, 2, 3, 4).foldLeft(0) {
      case (acc, x) => acc - x
    })
  }

  @Test
  def testFoldRight(): Unit = {
    assertEquals("empty fold right", 0, Spandex.empty[Int].foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("single fold right", 1, Spandex(1).foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("multiple fold right", -2, Spandex(1, 2, 3, 4).foldRight(0) {
      case (x, acc) => x - acc
    })
  }

  @Test
  def testFromIterable(): Unit = {
    assertEquals("empty list is empty", Spandex.empty, Spandex.fromIterable(List.empty))
    assertEquals("empty scala list is empty", Spandex.empty, Spandex.fromIterable(Nil))
    assertEquals("empty lazy list is empty", Spandex.empty, Spandex.fromIterable[Int](LazyList.empty))
  }

  @Test
  def testTrim(): Unit = {
    assertEquals("empty trimmed is empty", Spandex.empty, Spandex.fromIterable(List.empty).trim())
    assertEquals("single trimmed has elements array of correct length", 1, Spandex(1).trim().primary.elements.length)
    assertEquals("single trimmed has elements array with correct element", 1, Spandex(1).trim().primary.elements(0))
    assertEquals("multiple trimmed has elements array of correct length", 3, Spandex(1, 2, 3).trim().primary.elements.length)
    assertEquals("multiple trimmed has elements array with correct element", 1, Spandex(1, 2, 3).trim().primary.elements(0))
    assertEquals("multiple trimmed has elements array with correct element", 2, Spandex(1, 2, 3).trim().primary.elements(1))
    assertEquals("multiple trimmed has elements array with correct element", 3, Spandex(1, 2, 3).trim().primary.elements(2))
  }

  @Test
  def testPrependEqElement(): Unit = {
    val a = Spandex(1, 2, 3)
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
    val a = Spandex(1, 2, 3)
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
    assertTrue("empty last fails", Try(Spandex.empty.last).isFailure)
    assertEquals("single last", 1, Spandex(1).last)
    assertEquals("single reversed last", 1, Spandex(1).reverse.last)
    assertEquals("multiple last", 3, Spandex(1, 2, 3).last)
    assertEquals("multiple reversed last", 1, Spandex(1, 2, 3).reverse.last)
  }

  @Test
  def testInit(): Unit = {
    assertTrue("empty init fails", Try(Spandex.empty.init).isFailure)
    assertEquals("single init", Spandex.empty, Spandex(1).init)
    assertEquals("single reversed init", Spandex.empty, Spandex(1).reverse.init)
    assertEquals("multiple init", Spandex(1, 2), Spandex(1, 2, 3).init)
    val r = Spandex(1, 2, 3).reverse
    val in = r.init
    assertEquals("multiple reverse init", Spandex(3, 2), in)
  }

  @Test
  def testSlice(): Unit = {
    assertEquals("empty slice of single is empty", Spandex.empty, Spandex(1).slice(0, 0))
    assertEquals("empty slice of single reversed is empty", Spandex.empty, Spandex(1).reverse.slice(0, 0))
    assertEquals("empty slice of multiple is empty", Spandex.empty, Spandex(1, 2, 3).slice(1, 1))
    assertEquals("empty slice of multiple reversed is empty", Spandex.empty, Spandex(1, 2, 3).reverse.slice(1, 1))
    assertEquals("single slice of single", Spandex(1), Spandex(1).slice(0, 1))
    assertEquals("single slice of single reversed", Spandex(1), Spandex(1).slice(0, 1))
    assertEquals("single slice of multiple", Spandex(3), Spandex(1, 2, 3).slice(2, 3))
    assertEquals("single slice of multiple reversed", Spandex(1), Spandex(1, 2, 3).reverse.slice(2, 3))
    assertEquals("multiple slice of multiple", Spandex(2, 3), Spandex(1, 2, 3).slice(1, 3))
    assertEquals("multiple slice of multiple reversed", Spandex(2, 1), Spandex(1, 2, 3).reverse.slice(1, 3))
    var sx = 4 +: Spandex(1, 2, 3).reverse
    assertEquals("multiple slice of multiple reversed with prepended", Spandex(3, 2), sx.slice(1, 3))
    sx = Spandex(1, 2, 3).reverse :+ 0
    assertEquals("multiple slice of multiple reversed with append", Spandex(2, 1), sx.slice(1, 3))
    assertEquals("multiple slice of full", Spandex(1, 2, 3), Spandex(1, 2, 3).slice(0, 3))
    assertEquals("multiple slice of full reversed", Spandex(3, 2, 1), Spandex(1, 2, 3).reverse.slice(0, 3))
  }

  @Test
  def testTakeRightAndDropRight(): Unit = {
    assertEquals("take right 0", Spandex.empty, Spandex(1, 2, 3, 4).takeRight(0))
    assertEquals("take right 1", Spandex(4), Spandex(1, 2, 3, 4).takeRight(1))
    assertEquals("take right 2", Spandex(3, 4), Spandex(1, 2, 3, 4).takeRight(2))
    assertEquals("drop right 0", Spandex(1, 2, 3, 4), Spandex(1, 2, 3, 4).dropRight(0))
    assertEquals("drop right 1", Spandex(1, 2, 3), Spandex(1, 2, 3, 4).dropRight(1))
    assertEquals("drop right 2", Spandex(1, 2), Spandex(1, 2, 3, 4).dropRight(2))
    assertEquals("drop right 1 take right 1", Spandex(3), Spandex(1, 2, 3, 4).dropRight(1).takeRight(1))
    assertEquals("drop right 2 take right 1", Spandex(2), Spandex(1, 2, 3, 4).dropRight(2).takeRight(1))
    assertEquals("drop right 2 take right 2", Spandex(1, 2), Spandex(1, 2, 3, 4).dropRight(2).takeRight(2))
    assertEquals("drop right 2 take right 0", Spandex.empty, Spandex(1, 2, 3, 4).dropRight(2).takeRight(0))
    assertEquals("drop right 2 take right 2 drop right 1 take right 1", Spandex(1), Spandex(1, 2, 3, 4).dropRight(2).takeRight(2).dropRight(1).takeRight(1))
  }

  @Test
  def testPatchToFront(): Unit = {
    assertEquals("Empty with empty at front", Spandex.empty, Spandex.empty.patch(0, Spandex.empty))
    assertEquals("Empty reversed with empty at front", Spandex.empty, Spandex.empty.reverse.patch(0, Spandex.empty))
    assertEquals("Empty with empty reversed at front", Spandex.empty, Spandex.empty.patch(0, Spandex.empty.reverse))
    assertEquals("Empty reversed with empty reversed at front", Spandex.empty, Spandex.empty.reverse.patch(0, Spandex.empty.reverse))
    assertEquals("Empty with single at front", Spandex(1), Spandex.empty.patch(0, Spandex(1)))
    assertEquals("Empty reversed with single at front", Spandex(1), Spandex.empty.reverse.patch(0, Spandex(1)))
    assertEquals("Empty with single reversed at front", Spandex(1), Spandex.empty.patch(0, Spandex(1).reverse))
    assertEquals("Empty reversed with single reversed at front", Spandex(1), Spandex.empty.reverse.patch(0, Spandex(1).reverse))
    assertEquals("Single with empty at front replacing 0", Spandex(1), Spandex(1).patch(0, Spandex.empty, 0))
    assertEquals("Single with empty at front replacing 0", Spandex(1), Spandex(1).patch(0, Spandex.empty, 0))
    assertEquals("Single with empty at front replacing 1", Spandex.empty, Spandex(1).patch(0, Spandex.empty, 1))
    assertEquals("Single with single at front replacing 0", Spandex(1, 2), Spandex(2).patch(0, Spandex(1), 0))
    assertEquals("Single reversed with single at front replacing 0", Spandex(1, 2), Spandex(2).reverse.patch(0, Spandex(1)))
    assertEquals("Single with single at front replacing 1", Spandex(1), Spandex(2).patch(0, Spandex(1), 1))
    assertEquals("Single reversed with single at front replacing 1", Spandex(1), Spandex(2).reverse.patch(0, Spandex(1), 1))
    assertEquals("Single with single at front replacing 2", Spandex(1), Spandex(2).patch(0, Spandex(1), 2))
    assertEquals("Single with single at front replacing max", Spandex(1), Spandex(2).patch(0, Spandex(1), 245))
    assertEquals("Single with multiple at front replacing 0", Spandex(1, 2, 3, 4), Spandex(4).patch(0, Spandex(1, 2, 3), 0))
    assertEquals("Single with multiple at front replacing 1", Spandex(1, 2, 3), Spandex(4).patch(0, Spandex(1, 2, 3), 1))
    assertEquals("Single with multiple at front replacing 2", Spandex(1, 2, 3), Spandex(4).patch(0, Spandex(1, 2, 3), 2))
    assertEquals("Single with multiple at front replacing max", Spandex(1, 2, 3), Spandex(2).patch(0, Spandex(1, 2, 3), 245))
    assertEquals("Multiple with empty at front replacing 0", Spandex(1, 2, 3), Spandex(1, 2, 3).patch(0, Spandex.empty, 0))
    assertEquals("Multiple with empty at front replacing 1", Spandex(2, 3), Spandex(1, 2, 3).patch(0, Spandex.empty, 1))
    assertEquals("Multiple with single at front replacing 0", Spandex(4, 1, 2, 3), Spandex(1, 2, 3).patch(0, Spandex(4), 0))
    assertEquals("Multiple with single at front replacing 1", Spandex(4, 2, 3), Spandex(1, 2, 3).patch(0, Spandex(4), 1))
    assertEquals("Multiple with single at front replacing 2", Spandex(4, 3), Spandex(1, 2, 3).patch(0, Spandex(4), 2))
    assertEquals("Multiple with single at front replacing max", Spandex(4), Spandex(1, 2, 3).patch(0, Spandex(4), 245))
  }
  @Test
  def testPatchToRear(): Unit = {
    assertEquals("Empty with empty at rear", Spandex.empty, Spandex.empty.patch(0, Spandex.empty))
    assertEquals("Empty reversed with empty at rear", Spandex.empty, Spandex.empty.reverse.patch(0, Spandex.empty))
    assertEquals("Empty with single at rear", Spandex(1), Spandex.empty.patch(0, Spandex(1)))
    assertEquals("Empty reversed with single at rear", Spandex(1), Spandex.empty.reverse.patch(0, Spandex(1)))
    assertEquals("Single with empty at rear", Spandex(1), Spandex(1).patch(1, Spandex.empty))
    assertEquals("Single reversed with empty at rear", Spandex(1), Spandex(1).reverse.patch(1, Spandex.empty))
    assertEquals("Single with single at rear replacing 0", Spandex(2, 1), Spandex(2).patch(1, Spandex(1)))
    assertEquals("Single reversed with single at rear replacing 0", Spandex(2, 1), Spandex(2).reverse.patch(1, Spandex(1)))
    assertEquals("Single with multiple at rear", Spandex(4, 1, 2, 3), Spandex(4).patch(1, Spandex(1, 2, 3)))
    assertEquals("Single reversed with multiple at rear", Spandex(4, 1, 2, 3), Spandex(4).reverse.patch(1, Spandex(1, 2, 3)))
    assertEquals("Multiple with empty at rear", Spandex(1, 2, 3), Spandex(1, 2, 3).patch(3, Spandex.empty))
    assertEquals("Multiple reversed with empty at rear", Spandex(3, 2, 1), Spandex(1, 2, 3).reverse.patch(3, Spandex.empty))
    assertEquals("Multiple with single at rear", Spandex(1, 2, 3, 4), Spandex(1, 2, 3).patch(3, Spandex(4)))
    assertEquals("Multiple reversed with single at rear", Spandex(3, 2, 1, 4), Spandex(1, 2, 3).reverse.patch(3, Spandex(4)))
    assertEquals("Multiple with multiple at rear", Spandex(1, 2, 3, 4, 5, 6), Spandex(1, 2, 3).patch(3, Spandex(4, 5, 6)))
    assertEquals("Multiple reversed with multiple at rear", Spandex(3, 2, 1, 4, 5, 6), Spandex(1, 2, 3).reverse.patch(3, Spandex(4, 5, 6)))
  }
  @Test
  def testPatchInMiddle(): Unit = {
    assertEquals("Multiple with empty in middle replacing 0", Spandex(1, 2, 3), Spandex(1, 2, 3).patch(1, Spandex.empty))
    assertEquals("Multiple reversed with empty in middle replacing 0", Spandex(3, 2, 1), Spandex(1, 2, 3).reverse.patch(1, Spandex.empty))
    assertEquals("Multiple with empty in middle replacing 1", Spandex(1, 3), Spandex(1, 2, 3).patch(1, Spandex.empty, 1))
    assertEquals("Multiple reversed with empty in middle replacing 1", Spandex(3, 1), Spandex(1, 2, 3).reverse.patch(1, Spandex.empty, 1))
    assertEquals("Multiple with empty in middle replacing max", Spandex(1), Spandex(1, 2, 3).patch(1, Spandex.empty, 245))
    assertEquals("Multiple reversed with empty in middle replacing max", Spandex(3), Spandex(1, 2, 3).reverse.patch(1, Spandex.empty, 245))
    assertEquals("Multiple with single in middle replacing 0", Spandex(1, 4, 2, 3), Spandex(1, 2, 3).patch(1, Spandex(4)))
    assertEquals("Multiple reversed with single in middle replacing 0", Spandex(3, 4, 2, 1), Spandex(1, 2, 3).reverse.patch(1, Spandex(4)))
    assertEquals("Multiple with single in middle replacing 1", Spandex(1, 4, 3), Spandex(1, 2, 3).patch(1, Spandex(4), 1))
    assertEquals("Multiple reversed with single in middle replacing 1", Spandex(3, 4, 1), Spandex(1, 2, 3).reverse.patch(1, Spandex(4), 1))
    assertEquals("Multiple with single in middle replacing max", Spandex(1, 4), Spandex(1, 2, 3).patch(1, Spandex(4), 245))
    assertEquals("Multiple reversed with single in middle replacing max", Spandex(3, 4), Spandex(1, 2, 3).reverse.patch(1, Spandex(4), 245))
    assertEquals("Multiple with multiple in middle replacing 0", Spandex(1, 4, 5, 6, 2, 3), Spandex(1, 2, 3).patch(1, Spandex(4, 5, 6)))
    assertEquals("Multiple reversed with multiple in middle replacing 0", Spandex(3, 4, 5, 6, 2, 1), Spandex(1, 2, 3).reverse.patch(1, Spandex(4, 5, 6)))
    assertEquals("Multiple with multiple in middle replacing 1", Spandex(1, 4, 5, 6, 3), Spandex(1, 2, 3).patch(1, Spandex(4, 5, 6), 1))
    assertEquals("Multiple reversed with multiple in middle replacing 1", Spandex(3, 4, 5, 6, 1), Spandex(1, 2, 3).reverse.patch(1, Spandex(4, 5, 6), 1))
    assertEquals("Multiple with multiple in middle replacing max", Spandex(1, 4, 5, 6), Spandex(1, 2, 3).patch(1, Spandex(4, 5, 6), 245))
    assertEquals("Multiple reversed with multiple in middle replacing max", Spandex(3, 4, 5, 6), Spandex(1, 2, 3).reverse.patch(1, Spandex(4, 5, 6), 245))
  }
  @Test
  def testShrink(): Unit = {
    assertEquals("1 out of 8 is not shrunk", 8, Spandex(1, 2, 3, 4, 5, 6).slice(0, 1).primary.elements.length)
    assertEquals("2 out of 8 is not shrunk", 8, Spandex(1, 2, 3, 4, 5, 6).slice(0, 2).primary.elements.length)
    assertEquals("3 out of 8 is not shrunk", 8, Spandex(1, 2, 3, 4, 5, 6).slice(0, 3).primary.elements.length)
    val s32 = Spandex.tabulate(32)(identity)
    assertEquals("2 out of 32 is shrunk to 8", 8, s32.slice(0, 2).primary.elements.length)
    assertEquals("8 out of 32 is shrunk to 8", 8, s32.slice(0, 8).primary.elements.length)
    assertEquals("9 out of 32 is not shrunk", 32, s32.slice(0, 9).primary.elements.length)
    var s = Spandex.empty[Int]
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
  }
  @Test
  def testPadTo(): Unit = {
    assertEquals("empty to 0 is empty", Spandex.empty, Spandex.empty.padTo(0, 0))
    assertEquals("empty to 3", Spandex(0, 0, 0), Spandex.empty.padTo(3, 0))
    assertEquals("single to 0 is same", Spandex(2), Spandex(2).padTo(0, 0))
    assertEquals("single to 1 is same", Spandex(2), Spandex(2).padTo(1, 0))
    assertEquals("single to 2", Spandex(2, 0), Spandex(2).padTo(2, 0))
    assertEquals("single to 3", Spandex(2, 0, 0), Spandex(2).padTo(3, 0))
    assertEquals("multiple to 0 is same", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(0, 0))
    assertEquals("reversed multiple to 0 is same", Spandex(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(0, 0))
    assertEquals("multiple to 1 is same", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(1, 0))
    assertEquals("reversed multiple to 1 is same", Spandex(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(1, 0))
    assertEquals("multiple to 12 is same", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(12, 0))
    assertEquals("reversed multiple to 12 is same", Spandex(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(12, 0))
    assertEquals("multiple to 24", Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).padTo(24, 0))
    assertEquals("reversed multiple to 24", Spandex(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Spandex(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse.padTo(24, 0))
  }
}

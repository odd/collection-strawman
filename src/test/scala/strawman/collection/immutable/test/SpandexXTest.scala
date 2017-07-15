package strawman
package collection
package immutable
package test

import org.junit.Assert._
import org.junit.Test

import scala.{Exception, Int, NoSuchElementException, Nothing, Unit}
import scala.Predef.identity
import scala.util.Try
import scala.util.control.NonFatal

class SpandexXTest {
  @Test
  def testEmpty(): Unit = {
    assertEquals("apply with no arguments is equal to empty", SpandexX.empty, SpandexX())
    assertEquals("apply with no arguments has size 0", 0, SpandexX().size)
    assertEquals("apply with no arguments has empty iterator", false, SpandexX.empty.iterator().hasNext)
  }

  @Test
  def testApply(): Unit = {
    val O = null
    assertEquals("apply with 00 arguments has correct elements array", List(), SpandexX().primary.elements.to(List))
    assertEquals("apply with 01 argument  has correct elements array", List(O, O, O, 1, O, O, O, O), SpandexX(1).primary.elements.to(List))
    assertEquals("apply with 02 arguments has correct elements array", List(O, O, O, 1, 2, O, O, O), SpandexX(1, 2).primary.elements.to(List))
    assertEquals("apply with 03 arguments has correct elements array", List(O, O, 1, 2, 3, O, O, O), SpandexX(1, 2, 3).primary.elements.to(List))
    assertEquals("apply with 04 arguments has correct elements array", List(O, O, 1, 2, 3, 4, O, O), SpandexX(1, 2, 3, 4).primary.elements.to(List))
    assertEquals("apply with 05 arguments has correct elements array", List(O, 1, 2, 3, 4, 5, O, O), SpandexX(1, 2, 3, 4, 5).primary.elements.to(List))
    assertEquals("apply with 06 arguments has correct elements array", List(O, 1, 2, 3, 4, 5, 6, O), SpandexX(1, 2, 3, 4, 5, 6).primary.elements.to(List))
    assertEquals("apply with 07 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, O), SpandexX(1, 2, 3, 4, 5, 6, 7).primary.elements.to(List))
    assertEquals("apply with 08 arguments has correct elements array", List(1, 2, 3, 4, 5, 6, 7, 8), SpandexX(1, 2, 3, 4, 5, 6, 7, 8).primary.elements.to(List))
    assertEquals("apply with 09 arguments has correct elements array", List(O, O, O, 1, 2, 3, 4, 5, 6, 7, 8, 9, O, O, O, O), SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9).primary.elements.to(List))
    assertEquals("apply with 10 arguments has correct elements array", List(O, O, O, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, O, O, O), SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).primary.elements.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(O, O, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, O, O, O), SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).primary.elements.to(List))
    assertEquals("apply with 11 arguments has correct elements array", List(O, O, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, O, O), SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).primary.elements.to(List))
  }

  @Test
  def testExpandFront(): Unit = {
    val O = null
    val a = 1 +: 2 +: SpandexX(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("no empty slots at end expands correctly", List(O, O, O, 0, 1, 2, 3, 4, 5, 6, 7, 8, O, O, O, O), (0 +: a).primary.elements.to(List))
    val b = 1 +: 2 +: SpandexX(3, 4, 5, 6) :+ 7
    assertEquals("one empty slot at end expands correctly", List(O, O, O, O, 0, 1, 2, 3, 4, 5, 6, 7, O, O, O, O), (0 +: b).primary.elements.to(List))
    val c = 1 +: 2 +: SpandexX(3, 4, 5, 6)
    assertEquals("two empty slots at end expands correctly", List(O, O, O, O, O, 0, 1, 2, 3, 4, 5, 6, O, O, O, O), (0 +: c).primary.elements.to(List))
    val d = 1 +: 2 +: SpandexX(3, 4, 5)
    assertEquals("three empty slots at end expands correctly", List(O, O, O, O, O, O, 0, 1, 2, 3, 4, 5, O, O, O, O), (0 +: d).primary.elements.to(List))
    val e = 0 +: 1 +: 2 +: SpandexX(3, 4)
    assertEquals("three empty slots II at end expands correctly", List(O, O, O, O, O, O, -1, 0, 1, 2, 3, 4, O, O, O, O), (-1 +: e).primary.elements.to(List))
    val f = 0 +: 1 +: 2 +: SpandexX(3)
    assertEquals("four empty slots at end expands correctly", List(O, O, O, -1, 0, 1, 2, 3), (-1 +: f).primary.elements.to(List))
    val g = SpandexX(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.dropRight(7)
    assertEquals("seven empty slots at end expands correctly", List(O, O, O, 0, 1, O, O, O), (0 +: h).primary.elements.to(List))
    val i = g.dropRight(8)
    assertEquals("eight empty slots at end expands correctly", List(O, O, O, 0, O, O, O, O), (0 +: i).primary.elements.to(List))
  }

  @Test
  def testExpandEnd(): Unit = {
    val O = null
    val a = 1 +: 2 +: SpandexX(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("no empty slots at front expands correctly", List(O, O, O, 1, 2, 3, 4, 5, 6, 7, 8, 9, O, O, O, O), (a :+ 9).primary.elements.to(List))
    val b = 2 +: SpandexX(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("one empty slot at front expands correctly", List(O, O, O, 2, 3, 4, 5, 6, 7, 8, 9, O, O, O, O, O), (b :+ 9).primary.elements.to(List))
    val c = SpandexX(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("two empty slots at front expands correctly", List(O, O, O, 3, 4, 5, 6, 7 ,8, 9, O, O, O, O, O, O), (c :+ 9).primary.elements.to(List))
    val d = SpandexX(3, 4) :+ 5 :+ 6 :+ 7
    assertEquals("three empty slots at front expands correctly", List(O, O, O, 3, 4, 5, 6, 7, 8, O, O, O, O, O, O, O), (d :+ 8).primary.elements.to(List))
    val g = SpandexX(1, 2, 3, 4, 5, 6, 7, 8)
    val h = g.drop(7)
    assertEquals("seven empty slots at front expands correctly", List(O, O, O, 8, 9, O, O, O), (h :+ 9).primary.elements.to(List))
    val i = g.drop(8)
    assertEquals("eight empty slots at front expands correctly", List(O, O, O, 9, O, O, O, O), (i :+ 9).primary.elements.to(List))
  }

  @Test
  def testSingleElement(): Unit = {
    assertEquals("apply with single argument has length 1", 1, SpandexX(1).length)
    assertEquals("apply with single argument has single element iterator with correct element", true,
      SpandexX(1).iterator().hasNext && SpandexX(1).iterator().next() == 1)
    assertEquals("apply with reversed single argument has single element iterator with correct element", true,
      SpandexX(1).reverse.iterator().hasNext && SpandexX(1).reverse.iterator().next() == 1)
    assertEquals("apply with single argument has correct element at index 0", 1, SpandexX(1)(0))
    assertEquals("apply with reversed single argument has correct element at index 0", 1, SpandexX(1).reverse.apply(0))
    assertEquals("apply with single argument has correct element as head", 1, SpandexX(1).head)
    assertEquals("apply with reversed single argument has correct element as head", 1, SpandexX(1).reverse.head)
    assertEquals("apply with single argument has empty tail", SpandexX.empty, SpandexX(1).tail)
    assertEquals("apply with reversed single argument has empty tail", SpandexX.empty, SpandexX(1).reverse.tail)
  }

  @Test
  def testHeadsAndTails(): Unit = {
    assertTrue("empty tail fails", Try(SpandexX.empty.tail).isFailure)
    assertTrue("empty reverse tail fails", Try(SpandexX.empty.reverse.tail).isFailure)
    assertEquals("tail", SpandexX(2, 3, 4), SpandexX(1, 2, 3, 4).tail)
    assertEquals("reverse tail", SpandexX(3, 2, 1), SpandexX(1, 2, 3, 4).reverse.tail)
    assertEquals("tail tail", SpandexX(3, 4), SpandexX(1, 2, 3, 4).tail.tail)
    assertEquals("tail reverse tail", SpandexX(3, 2), SpandexX(1, 2, 3, 4).tail.reverse.tail)
    assertEquals("tail tail tail", SpandexX(4), SpandexX(1, 2, 3, 4).tail.tail.tail)
    assertEquals("tail tail reverse tail", SpandexX(3), SpandexX(1, 2, 3, 4).tail.tail.reverse.tail)
    assertEquals("tail tail tail tail", SpandexX.empty, SpandexX(1, 2, 3, 4).tail.tail.tail.tail)
    assertEquals("tail tail tail reverse tail", SpandexX.empty, SpandexX(1, 2, 3, 4).tail.tail.tail.reverse.tail)
    assertTrue("empty head fails", Try(SpandexX.empty.head).isFailure)
    assertEquals("head", 1, SpandexX(1, 2, 3, 4).head)
    assertEquals("reverse head", 4, SpandexX(1, 2, 3, 4).reverse.head)
    assertEquals("reverse tail head", 3, SpandexX(1, 2, 3, 4).reverse.tail.head)
    assertEquals("tail tail head", 3, SpandexX(1, 2, 3, 4).tail.tail.head)
    assertEquals("reverse tail tail reverse head", 1, SpandexX(1, 2, 3, 4).reverse.tail.tail.reverse.head)
    assertEquals("tail tail tail head", 4, SpandexX(1, 2, 3, 4).tail.tail.tail.head)
    assertEquals("tail reverse tail reverse tail reverse head", 3, SpandexX(1, 2, 3, 4).tail.reverse.tail.reverse.tail.reverse.head)
  }

  @Test
  def testTakeAndDrop(): Unit = {
    assertEquals("take 0", SpandexX.empty, SpandexX(1, 2, 3, 4).take(0))
    assertEquals("reverse take 0", SpandexX.empty, SpandexX(1, 2, 3, 4).reverse.take(0))
    assertEquals("take 1", SpandexX(1), SpandexX(1, 2, 3, 4).take(1))
    assertEquals("reverse take 1", SpandexX(4), SpandexX(1, 2, 3, 4).reverse.take(1))
    assertEquals("take 2", SpandexX(1, 2), SpandexX(1, 2, 3, 4).take(2))
    assertEquals("reverse take 2", SpandexX(4, 3), SpandexX(1, 2, 3, 4).reverse.take(2))
    assertEquals("drop 0", SpandexX(1, 2, 3, 4), SpandexX(1, 2, 3, 4).drop(0))
    assertEquals("reverse drop 0", SpandexX(4, 3, 2, 1), SpandexX(1, 2, 3, 4).reverse.drop(0))
    assertEquals("drop 1", SpandexX(2, 3, 4), SpandexX(1, 2, 3, 4).drop(1))
    assertEquals("reverse drop 1", SpandexX(3, 2, 1), SpandexX(1, 2, 3, 4).reverse.drop(1))
    assertEquals("drop 2", SpandexX(3, 4), SpandexX(1, 2, 3, 4).drop(2))
    assertEquals("reverse drop 2", SpandexX(2, 1), SpandexX(1, 2, 3, 4).reverse.drop(2))
    assertEquals("drop 1 take 1", SpandexX(2), SpandexX(1, 2, 3, 4).drop(1).take(1))
    assertEquals("reverse drop 1 take 1", SpandexX(3), SpandexX(1, 2, 3, 4).reverse.drop(1).take(1))
    assertEquals("drop 2 take 1", SpandexX(3), SpandexX(1, 2, 3, 4).drop(2).take(1))
    assertEquals("reverse drop 2 take 1", SpandexX(2), SpandexX(1, 2, 3, 4).reverse.drop(2).take(1))
    assertEquals("drop 2 take 2", SpandexX(3, 4), SpandexX(1, 2, 3, 4).drop(2).take(2))
    assertEquals("reverse drop 2 take 2", SpandexX(2, 1), SpandexX(1, 2, 3, 4).reverse.drop(2).take(2))
    assertEquals("drop 2 take 0", SpandexX.empty, SpandexX(1, 2, 3, 4).drop(2).take(0))
    assertEquals("reverse drop 2 take 0", SpandexX.empty, SpandexX(1, 2, 3, 4).reverse.drop(2).take(0))
    assertEquals("drop 2 take 2 drop 1 take 1", SpandexX(4), SpandexX(1, 2, 3, 4).drop(2).take(2).drop(1).take(1))
    assertEquals("reverse drop 2 take 2 drop 1 take 1", SpandexX(1), SpandexX(1, 2, 3, 4).reverse.drop(2).take(2).drop(1).take(1))
  }

  @Test
  def testFilter(): Unit = {
    assertEquals("filter empty", SpandexX.empty, SpandexX().filter(_ != null))
    assertEquals("filter single inclusive", SpandexX(2), SpandexX(2).filter(_ > 1))
    assertEquals("filter single exclusive", SpandexX(), SpandexX(1).filter(_ > 1))
    assertEquals("filter multiple even", SpandexX(2, 4), SpandexX(1, 2, 3, 4).filter(_ % 2 == 0))
    assertEquals("reverse filter multiple even", SpandexX(4, 2), SpandexX(1, 2, 3, 4).reverse.filter(_ % 2 == 0))
    assertEquals("filter multiple odd", SpandexX(1, 3), SpandexX(1, 2, 3, 4).filter(_ % 2 == 1))
    assertEquals("reverse filter multiple odd", SpandexX(3, 1), SpandexX(1, 2, 3, 4).reverse.filter(_ % 2 == 1))
  }

  @Test
  def testMap(): Unit = {
    assertEquals("map empty", SpandexX.empty, SpandexX().map(identity))
    assertEquals("reverse map empty", SpandexX.empty, SpandexX().reverse.map(identity))
    assertEquals("map single", SpandexX(4), SpandexX(2).map(n => n * n))
    assertEquals("reverse map single", SpandexX(4), SpandexX(2).reverse.map(n => n * n))
    assertEquals("map multiple", SpandexX(1, 4, 9, 16), SpandexX(1, 2, 3, 4).map(n => n * n))
    assertEquals("reverse map multiple", SpandexX(16, 9, 4, 1), SpandexX(1, 2, 3, 4).reverse.map(n => n * n))
  }

  @Test
  def testFlatMap(): Unit = {
    assertEquals("flatmap empty", SpandexX.empty, SpandexX().flatMap(x => SpandexX(x)))
    assertEquals("flatmap single", SpandexX(2), SpandexX(2).flatMap(n => SpandexX(n)))
    assertEquals("flatmap multiple", SpandexX(2, 4, 4, 16), SpandexX(1, 2, 3, 4).flatMap {
      case n if n % 2 == 0 => SpandexX(n, n * n)
      case _ => SpandexX.empty
    })
    assertEquals("reverse flatmap multiple", SpandexX(4, 16, 2, 4), SpandexX(1, 2, 3, 4).reverse.flatMap {
      case n if n % 2 == 0 => SpandexX(n, n * n)
      case _ => SpandexX.empty
    })
  }

  @Test
  def testFill(): Unit = {
    assertEquals("fill empty", SpandexX.empty, SpandexX.fill(0)(9))
    assertEquals("fill single", SpandexX(9), SpandexX.fill(1)(9))
    assertEquals("fill multiple", SpandexX(9, 9, 9), SpandexX.fill(3)(9))
    assertEquals("fill multiple reverse", SpandexX(9, 9, 9), SpandexX.fill(3)(9).reverse)
  }

  @Test
  def testPrepend(): Unit = {
    assertEquals("prepend on empty", SpandexX(1), 1 +: SpandexX.empty)
    assertEquals("prepend single", SpandexX(1, 2, 3, 4), 1 +: SpandexX(2, 3, 4))
    assertEquals("prepend multiple", SpandexX(0, 1, 2, 3, 4), 0 +: 1 +: SpandexX(2, 3, 4))
    assertEquals(
      "prepend multiple over beginning of array",
      SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: SpandexX(1, 2, 3, 4, 5, 6))
  }

  @Test
  def testAppend(): Unit = {
    assertEquals("append on empty", SpandexX(1), SpandexX.empty :+ 1)
    assertEquals("append single", SpandexX(1, 2, 3, 4), SpandexX(1, 2, 3) :+ 4)
    assertEquals("append multiple", SpandexX(1, 2, 3, 4, 5), SpandexX(1, 2, 3) :+ 4 :+ 5)
    assertEquals(
      "append multiple over ending of array",
      SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      SpandexX(1, 2, 3, 4, 5, 6) :+ 7 :+ 8 :+ 9 :+ 10)
  }

  @Test
  def testPrependAndAppendEqual(): Unit = {
    assertEquals("prepend equal on empty", SpandexX(1), 1 +: (1 +: SpandexX.empty).tail)
    assertEquals("append equal on empty", SpandexX(1), (SpandexX.empty :+ 1).take(0) :+ 1)
    assertEquals("prepend equal on single", SpandexX(0, 1), 0 +: (0 +: SpandexX(1)).tail)
    assertEquals("append equal on single", SpandexX(1, 2), (SpandexX(1) :+ 2).take(1) :+ 2)
    val a = SpandexX(1, 2, 3)
    val b = 0 +: a
    val c = b.tail
    val d = 0 +: c
    val e = -1 +: b
    val f = -2 +: d
    assertEquals("prepend equal", SpandexX(1, 2, 3), a)
    assertEquals("prepend equal", SpandexX(0, 1, 2, 3), b)
    assertEquals("prepend equal", SpandexX(1, 2, 3), c)
    assertEquals("prepend equal", SpandexX(0, 1, 2, 3), d)
    assertTrue("prepend eq primaries", b.primary eq d.primary)
    assertEquals("prepend equal", SpandexX(-1, 0, 1, 2, 3), e)
    assertEquals("prepend equal", SpandexX(-2, 0, 1, 2, 3), f)
  }

  @Test
  def testPrependAll(): Unit = {
    assertEquals("prepend all with list on empty", SpandexX(1, 2, 3), List(1, 2, 3) ++: SpandexX.empty)
    assertEquals("prepend all with iterator", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9), List(1, 2, 3, 4).iterator() ++: SpandexX(5, 6, 7, 8, 9))
    assertEquals("prepend all with iterator on reversed", SpandexX(1, 2, 3, 4, 9, 8, 7, 6, 5), List(1, 2, 3, 4).iterator() ++: SpandexX(5, 6, 7, 8, 9).reverse)
    assertEquals("prepend all with multiple lists", SpandexX(-3, -2, -1, 0, 1, 2, 3, 4), List(-3, -2) ++: List(-1, 0) ++: 1 +: SpandexX(2, 3, 4))
    assertEquals("prepend all with multiple lists reversed", SpandexX(-3, -2, 4, 3, 2, 1, 0, -1), List(-3, -2) ++: (List(-1, 0) ++: 1 +: SpandexX(2, 3, 4)).reverse)
  }

  @Test
  def testConcat(): Unit = {
    assertEquals("concat empty with empty", SpandexX.empty, SpandexX.empty[Nothing] ++ SpandexX.empty) // Explicit type annotation needed to avoid dotty bug.
    assertEquals("concat empty with single", SpandexX(1), SpandexX.empty ++ SpandexX(1))
    assertEquals("concat single with empty", SpandexX(1), SpandexX(1) ++ SpandexX.empty)
    assertEquals("concat empty with multiple", SpandexX(1, 2, 3), SpandexX.empty ++ SpandexX(1, 2, 3))
    assertEquals("concat multiple with empty", SpandexX(1, 2, 3), SpandexX(1, 2, 3) ++ SpandexX.empty)
    assertEquals("concat single with single", SpandexX(1, 2), SpandexX(1) ++ SpandexX(2))
    assertEquals("concat single with multiple", SpandexX(1, 2, 3, 4), SpandexX(1) ++ (SpandexX(2) :+ 3 :+ 4))
    assertEquals("concat multiple with single", SpandexX(1, 2, 3, 4), (SpandexX(1) :+ 2 :+ 3) ++ SpandexX(4))
    assertEquals("concat multiple with mix", SpandexX(-3, -2, -1, 0, 1, 2, 3, 4), SpandexX(-3, -2) ++ SpandexX(-1, 0) ++ (1 +: SpandexX(2, 3, 4)))
    assertEquals("concat multiple with mix reversed", SpandexX(-3, -2, 0, -1, 1, 4, 3, 2), SpandexX(-3, -2) ++ SpandexX(-1, 0).reverse ++ (1 +: SpandexX(2, 3, 4).reverse))
    val full = 1 +: 2 +: SpandexX(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("concat empty with full", SpandexX(1, 2, 3, 4, 5, 6, 7, 8), SpandexX.empty ++ full)
    assertEquals("concat full with empty", SpandexX(1, 2, 3, 4, 5, 6, 7, 8), full ++ SpandexX.empty)
    assertEquals("concat single with full", SpandexX(0, 1, 2, 3, 4, 5, 6, 7, 8), SpandexX(0) ++ full)
    assertEquals("concat full with single", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9), full ++ SpandexX(9))
    assertEquals("concat full with full", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8), full ++ full)
    assertEquals("concat full with full reversed", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1), full ++ full.reverse)
    assertEquals("concat full reversed with full", SpandexX(8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse ++ full)
    assertEquals("concat full with full mapped reversed", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 64, 49, 36, 25, 16, 9, 4, 1), full ++ full.reverse.map(x => x * x))
    assertEquals("concat full mapped reversed with full", SpandexX(64, 49, 36, 25, 16, 9, 4, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse.map(x => x * x) ++ full)
    val large = -1 +: 0 +: full :+ 9 :+ 10 :+ 11
    assertEquals(
      "concat large with multiple",
      SpandexX(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
      large ++ SpandexX(12, 13, 14))
    assertEquals(
      "concat large with itself reversed",
      SpandexX(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1),
      large ++ large.reverse)
  }

  @Test
  def testImmutablity(): Unit = {
    val a = SpandexX()
    assertEquals("immutablity of empty", SpandexX.empty, a)
    val b = 1 +: a
    assertEquals("immutablity of empty", SpandexX(1), b)
    val c = a :+ 2
    assertEquals("immutablity of empty", SpandexX(2), c)
    val d = SpandexX(2, 3)
    assertEquals("immutablity of multiple", SpandexX(2, 3), d)
    val e = 0 +: 1 +: d
    assertEquals("immutablity of multiple", SpandexX(0, 1, 2, 3), e)
    val f = d :+ 4 :+ 5
    assertEquals("immutablity of multiple", SpandexX(2, 3, 4, 5), f)
    val g = 0 +: 1 +: f
    assertEquals("immutablity of multiple", SpandexX(0, 1, 2, 3, 4, 5), g)

    assertEquals("immutablity of empty still", SpandexX.empty, a)
    assertEquals("immutablity of empty still", SpandexX(1), b)
    assertEquals("immutablity of empty still", SpandexX(2), c)
    assertEquals("immutablity of multiple still", SpandexX(2, 3), d)
    assertEquals("immutablity of multiple still", SpandexX(0, 1, 2, 3), e)
    assertEquals("immutablity of multiple still", SpandexX(2, 3, 4, 5), f)
    assertEquals("immutablity of multiple still", SpandexX(0, 1, 2, 3, 4, 5), g)
  }

  @Test
  def testLarge(): Unit = {
    val a = SpandexX.tabulate(10)(identity)
    val ar = a.reverse
    assertEquals("large a", SpandexX(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed", SpandexX(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse", a, ar.reverse)
    val b = -2 +: -1 +: a
    val br = ar :+ -1 :+ -2
    assertEquals("large b", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed", SpandexX(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse", b, br.reverse)
    val b2 = -4 +: -3 +: -2 +: -1 +: a
    val b2r = ar :+ -1 :+ -2 :+ -3 :+ -4
    assertEquals("large b2", SpandexX(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed", SpandexX(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse", b2, b2r.reverse)
    val c = b :+ 10
    val cr = 10 +: br
    assertEquals("large c", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed", SpandexX(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse", c, cr.reverse)
    val d2 = b2 :+ 10 :+ 11 :+ 12
    val d2r = 12 +: 11 +: 10 +: b2r
    assertEquals("large d2", SpandexX(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed", SpandexX(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse", d2, d2r.reverse)
    val d = b :+ 10 :+ 11
    val dr = 11 +: 10 +: br
    assertEquals("large d", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed", SpandexX(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse", d, dr.reverse)
    val e = c :+ 11
    val er = 11 +: cr
    assertEquals("large e", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed", SpandexX(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse", e, er.reverse)
    val f = -3 +: c
    val fr = cr :+ -3
    assertEquals("large f", SpandexX(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed", SpandexX(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse", f, fr.reverse)
    val g = e.tail.tail.tail
    val gr = er.tail.tail.tail
    assertEquals("large g", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed", SpandexX(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
    val h = (g.tail ++ g.tail.tail.tail).tail
    val h2 = (gr.tail.reverse ++ g.tail.reverse).tail.tail
    assertEquals("large h", SpandexX(3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5, 6, 7, 8, 9, 10, 11), h)
    assertEquals("large h2", SpandexX(0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2), h2)

    assertEquals("large a still", SpandexX(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed still", SpandexX(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse still", a, ar.reverse)
    assertEquals("large b still", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed still", SpandexX(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse still", b, br.reverse)
    assertEquals("large b2 still", SpandexX(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed still", SpandexX(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse still", b2, b2r.reverse)
    assertEquals("large c still", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed still", SpandexX(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse still", c, cr.reverse)
    assertEquals("large d2 still", SpandexX(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed still", SpandexX(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse still", d2, d2r.reverse)
    assertEquals("large d still", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed still", SpandexX(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse still", d, dr.reverse)
    assertEquals("large e still", SpandexX(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed still", SpandexX(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse still", e, er.reverse)
    assertEquals("large f still", SpandexX(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed still", SpandexX(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse still", f, fr.reverse)
    assertEquals("large g still", SpandexX(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed still", SpandexX(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
    assertEquals("large h still", SpandexX(3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 5, 6, 7, 8, 9, 10, 11), h)
    assertEquals("large h reversed still", SpandexX(0, 1, 2, 3, 4, 5, 6, 7, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2), h2)
    assertEquals("large h equals its reversed reverse", h, h.reverse.reverse)
    assertEquals("large h2 equals its reversed reverse", h2, h2.reverse.reverse)
  }

  @Test
  def testIterator(): Unit = {
    var it = SpandexX.empty[Int].iterator()
    assertFalse("empty has empty iterator", it.hasNext)

    it = SpandexX(1).iterator()
    assertTrue("single has non empty iterator", it.hasNext)
    assertEquals("single iterator has correct element", it.next(), 1)
    assertFalse("single iterator has only one element", it.hasNext)

    it = (0 +: 1 +: SpandexX(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).iterator()
    assertTrue("large has non empty iterator", it.hasNext)
    assertTrue(
      "large iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))

    /*
    val it2 = (Spandex(1, 2, 3) :+ "kalle").iterator()
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
    val it3= (SpandexX(a1, a2) :+ b).iterator()
    assertTrue("subs has non empty iterator", it3.hasNext)
    assertTrue(
      "subs iterator has correct elements",
      it3.sameElements(List(a1, a2, b)))

    // Test iterator index bounds checking
    it = SpandexX.empty[Int].iterator()
    try {
      it.next()
      fail("should be out of bounds")
    } catch {
      case _: NoSuchElementException => // expected
      case NonFatal(_) => fail("wrong exception")
    }

    it = SpandexX(1, 2, 3, 4).dropRight(1).iterator()
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
    SpandexX.empty[Int].foreach(_ => n += 1)
    assertEquals("empty foreach", 0, n)
    SpandexX(1).foreach(x => n += x)
    assertEquals("single foreach", 1, n)
    SpandexX(2, 3, 4, 5, 6).foreach(x => n += x)
    assertEquals("multiple foreach", 21, n)
    var m = 6
    SpandexX(1, 2, 3).reverse.foreach(x => m -= x)
    assertEquals("multiple foreach reversed", 0, m)
  }

  @Test
  def testIndexWhere(): Unit = {
    assertEquals("empty index where", -1, SpandexX.empty[Int].indexWhere(_ => true))
    assertEquals("single index where exists", 0, SpandexX(1).indexWhere(x => x == 1))
    assertEquals("single index where not exists", -1, SpandexX(1).indexWhere(x => x == 2))
    assertEquals("multiple index where exists", 1, SpandexX(1, 2, 3, 4).indexWhere(x => x % 2 == 0))
    assertEquals("multiple index where not exists", -1, SpandexX(1, 2, 3, 4).indexWhere(x => x == 5))
  }

  @Test
  def testFoldLeft(): Unit = {
    assertEquals("empty fold left", 0, SpandexX.empty[Int].foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("single fold left", -1, SpandexX(1).foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("multiple fold left", -10, SpandexX(1, 2, 3, 4).foldLeft(0) {
      case (acc, x) => acc - x
    })
  }

  @Test
  def testFoldRight(): Unit = {
    assertEquals("empty fold right", 0, SpandexX.empty[Int].foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("single fold right", 1, SpandexX(1).foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("multiple fold right", -2, SpandexX(1, 2, 3, 4).foldRight(0) {
      case (x, acc) => x - acc
    })
  }

  @Test
  def testFromIterable(): Unit = {
    assertEquals("empty list is empty", SpandexX.empty, SpandexX.fromIterable(List.empty))
    assertEquals("empty scala list is empty", SpandexX.empty, SpandexX.fromIterable(Nil))
    assertEquals("empty lazy list is empty", SpandexX.empty, SpandexX.fromIterable[Int](LazyList.empty))
  }

  @Test
  def testTrim(): Unit = {
    assertEquals("empty trimmed is empty", SpandexX.empty, SpandexX.fromIterable(List.empty).trim())
    assertEquals("single trimmed has elements array of correct length", 1, SpandexX(1).trim().primary.elements.length)
    assertEquals("single trimmed has elements array with correct element", 1, SpandexX(1).trim().primary.elements(0))
    assertEquals("multiple trimmed has elements array of correct length", 3, SpandexX(1, 2, 3).trim().primary.elements.length)
    assertEquals("multiple trimmed has elements array with correct element", 1, SpandexX(1, 2, 3).trim().primary.elements(0))
    assertEquals("multiple trimmed has elements array with correct element", 2, SpandexX(1, 2, 3).trim().primary.elements(1))
    assertEquals("multiple trimmed has elements array with correct element", 3, SpandexX(1, 2, 3).trim().primary.elements(2))
  }

  @Test
  def testPrependEqElement(): Unit = {
    val a = SpandexX(1, 2, 3)
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
    val a = SpandexX(1, 2, 3)
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
    assertTrue("empty last fails", Try(SpandexX.empty.last).isFailure)
    assertEquals("single last", 1, SpandexX(1).last)
    assertEquals("single reversed last", 1, SpandexX(1).reverse.last)
    assertEquals("multiple last", 3, SpandexX(1, 2, 3).last)
    assertEquals("multiple reversed last", 1, SpandexX(1, 2, 3).reverse.last)
  }

  @Test
  def testInit(): Unit = {
    assertTrue("empty init fails", Try(SpandexX.empty.init).isFailure)
    assertEquals("single init", SpandexX.empty, SpandexX(1).init)
    assertEquals("single reversed init", SpandexX.empty, SpandexX(1).reverse.init)
    assertEquals("multiple init", SpandexX(1, 2), SpandexX(1, 2, 3).init)
    assertEquals("multiple reverse init", SpandexX(3, 2), SpandexX(1, 2, 3).reverse.init)
  }

  @Test
  def testSlice(): Unit = {
    assertEquals("empty slice of single is empty", SpandexX.empty, SpandexX(1).slice(0, 0))
    assertEquals("empty slice of single reversed is empty", SpandexX.empty, SpandexX(1).reverse.slice(0, 0))
    assertEquals("empty slice of multiple is empty", SpandexX.empty, SpandexX(1, 2, 3).slice(1, 1))
    assertEquals("empty slice of multiple reversed is empty", SpandexX.empty, SpandexX(1, 2, 3).reverse.slice(1, 1))
    assertEquals("single slice of single", SpandexX(1), SpandexX(1).slice(0, 1))
    assertEquals("single slice of single reversed", SpandexX(1), SpandexX(1).slice(0, 1))
    assertEquals("single slice of multiple", SpandexX(3), SpandexX(1, 2, 3).slice(2, 3))
    assertEquals("single slice of multiple reversed", SpandexX(1), SpandexX(1, 2, 3).reverse.slice(2, 3))
    assertEquals("multiple slice of multiple", SpandexX(2, 3), SpandexX(1, 2, 3).slice(1, 3))
    assertEquals("multiple slice of multiple reversed", SpandexX(2, 1), SpandexX(1, 2, 3).reverse.slice(1, 3))
    assertEquals("multiple slice of multiple reversed with prepended", SpandexX(3, 2), (4 +: SpandexX(1, 2, 3).reverse).slice(1, 3))
    assertEquals("multiple slice of full", SpandexX(1, 2, 3), SpandexX(1, 2, 3).slice(0, 3))
    assertEquals("multiple slice of full reversed", SpandexX(3, 2, 1), SpandexX(1, 2, 3).reverse.slice(0, 3))
  }

  @Test
  def testTakeRightAndDropRight(): Unit = {
    assertEquals("take right 0", SpandexX.empty, SpandexX(1, 2, 3, 4).takeRight(0))
    assertEquals("take right 1", SpandexX(4), SpandexX(1, 2, 3, 4).takeRight(1))
    assertEquals("take right 2", SpandexX(3, 4), SpandexX(1, 2, 3, 4).takeRight(2))
    assertEquals("drop right 0", SpandexX(1, 2, 3, 4), SpandexX(1, 2, 3, 4).dropRight(0))
    assertEquals("drop right 1", SpandexX(1, 2, 3), SpandexX(1, 2, 3, 4).dropRight(1))
    assertEquals("drop right 2", SpandexX(1, 2), SpandexX(1, 2, 3, 4).dropRight(2))
    assertEquals("drop right 1 take right 1", SpandexX(3), SpandexX(1, 2, 3, 4).dropRight(1).takeRight(1))
    assertEquals("drop right 2 take right 1", SpandexX(2), SpandexX(1, 2, 3, 4).dropRight(2).takeRight(1))
    assertEquals("drop right 2 take right 2", SpandexX(1, 2), SpandexX(1, 2, 3, 4).dropRight(2).takeRight(2))
    assertEquals("drop right 2 take right 0", SpandexX.empty, SpandexX(1, 2, 3, 4).dropRight(2).takeRight(0))
    assertEquals("drop right 2 take right 2 drop right 1 take right 1", SpandexX(1), SpandexX(1, 2, 3, 4).dropRight(2).takeRight(2).dropRight(1).takeRight(1))
  }
}

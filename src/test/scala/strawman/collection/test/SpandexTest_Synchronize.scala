package strawman
package collection.test

import java.lang.String
import scala.{Any, Array, Boolean, Char, Either, Int, Left, Nothing, Option, StringContext, Unit}
import scala.Predef.{assert, charWrapper, identity, println}

import collection._
import collection.immutable.{LazyList, List, Nil, Spandex_Synchronize}
import collection.mutable.{ArrayBuffer, ListBuffer}
import org.junit.Test
import org.junit.Assert._

class SpandexTest_Synchronize {
  @Test
  def testEmpty(): Unit = {
    assertEquals("apply with no arguments is equal to empty", Spandex_Synchronize.empty, Spandex_Synchronize())
    assertEquals("apply with no arguments has size 0", 0, Spandex_Synchronize().size)
    assertEquals(
      "apply with no arguments has empty iterator",
      false,
      Spandex_Synchronize.empty.iterator().hasNext)
  }

  @Test
  def testSingleElement(): Unit = {
    assertEquals("apply with single argument has length 1", 1, Spandex_Synchronize(1).length)
    assertEquals(
      "apply with single argument has single element iterator with correct element",
      true,
      Spandex_Synchronize(1).iterator().hasNext && Spandex_Synchronize(1).iterator().next() == 1)
    assertEquals("apply with single argument has correct element at index 0", 1, Spandex_Synchronize(1)(0))
    assertEquals("apply with single argument has correct element as head", 1, Spandex_Synchronize(1).head)
    assertEquals("apply with single argument has empty tail", Spandex_Synchronize.empty, Spandex_Synchronize(1).tail)
  }
  @Test
  def testHeadsAndTails(): Unit = {
    assertEquals("tail", Spandex_Synchronize(2, 3, 4), Spandex_Synchronize(1, 2, 3, 4).tail)
    assertEquals("tail tail", Spandex_Synchronize(3, 4), Spandex_Synchronize(1, 2, 3, 4).tail.tail)
    assertEquals("tail tail tail", Spandex_Synchronize(4), Spandex_Synchronize(1, 2, 3, 4).tail.tail.tail)
    assertEquals("tail tail tail tail", Spandex_Synchronize.empty, Spandex_Synchronize(1, 2, 3, 4).tail.tail.tail.tail)
    assertEquals("head", 1, Spandex_Synchronize(1, 2, 3, 4).head)
    assertEquals("tail head", 2, Spandex_Synchronize(1, 2, 3, 4).tail.head)
    assertEquals("tail tail head", 3, Spandex_Synchronize(1, 2, 3, 4).tail.tail.head)
    assertEquals("tail tail tail head", 4, Spandex_Synchronize(1, 2, 3, 4).tail.tail.tail.head)
  }
  @Test
  def testTakeAndDrop(): Unit = {
    assertEquals("take 0", Spandex_Synchronize.empty, Spandex_Synchronize(1, 2, 3, 4).take(0))
    assertEquals("take 1", Spandex_Synchronize(1), Spandex_Synchronize(1, 2, 3, 4).take(1))
    assertEquals("take 2", Spandex_Synchronize(1, 2), Spandex_Synchronize(1, 2, 3, 4).take(2))
    assertEquals("drop 0", Spandex_Synchronize(1, 2, 3, 4), Spandex_Synchronize(1, 2, 3, 4).drop(0))
    assertEquals("drop 1", Spandex_Synchronize(2, 3, 4), Spandex_Synchronize(1, 2, 3, 4).drop(1))
    assertEquals("drop 2", Spandex_Synchronize(3, 4), Spandex_Synchronize(1, 2, 3, 4).drop(2))
    assertEquals("drop 1 take 1", Spandex_Synchronize(2), Spandex_Synchronize(1, 2, 3, 4).drop(1).take(1))
    assertEquals("drop 2 take 1", Spandex_Synchronize(3), Spandex_Synchronize(1, 2, 3, 4).drop(2).take(1))
    assertEquals("drop 2 take 2", Spandex_Synchronize(3, 4), Spandex_Synchronize(1, 2, 3, 4).drop(2).take(2))
    assertEquals("drop 2 take 0", Spandex_Synchronize.empty, Spandex_Synchronize(1, 2, 3, 4).drop(2).take(0))
    assertEquals("drop 2 take 2 drop 1 take 1", Spandex_Synchronize(4), Spandex_Synchronize(1, 2, 3, 4).drop(2).take(2).drop(1).take(1))
  }
  @Test
  def testFilter(): Unit = {
    assertEquals("filter empty", Spandex_Synchronize.empty, Spandex_Synchronize().filter(_ != null))
    assertEquals("filter single inclusive", Spandex_Synchronize(2), Spandex_Synchronize(2).filter(_ > 1))
    assertEquals("filter single exclusive", Spandex_Synchronize(), Spandex_Synchronize(1).filter(_ > 1))
    assertEquals("filter multiple even", Spandex_Synchronize(2, 4), Spandex_Synchronize(1, 2, 3, 4).filter(_ % 2 == 0))
    assertEquals("filter multiple odd", Spandex_Synchronize(1, 3), Spandex_Synchronize(1, 2, 3, 4).filter(_ % 2 == 1))
  }
  @Test
  def testMap(): Unit = {
    assertEquals("map empty", Spandex_Synchronize.empty, Spandex_Synchronize().map(identity))
    assertEquals("map single", Spandex_Synchronize(4), Spandex_Synchronize(2).map(n => n * n))
    assertEquals("map multiple", Spandex_Synchronize(1, 4, 9, 16), Spandex_Synchronize(1, 2, 3, 4).map(n => n * n))
  }
  @Test
  def testFlatMap(): Unit = {
    assertEquals("flatmap empty", Spandex_Synchronize.empty, Spandex_Synchronize().flatMap(x => Spandex_Synchronize(x)))
    assertEquals("flatmap single", Spandex_Synchronize(2), Spandex_Synchronize(2).flatMap(n => Spandex_Synchronize(n)))
    assertEquals("flatmap multiple", Spandex_Synchronize(2, 4, 4, 16), Spandex_Synchronize(1, 2, 3, 4).flatMap {
      case n if n % 2 == 0 => Spandex_Synchronize(n, n * n)
      case _ => Spandex_Synchronize.empty
    })
    assertEquals("flatmap multiple reversed", Spandex_Synchronize(4, 16, 2, 4), Spandex_Synchronize(1, 2, 3, 4).reverse.flatMap {
      case n if n % 2 == 0 => Spandex_Synchronize(n, n * n)
      case _ => Spandex_Synchronize.empty
    })
  }
  @Test
  def testFill(): Unit = {
    assertEquals("fill empty", Spandex_Synchronize.empty, Spandex_Synchronize.fill(0)(9))
    assertEquals("fill single", Spandex_Synchronize(9), Spandex_Synchronize.fill(1)(9))
    assertEquals("fill mutiple", Spandex_Synchronize(9, 9, 9), Spandex_Synchronize.fill(3)(9))
  }
  @Test
  def testPrepend(): Unit = {
    assertEquals("prepend on empty", Spandex_Synchronize(1), 1 +: Spandex_Synchronize.empty)
    assertEquals("prepend single", Spandex_Synchronize(1, 2, 3, 4), 1 +: Spandex_Synchronize(2, 3, 4))
    assertEquals("prepend multiple", Spandex_Synchronize(0, 1, 2, 3, 4), 0 +: 1 +: Spandex_Synchronize(2, 3, 4))
    assertEquals(
      "prepend multiple over beginning of array",
      Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: Spandex_Synchronize(1, 2, 3, 4, 5, 6))
    assertEquals(
      "prepend multiple with itself reversed",
      Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: Spandex_Synchronize(1, 2, 3, 4, 5, 6))
  }
  @Test
  def testAppend(): Unit = {
    assertEquals("append on empty", Spandex_Synchronize(1), Spandex_Synchronize.empty :+ 1)
    assertEquals("append single", Spandex_Synchronize(1, 2, 3, 4), Spandex_Synchronize(1, 2, 3) :+ 4)
    assertEquals("append multiple", Spandex_Synchronize(1, 2, 3, 4, 5), Spandex_Synchronize(1, 2, 3) :+ 4 :+ 5)
  }
  @Test
  def testConcat(): Unit = {
    assertEquals("concat empty with empty", Spandex_Synchronize.empty, Spandex_Synchronize.empty ++ Spandex_Synchronize.empty)
    assertEquals("concat empty with single", Spandex_Synchronize(1), Spandex_Synchronize.empty ++ Spandex_Synchronize(1))
    assertEquals("concat single with empty", Spandex_Synchronize(1), Spandex_Synchronize(1) ++ Spandex_Synchronize.empty)
    assertEquals("concat empty with multiple", Spandex_Synchronize(1, 2, 3), Spandex_Synchronize.empty ++ Spandex_Synchronize(1, 2, 3))
    assertEquals("concat multiple with empty", Spandex_Synchronize(1, 2, 3), Spandex_Synchronize(1, 2, 3) ++ Spandex_Synchronize.empty)
    assertEquals("concat single with single", Spandex_Synchronize(1, 2), Spandex_Synchronize(1) ++ Spandex_Synchronize(2))
    assertEquals("concat single with multiple", Spandex_Synchronize(1, 2, 3, 4), Spandex_Synchronize(1) ++ (Spandex_Synchronize(2) :+ 3 :+ 4))
    assertEquals("concat multiple with single", Spandex_Synchronize(1, 2, 3, 4), (Spandex_Synchronize(1) :+ 2 :+ 3) ++ Spandex_Synchronize(4))
    val full = 1 +: 2 +: Spandex_Synchronize(3, 4, 5, 6) :+ 7 :+ 8
    assertEquals("concat empty with full", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8), Spandex_Synchronize.empty ++ full)
    assertEquals("concat full with empty", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8), full ++ Spandex_Synchronize.empty)
    assertEquals("concat single with full", Spandex_Synchronize(0, 1, 2, 3, 4, 5, 6, 7, 8), Spandex_Synchronize(0) ++ full)
    assertEquals("concat full with single", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8, 9), full ++ Spandex_Synchronize(9))
    assertEquals("concat full with full", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8), full ++ full)
    assertEquals("concat full with full reversed", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8, 8, 7, 6, 5, 4, 3, 2, 1), full ++ full.reverse)
    assertEquals("concat full reversed with full", Spandex_Synchronize(8, 7, 6, 5, 4, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse ++ full)
    assertEquals("concat full with full mapped reversed", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8, 64, 49, 36, 25, 16, 9, 4, 1), full ++ full.reverse.map(x => x * x))
    assertEquals("concat full mapped reversed with full", Spandex_Synchronize(64, 49, 36, 25, 16, 9, 4, 1, 1, 2, 3, 4, 5, 6, 7, 8), full.reverse.map(x => x * x) ++ full)
    val large = -1 +: 0 +: full :+ 9 :+ 10 :+ 11
    assertEquals(
      "concat large with multiple",
      Spandex_Synchronize(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
      large ++ Spandex_Synchronize(12, 13, 14))
    assertEquals(
      "concat large with itself reversed",
      Spandex_Synchronize(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1),
      large ++ large.reverse)
  }
  @Test
  def testImmutablity(): Unit = {
    val a = Spandex_Synchronize()
    assertEquals("immutablity of empty", Spandex_Synchronize.empty, a)
    val b = 1 +: a
    assertEquals("immutablity of empty", Spandex_Synchronize(1), b)
    val c = a :+ 2
    assertEquals("immutablity of empty", Spandex_Synchronize(2), c)
    val d = Spandex_Synchronize(2, 3)
    assertEquals("immutablity of multiple", Spandex_Synchronize(2, 3), d)
    val e = 0 +: 1 +: d
    assertEquals("immutablity of multiple", Spandex_Synchronize(0, 1, 2, 3), e)
    val f = d :+ 4 :+ 5
    assertEquals("immutablity of multiple", Spandex_Synchronize(2, 3, 4, 5), f)
    val g = 0 +: 1 +: f
    assertEquals("immutablity of multiple", Spandex_Synchronize(0, 1, 2, 3, 4, 5), g)

    assertEquals("immutablity of empty still", Spandex_Synchronize.empty, a)
    assertEquals("immutablity of empty still", Spandex_Synchronize(1), b)
    assertEquals("immutablity of empty still", Spandex_Synchronize(2), c)
    assertEquals("immutablity of multiple still", Spandex_Synchronize(2, 3), d)
    assertEquals("immutablity of multiple still", Spandex_Synchronize(0, 1, 2, 3), e)
    assertEquals("immutablity of multiple still", Spandex_Synchronize(2, 3, 4, 5), f)
    assertEquals("immutablity of multiple still", Spandex_Synchronize(0, 1, 2, 3, 4, 5), g)
  }
  @Test
  def testLarge(): Unit = {
    val a = Spandex_Synchronize.tabulate(10)(identity)
    val ar = a.reverse
    assertEquals("large a", Spandex_Synchronize(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed", Spandex_Synchronize(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse", a, ar.reverse)
    val b = -2 +: -1 +: a
    val br = ar :+ -1 :+ -2
    assertEquals("large b", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed", Spandex_Synchronize(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse", b, br.reverse)
    val b2 = -4 +: -3 +: -2 +: -1 +: a
    val b2r = ar :+ -1 :+ -2 :+ -3 :+ -4
    assertEquals("large b2", Spandex_Synchronize(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed", Spandex_Synchronize(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse", b2, b2r.reverse)
    val c = b :+ 10
    val cr = 10 +: br
    assertEquals("large c", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed", Spandex_Synchronize(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse", c, cr.reverse)
    val d2 = b2 :+ 10 :+ 11 :+ 12
    val d2r = 12 +: 11 +: 10 +: b2r
    assertEquals("large d2", Spandex_Synchronize(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed", Spandex_Synchronize(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse", d2, d2r.reverse)
    val d = b :+ 10 :+ 11
    val dr = 11 +: 10 +: br
    assertEquals("large d", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed", Spandex_Synchronize(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse", d, dr.reverse)
    val e = c :+ 11
    val er = 11 +: cr
    assertEquals("large e", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed", Spandex_Synchronize(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse", e, er.reverse)
    val f = -3 +: c
    val fr = cr :+ -3
    assertEquals("large f", Spandex_Synchronize(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed", Spandex_Synchronize(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse", f, fr.reverse)
    val g = e.tail.tail.tail
    val gr = er.tail.tail.tail
    assertEquals("large g", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed", Spandex_Synchronize(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)

    assertEquals("large a still", Spandex_Synchronize(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), a)
    assertEquals("large a reversed still", Spandex_Synchronize(9, 8, 7, 6, 5, 4, 3, 2, 1, 0), ar)
    assertEquals("large a equals its reversed reverse still", a, ar.reverse)
    assertEquals("large b still", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b)
    assertEquals("large b reversed still", Spandex_Synchronize(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), br)
    assertEquals("large b equals its reversed reverse still", b, br.reverse)
    assertEquals("large b2 still", Spandex_Synchronize(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), b2)
    assertEquals("large b2 reversed still", Spandex_Synchronize(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), b2r)
    assertEquals("large b2 equals its reversed reverse still", b2, b2r.reverse)
    assertEquals("large c still", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), c)
    assertEquals("large c reversed still", Spandex_Synchronize(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), cr)
    assertEquals("large c equals its reversed reverse still", c, cr.reverse)
    assertEquals("large d2 still", Spandex_Synchronize(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), d2)
    assertEquals("large d2 reversed still", Spandex_Synchronize(12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4), d2r)
    assertEquals("large d2 equals its reversed reverse still", d2, d2r.reverse)
    assertEquals("large d still", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), d)
    assertEquals("large d reversed still", Spandex_Synchronize(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), dr)
    assertEquals("large d equals its reversed reverse still", d, dr.reverse)
    assertEquals("large e still", Spandex_Synchronize(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), e)
    assertEquals("large e reversed still", Spandex_Synchronize(11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), er)
    assertEquals("large e equals its reversed reverse still", e, er.reverse)
    assertEquals("large f still", Spandex_Synchronize(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), f)
    assertEquals("large f reversed still", Spandex_Synchronize(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3), fr)
    assertEquals("large f equals its reversed reverse still", f, fr.reverse)
    assertEquals("large g still", Spandex_Synchronize(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), g)
    assertEquals("large g reversed still", Spandex_Synchronize(8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2), gr)
  }
  @Test
  def testIterator(): Unit = {
    var it = Spandex_Synchronize.empty[Int].iterator()
    assertFalse("empty has empty iterator", it.hasNext)

    it = Spandex_Synchronize(1).iterator()
    assertTrue("single has non empty iterator", it.hasNext)
    assertEquals("single iterator has correct element", it.next(), 1)
    assertFalse("single iterator has only one element", it.hasNext)

    it = (0 +: 1 +: Spandex_Synchronize(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).iterator()
    assertTrue("large has non empty iterator", it.hasNext)
    assertTrue(
      "large iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))

    val it2 = (Spandex_Synchronize(1, 2, 3) :+ "kalle").iterator()
    assertTrue("any has non empty iterator", it2.hasNext)
    assertTrue(
      "any iterator has correct elements",
      it2.sameElements(List(1, 2, 3, "kalle")))

    class A()
    class B() extends A()
    val a1 = new A()
    val a2 = new A()
    val b = new B()
    val it3= (Spandex_Synchronize(a1, a2) :+ b).iterator()
    assertTrue("subs has non empty iterator", it3.hasNext)
    assertTrue(
      "subs iterator has correct elements",
      it3.sameElements(List(a1, a2, b)))
  }
  @Test
  def testForeach(): Unit = {
    var n = 0
    Spandex_Synchronize.empty[Int].foreach(_ => n += 1)
    assertEquals("empty foreach", 0, n)
    Spandex_Synchronize(1).foreach(x => n += x)
    assertEquals("single foreach", 1, n)
    Spandex_Synchronize(2, 3, 4, 5, 6).foreach(x => n += x)
    assertEquals("multiple foreach", 21, n)
  }
  @Test
  def testIndexWhere(): Unit = {
    assertEquals("empty index where", -1, Spandex_Synchronize.empty[Int].indexWhere(_ => true))
    assertEquals("single index where exists", 0, Spandex_Synchronize(1).indexWhere(x => x == 1))
    assertEquals("single index where not exists", -1, Spandex_Synchronize(1).indexWhere(x => x == 2))
    assertEquals("multiple index where exists", 1, Spandex_Synchronize(1, 2, 3, 4).indexWhere(x => x % 2 == 0))
    assertEquals("multiple index where not exists", -1, Spandex_Synchronize(1, 2, 3, 4).indexWhere(x => x == 5))
  }
  @Test
  def testFoldLeft(): Unit = {
    assertEquals("empty fold left", 0, Spandex_Synchronize.empty[Int].foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("single fold left", -1, Spandex_Synchronize(1).foldLeft(0) {
      case (acc, x) => acc - x
    })
    assertEquals("multiple fold left", -10, Spandex_Synchronize(1, 2, 3, 4).foldLeft(0) {
      case (acc, x) => acc - x
    })
  }
  @Test
  def testFoldRight(): Unit = {
    assertEquals("empty fold right", 0, Spandex_Synchronize.empty[Int].foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("single fold right", 1, Spandex_Synchronize(1).foldRight(0) {
      case (x, acc) => x - acc
    })
    assertEquals("multiple fold right", -2, Spandex_Synchronize(1, 2, 3, 4).foldRight(0) {
      case (x, acc) => x - acc
    })
  }

}

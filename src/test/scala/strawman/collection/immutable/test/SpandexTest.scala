package strawman.collection.immutable
package test

import org.junit.Assert._
import org.junit.Test
import scala.{Int, Unit}
import scala.Predef.identity
import strawman.collection.immutable

class SpandexTest {
  @Test
  def testEmpty(): Unit = {
    assertEquals("apply with no arguments is equal to empty", Spandex.empty, Spandex())
    assertEquals("apply with no arguments has size 0", 0, Spandex().size)
    assertEquals(
      "apply with no arguments has empty iterator",
      false,
      Spandex.empty.iterator().hasNext)
  }

  @Test
  def testSingleElement(): Unit = {
    assertEquals("apply with single argument has length 1", 1, Spandex(1).length)
    assertEquals(
      "apply with single argument has single element iterator with correct element",
      true,
      Spandex(1).iterator().hasNext && Spandex(1).iterator().next() == 1)
    assertEquals("apply with single argument has correct element at index 0", 1, Spandex(1)(0))
    assertEquals("apply with single argument has correct element as head", 1, Spandex(1).head)
    assertEquals("apply with single argument has empty tail", Spandex.empty, Spandex(1).tail)
  }
  @Test
  def testHeadsAndTails(): Unit = {
    assertEquals("tail", Spandex(2, 3, 4), Spandex(1, 2, 3, 4).tail)
    assertEquals("tail tail", Spandex(3, 4), Spandex(1, 2, 3, 4).tail.tail)
    assertEquals("tail tail tail", Spandex(4), Spandex(1, 2, 3, 4).tail.tail.tail)
    assertEquals("tail tail tail tail", Spandex.empty, Spandex(1, 2, 3, 4).tail.tail.tail.tail)
    assertEquals("head", 1, Spandex(1, 2, 3, 4).head)
    assertEquals("tail head", 2, Spandex(1, 2, 3, 4).tail.head)
    assertEquals("tail tail head", 3, Spandex(1, 2, 3, 4).tail.tail.head)
    assertEquals("tail tail tail head", 4, Spandex(1, 2, 3, 4).tail.tail.tail.head)
  }
  @Test
  def testTakeAndDrop(): Unit = {
    assertEquals("take 0", Spandex.empty, Spandex(1, 2, 3, 4).take(0))
    assertEquals("take 1", Spandex(1), Spandex(1, 2, 3, 4).take(1))
    assertEquals("take 2", Spandex(1, 2), Spandex(1, 2, 3, 4).take(2))
    assertEquals("drop 0", Spandex(1, 2, 3, 4), Spandex(1, 2, 3, 4).drop(0))
    assertEquals("drop 1", Spandex(2, 3, 4), Spandex(1, 2, 3, 4).drop(1))
    assertEquals("drop 2", Spandex(3, 4), Spandex(1, 2, 3, 4).drop(2))
    assertEquals("drop 1 take 1", Spandex(2), Spandex(1, 2, 3, 4).drop(1).take(1))
    assertEquals("drop 2 take 1", Spandex(3), Spandex(1, 2, 3, 4).drop(2).take(1))
    assertEquals("drop 2 take 2", Spandex(3, 4), Spandex(1, 2, 3, 4).drop(2).take(2))
    assertEquals("drop 2 take 0", Spandex.empty, Spandex(1, 2, 3, 4).drop(2).take(0))
    assertEquals("drop 2 take 2 drop 1 take 1", Spandex(4), Spandex(1, 2, 3, 4).drop(2).take(2).drop(1).take(1))
  }
  @Test
  def testFilter(): Unit = {
    assertEquals("filter empty", Spandex.empty, Spandex().filter(_ != null))
    assertEquals("filter single inclusive", Spandex(2), Spandex(2).filter(_ > 1))
    assertEquals("filter single exclusive", Spandex(), Spandex(1).filter(_ > 1))
    assertEquals("filter multiple even", Spandex(2, 4), Spandex(1, 2, 3, 4).filter(_ % 2 == 0))
    assertEquals("filter multiple odd", Spandex(1, 3), Spandex(1, 2, 3, 4).filter(_ % 2 == 1))
  }
  @Test
  def testMap(): Unit = {
    assertEquals("map empty", Spandex.empty, Spandex().map(identity))
    assertEquals("map single", Spandex(4), Spandex(2).map(n => n * n))
    assertEquals("map multiple", Spandex(1, 4, 9, 16), Spandex(1, 2, 3, 4).map(n => n * n))
  }
  @Test
  def testFlatMap(): Unit = {
    assertEquals("flatmap empty", Spandex.empty, Spandex().flatMap(x => Spandex(x)))
    assertEquals("flatmap single", Spandex(2), Spandex(2).flatMap(n => Spandex(n)))
    assertEquals("flatmap multiple", Spandex(2, 4, 4, 16), Spandex(1, 2, 3, 4).flatMap {
      case n if n % 2 == 0 => Spandex(n, n * n)
      case _ => Spandex.empty
    })
    assertEquals("flatmap multiple reversed", Spandex(4, 16, 2, 4), Spandex(1, 2, 3, 4).reverse.flatMap {
      case n if n % 2 == 0 => Spandex(n, n * n)
      case _ => Spandex.empty
    })
  }
  @Test
  def testFill(): Unit = {
    assertEquals("fill empty", Spandex.empty, Spandex.fill(0)(9))
    assertEquals("fill single", Spandex(9), Spandex.fill(1)(9))
    assertEquals("fill mutiple", Spandex(9, 9, 9), Spandex.fill(3)(9))
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
    assertEquals(
      "prepend multiple with itself reversed",
      Spandex(-2, -1, 0, 1, 2, 3, 4, 5, 6),
      -2 +: -1 +: 0 +: Spandex(1, 2, 3, 4, 5, 6))
  }
  @Test
  def testAppend(): Unit = {
    assertEquals("append on empty", Spandex(1), Spandex.empty :+ 1)
    assertEquals("append single", Spandex(1, 2, 3, 4), Spandex(1, 2, 3) :+ 4)
    assertEquals("append multiple", Spandex(1, 2, 3, 4, 5), Spandex(1, 2, 3) :+ 4 :+ 5)
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
    assertEquals("concat empty with empty", Spandex.empty, Spandex.empty ++ Spandex.empty)
    assertEquals("concat empty with single", Spandex(1), Spandex.empty ++ Spandex(1))
    assertEquals("concat single with empty", Spandex(1), Spandex(1) ++ Spandex.empty)
    assertEquals("concat empty with multiple", Spandex(1, 2, 3), Spandex.empty ++ Spandex(1, 2, 3))
    assertEquals("concat multiple with empty", Spandex(1, 2, 3), Spandex(1, 2, 3) ++ Spandex.empty)
    assertEquals("concat single with single", Spandex(1, 2), Spandex(1) ++ Spandex(2))
    assertEquals("concat single with multiple", Spandex(1, 2, 3, 4), Spandex(1) ++ (Spandex(2) :+ 3 :+ 4))
    assertEquals("concat multiple with single", Spandex(1, 2, 3, 4), (Spandex(1) :+ 2 :+ 3) ++ Spandex(4))
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

    it = Spandex(1).iterator()
    assertTrue("single has non empty iterator", it.hasNext)
    assertEquals("single iterator has correct element", it.next(), 1)
    assertFalse("single iterator has only one element", it.hasNext)

    it = (0 +: 1 +: Spandex(2, 3, 4, 5, 6, 7, 8, 9) :+ 10 :+ 11).iterator()
    assertTrue("large has non empty iterator", it.hasNext)
    assertTrue(
      "large iterator has correct elements",
      it.sameElements(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))

    val it2 = (Spandex(1, 2, 3) :+ "kalle").iterator()
    assertTrue("any has non empty iterator", it2.hasNext)
    assertTrue(
      "any iterator has correct elements",
      it2.sameElements(List(1, 2, 3, "kalle")))

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
    assertEquals("single trimmed has elements array of correct length", 1, Spandex(1).trim().asInstanceOf[Spandex.Primary[Int]].elements.length)
    assertEquals("single trimmed has elements array with correct element", 1, Spandex(1).trim().asInstanceOf[Spandex.Primary[Int]].elements(0))
    assertEquals("multiple trimmed has elements array of correct length", 3, Spandex(1, 2, 3).trim().asInstanceOf[Spandex.Primary[Int]].elements.length)
    assertEquals("multiple trimmed has elements array with correct element", 1, Spandex(1, 2, 3).trim().asInstanceOf[Spandex.Primary[Int]].elements(0))
    assertEquals("multiple trimmed has elements array with correct element", 2, Spandex(1, 2, 3).trim().asInstanceOf[Spandex.Primary[Int]].elements(1))
    assertEquals("multiple trimmed has elements array with correct element", 3, Spandex(1, 2, 3).trim().asInstanceOf[Spandex.Primary[Int]].elements(2))
  }
}

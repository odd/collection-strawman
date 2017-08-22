package strawman
package collection
package immutable

import java.util.concurrent.atomic.AtomicLong
import scala.{Any, AnyRef, Array, IllegalArgumentException, IndexOutOfBoundsException, ArrayIndexOutOfBoundsException, Boolean, Int, Long, NoSuchElementException, UnsupportedOperationException, Nothing, StringContext, Unit, `inline`}
import scala.Predef.{String, genericWrapArray, intWrapper, longWrapper, <:<}
import scala.math
import scala.math._
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import strawman.collection.{IterableFactory, IterableOnce, Iterator, LinearSeq, View}
import strawman.collection.mutable.{ArrayBuffer, Builder, GrowableBuilder}

/**
  * A <i>Spandex</i> is an (ostensible) immutable array-like collection designed to support the following performance characteristics:
  * <ul>
  * <li>constant time <code>head</code>, <code>last</code>, <code>tail</code>, <code>init</code>, <code>take</code>, <code>takeRight</code>, <code>drop</code>, <code>dropRight</code>, <code>slice</code> and <code>reverse</code></li>
  * <li>amortised constant time <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and <code>concat</code> (depending on the complexity of <code>java.lang.System.arraycopy</code>)</li>
  * <li>efficient indexed access</li>
  * <li>linear time iteration (i.e. <code>iterator</code> and <code>foreach</code>) with low constant factors
  * <li>reasonable memory usage (approximately double that of an array with the same number of elements)
  * </ul>
  * <br/>
  * Elements can be added to both the front and the rear of the underlying array (via <code>prepend</code>/<code>prependAll</code>
  * and <code>append</code>/<code>appendAll</code>/<code>concat</code> respectively), but never more than once for any given array slot
  * (any later modification attempts to an already modified slot results in a copy being made of the underlying array).
  * <br/>
  * <br/>
  * To guarantee that only a single thread can write to an array slot an atomic long is used to guard the low and high index
  * assignments.
  * <br/>
  * <br/>
  * The smallest array size is eight and when the underlying array is too small to fit a requested addition a new array twice as large is
  * allocated.
  * <br/>
  * <br/>
  * A configurable auto shrink threshold percentage (default is 25%) can be configured via the <code>withAutoShrinking</code> method.
  * When this threshold percentage is set to <code>T</code> (an integer between 0 and 100), any operation having a result size which
  * is at most <code>T</code> percent of the capacity of its primary will lead to the underlying array being copied to a new array
  * having a capacity suitable to the size of the result. The same effect can be obtained on a <code>Spandex</code> having <code>T</code>
  * set to zero (i.e. auto shrinking disabled) and instead calling <code>trim</code> on the result.
  * <br/>
  * <br/>
  * <b>Examples</b>
  * <br/>
  <pre><code>
    // expression                                               (array, start, stop)
    Spandex()                                               ==> ([], 0, 0)
    Spandex() :+ a                                          ==> ([a, _, _, _, _, _, _, _], 0, 1)
    a +: Spandex()                                          ==> ([_, _, _, _, _, _, _, a], -1, 0)
    Spandex() :++ Seq(a, b, c, d)                           ==> ([a, b, c, d, _, _, _, _], 0, 4)
    Seq(a, b, c, d) ++: Spandex()                           ==> ([_, _, _, _, a, b, c, d], -4, 0)
    Seq(a, b, c, d) ++: Spandex() :++ Seq(e, f, g, h)       ==> ([e, f, g, h, a, b, c, d], -4, 4)
    Spandex(a, b, c, d, e, f, g, h)                         ==> ([a, b, c, d, e, f, g, h], 0, 8)
    Spandex() :++ Seq(a, b, c, d, e, f, g, h)               ==> ([a, b, c, d, e, f, g, h], 0, 8)
    Seq(a, b, c, d, e, f, g, h) ++: Spandex()               ==> ([a, b, c, d, e, f, g, h], -8, 0)
    (Seq(a, b, c, d) ++: Spandex()).slice(1, 3)             ==> ([_, _, _, _, a, b, c, d], -3, -1)
    (Spandex() :++ Seq(a, b, c, d)).slice(1, 3)             ==> ([a, b, c, d, _, _, _, _], 1, 3)
    (Seq(a, b) ++: Spandex() :++ Seq(c, d)).slice(1, 3)     ==> ([c, d, _, _, _, _, a, b], -1, 1)
  </code></pre>
  */
sealed abstract class Spandex[+A] private (protected val start: Int, protected val stop: Int)
  extends Seq[A]
    //with LinearSeq[A]
    with IndexedSeq[A]
    //with LinearSeqOps[A, Spandex, Spandex[A]]
    with IndexedSeqOps[A, Spandex, Spandex[A]]
    with StrictOptimizedSeqOps[A, Spandex, Spandex[A]] {

  def toDebugString: String

  override def toString = toDebugString // Should be removed once in production

  // Could be private but is not to allow for access from test cases
  private[immutable] def primary: Spandex.Primary[A]

  protected def elements: Array[Any] = primary.elements
  protected final def capacity: Int = elements.length

  //@`inline` // Causes problems with errors and eternal inlining when running test cases on Dotty
  private final def isReversed: Boolean = start > stop
  private final def nonReversed: Boolean = start <= stop

  @`inline`
  protected final def element(i: Int): A =
    ScalaRunTime.array_apply(elements, i).asInstanceOf[A]

  @`inline`
  protected final def index(i: Int): Int =
    if (i < 0) capacity + i
    else i

  @`inline`
  private[this] final def position(i: Int): Int =
    if (isReversed) index(start - i - 1)
    else index(start + i)

  @`inline`
  private[this] final def fetch(i: Int): A = element(position(i))

  /**
    * The maximum <code>size / capacity</code> percentage threshold allowed before auto shrinking.
    * A value <code>&lt;= 0</code> means no auto shrinking will take place, a value <code>&gt;= 100</code> means all returned collections will be auto
    * shrunk (to their closest matching capacity).
    */
  def shrinkThreshold: Int = Spandex.DefaultAutoShrinkThreshold

  /**
    * Returns a <code>Spandex</code> that uses the specified auto shrinking percentage threshold.
    * @param threshold the maximum <code>size / capacity</code> percentage (defaults to <code>DefaultAutoShrinkingThreshold</code>).
    */
  def withAutoShrinking(threshold: Int = Spandex.DefaultAutoShrinkThreshold): Spandex[A] =
    if (threshold == Spandex.DefaultAutoShrinkThreshold) this
    else Spandex.createSecondary(this, start, stop, threshold)

  /**
    * Returns a <code>Spandex</code> that uses no auto shrinking (has an auto shrinking percentage threshold of <code>0</code>).
    */
  def withoutAutoShrinking: Spandex[A] = withAutoShrinking(0)

  override final def length = size

  override final def size: Int = Spandex.distance(start, stop)

  override final def knownSize: Int = size

  override final def isEmpty: Boolean = start == 0 && stop == 0

  override final def nonEmpty: Boolean = !isEmpty

  override final def apply(i: Int): A =
    if (i < 0 || i >= size) throw new ArrayIndexOutOfBoundsException(i)
    else fetch(i)

  override final def slice(from: Int, until: Int): Spandex[A] =
    if (until <= from) Spandex.empty
    else {
      val f = (from max 0) max 0
      val u = (until min size) max 0
      if (isReversed) Spandex.createSecondary(primary, start - f, start - u, shrinkThreshold)
      else Spandex.createSecondary(primary, start + f, start + u, shrinkThreshold)
    }

  override final def take(n: Int): Spandex[A] =
    slice(0, (n min size) max 0)

  override final def takeRight(n: Int): Spandex[A] =
    slice(size - (n min size) max 0, size)

  override final def drop(n: Int): Spandex[A] =
    slice(n, size)

  override final def dropRight(n: Int): Spandex[A] = {
    val l = size
    slice(0, l - (n min l))
  }

  override def span(p: A => Boolean): (Spandex[A], Spandex[A]) = {
    val prefix = takeWhile(p)
    val suffix = drop(prefix.size)
    (prefix, suffix)
  }

  override final def head: A =
    if (isEmpty) throw new UnsupportedOperationException
    else fetch(0)

  override final def tail: Spandex[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else drop(1)

  override final def last: A =
    if (isEmpty) throw new UnsupportedOperationException
    else fetch(size - 1)

  override final def init: Spandex[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else dropRight(1)

  override def padTo[B >: A](len: Int, elem: B): Spandex[B] =
    if (len <= size) this
    else appendAll1(Iterator.continually(elem).take(len - size))

  override final def prepend[B >: A](elem: B): Spandex[B] =
    if (isEmpty) Spandex(elem)
    else if (nonReversed && primary.prependElement(elem, start)) Spandex.createSecondary(primary, start - 1, stop, shrinkThreshold)
    else if (isReversed && primary.appendElement(elem, start)) Spandex.createSecondary(primary, start + 1, stop, shrinkThreshold) // as it is reversed start is really stop and stop is really start
    else elem +: Spandex.grow(this, Spandex.capacitate((size - 1) * 2))

  override final def append[B >: A](elem: B): Spandex[B] =
    if (isEmpty) Spandex(elem)
    else if (nonReversed && primary.appendElement(elem, stop)) Spandex.createSecondary(primary, start, stop + 1, shrinkThreshold)
    else if (isReversed && primary.prependElement(elem, stop)) Spandex.createSecondary(primary, start, stop - 1, shrinkThreshold)  // as it is reversed start is really stop and stop is really start
    else Spandex.grow(this, Spandex.capacitate((size - 1) * 2)) :+ elem

  override final def prependAll[B >: A](xs: collection.Iterable[B]): Spandex[B] = prependAll1(xs)

  private final def prependAll1[B >: A](xs: collection.IterableOnce[B]): Spandex[B] =
    xs match {
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty =>
        this
      case that: Iterable[B] if that.knownSize == 1 =>
        prepend(that.head)
      case that: Spandex[B] if this.isReversed || that.isReversed =>
        Spandex.fromIterable(View.Concat(that, this), this.knownSize + that.knownSize)
      case that: Spandex[B] if that.primary.appendElements(this, that.stop) =>
        Spandex.createSecondary(that.primary, that.start, that.stop + this.size, shrinkThreshold)
      case that: Spandex[B] if this.primary.prependElements(that, this.start) =>
        Spandex.createSecondary(this.primary, this.start - that.size, this.stop, shrinkThreshold)
      case that: Spandex[B] =>
        Spandex.grow(this, Spandex.capacitate(this.size + that.size)).prependAll(that)
      case that: Iterable[B] if that.knownSize > -1 =>
        Spandex.fromIterable(View.Concat(that, this), that.knownSize + this.knownSize)
      case _ if this.isEmpty =>
        Spandex.fromIterable(xs)
      case _ =>
        prependAll1(Spandex.fromIterable(xs))
    }

  override final def appendAll[B >: A](xs: collection.Iterable[B]): Spandex[B] = appendAll1(xs)

  private final def appendAll1[B >: A](xs: collection.IterableOnce[B]): Spandex[B] =
    xs match {
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty =>
        this
      case that: Iterable[B] if that.knownSize == 1 =>
        append(that.head)
      case that: Spandex[B] if this.isReversed || that.isReversed =>
        Spandex.fromIterable(View.Concat(this, that), this.knownSize + that.knownSize)
      case that: Spandex[B] if that.primary.prependElements(this, that.start) =>
        Spandex.createSecondary(that.primary, that.start - this.size, that.stop, shrinkThreshold)
      case that: Spandex[B] if this.primary.appendElements(that, this.stop) =>
        Spandex.createSecondary(this.primary, this.start, this.stop + that.size, shrinkThreshold)
      case that: Spandex[B] =>
        Spandex.grow(this, Spandex.capacitate(this.size + that.size)).appendAll(that)
      case that: Iterable[B] if that.knownSize > -1 =>
        Spandex.fromIterable(View.Concat(this, that), this.knownSize + that.knownSize)
      case _ if this.isEmpty =>
        Spandex.fromIterable(xs)
      case _ =>
        appendAll1(Spandex.fromIterable(xs))
    }

  override final def map[B](f: A => B): Spandex[B] =
    if (isEmpty) Spandex.empty
    else {
      val capacity = Spandex.capacitate(size)
      val array = new Array[Any](capacity)
      var i = size - 1
      while (i >= 0) {
        val j = position(i)
        array(j) = f(element(j))
        i -= 1
      }
      Spandex.createPrimary[B](array, start, stop).withAutoShrinking(shrinkThreshold)
    }

  override final def zip[B](xs: collection.Iterable[B]): Spandex[(A, B)] = xs match {
    case that: Spandex[B] =>
      Spandex.tabulate(this.size min that.size) { i =>
        (this.apply(i), that.apply(i))
      }
    case _ =>
      fromIterable[(A, B)](View.Zip[A, B](coll, xs), xs.knownSize)
  }

  override def unzip[A1, A2](implicit asPair: <:<[A, (A1, A2)]): (Spandex[A1], Spandex[A2]) = {
    val arr1 = new Array[Any](capacity)
    val arr2 = new Array[Any](capacity)
    var i = 0
    val sz = size
    while (i < sz) {
      val (a1, a2) = asPair(fetch(i))
      arr1(i) = a1
      arr2(i) = a2
      i += 1
    }
    (Spandex.createPrimary(arr1, 0, sz).withAutoShrinking(shrinkThreshold), Spandex.createPrimary(arr2, 0, sz).withAutoShrinking(shrinkThreshold))
  }

  override final def iterator(): Iterator[A] =
    new Iterator[A] {
      private[this] final val n = Spandex.this.size
      private[this] var i = 0
      override final def hasNext: Boolean = i < n
      override final def next(): A = {
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")
        val elem = fetch(i)
        i += 1
        elem
      }
    }

  override final def foreach[U](f: (A) => U): Unit = {
      var i = 0
      val n = size
      while (i < n) {
        f(fetch(i))
        i += 1
      }
    }

  override final def reverse: Spandex[A] =
    if (isEmpty) Spandex.empty.withAutoShrinking(shrinkThreshold)
    else if (size == 1) this
    else Spandex.createSecondary(primary, stop, start, shrinkThreshold)

  private[Spandex] final def withReversed(reversed: Boolean): Spandex[A] =
    if (!reversed) this
    else reverse

  override protected[this] def reversed: Spandex[A] = reverse

  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var acc = z
    foreach(x => acc = op(acc, x))
    acc
  }

  override final def foldRight[B](z: B)(op: (A, B) => B): B =
    reverse.foldLeft(z) {
      case (acc, x) => op(x, acc)
    }

  override final def indexWhere(p: (A) => Boolean, from: Int = 0): Int = {
    var i = from
    while (i < size) {
      if (p(fetch(i))) return i
      i += 1
    }
    -1
  }

  override final def updated[B >: A](i: Int, elem: B): Spandex[B] = {
    if (i < 0 || i >= size) throw new IndexOutOfBoundsException(i.toString)
    else if (Spandex.isSame(elem, element(position(i)))) this
    else {
      val array = new Array[Any](capacity)
      java.lang.System.arraycopy(elements, 0, array, 0, capacity)
      array.update(position(i), elem)
      Spandex.createPrimary(array, start, stop).withAutoShrinking(shrinkThreshold)
    }
  }

  override final def patch[B >: A](from: Int, xs: IterableOnce[B], replaced: Int = 0): Spandex[B] = {
    if (from < 0 || from > size) throw new IndexOutOfBoundsException(from.toString)
    else if (from == 0 && (replaced max 0) == 0) prependAll1(xs)
    else if (from == 0) coll.drop(replaced).prependAll1(xs)
    else if (from == size) coll.appendAll1(xs)
    else coll.take(from).appendAll1(xs).appendAll1(coll.slice(from + replaced, size))
  }

  override final def toArray[B >: A: ClassTag]: Array[B] =
    copyToArray(new Array[B](this.size))

  override final def copyToArray[B >: A](array: Array[B], start: Int): array.type =
    if (start < 0 || start > size)
      throw new ArrayIndexOutOfBoundsException(start)
    else if (isReversed) super.copyToArray(array, start)
    else {
      val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = Spandex.indices(start, size, size)
      if (frontStopIndex > 0)
        java.lang.System.arraycopy(elements, frontStartIndex, array, 0, frontStopIndex - frontStartIndex)
      if (rearStopIndex > 0)
        java.lang.System.arraycopy(elements, rearStartIndex, array, frontStopIndex - frontStartIndex, rearStopIndex - rearStartIndex)
      array
    }

  final def grow(size: Int, padded: Boolean = true): Spandex[A] = resize(size, padded)

  final def shrink(padded: Boolean = true): Spandex[A] = resize(this.size, padded)

  final def trim(): Spandex[A] = shrink(false)

  final def resize(size: Int, padded: Boolean = true): Spandex[A] =
    if (size == 0) Spandex.Empty
    else if (size == capacity) this
    else if (size < this.size) throw new IllegalArgumentException(s"Specified size of $size is smaller than the current size ${this.size}.")
    else {
      val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = Spandex.indices(start min stop, start max stop, this.size)
      Spandex.resize(this, if (padded) Spandex.capacitate(size) else size, frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex)
    }

  protected[this] final def fromIterable[B](c: collection.Iterable[B], size: Int = -1): Spandex[B] = Spandex.fromIterable(c, size)

  override def iterableFactory: SeqFactory[Spandex] = Spandex

  protected[this] final def fromSpecificIterable(coll: collection.Iterable[A]): Spandex[A] = fromIterable(coll)

  override protected[this] def newSpecificBuilder(): Builder[A, Spandex[A]] = Spandex.newBuilder()

  override final def className = "Spandex"
}

object Spandex extends SeqFactory[Spandex] {
  /**
    * The default maximum <code>size / capacity</code> percentage threshold allowed before auto shrinking is <code>25</code>.
    */
  final val DefaultAutoShrinkThreshold = 25

  private[immutable] class Primary[+A](
    override val elements: scala.Array[Any],
    x: Int,
    y: Int)
    extends Spandex[A](x, y) {

    def toDebugString: String = s"${mkString("[", ", ", "]")}: Primary(start: $start, stop: $stop, elements: ${elements.toList.zipWithIndex.map(t => s"${t._2}: ${t._1}").mkString("[", ", ", "]").replace("null", "_")})"

    private[this] val bounds = new AtomicLong(pack(start, stop))

    @`inline`
    private[this] def pack(low: Int, high: Int): Long = low.toLong << 32 | (high & 0xffffffffL)
    @`inline`
    private[this] def unpackLow(l: Long): Int = (l >> 32).toInt
    @`inline`
    private[this] def unpackHigh(l: Long): Int = l.toInt

    private[Spandex] def prependElement[B >: A](elem: B, start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      if (Spandex.distance(low, high) == capacity) false
      else if (low < start && isSame(elem, element(index(start - 1)))) true
      else if (low > -capacity && low == start && bounds.compareAndSet(bnds, pack(low - 1, high))) {
        elements(index(low - 1)) = elem
        true
      } else false
    }

    private[Spandex] final def prependElements[B >: A](that: Spandex[B], start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val lowered = low - that.size
      if (Spandex.distance(lowered, high) > capacity) false
      else if (low == start && lowered >= -capacity && bounds.compareAndSet(bnds, pack(lowered, high))) {
        val (sourceFrontStartIndex, sourceFrontStopIndex, sourceRearStartIndex, sourceRearStopIndex) = Spandex.indices(that.start, that.stop, that.capacity)
        if (sourceFrontStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceFrontStartIndex, this.elements, index(lowered), sourceFrontStopIndex - sourceFrontStartIndex)
        if (sourceRearStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceRearStartIndex, this.elements, index(lowered) + sourceFrontStopIndex - sourceFrontStartIndex, sourceRearStopIndex - sourceRearStartIndex)
        true
      } else false
    }

    private[Spandex] def appendElement[B >: A](elem: B, stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      if (Spandex.distance(low, high) == capacity) false
      else if (high > stop && isSame(elem, element(index(stop)))) true
      else if (high < capacity && high == stop && bounds.compareAndSet(bnds, pack(low, high + 1))) {
        elements(index(high)) = elem
        true
      } else false
    }

    private[Spandex] final def appendElements[B >: A](that: Spandex[B], stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val raised = high + that.size
      if (Spandex.distance(low, raised) > capacity) false
      else if (high == stop && raised <= capacity && bounds.compareAndSet(bnds, pack(low, raised))) {
        val (sourceFrontStartIndex, sourceFrontStopIndex, sourceRearStartIndex, sourceRearStopIndex) = Spandex.indices(that.start, that.stop, that.capacity)
        if (sourceFrontStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceFrontStartIndex, this.elements, high, sourceFrontStopIndex - sourceFrontStartIndex)
        if (sourceRearStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceRearStartIndex, this.elements, high + sourceFrontStopIndex - sourceFrontStartIndex, sourceRearStopIndex - sourceRearStartIndex)
        true
      } else false
    }

    override private[immutable] final def primary: Primary[A] = this
  }

  private[immutable] final class Secondary[+A](
    override val primary: Primary[A],
    override val start: Int,
    override val stop: Int,
    override val shrinkThreshold: Int)
    extends Spandex[A](start, stop) {
    def toDebugString: String = s"${mkString("[", ", ", "]")}: Secondary(start: $start, stop: $stop, shrinkThreshold: $shrinkThreshold, primary: ${primary.toDebugString})"
  }

  private[immutable] final object Empty extends Primary[Nothing](Array.empty, 0, 0) {
    override def toDebugString: String = s"[]: Empty"
  }

  override def apply[A](xs: A*): Spandex[A] =
    if (xs.isEmpty) Spandex.Empty
    else {
      val n = xs.size
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      xs.copyToArray(array, 0, n)
      createPrimary(array, 0, n)
    }

  def apply[A](it: Iterable[A]): Spandex[A] = Spandex.fromIterable(it)

  def apply[A](array: Array[A]): Spandex[A] = {
    if (array.length == 0) Spandex.empty
    else {
      val capacity = capacitate(array.length)
      val arr = new Array[Any](capacity)
      java.lang.System.arraycopy(array, 0, arr, 0, array.length)
      createPrimary(arr, 0, array.length)
    }
  }

  /*
  private[Spandex] def create[A](array: Array[Any], start: Int, stop: Int, shrinkThreshold: Int): Spandex[A] =
    if (shrinkThreshold == 0) new Primary[A](array, start, stop)
    else new Primary[A](array, start, stop).withAutoShrinking(shrinkThreshold)

  private[Spandex] def create[A](primary: Primary[A], start: Int, stop: Int, shrinkThreshold: Int): Spandex[A] = {
    if (start == stop) Spandex.empty
    else if (isExcessive(primary, start, stop, shrinkThreshold))
      shrink(primary, start, stop, shrinkThreshold)
    else
      new Secondary[A](primary, start, stop, shrinkThreshold)
  }
  */

  private[Spandex] def createPrimary[A](array: Array[Any], start: Int, stop: Int): Primary[A] = new Primary[A](array, start, stop)

  private[Spandex] def createSecondary[A](xs: Spandex[A], start: Int, stop: Int, shrinkThreshold: Int): Spandex[A] = {
    if (start == stop) Empty.withAutoShrinking(shrinkThreshold)
    else if (isExcessive(xs, start, stop, shrinkThreshold))
      shrink(xs, start, stop)
    else
      new Secondary[A](xs.primary, start, stop, shrinkThreshold)
  }

  @`inline` private def isExcessive[A](xs: Spandex[A], start: Int, stop: Int, shrinkThreshold: Int) = {
    xs.capacity > 8 && (capacitate(distance(start, stop)) * 100) / xs.capacity <= shrinkThreshold
  }

  private[Spandex] def capacitate(n: Long): Int = ((((n max 8) + 7) / 8) * 8).toInt
  private[Spandex] def grow[A](xs: Spandex[A], capacity: Int) = {
    val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = Spandex.indices(xs.start min xs.stop, xs.start max xs.stop, xs.capacity)
    resize(xs, capacity, frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex)
  }
  private[Spandex] def shrink[A](xs: Spandex[A], start: Int, stop: Int) = {
    val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = Spandex.indices(start min stop, start max stop, xs.capacity)
    resize(xs, capacitate(distance(start, stop)), frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex)
  }
  private[Spandex] def resize[A](xs: Spandex[A], capacity: Int, frontStartIndex: Int, frontStopIndex: Int, rearStartIndex: Int, rearStopIndex: Int) = {
    val array = new Array[Any](capacity)
    val start = frontStopIndex - frontStartIndex
    val stop = rearStopIndex - rearStartIndex
    if (rearStopIndex > 0)
      java.lang.System.arraycopy(xs.elements, rearStartIndex, array, 0, stop)
    if (frontStopIndex > 0)
      java.lang.System.arraycopy(xs.elements, frontStartIndex, array, array.length - start, start)
    // Can't use createSecondary here since it will autoShrink
    new Secondary[A](createPrimary[A](array, -start, stop), -start, stop, xs.shrinkThreshold).withReversed(xs.isReversed)
  }

  private[Spandex] def isSame(a: Any, b: Any) = (
    (a.isInstanceOf[AnyRef] && a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef]))
    || (!a.isInstanceOf[AnyRef] && a == b))

  override def tabulate[A](n: Int)(f: Int => A): Spandex[A] =
    if (n == 0) Empty
    else {
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      var i = 0
      while (i < n) {
        array(i) = f(i)
        i += 1
      }
      createPrimary(array, 0, n)
    }

  override def fill[A](n: Int)(elem: => A): Spandex[A] = tabulate(n)(_ => elem)

  def fromIterable[A](it: collection.Iterable[A]): Spandex[A] = fromIterable(it, -1)

  def fromIterable[A](it: collection.IterableOnce[A], knownSize: Int = -1): Spandex[A] = it match {
    case that: Spandex[A] ⇒ that
    case that: Iterable[A] if knownSize == 0 || that.knownSize == 0 || that.isEmpty => Empty
    case that: Iterable[A] if knownSize > 0 || that.knownSize > 0 =>
      val n = math.max(knownSize, that.knownSize)
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      var i = 0
      val it = that.iterator()
      while (i < n) {
        array(i) = it.next()
        i += 1
      }
      createPrimary(array, 0, n)
    case _ =>
      val builder = Spandex.newBuilder[A]()
      builder ++= it
      builder.result()
  }

  /**
    * Used to wrap an existing array (without copying). Observe that later modifications to the specified array will show
    * through to the returned instance.
    * @param xs array to use for elements
    * @param length the number of elements present in the array (defaults to -1 which will use the array length)
    * @tparam A the element type to use
    * @return a new instance using the specified array as element storage (or the empty Spandex if the array is empty).
    */
  private[strawman] def wrap[A](xs: Array[Any], length: Int = -1): Spandex[A] =
    if (xs.length == 0) Empty
    else createPrimary(xs, 0, if (length > -1) length else xs.length)

  def newBuilder[A](): Builder[A, Spandex[A]] =
    new GrowableBuilder(ArrayBuffer.empty[A])
      .mapResult { b =>
        val array = new Array[Any](capacitate(b.length))
        b.asInstanceOf[ArrayBuffer[Any]].copyToArray(array)
        wrap(array, b.length)
      }

  override def empty[A <: Any]: Spandex[A] = Empty

  /*
  @`inline`
  private[Spandex] def frontIndexes(start: Int, stop: Int, capacity: Int) = {
    if (start < 0 && stop <= 0) (capacity + start, capacity + stop)
    else if (start < 0) (capacity + start, capacity)
    else if (stop < 0) (capacity + stop, capacity)
    else (0, 0)
  }

  @`inline`
  private[Spandex] def rearIndexes(start: Int, stop: Int) = {
    if (start >= 0 && stop > 0) (start, stop)
    else if (stop > 0) (0, stop)
    else if (start > 0) (0, start)
    else (0, 0)
  }
  */

  @`inline`
  private[Spandex] def indices(start: Int, stop: Int, capacity: Int): (Int, Int, Int, Int) = {
    if (start < 0 && stop <= 0) (capacity + start, capacity + stop, 0, 0)
    else if (start < 0) (capacity + start, capacity, 0, stop)
    else if (stop < 0) (capacity + stop, capacity, 0, start)
    else (0, 0, start, stop)
  }

  @`inline`
  private[Spandex] final def distance(x: Int, y: Int): Int =
    if (x < y) y - x
    else x - y
}

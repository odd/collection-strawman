package strawman
package collection
package immutable

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.{Any, AnyRef, Array, ArrayIndexOutOfBoundsException, Boolean, Int, Long, NoSuchElementException, UnsupportedOperationException, Nothing, StringContext, Unit, `inline`}
import scala.Predef.{String, genericWrapArray, intWrapper}
import scala.math
import scala.math._
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import strawman.collection.{IterableFactory, IterableOnce, Iterator, LinearSeq, View}
import strawman.collection.mutable.{ArrayBuffer, Builder, GrowableBuilder}

/**
  * A <i>Spandex_Z</i> is an (ostensible) immutable array-like collection designed to support the following performance characteristics:
  * <ul>
  * <li>constant time <code>head</code>, <code>last</code>, <code>tail</code>, <code>init</code>, <code>take</code>, <code>takeRight</code>, <code>drop</code>, <code>dropRight</code>, <code>slice</code> and <code>reverse</code></li>
  * <li>amortised constant time <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and <code>concat</code> (depending on the complexity of <code>java.lang.System.arraycopy</code>)</li>
  * <li>efficient indexed access</li>
  * <li>linear time iteration (i.e. <code>iterator</code> and <code>foreach</code>) with low constant factors
  * <li>reasonable memory usage (approximately double that of an array with the same number of elements)
  * </ul>
  * <br/>
  * To allow efficient prepend/prependAll (in addition to append/appendAll)
  * The underlying array is only mutated on <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and
  * <code>concat</code> but never more than once for any given position (any later modification attempts to an already modified position
  * results in a copy being made of the underlying array).
  * <br/>
  * <br/>
  * To guarantee that only a single thread can write to an array slot a pair of atomic integers are used to guard the low and high
  * assignments in <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and <code>concat</code>.
  * <br/>
  * <br/>
  * Expansion occurs when the underlying array is to small to allow the specified element or elements to be added.
  *
  * <h1>Exsamples</h1>
  * Spandex()                                               ==> ([], 0, 0)
  * Spandex() :+ a                                          ==> ([a, _, _, _, _, _, _, _], 0, 1)
  * a +: Spandex()                                          ==> ([_, _, _, _, _, _, _, a], -1, 0)
  * Spandex() :++ Seq(a, b, c, d)                           ==> ([a, b, c, d, _, _, _, _], 0, 4)
  * Seq(a, b, c, d) ++: Spandex()                           ==> ([_, _, _, _, a, b, c, d], -4, 0)
  * Seq(a, b, c, d) ++: Spandex() :++ Seq(e, f, g, h)       ==> ([e, f, g, h, a, b, c, d], -4, 4)
  * Spandex(a, b, c, d, e, f, g, h)                         ==> ([a, b, c, d, e, f, g, h], 0, 8)
  * Spandex() :++ Seq(a, b, c, d, e, f, g, h)               ==> ([a, b, c, d, e, f, g, h], 0, 8)
  * Seq(a, b, c, d, e, f, g, h) ++: Spandex()               ==> ([a, b, c, d, e, f, g, h], -8, 0)
  * (Seq(a, b, c, d) ++: Spandex()).slice(1, 3)             ==> ([_, _, _, _, a, b, c, d], -3, -1)
  * (Spandex() :++ Seq(a, b, c, d)).slice(1, 3)             ==> ([a, b, c, d, _, _, _, _], 1, 3)
  * (Seq(a, b) ++: Spandex() :++ Seq(c, d)).slice(1, 3)     ==> ([c, d, _, _, _, _, a, b], -1, 1)
  */
sealed abstract class Spandex[+A] private (protected val start: Int, protected val stop: Int)
  extends Seq[A]
    with LinearSeq[A]
    with IndexedSeq[A]
    with LinearSeqOps[A, Spandex, Spandex[A]]
    with IndexedSeqOps[A, Spandex, Spandex[A]]
    with StrictOptimizedIterableOps[A, Spandex[A]] {

  def toDebugString: String

  override def toString = toDebugString

  // Could be private but for test case access
  private[immutable] def primary: Spandex.Primary[A]

  protected def elements: Array[Any] = primary.elements

  protected def capacity: Int = elements.length

  //@`inline` // Causes problems with errors and eternal inlining when running test cases on Dotty
  private def isReversed: Boolean = start > stop

  @`inline`
  private def frontIndexes = {
    if (start < 0 && stop <= 0) (capacity + start, capacity + stop)
    else if (start < 0) (capacity + start, capacity)
    else if (stop < 0) (capacity + stop, capacity)
    else (0, 0)
  }

  @`inline`
  private def rearIndexes = {
    if (start >= 0 && stop > 0) (start, stop)
    else if (stop > 0) (0, stop)
    else if (start > 0) (0, start)
    else (0, 0)
  }

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

  override final def length: Int = Spandex.distance(start, stop)

  override final def size: Int = length

  override final def knownSize: Int = length

  override final def isEmpty: Boolean = start == 0 && stop == 0

  override final def nonEmpty: Boolean = !isEmpty

  override final def apply(i: Int): A =
    if (i < 0 || i >= length) throw new ArrayIndexOutOfBoundsException(i)
    else fetch(i)

  override final def slice(from: Int, until: Int): Spandex[A] =
    if (until <= from) Spandex.empty
    else {
      val f = (from max 0) max 0
      val u = (until min length) max 0
      if (isReversed) new Spandex.Secondary[A](primary, start - f, start - u)
      else new Spandex.Secondary[A](primary, start + f, start + u)
    }

  override final def take(n: Int): Spandex[A] =
    slice(0, (n min length) max 0)

  override final def takeRight(n: Int): Spandex[A] =
    slice(length - (n min length) max 0, length)

  override final def drop(n: Int): Spandex[A] =
    slice(n, length)

  override final def dropRight(n: Int): Spandex[A] = {
    val l = length
    slice(0, l - (n min l))
  }

  override final def head: A =
    if (isEmpty) throw new UnsupportedOperationException
    else fetch(0)

  override final def tail: Spandex[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else drop(1)

  override final def last: A =
    if (isEmpty) throw new UnsupportedOperationException
    else fetch(length - 1)

  override final def init: Spandex[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else dropRight(1)

  final def trim(): Spandex[A] =
    if (length == 0) Spandex.Empty
    else if (length == capacity) this
    else resize(length)

  private def resize(capacity: Int) = {
    val array = new Array[Any](capacity)
    val (frontStartIndex, frontStopIndex) = frontIndexes
    val (rearStartIndex, rearStopIndex) = rearIndexes
    if (rearStopIndex > 0) java.lang.System.arraycopy(elements, rearStartIndex, array, 0, rearStopIndex - rearStartIndex)
    if (frontStopIndex > 0) java.lang.System.arraycopy(elements, frontStartIndex, array, array.length - (frontStopIndex - frontStartIndex), frontStopIndex - frontStartIndex)
    if (isReversed) new Spandex.Primary[A](array, -(frontStopIndex - frontStartIndex), rearStopIndex - rearStartIndex).reverse
    else new Spandex.Primary[A](array, -(frontStopIndex - frontStartIndex), rearStopIndex - rearStartIndex)
  }

  override final def prepend[B >: A](elem: B): Spandex[B] =
    if (isEmpty) Spandex(elem)
    else if (isReversed && primary.appendElement(elem, start))
      new Spandex.Secondary[B](primary, start + 1, stop)
    else if (!isReversed && primary.prependElement(elem, start))
      new Spandex.Secondary[B](primary, start - 1, stop)
    else elem +: resize(Spandex.capacitate((length - 1) * 2))

  override final def append[B >: A](elem: B): Spandex[B] =
    if (isEmpty) Spandex(elem)
    else if (isReversed && primary.prependElement(elem, stop))
      new Spandex.Secondary[B](primary, start, stop - 1)
    else if (!isReversed && primary.appendElement(elem, stop))
      new Spandex.Secondary[B](primary, start, stop + 1)
    else resize(Spandex.capacitate((length - 1) * 2)) :+ elem

  /** Alias for `prependAll` */
  @`inline` final def ++:[B >: A](xs: IterableOnce[B]): Spandex[B] = prependAll(xs)

  final def prependAll[B >: A](xs: IterableOnce[B]): Spandex[B] =
    xs match {
      case _ if this.isEmpty => Spandex.fromIterable(xs)
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty => this
      case that: Spandex[B] if this.isReversed || that.isReversed => fromIterable(View.Concat(that, coll))
      case that: Spandex[B]
        if that.primary.appendElements(this, that.stop) =>
        new Spandex.Secondary[B](that.primary, that.start, that.stop + this.length)
      case that: Spandex[B]
        if this.primary.prependElements(that, this.start) =>
        new Spandex.Secondary[B](this.primary, this.start - that.length, this.stop)
      case that: Spandex[B] => resize(Spandex.capacitate(this.size + that.size)).prependAll(that)
      case _ => Spandex.fromIterable(xs).concat(this)
    }

  /** Alias for `appendAll` */
  @`inline` final def :++[B >: A](xs: IterableOnce[B]): Spandex[B] = appendAll(xs)

  /** Alias for `concat` */
  @`inline` final def appendAll[B >: A](xs: IterableOnce[B]): Spandex[B] = concat(xs)

  override final def concat[B >: A](xs: IterableOnce[B]): Spandex[B] =
    xs match {
      case _ if this.isEmpty => Spandex.fromIterable(xs)
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty => this
      case that: Spandex[B] if this.isReversed || that.isReversed => fromIterable(View.Concat(coll, that))
      case that: Spandex[B]
        if that.primary.prependElements(this, that.start) =>
        new Spandex.Secondary[B](that.primary, that.start - this.length, that.stop)
      case that: Spandex[B]
        if this.primary.appendElements(that, this.stop) =>
        new Spandex.Secondary[B](this.primary, this.start, this.stop + that.length)
      case that: Spandex[B] => resize(Spandex.capacitate(this.size + that.size)).concat(that)
      case _ => concat(Spandex.fromIterable(xs))
    }

  override final def map[B](f: A => B): Spandex[B] =
    if (isEmpty) Spandex.empty
    else {
      val capacity = Spandex.capacitate(length)
      val array = new Array[Any](capacity)
      var i = length - 1
      while (i >= 0) {
        val j = position(i)
        array(j) = f(element(j))
        i -= 1
      }
      new Spandex.Primary[B](array, start, stop)
    }

  override final def zip[B](xs: IterableOnce[B]): Spandex[(A, B)] = xs match {
    case that: Spandex[B] =>
      Spandex.tabulate(this.length min that.length) { i =>
        (this.apply(i), that.apply(i))
      }
    case _ => fromIterable[(A, B)](View.Zip[A, B](coll, xs))
  }

  override final def iterator(): Iterator[A] =
    new Iterator[A] {
      private[this] final val n = Spandex.this.length
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
      val n = length
      while (i < n) {
        f(fetch(i))
        i += 1
      }
    }

  override final def reverse: Spandex[A] =
    new Spandex.Secondary[A](primary, stop, start)

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
    while (i < length) {
      if (p(fetch(i))) return i
      i += 1
    }
    -1
  }

  override final def toArray[B >: A: ClassTag]: Array[B] =
    copyToArray(new Array[B](this.length))

  override final def copyToArray[B >: A](array: Array[B], start: Int): array.type =
    if (start < 0 || start > length)
      throw new ArrayIndexOutOfBoundsException(start)
    else if (isReversed) super.copyToArray(array, start)
    else {
      val sliced = slice(start, length)
      val (frontStartIndex, frontStopIndex) = sliced.frontIndexes
      val (rearStartIndex, rearStopIndex) = sliced.rearIndexes
      if (frontStopIndex > 0) java.lang.System.arraycopy(elements, frontStartIndex, array, 0, frontStopIndex - frontStartIndex)
      if (rearStopIndex > 0) java.lang.System.arraycopy(elements, rearStartIndex, array, frontStopIndex - frontStartIndex, rearStopIndex - rearStartIndex)
      array
    }

  override protected[this] final def fromIterable[B](c: collection.Iterable[B]): Spandex[B] = Spandex.fromIterable(c)

  override def iterableFactory: IterableFactory[Spandex] = Spandex

  protected[this] final def fromSpecificIterable(coll: collection.Iterable[A]): Spandex[A] = fromIterable(coll)

  //protected[this] final def newBuilder: Builder[A, Spandex_Z[A]] = Spandex_Z.newBuilder[A]()
  override protected[this] def newSpecificBuilder(): Builder[A, Spandex[A]] = Spandex.newBuilder()

  override final def className = "Spandex_Z"
}

object Spandex extends IterableFactory[Spandex] {
  private[immutable] class Primary[+A](
    override val elements: scala.Array[Any],
    x: Int,
    y: Int)
    extends Spandex[A](x, y) {

    def toDebugString: String = s"${mkString("[", ", ", "]")}: Primary(start: $start, stop: $stop, elements: ${elements.toList.zipWithIndex.map(t => s"${t._2}: ${t._1}").mkString("[", ", ", "]").replace("null", "_")})"

    private[this] val bounds = new AtomicLong(pack(start, stop))

    //@`inline`
    private[this] def pack(low: Int, high: Int): Long = low.toLong << 32 | (high & 0xffffffffL)
    //@`inline`
    private[this] def unpackLow(l: Long): Int = (l >> 32).toInt
    //@`inline`
    private[this] def unpackHigh(l: Long): Int = l.toInt

    private[Spandex] def prependElement[B >: A](elem: B, start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      if (Spandex.distance(low, high) == capacity) false
      else if (low < start
        && ((elem.isInstanceOf[AnyRef] && elem.asInstanceOf[AnyRef].eq(element(index(start - 1)).asInstanceOf[AnyRef]))
        || (!elem.isInstanceOf[AnyRef] && elem == element(index(start - 1))))) true
      else if (low > -capacity && low == start && bounds.compareAndSet(bnds, pack(low - 1, high))) {
        elements(index(low - 1)) = elem
        true
      } else false
    }

    private[Spandex] final def prependElements[B >: A](that: Spandex[B], start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val lowered = low - that.length
      if (Spandex.distance(lowered, high) > capacity) false
      else if (low == start && lowered >= -capacity && bounds.compareAndSet(bnds, pack(lowered, high))) {
        val (sourceFrontStartIndex, sourceFrontStopIndex) = that.frontIndexes
        val (sourceRearStartIndex, sourceRearStopIndex) = that.rearIndexes
        if (sourceFrontStopIndex > 0) java.lang.System.arraycopy(that.elements, sourceFrontStartIndex, this.elements, index(lowered), sourceFrontStopIndex - sourceFrontStartIndex)
        if (sourceRearStopIndex > 0) java.lang.System.arraycopy(that.elements, sourceRearStartIndex, this.elements, index(lowered) + sourceFrontStopIndex - sourceFrontStartIndex, sourceRearStopIndex - sourceRearStartIndex)
        true
      } else false
    }

    private[Spandex] def appendElement[B >: A](elem: B, stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      if (Spandex.distance(low, high) == capacity) false
      else if (high > stop
        && ((elem.isInstanceOf[AnyRef] && elem.asInstanceOf[AnyRef].eq(element(index(stop)).asInstanceOf[AnyRef]))
        || (!elem.isInstanceOf[AnyRef] && elem == element(index(stop))))) true
      else if (high < capacity && high == stop && bounds.compareAndSet(bnds, pack(low, high + 1))) {
        elements(index(high)) = elem
        true
      } else false
    }

    private[Spandex] final def appendElements[B >: A](that: Spandex[B], stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val raised = high + that.length
      if (Spandex.distance(low, raised) > capacity) false
      else if (high == stop && raised <= capacity && bounds.compareAndSet(bnds, pack(low, raised))) {
        val (sourceFrontStartIndex, sourceFrontStopIndex) = that.frontIndexes
        val (sourceRearStartIndex, sourceRearStopIndex) = that.rearIndexes
        if (sourceFrontStopIndex > 0) java.lang.System.arraycopy(that.elements, sourceFrontStartIndex, this.elements, high, sourceFrontStopIndex - sourceFrontStartIndex)
        if (sourceRearStopIndex > 0) java.lang.System.arraycopy(that.elements, sourceRearStartIndex, this.elements, high + sourceFrontStopIndex - sourceFrontStartIndex, sourceRearStopIndex - sourceRearStartIndex)
        true
      } else false
    }

    override private[immutable] final def primary: Primary[A] = this
  }

  private[immutable] final class Secondary[+A](
    override val primary: Primary[A],
    override val start: Int,
    override val stop: Int)
    extends Spandex[A](start, stop) {
    def toDebugString: String = s"${mkString("[", ", ", "]")}: Secondary(start: $start, stop: $stop, primary: ${primary.toDebugString})"
  }

  private[immutable] final object Empty extends Primary[Nothing](Array.empty, 0, 0) {
    override def toDebugString: String = s"[]: Empty"
  }

  override def apply[A](xs: A*): Spandex[A] =
    if (xs.isEmpty) Spandex.Empty
    else {
      val n = xs.length
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      xs.copyToArray(array, 0, n)
      new Primary[A](array, 0, n)
    }

  def apply[A](xs: Spandex[A]): Spandex[A] =
    xs.trim()

  def apply[A](it: Iterable[A]): Spandex[A] =
    fromIterable(it)

  def apply[A](array: Array[A]): Spandex[A] = {
    if (array.length == 0) Spandex.empty
    else {
      val capacity = capacitate(array.length)
      val arr = new Array[Any](capacity)
      java.lang.System.arraycopy(array, 0, arr, 0, array.length)
      new Primary[A](arr, 0, array.length)
    }
  }
  private[Spandex] def capacitate(n: Int): Int = (((n max 8) + 7) / 8) * 8

  def tabulate[A](n: Int)(f: Int => A): Spandex[A] =
    if (n == 0) Spandex.Empty
    else {
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      var i = 0
      while (i < n) {
        array(i) = f(i)
        i += 1
      }
      new Primary[A](array, 0, n)
    }

  /*
  private[Spandex_Z] def expand[A](n: Int)(f: Int => A): Spandex_Z[A] =
    if (n == 0) Spandex_Z.Empty
    else {
      val capacity = capacitate((n - 1) * 2)
      val array = new Array[Any](capacity)
      val (frontStartIndex, frontStopIndex) = frontIndexes
      val (rearStartIndex, rearStopIndex) = rearIndexes
      if (rearStopIndex > 0) java.lang.System.arraycopy(elements, rearStartIndex, array, 0, rearStopIndex - rearStartIndex)
      if (frontStopIndex > 0) java.lang.System.arraycopy(elements, frontStartIndex, array, array.length - (frontStopIndex - frontStartIndex), frontStopIndex - frontStartIndex)
      if (isReversed) new Spandex_Z.Primary[A](array, start, stop).reverse
      else new Spandex_Z.Primary[A](array, start, stop)



      val first = (capacity - n + margin) / 2
      val last = first + n - 1
      var i = first
      while (i <= last) {
        array(i) = f(i - first)
        i += 1
      }
      new Primary[A](array, first, n)
    }
    */

  override def fill[A](n: Int)(elem: => A): Spandex[A] = tabulate(n)(_ => elem)

  def fromIterable[A](it: collection.IterableOnce[A]): Spandex[A] = it match {
    case c: Iterable[A] => fromIterable(c)
    case _ =>
      val builder = Spandex.newBuilder[A]()
      builder ++= it
      builder.result()
  }

  def fromIterable[A](it: collection.Iterable[A]): Spandex[A] = it match {
    case that: Spandex[A] ⇒ that
    case c if c.knownSize == 0 || c.isEmpty => Spandex.Empty
    case c if c.knownSize > 0 =>
      val n = c.knownSize
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      var i = 0
      val it = c.iterator()
      while (i < n) {
        array(i) = it.next()
        i += 1
      }
      new Spandex.Primary[A](array, 0, n)
    case _ ⇒
      val buffer = ArrayBuffer.fromIterable(it).asInstanceOf[ArrayBuffer[Any]]
      val capacity = capacitate(buffer.size)
      val array = new Array[Any](capacity)
      wrap(buffer.copyToArray(array), buffer.size)
  }

  /**
    * Used to wrap an existing array (without copying). Observe that later modifications to the specified array will show through to the returned instance.
    * @param xs array to use for elements
    * @tparam A the element type to use
    * @return a new instance using the specified array as element storage (or the empty Spandex_Z if the array is empty).
    */
  private[strawman] def wrap[A](xs: Array[Any], length: Int = -1): Spandex[A] =
    if (xs.length == 0) Spandex.Empty
    else new Primary[A](xs, 0, if (length > -1) length else xs.length)

  def newBuilder[A](): Builder[A, Spandex[A]] =
    new GrowableBuilder(ArrayBuffer.empty[A])
      .mapResult { b =>
        val array = new Array[Any](capacitate(b.length))
        b.asInstanceOf[ArrayBuffer[Any]].copyToArray(array)
        Spandex.wrap(array, b.length)
      }

  override def empty[A <: Any]: Spandex[A] = Spandex.Empty

  @`inline`
  private[Spandex] final def distance(x: Int, y: Int): Int =
    if (x < y) y - x
    else x - y
}

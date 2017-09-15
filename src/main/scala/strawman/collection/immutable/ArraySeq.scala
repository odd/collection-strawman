package strawman
package collection
package immutable

import java.util.concurrent.atomic.AtomicLong
import scala.{`inline`, Any, AnyRef, Array, ArrayIndexOutOfBoundsException, Boolean, IllegalArgumentException, IndexOutOfBoundsException, Int, Long, NoSuchElementException, Nothing, StringContext, Unit, UnsupportedOperationException}
import scala.Predef.{<:<, genericWrapArray, intWrapper, String}
import scala.math._
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import strawman.collection.mutable.{Builder, ReusableBuilder}

/**
  * A <i>ArraySeq</i> is an (ostensible) immutable array-like collection designed to support the following performance characteristics:
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
  * A configurable auto shrink percentage threshold (defaults to 25%) can be configured via the <code>withAutoShrinking</code> method.
  * When this percentage threshold is set to <code>T</code> (an integer between 0 and 100), any operation having a result with a size which
  * is at most <code>T</code> percent of the capacity of its primary will lead to the underlying array being copied to a new array
  * with a capacity suitable to the size of the result. The same effect can be obtained on a <code>ArraySeq</code> having <code>T</code>
  * set to zero (i.e. auto shrinking disabled) and instead calling <code>trim</code> on the result.
  * <br/>
  * <br/>
  * <b>Examples</b>
  * <br/>
  <pre><code>
    // expression                                               (array, start, stop)
    ArraySeq()                                               ==> ([], 0, 0)
    ArraySeq() :+ a                                          ==> ([a, _, _, _, _, _, _, _], 0, 1)
    a +: ArraySeq()                                          ==> ([_, _, _, _, _, _, _, a], -1, 0)
    ArraySeq() :++ Seq(a, b, c, d)                           ==> ([a, b, c, d, _, _, _, _], 0, 4)
    Seq(a, b, c, d) ++: ArraySeq()                           ==> ([_, _, _, _, a, b, c, d], -4, 0)
    Seq(a, b, c, d) ++: ArraySeq() :++ Seq(e, f, g, h)       ==> ([e, f, g, h, a, b, c, d], -4, 4)
    ArraySeq(a, b, c, d, e, f, g, h)                         ==> ([a, b, c, d, e, f, g, h], 0, 8)
    ArraySeq() :++ Seq(a, b, c, d, e, f, g, h)               ==> ([a, b, c, d, e, f, g, h], 0, 8)
    Seq(a, b, c, d, e, f, g, h) ++: ArraySeq()               ==> ([a, b, c, d, e, f, g, h], -8, 0)
    (Seq(a, b, c, d) ++: ArraySeq()).slice(1, 3)             ==> ([_, _, _, _, a, b, c, d], -3, -1)
    (ArraySeq() :++ Seq(a, b, c, d)).slice(1, 3)             ==> ([a, b, c, d, _, _, _, _], 1, 3)
    (Seq(a, b) ++: ArraySeq() :++ Seq(c, d)).slice(1, 3)     ==> ([c, d, _, _, _, _, a, b], -1, 1)
  </code></pre>
  */
sealed abstract class ArraySeq[+A] private (protected val start: Int, protected val stop: Int)
  extends Seq[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, ArraySeq, ArraySeq[A]]
    with StrictOptimizedSeqOps[A, ArraySeq, ArraySeq[A]] {

  def toDebugString: String

  override def toString = toDebugString // Should be removed once finished

  // Could be private but is not to allow for access from test cases
  private[immutable] def primary: ArraySeq.Primary[A]

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
  def shrinkThreshold: Int = ArraySeq.DefaultAutoShrinkThreshold

  /**
    * Returns a <code>ArraySeq</code> that uses the specified auto shrinking percentage threshold.
    * @param threshold the maximum <code>size / capacity</code> percentage (defaults to <code>DefaultAutoShrinkingThreshold</code>).
    */
  def withAutoShrinking(threshold: Int = ArraySeq.DefaultAutoShrinkThreshold): ArraySeq[A] =
    //if (threshold == ArraySeq.DefaultAutoShrinkThreshold) this
    //else ArraySeq.createSecondary(this, start, stop, threshold)
    ArraySeq.createSecondary(this, start, stop, threshold)

  /**
    * Returns a <code>ArraySeq</code> that uses no auto shrinking (has an auto shrinking percentage threshold of <code>0</code>).
    */
  def withoutAutoShrinking: ArraySeq[A] = withAutoShrinking(0)

  override final def length = size

  override final def size: Int = ArraySeq.distance(start, stop)

  override final def knownSize: Int = size

  override final def isEmpty: Boolean = start == 0 && stop == 0

  override final def nonEmpty: Boolean = !isEmpty

  override final def apply(i: Int): A =
    if (i < 0 || i >= size) throw new ArrayIndexOutOfBoundsException(i)
    else fetch(i)

  override final def slice(from: Int, until: Int): ArraySeq[A] =
    if (until <= from) ArraySeq.empty
    else {
      val f = (from max 0) min size
      val u = (until min size) max 0
      if (f == 0 && u == size) this
      else if (isReversed) ArraySeq.createSecondary(primary, start - f, start - u, shrinkThreshold)
      else ArraySeq.createSecondary(primary, start + f, start + u, shrinkThreshold)
    }

  override final def take(n: Int): ArraySeq[A] =
    slice(0, (n min size) max 0)

  override final def takeRight(n: Int): ArraySeq[A] =
    slice(size - ((n min size) max 0), size)

  override final def drop(n: Int): ArraySeq[A] =
    slice(n, size)

  override final def dropRight(n: Int): ArraySeq[A] = {
    val l = size
    slice(0, l - ((n min l) max 0))
  }

  override def span(p: A => Boolean): (ArraySeq[A], ArraySeq[A]) = {
    val prefix = takeWhile(p)
    val suffix = drop(prefix.size)
    (prefix, suffix)
  }

  override final def head: A =
    if (isEmpty) throw new UnsupportedOperationException
    else fetch(0)

  override final def tail: ArraySeq[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else drop(1)

  override final def last: A =
    if (isEmpty) throw new UnsupportedOperationException
    else fetch(size - 1)

  override final def init: ArraySeq[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else dropRight(1)

  override final def padTo[B >: A](len: Int, elem: B): ArraySeq[B] =
    if (len <= size) this
    else appendAll1(Iterator.continually(elem).take(len - size), len - size)


  final def leftPadTo[B >: A](len: Int, elem: B): ArraySeq[B] =
    if (len <= size) this
    else prependAll1(Iterator.continually(elem).take(len - size), len - size)

  @`inline`
  final def rightPadTo[B >: A](len: Int, elem: B): ArraySeq[B] = padTo(len, elem)

  override final def prepend[B >: A](elem: B): ArraySeq[B] =
    if (isEmpty) ArraySeq(elem)
    else if (size == capacity) elem +: ArraySeq.allocate(this, ArraySeq.capacitate(capacity + 1))
    else if (nonReversed && primary.prependElement(elem, start)) ArraySeq.createSecondary(primary, start - 1, stop, shrinkThreshold)
    else if (isReversed && primary.appendElement(elem, start)) ArraySeq.createSecondary(primary, start + 1, stop, shrinkThreshold) // as it is reversed start is really stop and stop is really start
    else elem +: ArraySeq.allocate(this, ArraySeq.capacitate(size + 1))

  override final def append[B >: A](elem: B): ArraySeq[B] =
    if (isEmpty) ArraySeq(elem)
    else if (size == capacity) ArraySeq.allocate(this, ArraySeq.capacitate(capacity + 1)) :+ elem
    else if (nonReversed && primary.appendElement(elem, stop)) ArraySeq.createSecondary(primary, start, stop + 1, shrinkThreshold)
    else if (isReversed && primary.prependElement(elem, stop)) ArraySeq.createSecondary(primary, start, stop - 1, shrinkThreshold) // as it is reversed start is really stop and stop is really start
    else ArraySeq.allocate(this, ArraySeq.capacitate(size + 1)) :+ elem

  override final def prependAll[B >: A](xs: collection.Iterable[B]): ArraySeq[B] = prependAll1(xs)

  private final def prependAll1[B >: A](xs: collection.IterableOnce[B], knownSize: Int = -1): ArraySeq[B] =
    xs match {
      case _ if knownSize == 0 =>
        this
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty =>
        this
      case _ if knownSize == 1 =>
        prepend(xs.iterator().next())
      case that: Iterable[B] if that.knownSize == 1 =>
        prepend(that.head)
      case _ if this.isEmpty =>
        ArraySeq.fromIterable(xs, knownSize)
      case that: ArraySeq[B] if this.isReversed || that.isReversed =>
        ArraySeq.fromIterable(View.Concat(that, this), this.knownSize + that.knownSize)
      case that: ArraySeq[B] if that.primary.appendElements(this, that.stop) =>
        ArraySeq.createSecondary(that.primary, that.start, that.stop + this.size, shrinkThreshold)
      case that: ArraySeq[B] if this.primary.prependElements(that, this.start) =>
        ArraySeq.createSecondary(this.primary, this.start - that.size, this.stop, shrinkThreshold)
      case that: ArraySeq[B] =>
        ArraySeq.allocate(this, ArraySeq.capacitate(this.size + that.size)).prependAll(that)
      case that: Iterable[B] if knownSize > -1 || that.knownSize > -1 =>
        ArraySeq.fromIterable(View.Concat(that, this), that.knownSize + this.knownSize)
      case _ if knownSize > -1 =>
        ArraySeq.fromIterable(new View.Patched(this, 0, xs, 0), knownSize + this.knownSize)
      case _ =>
        prependAll1(ArraySeq.fromIterable(xs, knownSize), knownSize)
    }

  override final def appendAll[B >: A](xs: collection.Iterable[B]): ArraySeq[B] = appendAll1(xs)

  private[ArraySeq] final def appendAll1[B >: A](xs: collection.IterableOnce[B], knownSize: Int = -1): ArraySeq[B] =
    xs match {
      case _ if knownSize == 0 =>
        this
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty =>
        this
      case _ if knownSize == 1 =>
        append(xs.iterator().next())
      case that: Iterable[B] if that.knownSize == 1 =>
        append(that.head)
      case _ if this.isEmpty =>
        ArraySeq.fromIterable(xs, knownSize)
      case that: ArraySeq[B] if this.isReversed || that.isReversed =>
        ArraySeq.fromIterable(View.Concat(this, that), this.knownSize + that.knownSize)
      case that: ArraySeq[B] if that.primary.prependElements(this, that.start) =>
        ArraySeq.createSecondary(that.primary, that.start - this.size, that.stop, shrinkThreshold)
      case that: ArraySeq[B] if this.primary.appendElements(that, this.stop) =>
        ArraySeq.createSecondary(this.primary, this.start, this.stop + that.size, shrinkThreshold)
      case that: ArraySeq[B] =>
        ArraySeq.allocate(this, ArraySeq.capacitate(this.size + that.size)).appendAll(that)
      case that: Iterable[B] if knownSize > -1 || that.knownSize > -1 =>
        ArraySeq.fromIterable(View.Concat(this, that), that.knownSize + this.knownSize)
      case _ if knownSize > -1 =>
        ArraySeq.fromIterable(new View.Patched(this, this.knownSize, xs, 0), knownSize + this.knownSize)
      case _ =>
        appendAll1(ArraySeq.fromIterable(xs, knownSize), knownSize)
    }

  override final def map[B](f: A => B): ArraySeq[B] =
    if (isEmpty) ArraySeq.empty
    else {
      val capacity = ArraySeq.capacitate(size)
      val array = scala.Array.ofDim[Any](capacity)
      var i = size - 1
      while (i >= 0) {
        val j = position(i)
        array(j) = f(element(j))
        i -= 1
      }
      ArraySeq.createPrimary[B](array, start, stop).withAutoShrinking(shrinkThreshold)
    }

  override final def zip[B](xs: collection.Iterable[B]): ArraySeq[(A, B)] = xs match {
    case that: ArraySeq[B] =>
      ArraySeq.tabulate(this.size min that.size) { i =>
        (this.apply(i), that.apply(i))
      }
    case _ =>
      fromIterable[(A, B)](View.Zip[A, B](coll, xs), xs.knownSize)
  }

  override def unzip[A1, A2](implicit asPair: <:<[A, (A1, A2)]): (ArraySeq[A1], ArraySeq[A2]) = {
    val arr1 = scala.Array.ofDim[Any](capacity)
    val arr2 = scala.Array.ofDim[Any](capacity)
    var i = 0
    val sz = size
    while (i < sz) {
      val (a1, a2) = asPair(fetch(i))
      arr1(i) = a1
      arr2(i) = a2
      i += 1
    }
    (ArraySeq.createPrimary(arr1, 0, sz).withAutoShrinking(shrinkThreshold), ArraySeq.createPrimary(arr2, 0, sz).withAutoShrinking(shrinkThreshold))
  }

  override final def iterator(): Iterator[A] =
    new Iterator[A] {
      private[this] final val n = ArraySeq.this.size
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

  override final def reverse: ArraySeq[A] =
    if (isEmpty) ArraySeq.empty.withAutoShrinking(shrinkThreshold)
    else if (size == 1) this
    else ArraySeq.createSecondary(primary, stop, start, shrinkThreshold)

  private[ArraySeq] final def withReversed(reversed: Boolean): ArraySeq[A] =
    if (!reversed) this
    else reverse

  override protected[this] def reversed: ArraySeq[A] = reverse

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

  override final def updated[B >: A](i: Int, elem: B): ArraySeq[B] = {
    if (i < 0 || i >= size) throw new IndexOutOfBoundsException(i.toString)
    else if (ArraySeq.isSame(elem, element(position(i)))) this
    else {
      val array = scala.Array.ofDim[Any](capacity)
      java.lang.System.arraycopy(elements, 0, array, 0, capacity)
      array.update(position(i), elem)
      ArraySeq.createPrimary(array, start, stop).withAutoShrinking(shrinkThreshold)
    }
  }

  override final def patch[B >: A](from: Int, xs: IterableOnce[B], replaced: Int = 0): ArraySeq[B] = {
    if (from < 0 || from > size) throw new IndexOutOfBoundsException(from.toString)
    else if (from == 0 && (replaced max 0) == 0) prependAll1(xs)
    else if (from == 0) coll.drop(replaced).prependAll1(xs)
    else if (from == size) coll.appendAll1(xs)
    else coll.take(from).appendAll1(xs).appendAll1(coll.slice(from + replaced, size))
  }

  override def toArray[B >: A: ClassTag]: Array[B] =
    copyToArray(scala.Array.ofDim[B](this.size))

  override def copyToArray[B >: A](array: Array[B], start: Int = 0): array.type = {
    if (start < 0 || start > size)
      throw new ArrayIndexOutOfBoundsException(start)
    else if (isReversed) super.copyToArray(array, start)
    else {
      if (array.isInstanceOf[Array[AnyRef]]) {
        val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = ArraySeq.indices(start, stop, size)
        if (frontStopIndex > 0) java.lang.System.arraycopy(elements, frontStartIndex, array, 0, frontStopIndex - frontStartIndex)
        if (rearStopIndex > 0) java.lang.System.arraycopy(elements, rearStartIndex, array, frontStopIndex - frontStartIndex, rearStopIndex - rearStartIndex)
      } else {
        super.copyToArray(array, start)
      }
      array
    }
  }

  final def grow(size: Int, padded: Boolean = true): ArraySeq[A] = resize(size, padded)

  final def shrink(padded: Boolean = true): ArraySeq[A] = resize(this.size, padded)

  final def trim(): ArraySeq[A] = shrink(false)

  final def resize(size: Int, padded: Boolean = true): ArraySeq[A] =
    if (size == 0) ArraySeq.Empty
    else if (size == capacity) this
    else if (size < this.size) throw new IllegalArgumentException(s"Specified size of $size is smaller than the current size ${this.size}.")
    else {
      ArraySeq.allocate(this, if (padded) ArraySeq.capacitate(size) else size)
    }

  protected[this] final def fromIterable[B](c: collection.Iterable[B], size: Int = -1): ArraySeq[B] = ArraySeq.fromIterable(c, size)

  override def iterableFactory: SeqFactory[ArraySeq] = ArraySeq

  protected[this] final def fromSpecificIterable(coll: collection.Iterable[A]): ArraySeq[A] = fromIterable(coll)

  override protected[this] def newSpecificBuilder(): Builder[A, ArraySeq[A]] = ArraySeq.newBuilder()

  override final def className = "ArraySeq"

  /**
    * This method is dangerous since it disregards the existing bounds of the current primary.
    * Only used during benchmarking to disregard any state affected in previous iterations.
    */
  private[strawman] def reset(): ArraySeq[A] = {
    ArraySeq.createSecondary(ArraySeq.createPrimary(elements, primary.start, primary.stop), start, stop, shrinkThreshold, autoShrink = false).withReversed(isReversed)
  }
}

object ArraySeq extends SeqFactory[ArraySeq] {
  /**
    * The default maximum <code>size / capacity</code> percentage threshold allowed before auto shrinking is <code>25</code>.
    */
  final val DefaultAutoShrinkThreshold = 25

  private[immutable] class Primary[+A](
    override val elements: scala.Array[Any],
    x: Int,
    y: Int)
    extends ArraySeq[A](x, y) {

    def toDebugString: String = s"${mkString("[", ", ", "]")}: Primary(start: $start, stop: $stop, elements: ${elements.toList.zipWithIndex.map(t => s"${t._2}: ${t._1}").mkString("[", ", ", "]").replace("null", "_")})"
    def toSecondary: ArraySeq[A] = new Secondary[A](this, start, stop, shrinkThreshold)

    private[this] val bounds = new AtomicLong(pack(start, stop))

    @`inline`
    private[this] def pack(low: Int, high: Int): Long = low.toLong << 32 | (high & 0xffffffffL)
    @`inline`
    private[this] def unpackLow(l: Long): Int = (l >> 32).toInt
    @`inline`
    private[this] def unpackHigh(l: Long): Int = l.toInt

    private[ArraySeq] def prependElement[B >: A](elem: B, start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      if (ArraySeq.distance(low, high) == capacity) false
      else if (low < start && isSame(elem, element(index(start - 1)))) true
      else if (low > -capacity && low == start && bounds.compareAndSet(bnds, pack(low - 1, high))) {
        elements(index(low - 1)) = elem
        true
      } else false
    }

    private[ArraySeq] final def prependElements[B >: A](that: ArraySeq[B], start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val lowered = low - that.size
      if (ArraySeq.distance(lowered, high) > capacity) false
      else if (low == start && lowered >= -capacity && bounds.compareAndSet(bnds, pack(lowered, high))) {
        val (sourceFrontStartIndex, sourceFrontStopIndex, sourceRearStartIndex, sourceRearStopIndex) = ArraySeq.indices(that.start, that.stop, that.capacity)
        if (sourceFrontStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceFrontStartIndex, this.elements, index(lowered), sourceFrontStopIndex - sourceFrontStartIndex)
        if (sourceRearStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceRearStartIndex, this.elements, index(lowered) + sourceFrontStopIndex - sourceFrontStartIndex, sourceRearStopIndex - sourceRearStartIndex)
        true
      } else false
    }

    private[ArraySeq] def appendElement[B >: A](elem: B, stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      if (ArraySeq.distance(low, high) == capacity) false
      else if (high > stop && isSame(elem, element(index(stop)))) true
      else if (high < capacity && high == stop && bounds.compareAndSet(bnds, pack(low, high + 1))) {
        elements(index(high)) = elem
        true
      } else false
    }

    private[ArraySeq] final def appendElements[B >: A](that: ArraySeq[B], stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val raised = high + that.size
      if (ArraySeq.distance(low, raised) > capacity) false
      else if (high == stop && raised <= capacity && bounds.compareAndSet(bnds, pack(low, raised))) {
        val (sourceFrontStartIndex, sourceFrontStopIndex, sourceRearStartIndex, sourceRearStopIndex) = ArraySeq.indices(that.start, that.stop, that.capacity)
        if (sourceFrontStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceFrontStartIndex, this.elements, high, sourceFrontStopIndex - sourceFrontStartIndex)
        if (sourceRearStopIndex > 0)
          java.lang.System.arraycopy(that.elements, sourceRearStartIndex, this.elements, high + sourceFrontStopIndex - sourceFrontStartIndex, sourceRearStopIndex - sourceRearStartIndex)
        true
      } else false
    }

    override private[immutable] final def primary: Primary[A] = this
  }

  private[immutable] class Secondary[+A](
    override val primary: Primary[A],
    override val start: Int,
    override val stop: Int,
    override val shrinkThreshold: Int)
    extends ArraySeq[A](start, stop) {
    def toDebugString: String = s"${mkString("[", ", ", "]")}: Secondary(start: $start, stop: $stop, shrinkThreshold: $shrinkThreshold, primary: ${primary.toDebugString})"
  }

  private[immutable] final object EmptyPrimary extends Primary[Nothing](Array.empty, 0, 0) {
    override def toDebugString: String = s"[]: EmptyPrimary"
  }

  private[immutable] final object Empty extends Secondary[Nothing](EmptyPrimary, 0, 0, DefaultAutoShrinkThreshold) {
    override def toDebugString: String = s"[]: Empty"
    override def toArray[B >: Nothing : ClassTag]: Array[B] = Array.empty
    override def copyToArray[B >: Nothing](array: Array[B], start: Int): array.type = array
  }

  override def apply[A](xs: A*): ArraySeq[A] =
    if (xs.isEmpty) ArraySeq.Empty
    else {
      val n = xs.size
      val capacity = capacitate(n)
      val array = scala.Array.ofDim[Any](capacity)
      xs.copyToArray(array, 0, n)
      createPrimary(array, 0, n).toSecondary
    }

  def apply[A](it: Iterable[A]): ArraySeq[A] = ArraySeq.fromIterable(it)

  def apply[A](array: Array[A]): ArraySeq[A] = {
    if (array.length == 0) ArraySeq.empty
    else {
      val capacity = capacitate(array.length)
      val arr = scala.Array.ofDim[Any](capacity)
      java.lang.System.arraycopy(array, 0, arr, 0, array.length)
      createPrimary(arr, 0, array.length).toSecondary
    }
  }

  private[ArraySeq] def createPrimary[A](array: Array[Any], start: Int, stop: Int): Primary[A] = new Primary[A](array, start, stop)

  private[ArraySeq] def createSecondary[A](xs: ArraySeq[A], start: Int, stop: Int, shrinkThreshold: Int, autoShrink: Boolean = true): ArraySeq[A] = {
    if (autoShrink && isExcessive(xs, start, stop, shrinkThreshold))
      allocate(xs, start, stop)
    else
      new Secondary[A](xs.primary, start, stop, shrinkThreshold)
  }

  @`inline` private def isExcessive[A](xs: ArraySeq[A], start: Int, stop: Int, shrinkThreshold: Int) = {
    xs.capacity > 8 && (capacitate(distance(start, stop)) * 100) / xs.capacity <= shrinkThreshold
  }

  private[this] def pow2(n: Int): Int = {
    var pow: Int = 1
    while (pow < n && pow > 0)
      pow <<= 1
    pow
  }
  private[ArraySeq] def capacitate(n: Int): Int = pow2(n) max 8 //((((n max 8) + 7) / 8) * 8).toInt
  private[ArraySeq] def allocate[A](xs: ArraySeq[A], capacity: Int): ArraySeq[A] = {
    val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = ArraySeq.indices(xs.start min xs.stop, xs.start max xs.stop, xs.capacity)
    allocate(xs, capacity, frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex)
  }
  private[ArraySeq] def allocate[A](xs: ArraySeq[A], start: Int, stop: Int): ArraySeq[A] = {
    val (frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex) = ArraySeq.indices(start min stop, start max stop, xs.capacity)
    allocate(xs, capacitate(distance(start, stop)), frontStartIndex, frontStopIndex, rearStartIndex, rearStopIndex)
  }
  private[ArraySeq] def allocate[A](xs: ArraySeq[A], capacity: Int, frontStartIndex: Int, frontStopIndex: Int, rearStartIndex: Int, rearStopIndex: Int): ArraySeq[A] = {
    val array = scala.Array.ofDim[Any](capacity)
    val start = frontStopIndex - frontStartIndex
    val stop = rearStopIndex - rearStartIndex
    if (rearStopIndex > 0)
      java.lang.System.arraycopy(xs.elements, rearStartIndex, array, 0, stop)
    if (frontStopIndex > 0)
      java.lang.System.arraycopy(xs.elements, frontStartIndex, array, array.length - start, start)
    createSecondary(createPrimary[A](array, -start, stop), -start, stop, xs.shrinkThreshold, autoShrink = false).withReversed(xs.isReversed)
  }

  private[ArraySeq] def isSame(a: Any, b: Any) = (
    (a.isInstanceOf[AnyRef] && a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef]))
    || (!a.isInstanceOf[AnyRef] && a == b))

  override def tabulate[A](n: Int)(f: Int => A): ArraySeq[A] =
    if (n == 0) Empty
    else {
      val capacity = capacitate(n)
      val array = scala.Array.ofDim[Any](capacity)
      var i = 0
      while (i < n) {
        array(i) = f(i)
        i += 1
      }
      createPrimary(array, 0, n).toSecondary
    }

  override def fill[A](n: Int)(elem: => A): ArraySeq[A] = tabulate(n)(_ => elem)

  def fromIterable[A](xs: collection.Iterable[A]): ArraySeq[A] = fromIterable(xs, -1)

  def fromIterable[A](xs: collection.IterableOnce[A], knownSize: Int = -1): ArraySeq[A] = xs match {
    case that: ArraySeq[A] ⇒ that
    case _ if knownSize == 0 => Empty
    case that: Iterable[A] if that.knownSize == 0 => Empty
    case _ if knownSize > 0 || (xs.isInstanceOf[Iterable[A]] && xs.asInstanceOf[Iterable[A]].knownSize > 0) =>
      val n = if (knownSize > 0) knownSize else xs.asInstanceOf[Iterable[A]].knownSize
      val capacity = capacitate(n)
      val array = scala.Array.ofDim[Any](capacity)
      var i = 0
      val it = xs.iterator()
      while (i < n) {
        array(i) = it.next()
        i += 1
      }
      createPrimary(array, 0, n).toSecondary
    case _ =>
      val builder = ArraySeq.newBuilder[A]()
      builder ++= xs
      builder.result().withAutoShrinking()
  }

  /**
    * Used to wrap an existing array (without copying). Observe that later modifications to the specified array will show
    * through to the returned instance.
    * @param xs array to use for elements
    * @param length the number of elements present in the array (defaults to -1 which will use the array length)
    * @tparam A the element type to use
    * @return a new instance using the specified array as element storage (or the empty ArraySeq if the array is empty).
    */
  private[strawman] def wrap[A](xs: Array[Any], length: Int = -1): ArraySeq[A] =
    if (xs.length == 0) Empty
    else createPrimary(xs, 0, if (length > -1) length else xs.length).toSecondary.withoutAutoShrinking

  /** A class to build instances of `ArraySeq`.  This builder is reusable. */
  final class Builder[A](var sizeHint: Int = 0) extends ReusableBuilder[A, ArraySeq[A]] {
    private var elems: ArraySeq[A] = empty

    private def empty = createPrimary(scala.Array.ofDim[Any](capacitate(sizeHint max 0)), 0, 0).toSecondary.withoutAutoShrinking

    override def sizeHint(size: Int) = sizeHint = size

    override def clear() = elems = empty

    override def result() = elems

    override def add(elem: A) = {
      elems :+= elem
      this
    }
  }

  def newBuilder[A](): Builder[A] = new Builder[A]()

  def newBuilder[A](sizeHint: Int): Builder[A] = new Builder[A](sizeHint)

  override def empty[A <: Any]: ArraySeq[A] = Empty

  @`inline`
  private[ArraySeq] def indices(start: Int, stop: Int, capacity: Int): (Int, Int, Int, Int) = {
    if (start < 0 && stop <= 0) (capacity + start, capacity + stop, 0, 0)
    else if (start < 0) (capacity + start, capacity, 0, stop)
    else if (stop < 0) (capacity + stop, capacity, 0, start)
    else (0, 0, start, stop)
  }

  @`inline`
  private[ArraySeq] final def distance(x: Int, y: Int): Int =
    if (x < y) y - x
    else x - y
}


package strawman
package collection
package immutable

import java.util.concurrent.atomic.AtomicLong
import scala.{`inline`, Any, AnyRef, Array, ArrayIndexOutOfBoundsException, Boolean, IllegalArgumentException, IndexOutOfBoundsException, Int, Long, NoSuchElementException, PartialFunction, Serializable, SerialVersionUID, StringContext, Unit, UnsupportedOperationException}
import scala.Predef.{<:<, ArrowAssoc, String}
import scala.annotation.switch
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime
import strawman.collection.mutable.ReusableBuilder

/**
  * An <i>ArraySeq</i> is an (ostensible) immutable array-like collection designed to support the following performance characteristics:
  * <ul>
  * <li>constant time <code>head</code>, <code>last</code>, <code>tail</code>*, <code>init</code>*, <code>take</code>*, <code>takeRight</code>*, <code>takeWhile</code>*, <code>drop</code>*, <code>dropRight</code>*, <code>dropWhile</code>*, <code>slice</code>* and <code>reverse</code> (* = depending on the <code>AutoTrimming</code> mode in use)</li>
  * <li>amortised constant time <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and <code>concat</code> (depending on the complexity of <code>java.util.System.arraycopy</code></li>
  * <li>efficient indexed access</li>
  * <li>linear time iteration (i.e. <code>iterator</code> and <code>foreach</code>) with low constant factors
  * <li>reasonable memory usage (approximately double that of an array with the same number of elements)
  * </ul>
  * <br/>
  * Elements can be added to both the front and the rear of the underlying array (via <code>prepend</code>/<code>prependAll</code>
  * and <code>append</code>/<code>appendAll</code>/<code>concat</code> respectively), but never more than once for any given array slot.
  * For any operation trying to use an already occupied slot (such as <code>appended</code>, <code>prepended</code> or <code>updated</code>) an overlay is provided that can store the index and element pair using an <code>immutable.HashMap</code> (thus avoiding having to copy the underlying array).
  * The overlay will only be allowed to grow according to the used <code>AutoTrimming</code> mode, after which it will be merged with the underlying array on the next transformation operation.
  * <br/>
  * <br/>
  * To guarantee that only a single thread can write to an array slot an atomic long is used to guard the low and high index
  * assignments.
  * <br/>
  * <br/>
  * When the underlying array is too small to fit a requested addition a new array will be allocated (the size is dependent on the <code>AutoTrimming</code> mode used).
  * <br/>
  * <br/>
  * To ensure that no longer accessible array slots (i.e. on the returned instance of a <code>slice</code> operation) are freed, trimming can be used.
  * Trimming can either be performed manually (via the <code>trim</code> method) or automatically via the <code>withAutoTrimming</code> method.
  * <br/>
  * The automatic trimming can be customized via these three properties:
  * <table>
  *   <tr>
  *     <th align="left">Property</th>
  *     <th align="left">Default</th>
  *     <th align="left">Description</th>
  *   </tr>
  *   <tr>
  *     <td>minUsagePercentage</td>
  *     <td>25</td>
  *     <td>The minimum <code>size / capacity</code> percentage allowed before auto trimming.</td>
  *   </tr>
  *   <tr>
  *     <td>maxOverlaidPercentage</td>
  *     <td>50</td>
  *     <td>The maximum <code>overlay.size / capacity</code> percentage allowed before auto trimming.</td>
  *   </tr>
  *   <tr>
  *     <td>usePadding</td>
  *     <td>true</td>
  *     <td>Indicates whether to use padding or not.</td>
  *   </tr>
  * </table>
  * <br/>
  * <br/>
  * There exists three predefined auto trimming modes:
  * <ul>
  *   <li><b>Always</b> will use auto trimming for all transformation operations and never use an overlay nor any padding. Safe but slow.</li>
  * </ul>
  * <ul>
  *   <li><b>Default</b> will use auto trimming for all transformation operations where the size of the returned instance is at most 25% of the capacity of the underlay.
  *   Will use an overlay with a size of at most 50% of the capacity of the underlay and will use padding. Somewhat leaky but fast except for creating small slices.</li>
  * </ul>
  * <ul>
  *   <li><b>Never</b> will never use auto trimming and will allow the overlay to grow to the same size as the capacity of the underlay and will use padding. Leaky but fast.</li>
  * </ul>
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
@SerialVersionUID(1974011501L)
final class ArraySeq[+A] private (
    private val underlay: ArraySeq.Underlay[A],
    private val overlay: ArraySeq.Overlay[A],
    private val start: Int,
    private val stop: Int,
    val autoTrimming: ArraySeq.AutoTrimming)
  extends IndexedSeq[A]
    with IndexedSeqOps[A, ArraySeq, ArraySeq[A]]
    with StrictOptimizedSeqOps[A, ArraySeq, ArraySeq[A]]
    with Serializable {

  import ArraySeq._

  def toDebugString: String = s"""${mkString("[", ", ", "]")}: ArraySeq(start: $start, stop: $stop, autoTrimming: $autoTrimming, underlay: ${underlay.toDebugString}${if (overlay != null) s""", overlay:  ${overlay.mkString("[", ", ", "]")}""" else ""}"""

  override def toString() = toDebugString // Should be removed once finished

  @`inline`
  private[immutable] def array: Array[Any] = underlay.elements

  @`inline`
  def capacity: Int = underlay.capacity
  @`inline`
  def isUncapacitated = capacity == 0

  //@`inline` // Causes problems with errors and infinite inlining when running test cases on Dotty
  @`inline`
  private def isReversed: Boolean = start > stop
  @`inline`
  private def nonReversed: Boolean = start <= stop

  @`inline`
  private[this] def in(slot: Int): A = {
    if (overlay == null) underlay(slot)
    else overlay.get(slot).getOrElse(underlay(slot))
  }

  @`inline`
  private[this] def at(index: Int): A = in(slot(index, start, stop, capacity))

  /**
    * Returns a <code>ArraySeq</code> that uses the specified auto trimming configuration.
    * @param autoTrimming the auto trimming configuration (defaults to <code>AutoTrimming.Default</code>).
    */
  def withAutoTrimming(autoTrimming: AutoTrimming = AutoTrimming.Default): ArraySeq[A] =
    if (autoTrimming == this.autoTrimming) this
    else create(underlay, overlay, start, stop, autoTrimming, trim = true)

  /**
    * Returns a <code>ArraySeq</code> that uses no auto trimming (<code>AutoTrimming.Never</code>).
    */
  def withoutAutoTrimming: ArraySeq[A] =
    withAutoTrimming(AutoTrimming.Never)
  /**
    * Executes <code>f</code> on this instance without auto trimming during the execution of <code>f</code>.
    * The returned instance has the same auto trimming setting as this instance.
    */
  def withoutAutoTrimming[B >: A](f: ArraySeq[A] => ArraySeq[B]): ArraySeq[B] =
    f(withAutoTrimming(AutoTrimming.Never)).withAutoTrimming(autoTrimming)

  override def length = size

  override def size: Int = distance(start, stop)

  override def knownSize: Int = size

  override def isEmpty: Boolean = size == 0

  override def nonEmpty: Boolean = !isEmpty

  override def apply(i: Int): A =
    if (i < 0 || i >= size) throw new ArrayIndexOutOfBoundsException(i)
    else at(i)

  override def slice(from: Int, until: Int): ArraySeq[A] = {
    val sz = size
    if (sz == 0 || until <= from) create(autoTrimming)
    else if (from <= 0 && until >= sz) this
    else {
      val f = minOf(maxOf(from, 0), sz)
      val u = maxOf(minOf(until, sz), 0)
      if (isReversed) create(underlay, if (overlay != null) overlay.slice(f, u) else null, start - f, start - u, autoTrimming, trim = true)
      else create(underlay, if (overlay != null) overlay.slice(f, u) else null, start + f, start + u, autoTrimming, trim = true)
    }
  }

  override def take(n: Int): ArraySeq[A] =
    slice(0, maxOf(minOf(n, size), 0))

  override def takeRight(n: Int): ArraySeq[A] = {
    val sz = size
    slice(sz - maxOf(minOf(n, sz), 0), sz)
  }

  override def takeWhile(p: (A) => Boolean): ArraySeq[A] = {
    val sz = size
    var i = 0
    while (i < sz) {
      if (!p(at(i))) return take(i)
      i += 1
    }
    this
  }

  override def drop(n: Int): ArraySeq[A] =
    slice(n, size)

  override def dropRight(n: Int): ArraySeq[A] = {
    val sz = size
    slice(0, sz - maxOf(minOf(n, sz), 0))
  }

  override def dropWhile(p: (A) => Boolean): ArraySeq[A] = {
    val sz = size
    var i = 0
    while (i < sz) {
      if (!p(at(i))) return drop(i)
      i += 1
    }
    create(autoTrimming)
  }

  override def span(p: (A) => Boolean) = {
    val prefix = takeWhile(p)
    (prefix, drop(prefix.size))
  }

  override def partition(p: (A) => Boolean) = {
    val sz = size
    val l, r = newBuilder[A](sz, autoTrimming)
    var i = 0
    while (i < sz) {
      val elem = at(i)
      if (p(elem)) l += elem
      else r += elem
      i += 1
    }
    (l.result(), r.result())
  }

  override def head: A =
    if (isEmpty) throw new UnsupportedOperationException
    else at(0)

  override def tail: ArraySeq[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else drop(1)

  override def last: A =
    if (isEmpty) throw new UnsupportedOperationException
    else at(size - 1)

  override def init: ArraySeq[A] =
    if (isEmpty) throw new UnsupportedOperationException
    else dropRight(1)

  override def padTo[B >: A](len: Int, elem: B): ArraySeq[B] = {
    val sz = size
    if (len <= sz) this
    else if (len <= 8 || len > 128) {
      val n = len - sz
      appendedAll0(Iterator.continually(elem).take(n), n)
    } else {
      val builder = newBuilder[B](len, autoTrimming)
      builder ++= this
      var i = len - sz
      while (i > 0) {
        builder += elem
        i -= 1
      }
      builder.result()
    }
  }

  def leftPadTo[B >: A](len: Int, elem: B): ArraySeq[B] = {
    val sz = size
    if (len <= sz) this
    else if (len <= 8 || len > 128) {
      val n = len - sz
      prependedAll0(Iterator.continually(elem).take(n), n)
    } else {
      val builder = newBuilder[B](len, autoTrimming)
      var i = len - sz
      while (i > 0) {
        builder += elem
        i -= 1
      }
      builder ++= this
      builder.result()
    }
  }

  @`inline`
  def rightPadTo[B >: A](len: Int, elem: B): ArraySeq[B] = padTo(len, elem)

  override def prepended[B >: A](elem: B): ArraySeq[B] =
    if (nonReversed && underlay.prependedElement(elem, start)) create(underlay, overlay, start - 1, stop, autoTrimming)
    else if (isReversed && underlay.appendedElement(elem, start)) create(underlay, overlay, start + 1, stop, autoTrimming) // as it is reversed start is really stop and stop is really start
    else if (underlay.isFull) elem +: allocate(capacitate(size + 1, autoTrimming.usePadding), underlay, overlay, start, stop, autoTrimming)
    else if (nonReversed) create(underlay, if (overlay != null) overlay + (slot(start - 1, capacity) -> elem) else Overlay((slot(start - 1, capacity), elem)), start - 1, stop, autoTrimming)
    else create(underlay, if (overlay != null) overlay + (slot(start, capacity) -> elem) else Overlay(slot(start, capacity) -> elem), start + 1, stop, autoTrimming) // as it is reversed start is really stop and stop is really start

  override def appended[B >: A](elem: B): ArraySeq[B] =
    if (nonReversed && underlay.appendedElement(elem, stop)) create(underlay, overlay, start, stop + 1, autoTrimming)
    else if (isReversed && underlay.prependedElement(elem, stop)) create(underlay, overlay, start, stop - 1, autoTrimming) // as it is reversed start is really stop and stop is really start
    else if (underlay.isFull) allocate(capacitate(size + 1, autoTrimming.usePadding), underlay, overlay, start, stop, autoTrimming) :+ elem
    else if (nonReversed) create(underlay, if (overlay != null) overlay + (slot(stop, capacity) -> elem) else Overlay(slot(stop, capacity) -> elem), start, stop + 1, autoTrimming)
    else create(underlay, if (overlay != null) overlay + (slot(stop - 1, capacity) -> elem) else Overlay(slot(stop - 1, capacity) -> elem), start, stop - 1, autoTrimming) // as it is reversed start is really stop and stop is really start

  override def prependedAll[B >: A](xs: collection.Iterable[B]): ArraySeq[B] = prependedAll0(xs)

  private def prependedAll0[B >: A](xs: collection.IterableOnce[B], knownSize: Int = -1): ArraySeq[B] =
    xs match {
      case _ if knownSize == 0 =>
        this
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty =>
        this
      case _ if knownSize == 1 =>
        prepended(xs.iterator().next())
      case that: Iterable[B] if that.knownSize == 1 =>
        prepended(that.head)
      case _ if isUncapacitated =>
        from(xs, knownSize)
      case that: ArraySeq[B] if this.isReversed || that.isReversed =>
        from(View.Concat(that, this), this.knownSize + that.knownSize)
      case that: ArraySeq[B] if this.underlay.prependedElements(that, this.start) =>
        create(this.underlay, this.overlay, this.start - that.size, this.stop, autoTrimming)
      case that: ArraySeq[B] if that.underlay.appendedElements(this, that.stop) =>
        create(that.underlay, that.overlay, that.start, that.stop + this.size, autoTrimming)
      case that: ArraySeq[B] =>
        allocate(capacitate(this.size + that.size, autoTrimming.usePadding), underlay, overlay, start, stop, autoTrimming).prependedAll(that)
      case that: Iterable[B] if knownSize > -1 || that.knownSize > -1 =>
        from(View.Concat(that, this), that.knownSize + this.knownSize)
      case _ if knownSize > -1 =>
        from(new View.Patched(this, 0, xs, 0), knownSize + this.knownSize)
      case _ =>
        prependedAll0(from(xs, knownSize), knownSize)
    }

  override def appendedAll[B >: A](xs: collection.Iterable[B]): ArraySeq[B] = appendedAll0(xs)

  private def appendedAll0[B >: A](xs: collection.IterableOnce[B], knownSize: Int = -1): ArraySeq[B] =
    xs match {
      case _ if knownSize == 0 =>
        this
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty =>
        this
      case _ if knownSize == 1 =>
        appended(xs.iterator().next())
      case that: Iterable[B] if that.knownSize == 1 =>
        appended(that.head)
      case _ if isUncapacitated =>
        from(xs, knownSize)
      case that: ArraySeq[B] if this.isReversed || that.isReversed =>
        from(View.Concat(this, that), this.knownSize + that.knownSize)
      case that: ArraySeq[B] if this.underlay.appendedElements(that, this.stop) =>
        create(this.underlay, this.overlay, this.start, this.stop + that.size, autoTrimming)
      case that: ArraySeq[B] if that.underlay.prependedElements(this, that.start) =>
        create(that.underlay, that.overlay, that.start - this.size, that.stop, autoTrimming)
      case that: ArraySeq[B] =>
        allocate(capacitate(this.size + that.size, autoTrimming.usePadding), underlay, overlay, start, stop, autoTrimming).appendedAll(that)
      case that: Iterable[B] if knownSize > -1 || that.knownSize > -1 =>
        from(View.Concat(this, that), that.knownSize + this.knownSize)
      case _ if knownSize > -1 =>
        from(new View.Patched(this, this.knownSize, xs, 0), knownSize + this.knownSize)
      case _ =>
        appendedAll0(from(xs, knownSize), knownSize)
    }

  override def map[B](f: A => B): ArraySeq[B] = {
    (size: @switch) match {
      case 0 => create[B](autoTrimming)
      case 1 => create[B](autoTrimming, f(at(0)))
      case 2 => create[B](autoTrimming, f(at(0)), f(at(1)))
      case 3 => create[B](autoTrimming, f(at(0)), f(at(1)), f(at(2)))
      case 4 => create[B](autoTrimming, f(at(0)), f(at(1)), f(at(2)), f(at(3)))
      case 5 => create[B](autoTrimming, f(at(0)), f(at(1)), f(at(2)), f(at(3)), f(at(4)))
      case 6 => create[B](autoTrimming, f(at(0)), f(at(1)), f(at(2)), f(at(3)), f(at(4)), f(at(5)))
      case 7 => create[B](autoTrimming, f(at(0)), f(at(1)), f(at(2)), f(at(3)), f(at(4)), f(at(5)), f(at(6)))
      case 8 => create[B](autoTrimming, f(at(0)), f(at(1)), f(at(2)), f(at(3)), f(at(4)), f(at(5)), f(at(6)), f(at(7)))
      case sz =>
        if (sz <= 64) {
          val capacity = capacitate(sz, autoTrimming.usePadding)
          val array = new Array[Any](capacity)
          var i = 0
          while (i < sz) {
            array(i) = f(at(i))
            i += 1
          }
          create(array, 0, i, autoTrimming)
        } else {
          val capacity = capacitate(sz, autoTrimming.usePadding)
          val array = new Array[Any](capacity)
          val it = iterator()
          var i = 0
          while (it.hasNext) {
            array(i) = f(it.next())
            i += 1
          }
          create(array, 0, i, autoTrimming)
        }
    }
  }

  override def collect[B](pf: PartialFunction[A, B]): ArraySeq[B] = {
    val sz = size
    if (sz == 0) create[B](autoTrimming)
    else {
      // Copied from `strawman.collection.IterableOps.collectFirst`:
      // Presumably the fastest way to get in and out of a partial function is for a sentinel function to return itself
      // (Tested to be lower-overhead than runWith.  Would be better yet to not need to (formally) allocate it)
      val sentinel: scala.Function1[A, Any] = (a: A) => this
      val builder = newBuilder[B](autoTrimming = autoTrimming)
      var i = 0
      while (i < sz) {
        val elem = at(i)
        val x = pf.applyOrElse(elem, sentinel)
        if (x.asInstanceOf[AnyRef] ne sentinel) builder += x.asInstanceOf[B]
        i += 1
      }
      builder.result()
    }
  }

  override def flatMap[B](f: (A) => IterableOnce[B]) = {
    val sz = size
    if (sz == 0) create(autoTrimming)
    else {
      val builder = newBuilder[B](sz, autoTrimming)
      var i = 0
      while (i < sz) {
        builder ++= f(at(i))
        i += 1
      }
      builder.result()
    }
  }

  override def distinctBy[B](f: A => B): ArraySeq[A] = {
    (size: @switch) match {
      case 0 => this
      case 1 => this
      case 2 =>
        val a0 = at(0)
        val a1 = at(1)
        if (f(a0) == f(a1)) create[A](autoTrimming, a0)
        else create[A](autoTrimming, a0, a1)
      case 3 =>
        val a0 = at(0)
        val a1 = at(1)
        val a2 = at(2)
        val f0 = f(a0)
        val f1 = f(a1)
        val f2 = f(a2)
        if (f0 == f1 && f0 == f2) create[A](autoTrimming, a0)
        else if (f0 == f1 || f1 == f2) create[A](autoTrimming, a0, a2)
        else create[A](autoTrimming, a0, a1, a2)
      case sz =>
        val builder = newBuilder[A](autoTrimming = autoTrimming)
        val seen = mutable.HashSet.empty[B]
        val sz = size
        var i = 0
        while (i < sz) {
          val elem = at(i)
          val id = f(elem)
          if (!seen.contains(id)) {
            seen.add(id)
            builder += elem
          }
          i += 1
        }
        builder.result()
    }
  }

  override def filter(p: (A) => Boolean) = {
    val sz = size
    if (sz == 0) this
    else if (sz == 1) { if (p(at(0))) this else create(autoTrimming) }
    else {
      if (sz <= 32 ) {
        val builder = newBuilder[A](autoTrimming = autoTrimming)
        var i = 0
        while (i < sz) {
          val elem = at(i)
          if (p(elem)) builder += elem
          i += 1
        }
        builder.result()
      } else {
       val builder = newBuilder[A](autoTrimming = autoTrimming)
        val it = iterator()
        while (it.hasNext) {
          val elem = it.next()
          if (p(elem)) builder += elem
        }
        builder.result()
      }
    }
  }

  override def filterNot(p: (A) => Boolean) = filter(x => !p(x))

  override def zipWithIndex = zip(Iterator.range(0, size), size)

  override def zip[B](xs: collection.Iterable[B]): ArraySeq[(A, B)] = zip(xs, minOf(size, xs.size))

  private def zip[B](xs: collection.IterableOnce[B], size: Int): ArraySeq[(A, B)] = {
    (size: @switch) match {
      case 0 => create[(A, B)](autoTrimming)
      case 1 => create[(A, B)](autoTrimming, (at(0), xs.iterator().next()))
      case 2 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()))
      case 3 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()), (at(2), it.next()))
      case 4 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()), (at(2), it.next()), (at(3), it.next()))
      case 5 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()), (at(2), it.next()), (at(3), it.next()), (at(4), it.next()))
      case 6 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()), (at(2), it.next()), (at(3), it.next()), (at(4), it.next()), (at(5), it.next()))
      case 7 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()), (at(2), it.next()), (at(3), it.next()), (at(4), it.next()), (at(5), it.next()), (at(6), it.next()))
      case 8 =>
        val it = xs.iterator()
        create[(A, B)](autoTrimming, (at(0), it.next()), (at(1), it.next()), (at(2), it.next()), (at(3), it.next()), (at(4), it.next()), (at(5), it.next()), (at(6), it.next()), (at(7), it.next()))
      case sz =>
        if (sz <= 32) {
          val builder = newBuilder[(A, B)](sz, autoTrimming)
          val jt = xs.iterator()
          var i = 0
          while (i < sz && jt.hasNext) {
            builder += ((at(i), jt.next()))
            i += 1
          }
          builder.result()
        } else {
          val builder = newBuilder[(A, B)](sz, autoTrimming)
          val it = iterator()
          val jt = xs.iterator()
          while (it.hasNext && jt.hasNext) {
            builder += ((it.next(), jt.next()))
          }
          builder.result()
        }
    }
  }

  override def unzip[L, R](implicit asPair: <:<[A, (L, R)]): (ArraySeq[L], ArraySeq[R]) = {
    (size: @switch) match {
      case 0 =>
        (create[L](autoTrimming), create[R](autoTrimming))
      case 1 =>
        val (l0, r0) = asPair(at(0))
        (create[L](autoTrimming, l0), create[R](autoTrimming, r0))
      case 2 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        (create[L](autoTrimming, l0, l1), create[R](autoTrimming, r0, r1))
      case 3 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        val (l2, r2) = asPair(at(2))
        (create[L](autoTrimming, l0, l1, l2), create[R](autoTrimming, r0, r1, r2))
      case 4 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        val (l2, r2) = asPair(at(2))
        val (l3, r3) = asPair(at(3))
        (create[L](autoTrimming, l0, l1, l2, l3), create[R](autoTrimming, r0, r1, r2, r3))
      case 5 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        val (l2, r2) = asPair(at(2))
        val (l3, r3) = asPair(at(3))
        val (l4, r4) = asPair(at(4))
        (create[L](autoTrimming, l0, l1, l2, l3, l4), create[R](autoTrimming, r0, r1, r2, r3, r4))
      case 6 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        val (l2, r2) = asPair(at(2))
        val (l3, r3) = asPair(at(3))
        val (l4, r4) = asPair(at(4))
        val (l5, r5) = asPair(at(5))
        (create[L](autoTrimming, l0, l1, l2, l3, l4, l5), create[R](autoTrimming, r0, r1, r2, r3, r4, r5))
      case 7 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        val (l2, r2) = asPair(at(2))
        val (l3, r3) = asPair(at(3))
        val (l4, r4) = asPair(at(4))
        val (l5, r5) = asPair(at(5))
        val (l6, r6) = asPair(at(6))
        (create[L](autoTrimming, l0, l1, l2, l3, l4, l5, l6), create[R](autoTrimming, r0, r1, r2, r3, r4, r5, r6))
      case 8 =>
        val (l0, r0) = asPair(at(0))
        val (l1, r1) = asPair(at(1))
        val (l2, r2) = asPair(at(2))
        val (l3, r3) = asPair(at(3))
        val (l4, r4) = asPair(at(4))
        val (l5, r5) = asPair(at(5))
        val (l6, r6) = asPair(at(6))
        val (l7, r7) = asPair(at(7))
        (create[L](autoTrimming, l0, l1, l2, l3, l4, l5, l6, l7), create[R](autoTrimming, r0, r1, r2, r3, r4, r5, r6, r7))
      case sz =>
        if (sz <= 32) {
          val cap = capacity
          val l = new Array[Any](cap)
          val r = new Array[Any](cap)
          var i = 0
          while (i < sz) {
            val (l0, r0) = asPair(at(i))
            l(i) = l0
            r(i) = r0
            i += 1
          }
          (create(l, 0, i, autoTrimming), create(r, 0, i, autoTrimming))
        } else {
          val cap = capacity
          val l = new Array[Any](cap)
          val r = new Array[Any](cap)
          val it = iterator()
          var i = 0
          while (it.hasNext) {
            val (l0, r0) = asPair(it.next())
            l(i) = l0
            r(i) = r0
            i += 1
          }
          (create(l, 0, i, autoTrimming), create(r, 0, i, autoTrimming))
        }
    }
  }

  @`inline`
  override def iterator(): Iterator[A] = iterator(start, stop)

  ////@`inline` // Causes compile error on Dotty
  private def iterator(x: Int, y: Int): Iterator[A] = {
    if (x == y) Iterator.empty
    else {
      val cap = capacity
      val sz = distance(x, y)
      if (sz == 1) Iterator.single(at(0))
      else if (x < y) {
        if (x < 0) {
          val frontStart = slot(x, cap)
          val frontStop = if (y >= 0) cap else slot(y, cap)
          val rearStart = slot(maxOf(x, 0), cap)
          val rearStop = slot(maxOf(y, 0), cap)
          new Iterator[A] {
            private[this] var i = frontStart
            private[this] var _hasNext = i >= frontStart && i < frontStop
            override def hasNext = _hasNext
            override def next() = {
              if (!_hasNext) throw new NoSuchElementException("next on empty iterator")
              val elem = if (overlay == null) underlay(i) else overlay.get(i).getOrElse(underlay(i))
              i += 1
              if (i == frontStop) {
                i = rearStart
                _hasNext = i < rearStop
              } else if (i == rearStop) {
                _hasNext = false
              }
              elem
            }
          }
        } else {
          val rearStart = slot(maxOf(x, 0), cap)
          val rearStop = slot(maxOf(y, 0), cap)
          new Iterator[A] {
            private[this] var i = rearStart
            private[this] var _hasNext = i >= rearStart && i < rearStop
            override def hasNext = _hasNext
            override def next() = {
              if (!_hasNext) throw new NoSuchElementException("next on empty iterator")
              val elem = if (overlay == null) underlay(i) else overlay.get(i).getOrElse(underlay(i))
              i += 1
              if (i >= rearStop) {
                _hasNext = false
              }
              elem
            }
          }
        }
      } else {
        if (x > 0) {
          val frontStart = if (y >= 0) cap else slot(y, cap)
          val frontStop = if (x >= 0) cap else slot(x, cap)
          val rearStart = slot(maxOf(y, 0), cap)
          val rearStop = slot(maxOf(x, 0), cap)
          new Iterator[A] {
            private[this] var i = rearStop - 1
            private[this] var _hasNext = i >= rearStart && i < rearStop
            override def hasNext = _hasNext
            override def next() = {
              if (!_hasNext) throw new NoSuchElementException("next on empty iterator")
              val elem = if (overlay == null) underlay(i) else overlay.get(i).getOrElse(underlay(i))
              if (i == rearStart) {
                i = frontStop
                _hasNext = i > frontStart
              } else if (i == frontStart) {
                _hasNext = false
              }
              i -= 1
              elem
            }
          }
        } else {
          val frontStart = if (y >= 0) cap else slot(y, cap)
          val frontStop = if (x >= 0) cap else slot(x, cap)
          new Iterator[A] {
            private[this] var i = frontStop -1
            private[this] var _hasNext = i >= frontStart && i < frontStop
            override def hasNext = _hasNext
            override def next() = {
              if (!_hasNext) throw new NoSuchElementException("next on empty iterator")
              val elem = if (overlay == null) underlay(i) else overlay.get(i).getOrElse(underlay(i))
              if (i <= frontStart) {
                _hasNext = false
              }
              i -= 1
              elem
            }
          }
        }
      }
    }
  }

  @`inline`
  override def reverseIterator() = iterator(stop, start)

  override def reverse: ArraySeq[A] =
    if (size <= 1) this
    else create(underlay, if (overlay != null) overlay else null, stop, start, autoTrimming)

  override protected[this] def reversed: ArraySeq[A] = reverse

  override def foldLeft[B](z: B)(op: (B, A) => B): B = {
    (size: @switch) match {
      case 0 => z
      case 1 => op(z, at(0))
      case 2 => op(op(z, at(0)), at(1))
      case 3 => op(op(op(z, at(0)), at(1)), at(2))
      case 4 => op(op(op(op(z, at(0)), at(1)), at(2)), at(3))
      case 5 => op(op(op(op(op(z, at(0)), at(1)), at(2)), at(3)), at(4))
      case 6 => op(op(op(op(op(op(z, at(0)), at(1)), at(2)), at(3)), at(4)), at(5))
      case 7 => op(op(op(op(op(op(op(z, at(0)), at(1)), at(2)), at(3)), at(4)), at(5)), at(6))
      case 8 => op(op(op(op(op(op(op(op(z, at(0)), at(1)), at(2)), at(3)), at(4)), at(5)), at(6)), at(7))
      case sz =>
        if (sz <= 32) {
          var acc = z
          var i = 0
          while (i < sz) {
            acc = op(acc, at(i))
            i += 1
          }
          acc
        } else {
          var acc = z
          val it = iterator()
          while (it.hasNext) {
            acc = op(acc, it.next())
          }
          acc
        }
      }
  }

  override def foldRight[B](z: B)(op: (A, B) => B): B = {
    (size: @switch) match {
      case 0 => z
      case 1 => op(at(0), z)
      case 2 => op(at(0), op(at(1), z))
      case 3 => op(at(0), op(at(1), op(at(2), z)))
      case 4 => op(at(0), op(at(1), op(at(2), op(at(3), z))))
      case 5 => op(at(0), op(at(1), op(at(2), op(at(3), op(at(4), z)))))
      case 6 => op(at(0), op(at(1), op(at(2), op(at(3), op(at(4), op(at(5), z))))))
      case 7 => op(at(0), op(at(1), op(at(2), op(at(3), op(at(4), op(at(5), op(at(6), z)))))))
      case 8 => op(at(0), op(at(1), op(at(2), op(at(3), op(at(4), op(at(5), op(at(6), op(at(7), z))))))))
      case sz =>
        if (sz <= 32) {
          var acc = z
          var i = sz - 1
          while (i >= 0) {
            acc = op(at(i), acc)
            i -= 1
          }
          acc
        } else {
          var acc = z
          val it = reverseIterator()
          while (it.hasNext) {
            acc = op(it.next(), acc)
          }
          acc
        }
    }
  }

  override def updated[B >: A](index: Int, elem: B): ArraySeq[B] = {
    if (index < 0 || index >= size) throw new IndexOutOfBoundsException(index.toString)
    else if (identical(elem, at(index))) this
    else if (capacity <= 64) {
      val array = underlay.elements.clone()
      val (frontStart, frontStop, rearStart, rearStop) = segments(start, stop, size)
      layout(null, overlay, start, stop, frontStart, distance(frontStart, frontStop), rearStart, distance(rearStart, rearStop), array)
      array.update(slot(index, start, stop, capacity), elem)
      create(array, start, stop, autoTrimming)
    } else create(underlay, if (overlay != null) overlay + (slot(index, start, stop, capacity) -> elem) else Overlay(slot(index, start, stop, capacity) -> elem), start, stop, autoTrimming, trim = true)
  }

  override def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): ArraySeq[B] = {
    val sz = size
    if (from < 0 || from > sz) throw new IndexOutOfBoundsException(from.toString)
    else if (from == sz) coll.appendedAll0(other)
    else if (from == 0 && maxOf(replaced, 0) == 0) prependedAll0(other)
    else if (from == 0) coll.drop(replaced).prependedAll0(other)
    else {
      val c = coll.withoutAutoTrimming
      val prefix = c.take(from)
      val front = prefix.appendedAll0(other)
      val suffix = c.slice(from + replaced, sz)
      val whole = front.appendedAll0(suffix)
      whole.withAutoTrimming(autoTrimming)
    }
  }

  override def toArray[B >: A: ClassTag]: Array[B] =
    copyToArray(new Array[B](this.size))

  override def copyToArray[B >: A](array: Array[B], start: Int = 0): array.type = {
    if (start < 0 || start > size)
      throw new ArrayIndexOutOfBoundsException(start)
    else if (isReversed) super.copyToArray(array, start)
    else {
      if (array.isInstanceOf[Array[AnyRef]]) {
        val (srcFrontStart, srcFrontStop, srcRearStart, srcRearStop) = segments(start, stop, size)
        val frontLength = distance(srcFrontStop, srcFrontStart)
        val rearLength = distance(srcRearStop, srcRearStart)
        layout(underlay, overlay, start, stop, srcFrontStart, frontLength, srcRearStart, rearLength, array.asInstanceOf[Array[Any]])
      } else {
        super.copyToArray(array, start)
      }
      array
    }
  }

  def grow(size: Int, padded: Boolean = AutoTrimming.DefaultUsePadding): ArraySeq[A] = resize(size, padded)

  def trim(padded: Boolean = false): ArraySeq[A] = resize(this.size, padded)

  def resize(size: Int, padded: Boolean = true): ArraySeq[A] = {
    val capacity = capacitate(size, padded)
    if (size == 0) create(autoTrimming)
    else if (capacity == this.capacity) this
    else if (size < this.size) throw new IllegalArgumentException(s"Specified size of $size is smaller than the current size ${this.size}.")
    else allocate(capacity, underlay, overlay, start, stop, autoTrimming)
  }

  override def iterableFactory: SeqFactory[ArraySeq] = ArraySeq

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): ArraySeq[A] = ArraySeq.from(coll)

  override protected[this] def newSpecificBuilder(): mutable.Builder[A, ArraySeq[A]] = newBuilder(size, autoTrimming = autoTrimming)

  /**
    * CAUTION! This method is highly dangerous since it resets the existing bounds of this instances underlay and removes any overlay. Only use when no other
    * referrers to this instance exist with absolute certainty (for example during benchmarking to disregard any state
    * affected in previous iterations).
    */
  def reset(): ArraySeq[A] = {
    underlay.reset()
    new ArraySeq[A](underlay, null, start, stop, autoTrimming)
  }

  override def className = "ArraySeq"
}

object ArraySeq extends SeqFactory[ArraySeq] {
  val MinCapacity = 8

  final case class AutoTrimming(
      /**
        * The minimum <code>size / capacity</code> percentage allowed before auto trimming.
        * A value <code>&lt;= 0</code> means no auto trimming will take place based on usage percentage.
        * a value <code>&gt;= 100</code> means all returned collections will be auto trimmed.
        */
      minUsagePercentage: Int = AutoTrimming.DefaultMinUsagePercentage,
      /**
        * The maximum <code>overlaySize / capacity</code> percentage allowed before auto trimming.
        * A value <code>&lt;= 0</code> means all returned collections having an overlay will be auto
        * trimmed,
        * a value <code>&gt;= 100</code> means no auto trimming will take place based on overlay size
        * percentage.
        */
      maxOverlaidPercentage: Int = AutoTrimming.DefaultMaxOverlaidPercentage,
      /**
        * Indicates whether to use padding or not.
        * A value of <code>true</code> will only trim down to the smallest possible capacity (which is always
        * a multiple of 2) capable of holding the current number of elements.
        * A value of <code>false</code> will trim down as far as possible while still holding the current
        * number of elements (note: the resulting instance can have a capacity smaller than 8).
        */
      usePadding: Boolean = AutoTrimming.DefaultUsePadding)
  object AutoTrimming {
    /**
      * The default minimum <code>size / capacity</code> percentage allowed before auto trimming.
      */
    val DefaultMinUsagePercentage = 25
    /**
      * The default maximum <code>overlaySize / capacity</code> percentage allowed before auto trimming.
      */
    val DefaultMaxOverlaidPercentage = 50
    /**
      * The default <code>usePadding</code> to use when auto trimming.
      */
    val DefaultUsePadding = true
  /**
      * Never auto trim the returned instance. NOTE: This could lead to memory leakage (see documentation above for more information).
      */
    val Never = AutoTrimming(minUsagePercentage = 0, maxOverlaidPercentage = 100, usePadding = true)
    /**
      * Always auto trim the returned instance. NOTE: This could lead to bad performance (see documentation above for more information).
    */
    val Always = AutoTrimming(minUsagePercentage = 100, maxOverlaidPercentage = 0, usePadding = false)
    /**
      * Auto trim the returned instance using default values for the fields (see documentation above for more information).
      */
    val Default = AutoTrimming()
  }

  val Empty = create(Underlay.empty)
  def empty[A <: Any]: ArraySeq[A] = Empty

  private[immutable] final class Underlay[+A](array: scala.Array[Any], x: Int, y: Int) {
    val start = minOf(x, y)
    val stop = maxOf(x, y)

    def toDebugString: String = s"${array.to(List).mkString("[", ", ", "]")}: Underlay(start: $start, stop: $stop, array: ${array.to(List).zipWithIndex.map(t => s"${t._2}: ${t._1}").mkString("[", ", ", "]").replace("null", "_")})"

    override def toString = toDebugString

    private[this] val bounds = new AtomicLong(pack(start, stop))

    @`inline`
    private[this] def pack(low: Int, high: Int): Long = low.toLong << 32 | (high & 0xffffffffL)
    @`inline`
    private[this] def unpackLow(l: Long): Int = (l >> 32).toInt
    @`inline`
    private[this] def unpackHigh(l: Long): Int = l.toInt

    @`inline`
    def capacity: Int = array.length

    @`inline`
    def elements: Array[Any] = array

    @`inline`
    def apply(s: Int): A = ScalaRunTime.array_apply(array, s).asInstanceOf[A]

    @`inline`
    def isFull: Boolean = {
      val bnds = bounds.get()
      isFull(unpackLow(bnds), unpackHigh(bnds))
    }
    @`inline`
    def isFull(low: Int, high: Int): Boolean = distance(low, high) == capacity

    def prependedElement[B >: A](elem: B, start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val cap = capacity
      if (low < start && identical(elem, apply(slot(start - 1, cap)))) true
      else if (isFull(low, high)) false
      else if (low > -cap && low == start && bounds.compareAndSet(bnds, pack(low - 1, high))) {
        array(slot(start - 1, cap)) = elem
        true
      } else false
    }

    def appendedElement[B >: A](elem: B, stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val cap = capacity
      if (high > stop && identical(elem, apply(slot(stop, cap)))) true
      else if (isFull(low, high)) false
      else if (high < cap && high == stop && bounds.compareAndSet(bnds, pack(low, high + 1))) {
        array(slot(stop, cap)) = elem
        true
      } else false
    }

    def prependedElements[B >: A](that: ArraySeq[B], start: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val lowered = low - that.size
      val cap = capacity
      if (distance(lowered, high) > cap) false
      else if (low == start && lowered >= -cap && bounds.compareAndSet(bnds, pack(lowered, high))) {
        val (srcFrontStart, srcFrontStop, srcRearStart, srcRearStop) = segments(that.start, that.stop, that.capacity)
        val dstFrontStart = slot(lowered, cap)
        val frontLength = distance(srcFrontStop, srcFrontStart)
        val dstRearStart = dstFrontStart + frontLength
        val rearLength = srcRearStop - srcRearStart
        layout(that.underlay, that.overlay, start, stop, srcFrontStart, frontLength, srcRearStart, rearLength, this.array, dstFrontStart, dstRearStart)
        true
      } else false
    }

    def appendedElements[B >: A](that: ArraySeq[B], stop: Int): Boolean = {
      val bnds = bounds.get()
      val low = unpackLow(bnds)
      val high = unpackHigh(bnds)
      val raised = high + that.size
      val cap = capacity
      if (distance(low, raised) > cap) false
      else if (high == stop && raised <= cap && bounds.compareAndSet(bnds, pack(low, raised))) {
        val (srcFrontStart, srcFrontStop, srcRearStart, srcRearStop) = segments(that.start, that.stop, that.capacity)
        val dstFrontStart = slot(high, cap)
        val frontLength = srcFrontStop - srcFrontStart
        val dstRearStart = dstFrontStart + frontLength
        val rearLength = srcRearStop - srcRearStart
        layout(that.underlay, that.overlay, start, stop, srcFrontStart, frontLength, srcRearStart, rearLength, this.array, dstFrontStart, dstRearStart)
        true
      } else false
    }

    /**
      * CAUTION! This method is highly dangerous since it resets the existing bounds of this instance. Only use when no other
      * referrers to this instance exist with absolute certainty (for example during benchmarking to disregard any state
      * affected in previous iterations).
      */
    def reset(): Unit = {
      bounds.set(pack(start, stop))
    }
  }

  private[immutable] object Underlay {
    val Empty = new Underlay(Array.empty, 0, 0)
    @`inline`
    def empty[A <: Any]: Underlay[A] = Empty
    @`inline`
    def apply[A](array: Array[Any], stop: Int): Underlay[A] = new Underlay[A](array, 0, stop)
    @`inline`
    def apply[A](array: Array[Any], start: Int, stop: Int): Underlay[A] = new Underlay[A](array, start, stop)
  }

  private type Overlay[+A] = HashMap[Int, A]
  private val Overlay = HashMap

  //@`inline` // Causes compile error on Dotty
  override def apply[A](xs: A*): ArraySeq[A] = {
    (xs.size: @switch) match {
      case 0 => Empty
      case 1 => create(AutoTrimming.Default, xs(0))
      case 2 => create(AutoTrimming.Default, xs(0), xs(1))
      case 3 => create(AutoTrimming.Default, xs(0), xs(1), xs(2))
      case 4 => create(AutoTrimming.Default, xs(0), xs(1), xs(2), xs(3))
      case 5 => create(AutoTrimming.Default, xs(0), xs(1), xs(2), xs(3), xs(4))
      case 6 => create(AutoTrimming.Default, xs(0), xs(1), xs(2), xs(3), xs(4), xs(5))
      case 7 => create(AutoTrimming.Default, xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6))
      case 8 => create(AutoTrimming.Default, xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7))
      case sz =>
        val capacity = capacitate(sz)
        val array = new Array[Any](capacity)
        xs.copyToArray(array, 0, sz)
        create(array, 0, sz)
    }
  }

  def apply[A](it: Iterable[A]): ArraySeq[A] = from(it)

  def apply[A](array: Array[A], index: Int = 0, length: Int = -1): ArraySeq[A] = {
    val i = minOf(maxOf(index, 0), array.length)
    val l = minOf(maxOf(length, 0), array.length - i)
    if (l == 0) Empty
    else {
      val capacity = capacitate(l)
      val arr = new Array[Any](capacity)
      java.lang.System.arraycopy(array, i, arr, 0, l)
      create(arr, 0, l)
    }
  }

  @`inline`
  private[this] def isExcessive(underlay: Underlay[_], start: Int, stop: Int, autoTrimming: AutoTrimming) = {
    underlay.capacity > capacitate(1, autoTrimming.usePadding) &&
      distance(start, stop) * 100 <= underlay.capacity * autoTrimming.minUsagePercentage
  }

  @`inline`
  private[this] def isOverlaid(underlay: Underlay[_], overlay: Overlay[_], autoTrimming: AutoTrimming) = {
    overlay != null &&
      overlay.size * 100 >= underlay.capacity * autoTrimming.maxOverlaidPercentage
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming): ArraySeq[A] = {
    empty[A].withAutoTrimming(autoTrimming)
  }
  
  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 1
    val array = new Array[Any](capacity)
    array(0) = a0
    create(array, 0, 1, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 2
    val array = new Array[Any](capacity)
    array(0) = a0
    array(1) = a1
    create(array, 0, 2, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A, a2: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 3
    val array = new Array[Any](capacity)
    array(0) = a0
    array(1) = a1
    array(2) = a2
    create(array, 0, 3, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A, a2: A, a3: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 4
    val array = new Array[Any](capacity)
    array(0) = a0
    array(1) = a1
    array(2) = a2
    array(3) = a3
    create(array, 0, 4, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A, a2: A, a3: A, a4: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 5
    val array = new Array[Any](capacity)
    array(0) = a0
    array(1) = a1
    array(2) = a2
    array(3) = a3
    array(4) = a4
    create(array, 0, 5, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A, a2: A, a3: A, a4: A, a5: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 6
    val array = new Array[Any](capacity)
    array(0) = a0
    array(1) = a1
    array(2) = a2
    array(3) = a3
    array(4) = a4
    array(5) = a5
    create(array, 0, 6, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A, a2: A, a3: A, a4: A, a5: A, a6: A): ArraySeq[A] = {
    val capacity = if (autoTrimming.usePadding) 8 else 7
    val array = new Array[Any](capacity)
    array(0) = a0
    array(1) = a1
    array(2) = a2
    array(3) = a3
    array(4) = a4
    array(5) = a5
    array(6) = a6
    create(array, 0, 7, autoTrimming)
  }

  @`inline`
  private def create[A](autoTrimming: AutoTrimming, a0: A, a1: A, a2: A, a3: A, a4: A, a5: A, a6: A, a7: A): ArraySeq[A] = {
    val array = new Array[Any](8)
    array(0) = a0
    array(1) = a1
    array(2) = a2
    array(3) = a3
    array(4) = a4
    array(5) = a5
    array(6) = a6
    array(7) = a7
    create(array, 0, 8, autoTrimming)
  }

  private def create[A](underlay: Underlay[A], overlay: Overlay[A], start: Int, stop: Int, autoTrimming: AutoTrimming, trim: Boolean): ArraySeq[A] = {
    if (trim && (
        isExcessive(underlay, start, stop, autoTrimming) ||
        isOverlaid(underlay, overlay, autoTrimming)))
      allocate(underlay, overlay, start, stop, autoTrimming)
    else
      create(underlay, overlay, start, stop, autoTrimming)
  }

  @`inline`
  private def create[A](array: Array[Any], start: Int, stop: Int): ArraySeq[A] =
    create(array, start, stop, AutoTrimming.Default)

  @`inline`
  private def create[A](array: Array[Any], start: Int, stop: Int, autoTrimming: AutoTrimming): ArraySeq[A] =
    create(Underlay(array, start, stop), null, start, stop, autoTrimming)

  @`inline`
  private def create[A](underlay: Underlay[A]): ArraySeq[A] =
    create(underlay, null, underlay.start, underlay.stop, AutoTrimming.Default)

  // @`inline` // No inlining of private constructors in Dotty.
  private def create[A](underlay: Underlay[A], overlay: Overlay[A], start: Int, stop: Int, autoTrimming: AutoTrimming): ArraySeq[A] =
    new ArraySeq[A](underlay, overlay, start, stop, autoTrimming)

  @`inline`
  private def slot(i: Int, start: Int, stop: Int, capacity: Int): Int =
    if (ArraySeq.isReversed(start, stop)) slot(start - i - 1, capacity)
    else slot(start + i, capacity)

  @`inline`
  private def slot(p: Int, capacity: Int): Int =
    if (p >= 0) p
    else capacity + p

  /*
  //@`inline`
  private def pow2(n: Int): Int = {
    if (n <= MinCapacity) MinCapacity
    else {
      var pow = n - 1
      pow |= (pow >>> 1)
      pow |= (pow >>> 2)
      pow |= (pow >>> 4)
      pow |= (pow >>> 8)
      pow |= (pow >>> 16)
      pow += 1
      if (pow < 0) pow >>>= 1
      pow
    }
  }
  */

  @`inline`
  private def capacitate(n: Int, padded: Boolean = AutoTrimming.DefaultUsePadding): Int = {
    if (padded) {
      if (n <= 0) 0
      else if (n <= 8) 8
      else if (n <= 16) 16
      else if (n <= 32) 32
      else if (n <= 64) 64
      else if (n <= 128) 128
      else {
        var pow = n - 1
        pow |= (pow >>> 1)
        pow |= (pow >>> 2)
        pow |= (pow >>> 4)
        pow |= (pow >>> 8)
        pow |= (pow >>> 16)
        pow += 1
        if (pow < 0) pow >>>= 1
        pow
      }
    } else maxOf(n, 0)
  }

  @`inline`
  private def allocate[A](underlay: Underlay[A], overlay: Overlay[A], start: Int, stop: Int, autoTrimming: AutoTrimming): ArraySeq[A] = {
    allocate(capacitate(distance(start, stop), autoTrimming.usePadding), underlay, overlay, start, stop, autoTrimming)
  }

  private def allocate[A](capacity: Int, underlay: Underlay[A], overlay: Overlay[A], start: Int, stop: Int, autoTrimming: AutoTrimming): ArraySeq[A] = {
    val (srcFrontStart, srcFrontStop, srcRearStart, srcRearStop) = segments(start, stop, underlay.capacity)
    val array = new Array[Any](capacity)
    val frontLength = distance(srcFrontStop, srcFrontStart)
    val rearLength = distance(srcRearStop, srcRearStart)
    layout(underlay, overlay, start, stop, srcFrontStart, frontLength, srcRearStart, rearLength, array)

    // Shift polarity of frontLength when creating underlay (since negative values are used to count from the end of the array).
    if (isReversed(start, stop))
      create(array, rearLength, -frontLength, autoTrimming)
    else
      create(array, -frontLength, rearLength, autoTrimming)
  }

  //@`inline` // Causes compile error on Dotty
  private def layout(underlay: Underlay[_], overlay: Overlay[_], start: Int, stop: Int, srcFrontStart: Int, frontLength: Int, srcRearStart: Int, rearLength: Int, dst: Array[Any], dstFrontStart: Int = -1, dstRearStart: Int = 0): Unit = {
    if (underlay != null) {
      if (rearLength > 0)
        java.lang.System.arraycopy(underlay.elements, srcRearStart, dst, dstRearStart, rearLength)
      if (frontLength > 0)
        java.lang.System.arraycopy(underlay.elements, srcFrontStart, dst, if (dstFrontStart >= 0) dstFrontStart else dst.length - frontLength, frontLength)
    }
    if (overlay != null) {
      val it = overlay.iterator()
      while (it.hasNext) {
        val (i, elem) = it.next()
        var j = i - srcRearStart
        if (j >= underlay.capacity) j -= underlay.capacity
        dst(j) = elem
      }
    }
  }

  @`inline`
  private def identical(a: Any, b: Any) = {
    (a.isInstanceOf[AnyRef] && b.isInstanceOf[AnyRef] && a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])) ||
    (a == b)
  }

  override def tabulate[A](n: Int)(f: Int => A): ArraySeq[A] = {
    (n: @switch) match {
      case 0 => Empty
      case 1 => apply(f(0))
      case 2 => apply(f(0), f(1))
      case 3 => apply(f(0), f(1), f(2))
      case 4 => apply(f(0), f(1), f(2), f(3))
      case 5 => apply(f(0), f(1), f(2), f(3), f(4))
      case 6 => apply(f(0), f(1), f(2), f(3), f(4), f(5))
      case 7 => apply(f(0), f(1), f(2), f(3), f(4), f(5), f(6))
      case 8 => apply(f(0), f(1), f(2), f(3), f(4), f(5), f(6), f(7))
      case _ =>
        val capacity = capacitate(n)
        val array = new Array[Any](capacity)
        var i = 0
        while (i < n) {
          array(i) = f(i)
          i += 1
        }
        create(array, 0, n)
    }
  }

  override def fill[A](n: Int)(elem: => A): ArraySeq[A] = tabulate(n)(_ => elem)

  @`inline`
  def from[A](xs: A*): ArraySeq[A] = from(View.Elems(xs: _*), -1)

  @`inline`
  def from[A](xs: collection.IterableOnce[A]): ArraySeq[A] = from(xs, -1)

  def from[A](xs: collection.IterableOnce[A], knownSize: Int): ArraySeq[A] = xs match {
    case that: ArraySeq[A] ⇒ that
    case _ =>
      val sz =
        if (knownSize > 0) knownSize
        else xs match {
          case ys: Iterable[_] => ys.knownSize
          case _ => -1
        }
      if (sz == 0) Empty
      else if (sz == 1) ArraySeq(xs.iterator().next())
      else {
        val builder = newBuilder[A](sz)
        builder ++= xs
        builder.result()
      }
  }

  /**
    * CAUTION! This method is dangerous since it does not copy the provided array but instead uses the instance directly.
    * Any later modifications to the provided array will show through to the returned `ArraySeq` instance.
    * @param array array to use for element storage
    * @param length the number of elements present in the array (defaults to -1 which will use the array length instead)
    * @tparam A the element type to use
    * @return a new instance using the provided array for element storage (or the empty `ArraySeq` if the provided array is empty).
    */
  def wrap[A](array: Array[Any], length: Int = -1): ArraySeq[A] =
    if (array.length == 0) Empty
    else {
      val sz = if (length > -1) length else array.length
      create(array, 0, sz)
    }

  /** A class to build instances of `ArraySeq`.  This builder is reusable. */
  final class Builder[A] private[ArraySeq] (private var sizeHint: Int, autoTrimming: AutoTrimming) extends ReusableBuilder[A, ArraySeq[A]] {
    private var length = 0
    private var capacity = 0
    private var elems: Array[Any] = _

    override def clear() = {
      sizeHint = -1
      length = 0
      capacity = 0
      elems = null
    }

    override def sizeHint(size: Int) = {
      sizeHint = size
    }

    override def add(elem: A) = {
      if (length == capacity) {
        capacity = capacitate(maxOf(length + 1, sizeHint))
        val array = new Array[Any](capacity)
        if (length > 0) java.lang.System.arraycopy(elems, 0, array, 0, length)
        elems = array
      }
      elems(length) = elem
      length += 1
      this
    }

    override def addAll(xs: IterableOnce[A]) = {
      val it = xs.iterator()
      while (it.hasNext)
        add(it.next())
      this
  }

    override def result(): ArraySeq[A] = {
      if (length == 0) create(autoTrimming)
      else {
        val array = elems
        elems = null
        if (!autoTrimming.usePadding && length < array.length) create(array, 0, length, autoTrimming).trim()
        else create(array, 0, length, autoTrimming)
      }
    }
  }

  def newBuilder[A](): Builder[A] = newBuilder(-1, AutoTrimming.Default)
  def newBuilder[A](sizeHint: Int = -1, autoTrimming: AutoTrimming = AutoTrimming.Default): Builder[A] = new Builder[A](sizeHint, autoTrimming)

  @`inline`
  private def isReversed(start: Int, stop: Int): Boolean = start > stop

  @`inline`
  private def segments(start: Int, stop: Int, capacity: Int): (Int, Int, Int, Int) = {
    val x = minOf(start, stop)
    val y = maxOf(start, stop)
    if (x < 0 && y <= 0) (capacity + x, capacity + y, 0, 0)
    else if (x < 0) (capacity + x, capacity, 0, y)
    else if (y < 0) (capacity + y, capacity, 0, x)
    else (0, 0, x, y)
  }

  @`inline`
  private def distance(x: Int, y: Int): Int =
    if (x < y) y - x
    else x - y

  @`inline`
  private def maxOf(x: Int, y: Int): Int = if (x >= y) x else y
  @`inline`
  private def minOf(x: Int, y: Int): Int = if (x <= y) x else y
}

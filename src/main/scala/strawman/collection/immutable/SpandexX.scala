package strawman
package collection
package immutable

import java.util.concurrent.atomic.AtomicInteger
import scala.{Any, AnyRef, Array, ArrayIndexOutOfBoundsException, Boolean, Int, NoSuchElementException, Nothing, StringContext, Unit, `inline`}
import scala.Predef.{String, genericWrapArray}
import scala.math
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
  * The underlying array is only mutated on <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and
  * <code>concat</code> but never more than once for any given position (any later modification attempts to an already modified position
  * results in a copy being made of the underlying array).
  * <br/>
  * <br/>
  * To guarantee that only a single thread can write to an array slot a pair of atomic integers are used to guard the low and high
  * assignments in <code>prepend</code>/<code>prependAll</code>, <code>append</code>/<code>appendAll</code> and <code>concat</code>.
  * <br/>
  * <br/>
  * Expansion occurs when the underlying array is full on the effected side; the new array will be populated to have its start position
  * adjusted proportionally to the unused capacity (margin) on the non effected side. This expansion scheme leads to more free slots being allocated on
  * the side mostly expanded (a margin of zero will allocate an equal amount of free slots on both sides with a right side bias if the
  * free slot count is odd).
  */
sealed abstract class SpandexX[+A] private (private[SpandexX] val index: Int, lengthVector: Int)
    extends Seq[A]
    with LinearSeq[A]
    with IndexedSeq[A]
    with LinearSeqOps[A, SpandexX, SpandexX[A]]
    with IndexedSeqOps[A, SpandexX, SpandexX[A]]
    with StrictOptimizedIterableOps[A, SpandexX[A]] {

  def toDebugString: String

  override def toString = toDebugString

  // Could be private but for test case access
  private[immutable] def primary: SpandexX.Primary[A]

  protected def elements: Array[Any] = primary.elements

  // The lengthVector field is used to represent both length and direction (if negative the Spandex is reversed)
  // Should be able to annotate this method as @`inline` but that makes concat tests case fail on Dotty... ^:-/
  //@`inline`
  private[SpandexX] def isReversed: Boolean = lengthVector < 0

  @`inline`
  protected final def element(i: Int): A =
    ScalaRunTime.array_apply(elements, i).asInstanceOf[A]

  @`inline`
  private[this] final def index(i: Int): Int =
    if (isReversed) index + (length - 1) - i
    else index + i

  @`inline`
  private[this] final def fetch(i: Int): A = element(index(i))

  // The lengthVector field is used to represent both length and direction (if negative the Spandex is reversed)
  override final def length: Int = if (lengthVector >= 0) lengthVector else -lengthVector

  override final def size: Int = length

  override final def knownSize: Int = length

  override final def isEmpty: Boolean = length == 0

  override final def nonEmpty: Boolean = !isEmpty

  override final def apply(i: Int): A =
    if (i < 0 || i >= length) throw new ArrayIndexOutOfBoundsException(i)
    else fetch(i)

  override final def head: A = apply(0)

  override final def last: A = apply(length - 1)

  override final def slice(from: Int, until: Int): SpandexX[A] =
    if (until <= from) SpandexX.empty
    else
      new SpandexX.Secondary[A](
        primary,
        if (isReversed) index(math.min(until, length)) + 1 else index(math.min(from, length)),
        until - from,
        isReversed)

  override final def take(n: Int): SpandexX[A] =
    slice(0, math.min(n, length))

  override final def takeRight(n: Int): SpandexX[A] =
    slice(length - math.min(n, length), length)

  override final def drop(n: Int): SpandexX[A] =
    slice(n, length)

  override final def dropRight(n: Int): SpandexX[A] =
    slice(0, length - math.min(n, length))

  final def trim(): SpandexX[A] =
    if (length == 0) SpandexX.Empty
    else if (length == elements.length) SpandexX.wrap(elements)
    else {
      val array = new Array[Any](length)
      java.lang.System.arraycopy(elements, index, array, 0, length)
      new SpandexX.Primary[A](array, 0, length)
    }

  override final def prepend[B >: A](b: B): SpandexX[B] =
    if (isEmpty) SpandexX(b)
    else if (isReversed && primary.raise(b, index, length))
      new SpandexX.Secondary[B](primary, index, length + 1, isReversed)
    else if (!isReversed && primary.lower(b, index, length))
      new SpandexX.Secondary[B](primary, index - 1, length + 1, isReversed)
    else {
      SpandexX.expand(length + 1, elements.length - primary.high.get() - 1) {
        case k if k == 0 ⇒ b
        case k ⇒ fetch(k - 1)
      }
    }

  override final def append[B >: A](b: B): SpandexX[B] =
    if (isEmpty) SpandexX(b)
    else if (isReversed && primary.lower(b, index, length))
      new SpandexX.Secondary[B](primary, index - 1, length + 1, isReversed)
    else if (!isReversed && primary.raise(b, index, length))
      new SpandexX.Secondary[B](primary, index, length + 1, isReversed)
    else {
      SpandexX.expand(length + 1, -primary.low.get()) {
        case k if k == length ⇒ b
        case k ⇒ fetch(k)
      }
    }

  /** Alias for `prependAll` */
  @`inline` final def ++:[B >: A](xs: IterableOnce[B]): SpandexX[B] = prependAll(xs)

  final def prependAll[B >: A](xs: IterableOnce[B]): SpandexX[B] =
    xs match {
      case _ if this.isEmpty => SpandexX.fromIterable(xs)
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty => this
      case that: SpandexX.Secondary[B] if that.isReversed => fromIterable(View.Concat(coll, xs))
      case that: SpandexX[B]
        if that.primary.raise(this.elements, this.index, this.length, that.index, that.length) =>
        new SpandexX.Secondary[B](that.primary, this.index, that.length + this.length, isReversed)
      case that: SpandexX[B]
          if this.primary.lower(that.elements, that.index, that.length, this.index, this.length) =>
        new SpandexX.Secondary[B](this.primary, this.index - that.length, this.length + that.length, isReversed)
      case that: SpandexX[B] =>
        val size = this.size + that.size
        val capacity = SpandexX.capacitate(size)
        val array = new Array[Any](capacity)
        val index = (capacity - size) / 2
        java.lang.System.arraycopy(that.elements, that.index, array, index, that.size)
        java.lang.System.arraycopy(this.elements, this.index, array, index + that.size, this.size)
        new SpandexX.Primary[A](array, index, size)
      case _ => SpandexX.fromIterable(xs).concat(this)
    }

  /** Alias for `appendAll` */
  @`inline` final def :++[B >: A](xs: IterableOnce[B]): SpandexX[B] = appendAll(xs)

  /** Alias for `concat` */
  @`inline` final def appendAll[B >: A](xs: IterableOnce[B]): SpandexX[B] = concat(xs)

  override final def concat[B >: A](xs: IterableOnce[B]): SpandexX[B] =
    xs match {
      case _ if this.isEmpty => SpandexX.fromIterable(xs)
      case that: Iterable[B] if that.knownSize == 0 || that.isEmpty => this
      case _ if this.isReversed => fromIterable(View.Concat(coll, xs))
      case that: SpandexX.Secondary[B] if that.isReversed => fromIterable(View.Concat(coll, xs))
      case that: SpandexX[B]
          if this.primary.raise(that.elements, that.index, that.length, this.index, this.length) =>
        new SpandexX.Secondary[B](this.primary, this.index, this.length + that.length, isReversed)
      case that: SpandexX[B]
          if that.primary.lower(this.elements, this.index, this.length, that.index, that.length) =>
        new SpandexX.Secondary[B](that.primary, that.index - this.length, that.length + this.length, isReversed)
      case that: SpandexX[B] =>
        val size = this.size + that.size
        val capacity = SpandexX.capacitate(size)
        val array = new Array[Any](capacity)
        val index = (capacity - size) / 2
        java.lang.System.arraycopy(this.elements, this.index, array, index, this.size)
        java.lang.System.arraycopy(that.elements, that.index, array, index + this.size, that.size)
        new SpandexX.Primary[A](array, index, size)
      case _ => fromIterable(View.Concat(coll, xs))
    }

  override final def map[B](f: A => B): SpandexX[B] =
    if (isEmpty) SpandexX.empty
    else {
      val array = new Array[Any](elements.length)
      var i = length - 1
      while (i >= 0) {
        array(index + i) = f(fetch(i))
        i -= 1
      }
      new SpandexX.Primary[B](array, index, length)
    }

  override final def zip[B](xs: IterableOnce[B]): SpandexX[(A, B)] = xs match {
    case that: SpandexX[B] =>
      SpandexX.tabulate(math.min(this.length, that.length)) { i =>
        (this.apply(i), that.apply(i))
      }
    case _ => fromIterable[(A, B)](View.Zip[A, B](coll, xs))
  }

  override final def iterator(): Iterator[A] =
    if (isReversed) new Iterator[A] {
      private[this] var i = index + SpandexX.this.length
      override def hasNext: Boolean = i > index
      override def next(): A = {
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")
        i -= 1
        element(i)
      }
    } else new Iterator[A] {
        private[this] final val n = index + SpandexX.this.length
        private[this] var i = index
        override final def hasNext: Boolean = i < n
        override final def next(): A = {
          if (!hasNext) throw new NoSuchElementException("next on empty iterator")
          val j = i
          i += 1
          element(j)
        }
      }

  override final def foreach[U](f: (A) => U): Unit =
    if (isReversed) {
      var i = index + length - 1
      val n = index
      while (i >= n) {
        f(element(i))
        i -= 1
      }
    } else {
      var i = index
      val n = index + length
      while (i < n) {
        f(element(i))
        i += 1
      }
    }

  override final def reverse: SpandexX[A] =
    new SpandexX.Secondary[A](primary, index, length, !isReversed)

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

  override final def copyToArray[B >: A](xs: Array[B], start: Int): xs.type =
    if (start < 0 || start > length)
      throw new ArrayIndexOutOfBoundsException(start)
    else if (isReversed) {
      var i = start
      val it = iterator()
      while (it.hasNext) {
        xs(i) = it.next()
        i += 1
      }
      xs
    } else {
      java.lang.System.arraycopy(this.elements, this.index + start, xs, 0, this.length - start)
      xs
    }

  override protected[this] final def fromIterable[B](c: collection.Iterable[B]): SpandexX[B] = SpandexX.fromIterable(c)

  override def iterableFactory: IterableFactory[SpandexX] = SpandexX

  protected[this] final def fromSpecificIterable(coll: collection.Iterable[A]): SpandexX[A] = fromIterable(coll)

  //protected[this] final def newBuilder: Builder[A, Spandex[A]] = Spandex.newBuilder[A]()
  override protected[this] def newSpecificBuilder(): Builder[A, SpandexX[A]] = SpandexX.newBuilder()

  override final def className = "Spandex"
}

object SpandexX extends IterableFactory[SpandexX] {
  private[immutable] class Primary[+A](
        override val elements: scala.Array[Any],
        index: Int,
        length: Int)
      extends SpandexX[A](index, length) {

    def toDebugString: String = s"Primary(index: $index, length: $length, elements: ${elements.toList.zipWithIndex.map(t => s"${t._2}: ${t._1}").mkString("[", ", ", "]").replace("null", "_")})"

    private[SpandexX] final val low: AtomicInteger = new AtomicInteger(index)
    private[SpandexX] final val high: AtomicInteger = new AtomicInteger(index + length - 1)

    override private[immutable] final def primary: Primary[A] = this

    private[SpandexX] final def lower[B >: A](b: B, i: Int, l: Int): Boolean = {
      val lo = low.get
      if (lo < i &&
        ((b
          .isInstanceOf[AnyRef] && b
          .asInstanceOf[AnyRef]
          .eq(elements(i - 1).asInstanceOf[AnyRef]))
        || (!b.isInstanceOf[AnyRef] && b == element(i - 1)))) true
      else if (lo > 0 && lo == i && low.compareAndSet(i, i - 1)) {
        elements(i - 1) = b
        true
      } else false
    }

    private[SpandexX] final def lower[B >: A](
          bs: Array[B],
          bi: Int,
          bl: Int,
          i: Int,
          l: Int): Boolean = {
      val lo = low.get
      val j = lo - bl
      if (lo > bl && lo == i && low.compareAndSet(lo, j)) {
        java.lang.System.arraycopy(bs, bi, elements, j, bl)
        true
      } else false
    }

    private[SpandexX] final def raise[B >: A](b: B, i: Int, l: Int): Boolean = {
      val hi = high.get
      val j = i + l
      if (hi > (j - 1) &&
        ((b.isInstanceOf[AnyRef] && b
          .asInstanceOf[AnyRef]
          .eq(elements(j).asInstanceOf[AnyRef]))
        || (!b.isInstanceOf[AnyRef] && b == element(j)))) true
      else if (hi < elements.length - 1 && hi == (j - 1) && high.compareAndSet(hi, j)) {
        elements(j) = b
        true
      } else false
    }

    private[SpandexX] final def raise[B >: A](
          bs: Array[B],
          bi: Int,
          bl: Int,
          i: Int,
          l: Int): Boolean = {
      val hi = high.get
      val j = i + l
      if (hi < elements.length - bl && hi == (j - 1) && high.compareAndSet(hi, hi + bl)) {
        java.lang.System.arraycopy(bs, bi, elements, j, bl)
        true
      } else false
    }
  }

  private[immutable] final class Secondary[+A](
        override val primary: Primary[A],
        index: Int,
        length: Int,
        reversed: Boolean)
      extends SpandexX[A](index, if (reversed) -length else length) {
    def toDebugString: String = s"Secondary(index: $index, length: $length, reversed: $reversed, primary: ${primary.toDebugString})"
  }

  private[immutable] final object Empty extends Primary[Nothing](Array.empty, 0, 0) {
    override def toDebugString: String = s"Empty"
  }

  override def apply[A](xs: A*): SpandexX[A] =
    apply(xs.toArray[Any], 0, xs.length)

  def apply[A](xs: SpandexX[A]): SpandexX[A] =
    xs.trim()

  def apply[A](it: Iterable[A]): SpandexX[A] =
    fromIterable(it)

  private[SpandexX] def capacitate(n: Int): Int = ((math.max(n, 8) + 7) / 8) * 8

  private[SpandexX] def apply[A](xs: Array[Any], i: Int, n: Int): SpandexX[A] =
    if (n == 0) SpandexX.Empty
    else {
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      val index = (capacity - n) / 2
      java.lang.System.arraycopy(xs, i, array, index, n)
      new Primary[A](array, index, n)
    }

  /**
    * Creates a new empty `Spandex` capable of holding the specified number of elements and initial position.
    * @param capacity the size of the underlying array
    * @param position the initial position in the array (default is `capacity / 2`)
    * @tparam A the type of the elements
    * @return an empty `Spandex` with the specified capacity and initial position
    */
  def create[A](capacity: Int, position: Int = -1): SpandexX[A] =
    if (capacity <= 0) SpandexX.Empty
    else {
      val array = new Array[Any](capacity)
      new Primary[A](array, if (position >= 0) math.min(position, capacity - 1) else capacity / 2, 0)
    }

  def tabulate[A](n: Int)(f: Int => A): SpandexX[A] =
    if (n == 0) SpandexX.Empty
    else {
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      val first = (capacity - n) / 2
      val last = first + n - 1
      var i = first
      while (i <= last) {
        array(i) = f(i - first)
        i += 1
      }
      new Primary[A](array, first, n)
    }

  private[SpandexX] def expand[A](n: Int, margin: Int)(f: Int => A): SpandexX[A] =
    if (n == 0) SpandexX.Empty
    else {
      val capacity = capacitate((n - 1) * 2)
      val array = new Array[Any](capacity)
      val first = (capacity - n + margin) / 2
      val last = first + n - 1
      var i = first
      while (i <= last) {
        array(i) = f(i - first)
        i += 1
      }
      new Primary[A](array, first, n)
    }

  override def fill[A](n: Int)(elem: => A): SpandexX[A] = tabulate(n)(_ => elem)

  def fromIterable[A](it: collection.IterableOnce[A]): SpandexX[A] = it match {
    case c: Iterable[A] => fromIterable(c)
    case _ =>
      val builder = SpandexX.newBuilder[A]()
      builder ++= it
      builder.result()
  }

  def fromIterable[A](it: collection.Iterable[A]): SpandexX[A] = it match {
    case that: SpandexX[A] ⇒ that
    case c if c.knownSize == 0 || c.isEmpty => SpandexX.Empty
    case c if c.knownSize > 0 =>
      val n = c.knownSize
      val capacity = capacitate(n)
      val array = new Array[Any](capacity)
      val index = (capacity - n) / 2
      var i = 0
      val it = c.iterator()
      while (i < n) {
        array(index + i) = it.next()
        i += 1
      }
      new SpandexX.Primary[A](array, index, n)
    case _ ⇒
      val array = ArrayBuffer.fromIterable(it).asInstanceOf[ArrayBuffer[Any]].toArray
      apply(array, 0, array.length)
  }

  /**
    * Used to wrap an existing array (without copying). Observe that later modifications to the specified array will show through to the returned instance.
    * @param xs array to use for elements
    * @tparam A the element type to use
    * @return a new instance using the specified array as element storage (or the empty spandex if the array is empty).
    */
  private[strawman] def wrap[A](xs: Array[Any]): SpandexX[A] =
    if (xs.length == 0) SpandexX.Empty
    else new Primary[A](xs, 0, xs.length)

  def newBuilder[A](): Builder[A, SpandexX[A]] =
    new GrowableBuilder(ArrayBuffer.empty[A])
      .mapResult(b => SpandexX.wrap(b.asInstanceOf[ArrayBuffer[Any]].toArray))

  override def empty[A <: Any]: SpandexX[A] = SpandexX.Empty
}

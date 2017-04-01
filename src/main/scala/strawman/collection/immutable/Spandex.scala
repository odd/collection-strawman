package strawman.collection.immutable

import java.util.concurrent.atomic.AtomicInteger
import scala.{Any, AnyRef, Array, ArrayIndexOutOfBoundsException, Boolean, Float, Int, None, Nothing, Option, Some, StringContext, Unit, volatile}
import scala.Predef.{???, String, identity, println}
import scala.math
import scala.reflect.ClassTag
import strawman.collection
import strawman.collection.{BuildFrom, IterableFactory, IterableOnce, Iterator, LinearSeq, MonoBuildable, PolyBuildable, SeqLike, View, immutable}
import strawman.collection.mutable.{ArrayBuffer, Builder}

/**
  * A <i>Spandex</i> is an (ostensible) immutable array-like collection designed to support the following performance characteristics:
  * <ul>
  * <li>constant time <code>head</code>, <code>last</code>, <code>tail</code>, <code>init</code>, <code>take</code>, <code>drop</code> and <code>reverse</code> (<code>last</code> and <code>init</code> are not included in this implementation however)</li>
  * <li>amortised constant time <code>prepend</code> and <code>append</code> (depending on the complexity of <code>scala.Array.copy</code>)</li>
  * <li>efficient indexed access</li>
  * <li>linear time iteration (i.e. <code>iterator</code> and <code>foreach</code>) with low constant factors
  * <li>reasonable memory usage (at most double (plus constants) that of an array with the same number of elements)
  * </ul>
  * <br/>
  * The underlying array is only mutated on <code>prepend</code> and <code>append</code> but never more than once for any given position (any later modification attempts to an already modified position results in a copy being made of the underlying array). To guarantee that only a single thread can write to an array slot synchronization is used during slot assignment in <code>prepend</code> and <code>append</code>. All other access is non synchronised.
  * <br/>
  * <br/>
  * Expansion occurs when the underlying array is full on the effected side; the new array will be populated to have its center position adjusted by the unused capacity (margin) on the non effected side according to the following formula: <code>starting position = (length + margin) / 2</code> (i.e. for a six element spandex with two slots unused at the end <code>[a, b, c, d, e, f, , ]</code>, a prepend operation would make the expanded array have a length of 12 and a starting position of <code>(6 + 2) / 2 = 4</code>, <code>[ , , , , a, b, c, d, e, f, , ]</code>). This expansion scheme leads to more free slots being allocated on the side mostly expanded (a margin of zero will allocate an equal amount of free slots on both sides).
  */
sealed abstract class Spandex[+A](protected val index: Int, override val length: Int)
  extends Seq[A]
    with SeqLike[A, Spandex]
    with LinearSeq[A]
    with MonoBuildable[A, Spandex[A]]
    with PolyBuildable[A, Spandex] {

  protected def primary: Spandex.Primary[A]
  protected def reversed: Boolean

  protected def elements: Array[Any] = primary.elements

  override final def size: Int = length
  override final def knownSize: Int = length
  override final def isEmpty: Boolean = length == 0
  override final def nonEmpty: Boolean = !isEmpty
  protected final def element(i: Int): A = scala.runtime.ScalaRunTime.array_apply(elements, i).asInstanceOf[A]
  protected final def shift(i: Int): Int =
    if (reversed) index + length - 1 - i
    else index + i
  protected final def fetch(i: Int): A = element(shift(i))

  override final def apply(i: Int): A =
    if (i < 0 || i >= length) throw new ArrayIndexOutOfBoundsException(i)
    else fetch(i)

  override final def head: A = apply(0)

  override final def tail: Spandex[A] =
    if (length > 0) new Spandex.Secondary[A](primary, shift(1), length - 1)
    else ???

  final def +:[B >: A](b: B): Spandex[B] =
    if (isEmpty) Spandex(b)
    else if (reversed && primary.raise(b, index, length)
      || !reversed && primary.lower(b, index, length))
      new Spandex.Secondary[B](primary, index - 1, length + 1, reversed)
    else if (!reversed) b +: Spandex[A](elements, index, length)
    else Spandex.tabulate(length + 1, elements.length - length - index) {
      case k if k == 0 ⇒ b
      case k ⇒ fetch(k - 1)
    }
  final def :+[B >: A](b: B): Spandex[B] =
    if (isEmpty) Spandex(b)
    else if (reversed && primary.lower(b, index, length)
      || !reversed && primary.raise(b, index, length))
      new Spandex.Secondary[B](primary, index, length + 1, reversed)
    else if (!reversed) Spandex[A](elements, index, length) :+ b
    else Spandex.tabulate(length + 1, -index) {
      case k if k == length ⇒ b
      case k ⇒ fetch(k)
    }

  override final def ++[B >: A](xs: IterableOnce[B]): Spandex[B] = xs match {
    case _ if this.isEmpty => Spandex.fromIterable(xs)
    case that: Iterable[B] if that.isEmpty => this
    case _ if this.reversed => fromIterable(View.Concat(coll, xs))
    case that: Spandex.Secondary[B] if that.reversed => fromIterable(View.Concat(coll, xs))
    case that: Spandex[B] if this.primary.raise(that.elements, that.index, that.length, this.index, this.length) =>
      new Spandex.Secondary[B](this.primary, this.index, this.length + that.length)
    case that: Spandex[B] if that.primary.lower(this.elements, this.index, this.length, that.index, that.length) =>
      new Spandex.Secondary[B](that.primary, that.index, that.length + this.length)
    case that: Spandex[B] =>
      val size = this.size + that.size
      val array = new Array[Any](size * 2)
      val index = size / 2
      Array.copy(this.elements, this.index, array, index, this.size)
      Array.copy(that.elements, that.index, array, this.size + index, that.size)
      new Spandex.Primary[A](array, index, size)
    case _ => fromIterable(View.Concat(coll, xs))
  }

  override final def map[B](f: A => B): Spandex[B] = {
    if (isEmpty) Spandex.empty
    else {
      val capacity = elements.length
      val array = new Array[Any](capacity)
      var i = 0
      while (i < length) {
        array(shift(i)) = f(fetch(i))
        i += 1
      }
      if (reversed) new Spandex.Primary[B](array, index, length).reverse
      else new Spandex.Primary[B](array, index, length)
    }
  }

  override final def filter(p: A => Boolean): Spandex[A] =
    fromIterable[A](View.Filter[A](coll, p))

  override final def flatMap[B](f: A => IterableOnce[B]): Spandex[B] =
    fromIterable[B](View.FlatMap[A, B](coll, f))

  override final def partition(p: A => Boolean): (Spandex[A], Spandex[A]) = {
    val pn = View.Partition(coll, p)
    (fromIterable(pn.left), fromIterable(pn.right))
  }

  override final def zip[B](xs: IterableOnce[B]): Spandex[(A, B)] = xs match {
    case that: Spandex[B] =>
      Spandex.tabulate(math.min(this.length, that.length)) { i =>
        (this.apply(i), that.apply(i))
      }
    case _ => fromIterable[(A, B)](View.Zip[A, B](coll, xs))
  }

  override final def take(n: Int): Spandex[A] =
    new Spandex.Secondary[A](primary, index, math.min(n, length))

  override final def drop(n: Int): Spandex[A] =
    new Spandex.Secondary[A](primary, shift(math.min(n, length)), math.max(0, length - n))

  override final def iterator(): Iterator[A] = {
    if (reversed) new Iterator[A] {
      private[this] var i = index + Spandex.this.length
      override def hasNext: Boolean = i > index
      override def next(): A = {
        i -= 1
        element(i)
      }
    } else new Iterator[A] {
      private[this] final val n = index + Spandex.this.length
      private[this] var i = index
      override def hasNext: Boolean = i < n
      override def next(): A = {
        val j = i
        i += 1
        element(j)
      }
    }
  }

  override final def foreach[U](f: (A) => U): Unit = {
    if (reversed) {
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
  }

  override final def reverse: Spandex[A] =
    new Spandex.Secondary[A](primary, index, length, !reversed)

  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    /*
    var acc = z
    foreach(x => acc = op(acc, x))
    acc
    */
    if (reversed) {
      var acc = z
      var i = index + length - 1
      val n = index
      while (i >= n) {
        acc = op(acc, element(i))
        i -= 1
      }
      acc
    } else {
      var acc = z
      var i = index
      val n = index + length
      while (i < n) {
        acc = op(acc, element(i))
        i += 1
      }
      acc
    }
  }

  override final def foldRight[B](z: B)(op: (A, B) => B): B = {
    reverse.foldLeft(z) {
      case (acc, x) => op(x, acc)
    }
  }

  override final def indexWhere(p: (A) => Boolean): Int = {
    var i = 0
    while (i < length) {
      if (p(fetch(i))) return i
      i += 1
    }
    -1
  }

  override final def toArray[B >: A: ClassTag]: Array[B] =
    copyToArray(new Array[B](this.length))

  override final def copyToArray[B >: A](xs: Array[B], start: Int): xs.type = {
    if (start < 0 || start > length) throw new ArrayIndexOutOfBoundsException(start)
    else if (reversed) {
      var i = start
      val it = iterator()
      while (it.hasNext) {
        xs(i) = it.next()
        i += 1
      }
      xs
    } else {
      Array.copy(this.elements, this.index + start, xs, 0, this.length - start)
      xs
    }
  }

  final def fromIterable[B](c: collection.Iterable[B]): Spandex[B] =
    Spandex.fromIterable(c)

  override final def className = "Spandex"

  override final protected[this] def newBuilderWithSameElemType: Builder[A, Spandex[A]] = Spandex.newBuilder

  override final def newBuilder[E]: Builder[E, Spandex[E]] = Spandex.newBuilder
}

object Spandex extends IterableFactory[Spandex] {
  private[immutable] class Primary[+A](
      override val elements: scala.Array[Any],
      index: Int,
      length: Int)
    extends Spandex[A](index, length) {

    override protected val primary: Primary[A] = this
    override protected def reversed: Boolean = false
    private[this] final val low: AtomicInteger = new AtomicInteger(index)
    private[this] final val high: AtomicInteger = new AtomicInteger(index + length - 1)
    private[Spandex] final def lower[B >: A](b: B, i: Int, l: Int): Boolean = {
      val lo = low.get
      if (lo > 0 && lo == i && low.compareAndSet(lo, lo - 1)) {
        elements(lo - 1) = b
        true
      } else false
    }
    private[Spandex] final def lower[B >: A](bs: Array[B], bi: Int, bl: Int, i: Int, l: Int): Boolean = {
      val lo = low.get
      if (lo > bl && lo == i && low.compareAndSet(lo, lo - bl)) {
        Array.copy(bs, bi, elements, lo - bl, bl)
        true
      } else false
    }
    private[Spandex] final def raise[B >: A](b: B, i: Int, l: Int): Boolean = {
      val hi = high.get
      if (hi < elements.length - 1 && hi == (i + l - 1) && high.compareAndSet(hi, hi + 1)) {
        elements(hi + 1) = b
        true
      } else false
    }
    private[Spandex] final def raise[B >: A](bs: Array[B], bi: Int, bl: Int, i: Int, l: Int): Boolean = {
      val hi = high.get
      if (hi < elements.length - bl && hi == (i + l - 1) && high.compareAndSet(hi, hi + bl)) {
        Array.copy(bs, bi, elements, hi + 1, bl)
        true
      } else false
    }
  }

  private[immutable] final class Secondary[+A](
      override val primary: Primary[A],
      index: Int,
      length: Int,
      val reversed: Boolean = false)
    extends Spandex[A](index, length)

  private[immutable] final object Empty
    extends Primary[Nothing](Array.empty, 0, 0)

  override def apply[A](xs: A*): Spandex[A] =
    apply(xs.toArray[Any], 0, xs.length)

  def apply[A](it: Iterable[A]): Spandex[A] =
    fromIterable(it)

  private[Spandex] def apply[A](xs: Array[Any], i: Int, n: Int): Spandex[A] = {
    if (n == 0) Spandex.Empty
    else {
      val capacity = math.max(n * 2, 8)
      val array = new Array[Any](capacity)
      val index = (capacity - n) / 2
      Array.copy(xs, i, array, index, n)
      new Primary[A](array, index, n)
    }
  }

  def tabulate[A](n: Int)(f: Int => A): Spandex[A] =
    tabulate(n, 0)(f)

  private[Spandex] def tabulate[A](n: Int, margin: Int)(f: Int => A): Spandex[A] =
    if (n == 0) Spandex.Empty
    else {
      val capacity = math.max(n * 2, 8)
      val array = scala.Array.ofDim[Any](capacity)
      val first = (capacity - n + margin) / 2
      val last = first + n - 1
      var i = first
      while (i <= last) {
        array(i) = f(i - first)
        i += 1
      }
      new Primary[A](array, first, n)
    }

  override def fill[A](n: Int)(elem: => A): Spandex[A] =
    tabulate(n)(_ => elem)

  def fromIterable[A](it: collection.IterableOnce[A]): Spandex[A] = it match {
    case c: Iterable[A] => fromIterable(c)
    case _ =>
      val builder = Spandex.newBuilder[A]
      builder ++= it
      Spandex.fromIterable(builder.result)
  }
  def fromIterable[A](it: collection.Iterable[A]): Spandex[A] = it match {
    case that: Spandex[A] ⇒ that
    case c if c.isEmpty => Spandex.Empty
    case c if c.knownSize >= 0 =>
      val length = c.knownSize
      val capacity = math.max(length * 2, 8)
      val array = new Array[Any](capacity)
      val index = (capacity - length) / 2
      var i = 0
      val it = c.iterator()
      while (i < length) {
        array(index + i) = it.next()
        i += 1
      }
      new Spandex.Primary[A](array, index, length)
    case _ ⇒
      val array = ArrayBuffer.fromIterable(it).asInstanceOf[ArrayBuffer[Any]].toArray
      apply(array, 0, array.length)
  }

  override def newBuilder[A]: Builder[A, Spandex[A]] =
    new ArrayBuffer[A].mapResult(b => b.to(Spandex))
}

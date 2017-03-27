package strawman.collection.immutable

import scala.{ volatile, Any, AnyRef, Array, Boolean, Float, Int, None, Nothing, Option, Some, StringContext, ArrayIndexOutOfBoundsException }
import scala.Predef.{???, println, String, identity}
import scala.math
import strawman.collection
import strawman.collection.{ immutable, IterableOnce, IterableFactory, MonoBuildable, PolyBuildable, Iterator, LinearSeq, SeqLike, View }
import strawman.collection.mutable.{ArrayBuffer, Builder}

/**
  * A <i>spandex</i> is an (ostensible) immutable array-like collection designed to support the following performance characteristics:
  * <ul>
  * <li>constant time <code>head</code>, <code>last</code>, <code>tail</code>, <code>init</code>, <code>take</code>, <code>drop</code> and <code>reverse</code> (<code>last</code> and <code>init</code> are not included in this implementation however)</li>
  * <li>amortised constant time <code>prepend</code> and <code>append</code> (depending on the complexity of <code>scala.Array.copy</code>)</li>
  * <li>efficient indexed access</li>
  * <li>linear time iteration (i.e. <code>filter</code>, <code>map</code>, <code>flatMap</code>, <code>foreach</code> etc) with low constant factors
  * <li>reasonable memory usage (at most double (plus constants) that of an array with the same number of elements)
  * </ul>
  * <br/>
  * The underlying array is only mutated on <code>prepend</code> and <code>append</code> but never more than once for any given position (any later modification attempts to an already modified position results in a copy being made of the underlying array). To guarantee that only a single thread can write to an array slot synchronization is used during slot assignment in <code>prepend</code> and <code>append</code>. All other access is non synchronised.
  * <br/>
  * <br/>
  * Expansion occurs when the underlying array is full on the effected side; the new array will be populated to have its center position adjusted by the unused capacity (margin) on the non effected side according to the following formula: <code>starting position = (length + margin) / 2</code> (i.e. for a six element spandex with two slots unused at the end <code>[a, b, c, d, e, f, , ]</code>, a prepend operation would make the expanded array have a length of 12 and a starting position of <code>(6 + 2) / 2 = 4</code>, <code>[ , , , , a, b, c, d, e, f, , ]</code>). This expansion scheme leads to more free slots being allocated on the side mostly expanded (a margin of zero will allocate an equal amount of free slots on both sides).
  */
sealed abstract class Spandex[+A]
  extends Seq[A]
    with SeqLike[A, Spandex]
    with LinearSeq[A]
    with MonoBuildable[A, Spandex[A]]
    with PolyBuildable[A, Spandex] {
  protected def index: Int
  protected def array: scala.Array[Any]
  protected def primary: Spandex.Primary[A]

  override def size: Int = length
  override def knownSize: Int = length

  protected def element(i: Int): A = scala.runtime.ScalaRunTime.array_apply(array, i).asInstanceOf[A]
  protected def shift(i: Int): Int = index + i
  protected def fetch(i: Int): A = element(shift(i))
  protected def lowerMargin: Int = index
  protected def upperMargin: Int = array.length - length - index

  override def apply(i: Int): A =
    if (i < 0 || i >= length) throw new ArrayIndexOutOfBoundsException(i)
    else fetch(i)

  def +:[B >: A](b: B): Spandex[B]

  def :+[B >: A](b: B): Spandex[B]

  override def ++[B >: A](xs: IterableOnce[B]): Spandex[B] = xs match {
    case bs: Spandex[B] =>
      Spandex.tabulate(length + bs.length) {
        case i if i < length => apply(i)
        case i => bs(i - length)
      }
    case _ => fromIterable(View.Concat(coll, xs))
  }

  override def zip[B](xs: IterableOnce[B]): Spandex[(A, B)] = xs match {
    case bs: Spandex[B] =>
      Spandex.tabulate(math.min(length, bs.length)) { i =>
        (apply(i), bs(i))
      }
    case _ => fromIterable[(A, B)](View.Zip[A, B](coll, xs))
  }

  override def filter(p: A => Boolean): Spandex[A] =
    fromIterable[A](View.Filter[A](coll, p))

  override def map[B](f: A => B): Spandex[B] =
    Spandex.tabulate(length)(i => f(fetch(i)))

  override def flatMap[B](f: A => IterableOnce[B]): Spandex[B] =
    fromIterable[B](View.FlatMap[A, B](coll, f))

  override def partition(p: A => Boolean): (Spandex[A], Spandex[A]) = {
    val pn = View.Partition(coll, p)
    (fromIterable(pn.left), fromIterable(pn.right))
  }

  override def take(n: Int): Spandex[A] =
    new Spandex.Secondary[A](primary, index, math.min(n, length))

  override def drop(n: Int): Spandex[A] =
    new Spandex.Secondary[A](primary, shift(math.min(n, length)), math.max(0, length - n))

  override def tail: Spandex[A] =
    if (length > 0) new Spandex.Secondary[A](primary, shift(1), length - 1)
    else ???

  override def reverse: Spandex[A] =
    new Spandex.Secondary[A](primary, index, length, true)

  override def iterator(): Iterator[A] = new Iterator[A] {
    private[this] var i = 0
    override def hasNext: Boolean = i < Spandex.this.length
    override def next(): A = {
      i += 1
      fetch(i - 1)
    }
  }

  def fromIterable[B](c: collection.Iterable[B]): Spandex[B] =
    Spandex.fromIterable(c)

  override def className = "Spandex"

  override protected[this] def newBuilderWithSameElemType: Builder[A, Spandex[A]] = Spandex.newBuilder

  override def newBuilder[E]: Builder[E, Spandex[E]] = Spandex.newBuilder
}

object Spandex extends IterableFactory[Spandex] {
  private[immutable] class Primary[+A](
      override val index: Int,
      override val length: Int,
      val array: scala.Array[Any])
      extends Spandex[A] {
    @volatile var low: Int = index
    @volatile var high: Int = index + length - 1
    override protected val primary: Primary[A] = this

    override def +:[B >: A](b: B): Spandex[B] = {
      if (lower(b, index, length)) new Secondary[B](this, index - 1, length + 1)
      else b +: Spandex[A](index, length, array)
    }
    override def :+[B >: A](b: B): Spandex[B] = {
      if (raise(b, index, length)) new Secondary[B](this, index, length + 1)
      else Spandex[A](index, length, array) :+ b
    }
    private[Spandex] def lower[B >: A](b: B, i: Int, l: Int): Boolean = {
      this.synchronized {
        if (low > 0 && low == i) {
          low -= 1
          array(low) = b
          true
        } else false
      }
    }
    private[Spandex] def raise[B >: A](b: B, i: Int, l: Int): Boolean = {
      this.synchronized {
        if (high < array.length - 1 && high == (i + l - 1)) {
          high += 1
          array(high) = b
          true
        } else false
      }
    }
  }

  private[immutable] final object Empty extends Primary[Nothing](0, 0, Array.empty) {
    override def +:[B >: Nothing](b: B): Spandex[B] = Spandex(b)
    override def :+[B >: Nothing](b: B): Spandex[B] = Spandex(b)
    override def ++[B >: Nothing](xs: IterableOnce[B]): Spandex[B] = {
      val builder = Spandex.newBuilder[B]
      builder ++= xs
      Spandex.fromIterable(builder.result)
    }
  }

  private[immutable] final class Secondary[+A](
      override val primary: Primary[A],
      override val index: Int,
      override val length: Int,
      val reversed: Boolean = false)
      extends Spandex[A] {
    override protected def array: Array[Any] = primary.array

    override protected def shift(i: Int): Int =
      if (reversed) index + length - 1 - i
      else super.shift(i)

    override def reverse =
      new Secondary[A](primary, index, length, !reversed)

    override def +:[B >: A](b: B): Spandex[B] =
      if (reversed && primary.raise(b, index, length)
          || !reversed && primary.lower(b, index, length))
        new Secondary[B](primary, index - 1, length + 1, reversed)
      else if (!reversed) b +: Spandex[A](index, length, array)
      else tabulate(length + 1, upperMargin) {
        case k if k == 0 ⇒ b
        case k ⇒ fetch(k - 1)
      }

    override def :+[B >: A](b: B): Spandex[B] =
      if (reversed && primary.lower(b, index, length)
          || !reversed && primary.raise(b, index, length))
        new Secondary[B](primary, index, length + 1, reversed)
      else if (!reversed) Spandex[A](index, length, array) :+ b
      else tabulate(length + 1, -lowerMargin) {
        case k if k == length ⇒ b
        case k ⇒ fetch(k)
      }
  }

  override def apply[A](xs: A*): Spandex[A] =
    apply(0, xs.length, xs.toArray[Any])

  def apply[A](it: Iterable[A]): Spandex[A] =
    fromIterable(it)

  private[Spandex] def apply[A](index: Int, n: Int, xs: Array[Any]): Spandex[A] = {
    if (n == 0) Spandex.Empty
    else {
      val capacity = math.max(n * 2, 8)
      val array = new Array[Any](capacity)
      val first = (capacity - n) / 2
      Array.copy(xs, index, array, first, n)
      new Primary[A](first, n, array)
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
      new Primary[A](first, n, array)
    }

  override def fill[A](n: Int)(elem: => A): Spandex[A] =
    tabulate(n)(_ => elem)

  def fromIterable[A](it: collection.Iterable[A]): Spandex[A] = it match {
    case b: Spandex[A] ⇒ b
    case c if c.isEmpty => Spandex.Empty
    case _ ⇒
      val array = ArrayBuffer.fromIterable(it).asInstanceOf[ArrayBuffer[Any]].toArray
      apply(0, array.length, array)
  }

  override def newBuilder[A]: Builder[A, Spandex[A]] =
    new ArrayBuffer[A].mapResult(b => b.to(Spandex))
}

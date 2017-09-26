package strawman.collection

import strawman.collection.mutable.{ArrayBuffer, Builder}

import scala.{Any, Boolean, Equals, IllegalArgumentException, Int, NoSuchElementException, Nothing, annotation, IndexOutOfBoundsException, throws}
import scala.Predef.{<:<, intWrapper}

/** Concrete collection type: View */
trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] {
  override def view = this

  def iterableFactory: IterableFactoryLike[View] = View

  protected[this] def fromSpecificIterable(coll: Iterable[A]): View[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, View[A]] =
    IndexedSeq.newBuilder().mapResult(_.view)

  final protected[this] def coll: this.type = this

  override def toString = "View(?)"

  override def className = "View"
}

/** This object reifies operations on views as case classes
  *
  * @define $Coll View
  * @define $coll view
  */
object View extends IterableFactoryLike[View] {

  // Views are just forwarders to a source collection’s iterator. Consequently, they have to be
  // build from an `Iterable` source in order to be themselves `Iterable`
  type Source[A] = Iterable[A]

  /**
    * @return A `View[A]` whose underlying iterator is provided by the `it` parameter-less function.
    *
    * @param it Function creating the iterator to be used by the view. This function must always return
    *           a fresh `Iterator`, otherwise the resulting view will be effectively iterable only once.
    *
    * @tparam A View element type
    */
  def fromIteratorProvider[A](it: () => Iterator[A]): View[A] = new View[A] {
    def iterator() = it()
  }

  /**
    * @return A view iterating over the given `Iterable`
    *
    * @param it The `Iterable` to view. It must be an `Iterable` (and not just an `IterableOnce`),
    *           otherwise an `IllegalArgumentException` is thrown.
    *
    * @tparam E View element type
    */
  def from[E](it: Iterable[E]): View[E] = it match {
    case it: View[E]     => it
    case _               => View.fromIteratorProvider(() => it.iterator())
  }

  def empty[A]: View[A] = Empty

  def newBuilder[A](): Builder[A, View[A]] = ArrayBuffer.newBuilder[A]().mapResult(from)

  override def apply[A](xs: A*): View[A] = Elems(xs: _*)

  /** The empty view */
  case object Empty extends View[Nothing] {
    def iterator() = Iterator.empty
    override def knownSize = 0
  }

  /** A view with exactly one element */
  case class Single[A](a: A) extends View[A] {
    def iterator(): Iterator[A] =
      new Iterator[A] {
        private var notConsumed: Boolean = true
        def next(): A =
          if (notConsumed) {
            notConsumed = false
            a
          } else Iterator.empty.next()
        def hasNext: Boolean = notConsumed
      }
    override def knownSize: Int = 1
  }

  /** A view with given elements */
  case class Elems[A](xs: A*) extends View[A] {
    def iterator() = Iterator(xs: _*)
    override def knownSize = xs.length // should be: xs.knownSize, but A*'s are not sequences in this strawman.
  }

  /** A view containing the results of some element computation a number of times. */
  case class Fill[A](n: Int)(elem: => A) extends View[A] {
    def iterator() = Iterator.fill(n)(elem)
    override def knownSize: Int = 0 max n
  }

  /** A view containing values of a given function over a range of integer values starting from 0. */
  case class Tabulate[A](n: Int)(f: Int => A) extends View[A] {
    def iterator(): Iterator[A] = Iterator.tabulate(n)(f)
    override def knownSize: Int = 0 max n
  }

  /** A view containing repeated applications of a function to a start value */
  case class Iterate[A](start: A, len: Int)(f: A => A) extends View[A] {
    def iterator(): Iterator[A] = Iterator.iterate(start)(f).take(len)
    override def knownSize: Int = 0 max len
  }

  /** A view that filters an underlying collection. */
  class Filter[A](val underlying: Iterable[A], val p: A => Boolean, val isFlipped: Boolean) extends View[A] {
    def iterator() = underlying.iterator().filterImpl(p, isFlipped)
  }

  object Filter {
    def apply[A](underlying: Iterable[A], p: A => Boolean, isFlipped: Boolean): Filter[A] =
      underlying match {
        case filter: Filter[A] if filter.isFlipped == isFlipped => new Filter(filter.underlying, a => filter.p(a) && p(a), isFlipped)
        case _ => new Filter(underlying, p, isFlipped)
      }
  }

  case class FilterKeys[K, V](underlying: Iterable[(K, V)], p: K => Boolean) extends View[(K, V)] {
    def iterator(): Iterator[(K, V)] = underlying.iterator().filter(kv => p(kv._1))
  }

  /** A view that removes the duplicated elements as determined by the transformation function `f` */
  case class DistinctBy[A, B](underlying: Iterable[A], f: A => B) extends View[A] {
    def iterator(): Iterator[A] = underlying.iterator().distinctBy(f)
  }

  /** A view that partitions an underlying collection into two views */
  case class Partition[A](underlying: Iterable[A], p: A => Boolean) {

    /** The view consisting of all elements of the underlying collection
     *  that satisfy `p`.
     */
    val first = Partitioned(this, cond = true)

    /** The view consisting of all elements of the underlying collection
     *  that do not satisfy `p`.
     */
    val second = Partitioned(this, cond = false)
  }

  /** A view representing one half of a partition. */
  case class Partitioned[A](partition: Partition[A], cond: Boolean) extends View[A] {
    def iterator() = partition.underlying.iterator().filter(x => partition.p(x) == cond)
  }

  /** A view that drops leading elements of the underlying collection. */
  case class Drop[A](underlying: Iterable[A], n: Int) extends View[A] {
    def iterator() = underlying.iterator().drop(n)
    protected val normN = n max 0
    override def knownSize =
      if (underlying.knownSize >= 0) (underlying.knownSize - normN) max 0 else -1
  }

  case class DropWhile[A](underlying: Iterable[A], p: A => Boolean) extends View[A] {
    def iterator() = underlying.iterator().dropWhile(p)
  }

  /** A view that takes leading elements of the underlying collection. */
  case class Take[A](underlying: Iterable[A], n: Int) extends View[A] {
    def iterator() = underlying.iterator().take(n)
    protected val normN = n max 0
    override def knownSize =
      if (underlying.knownSize >= 0) underlying.knownSize min normN else -1
  }

  case class TakeWhile[A](underlying: Iterable[A], p: A => Boolean) extends View[A] {
    def iterator(): Iterator[A] = underlying.iterator().takeWhile(p)
  }

  case class ScanLeft[A, B](underlying: Iterable[A], z: B, op: (B, A) => B) extends View[B] {
    def iterator(): Iterator[B] = underlying.iterator().scanLeft(z)(op)
    override def knownSize: Int =
      if (underlying.knownSize >= 0) underlying.knownSize + 1 else -1
  }

  /** A view that maps elements of the underlying collection. */
  case class Map[A, B](underlying: Iterable[A], f: A => B) extends View[B] {
    def iterator() = underlying.iterator().map(f)
    override def knownSize = underlying.knownSize
  }

  case class MapValues[K, V, W](underlying: Iterable[(K, V)], f: V => W) extends View[(K, W)] {
    def iterator(): Iterator[(K, W)] = underlying.iterator().map(kv => (kv._1, f(kv._2)))
    override def knownSize: Int = underlying.knownSize
  }

  /** A view that flatmaps elements of the underlying collection. */
  case class FlatMap[A, B](underlying: Iterable[A], f: A => IterableOnce[B]) extends View[B] {
    def iterator() = underlying.iterator().flatMap(f)
  }

  /** A view that concatenates elements of the prefix collection or iterator with the elements
   *  of the suffix collection or iterator.
   */
  case class Concat[A](prefix: Iterable[A], suffix: Iterable[A]) extends View[A] {
    def iterator() = prefix.iterator() ++ suffix.iterator()
    override def knownSize =
      if (prefix.knownSize >= 0 && suffix.knownSize >= 0) prefix.knownSize + prefix.knownSize
      else -1
  }

  /** A view that Computes the union between a set and another set.
   */
  case class Union[A](prefix: Iterable[A], suffix: Iterable[A]) extends View[A] {
    def iterator() = prefix.iterator() ++ suffix.iterator()
  }

  /** A view that zips elements of the underlying collection with the elements
   *  of another collection or iterator.
   */
  case class Zip[A, B](underlying: Iterable[A], other: Iterable[B]) extends View[(A, B)] {
    def iterator() = underlying.iterator().zip(other)
    override def knownSize = underlying.knownSize min other.knownSize
  }

  case class Updated[A](underlying: Iterable[A], index: Int, elem: A) extends View[A] {
    def iterator(): Iterator[A] = new Iterator[A] {
      private val it = underlying.iterator()
      private var i = 0
      def next(): A = {
        val value = if (i == index) { it.next(); elem } else it.next()
        i += 1
        value
      }
      def hasNext: Boolean = it.hasNext
    }
    override def knownSize: Int = underlying.knownSize
  }

  case class Patched[A] private[collection] (underlying: Iterable[A], from: Int, other: IterableOnce[A], replaced: Int) extends View[A] {
    if (from < 0 || (knownSize > -1 && from > knownSize)) throw new IndexOutOfBoundsException(from.toString)
    def iterator(): Iterator[A] = underlying.iterator().patch(from, other.iterator(), replaced)
  }
  object Patched {
    def apply[A](underlying: Iterable[A], from: Int, other: Iterable[A], replaced: Int) = new Patched[A](underlying, from, other, replaced)
  }

  case class ZipWithIndex[A](underlying: Iterable[A]) extends View[(A, Int)] {
    def iterator(): Iterator[(A, Int)] = underlying.iterator().zipWithIndex
    override def knownSize: Int = underlying.knownSize
  }

  case class Unzip[A, A1, A2](underlying: Iterable[A])(implicit asPair: A <:< (A1, A2)) {
    val first: View[A1] =
      new View[A1] {
        def iterator(): Iterator[A1] = underlying.iterator().map(_._1)
        override def knownSize: Int = underlying.knownSize
      }
    val second: View[A2] =
      new View[A2] {
        def iterator(): Iterator[A2] = underlying.iterator().map(_._2)
        override def knownSize: Int = underlying.knownSize
      }
  }

  case class PadTo[A](underlying: Iterable[A], len: Int, elem: A) extends View[A] {
    def iterator(): Iterator[A] = new Iterator[A] {
      private var i = 0
      private val it = underlying.iterator()
      def next(): A = {
        val a =
          if (it.hasNext) it.next()
          else if (i < len) elem
          else Iterator.empty.next()
        i += 1
        a
      }
      def hasNext: Boolean = it.hasNext || i < len
    }
    override def knownSize: Int = if (underlying.knownSize >= 0) underlying.knownSize max len else -1
  }

}

/** A trait representing indexable collections with finite length */
trait ArrayLike[+A] extends Any {
  def length: Int
  @throws[IndexOutOfBoundsException]
  def apply(i: Int): A
}

trait SeqView[+A] extends View[A] with SeqOps[A, SeqView, SeqView[A]] { self =>
  override def view: SeqView[A] = this

  override def toSeq = immutable.IndexedSeq.from(this)

  override def iterableFactory: IterableFactoryLike[SeqView] = SeqView

  override protected[this] def fromSpecificIterable(coll: Iterable[A]): SeqView[A] = fromIterable(coll)

  override protected[this] def newSpecificBuilder(): Builder[A, SeqView[A]] =
    IndexedSeq.newBuilder().mapResult(_.view)

  override def toString = "SeqView(?)"
  override def className = "SeqView"

  override def apply(i: Int) = iterator().drop(i - 1).next()
  override def prepended[B >: A](elem: B): SeqView[B] = SeqView.Prepend(elem, this)
  override def appended[B >: A](elem: B): SeqView[B] = SeqView.Append(this, elem)
  override def prependedAll[B >: A](prefix: Iterable[B]): SeqView[B] = SeqView.PrependAll(prefix, this)
  override def appendedAll[B >: A](suffix: Iterable[B]): SeqView[B] = SeqView.AppendAll(this, suffix)
}

object SeqView extends SeqFactory[SeqView] {
  /**
    * @return A `SeqView[A]` whose underlying iterator is provided by the `it` parameter-less function.
    *
    * @param it Function creating the iterator to be used by the view. This function must always return
    *           a fresh `Iterator`, otherwise the resulting view will be effectively iterable only once.
    *
    * @tparam A View element type
    */
  def fromIteratorProvider[A](it: () => Iterator[A]): SeqView[A] = new SeqView[A] {
    override def length = it().size
    def iterator() = it()
  }

  /**
    * @return A seq view iterating over the given `Iterable`
    *
    * @param it The `Iterable` to view. It must be an `Iterable` (and not just an `IterableOnce`),
    *           otherwise an `IllegalArgumentException` is thrown.
    *
    * @tparam E View element type
    */
  def from[E](it: Source[E]): SeqView[E] = it match {
    case it: SeqView[E] => it
    case _ => SeqView.fromIteratorProvider(() => it.iterator())
  }

  def empty[A]: SeqView[A] = Empty

  /** The empty `SeqView` */
  case object Empty extends SeqView[Nothing] {
    override def length = 0
    def iterator() = Iterator.empty
    override def knownSize = 0
  }

  def newBuilder[A](): Builder[A, SeqView[A]] = ArrayBuffer.newBuilder[A]().mapResult(from)

  /** A view that appends an element to its elements */
  case class Append[A](underlying: Iterable[A] with ArrayLike[A], elem: A) extends SeqView[A] {
    override def length: Int = underlying.length + 1
    override def knownSize: Int = length
    def iterator(): Iterator[A] = View.Concat(underlying, View.Single(elem)).iterator()
  }

  /** A view that prepends an element to its elements */
  case class Prepend[A](elem: A, underlying: Iterable[A] with ArrayLike[A]) extends SeqView[A] {
    override def length: Int = underlying.length + 1
    override def knownSize: Int = length
    def iterator(): Iterator[A] = View.Concat(View.Single(elem), underlying).iterator()
  }

  /** A view that appends a collection of elements to its elements */
  case class AppendAll[A](underlying: Iterable[A] with ArrayLike[A], suffix: Iterable[A]) extends SeqView[A] {
    override val length = underlying.length + suffix.size
    def iterator(): Iterator[A] = underlying.iterator() ++ suffix.iterator()
    override def knownSize: Int = length
  }

  /** A view that prepends an element to its elements */
  case class PrependAll[A](prefix: Iterable[A], underlying: Iterable[A] with ArrayLike[A]) extends SeqView[A] {
    override val length = prefix.size + underlying.length
    def iterator(): Iterator[A] = prefix.iterator() ++ underlying.iterator()
    override def knownSize: Int = length
  }
}

/** View defined in terms of indexing a range */
trait IndexedSeqView[+A] extends SeqView[A] with IndexedSeqOps[A, IndexedSeqView, IndexedSeqView[A]] { self =>
  override def view: IndexedSeqView[A] = this

  override def iterableFactory: SeqFactory[IndexedSeqView] = IndexedSeqView

  override protected[this] def fromSpecificIterable(coll: Iterable[A]): IndexedSeqView[A] = fromIterable(coll)

  override protected[this] def newSpecificBuilder(): Builder[A, IndexedSeqView[A]] =
    IndexedSeq.newBuilder[A]().mapResult(_.view)

  override def iterator(): Iterator[A] = new Iterator[A] {
    private var current = 0
    def hasNext = current < self.length
    def next(): A = {
      val r = apply(current)
      current += 1
      r
    }
  }

  override def toString = "IndexedSeqView(?)"
  override def className = "IndexedSeqView"

  override def knownSize: Int = length

  override def apply(i: Int): A // make abstract to force override in implementors

  override def take(n: Int): IndexedSeqView[A] = IndexedSeqView.Take(this, n)
  override def takeRight(n: Int): IndexedSeqView[A] = IndexedSeqView.TakeRight(this, n)
  override def drop(n: Int): IndexedSeqView[A] = IndexedSeqView.Drop(this, n)
  override def dropRight(n: Int): IndexedSeqView[A] = IndexedSeqView.DropRight(this, n)
  override def map[B](f: A => B): IndexedSeqView[B] = IndexedSeqView.Map(this, f)
  override def reverse: IndexedSeqView[A] = IndexedSeqView.Reverse(this)
}

object IndexedSeqView extends SeqFactory[IndexedSeqView] {

  def fromIterator[A](it: => Iterator[A]): IndexedSeqView[A] = new IndexedSeqView[A] {
    override def length = it.length
    override def apply(i: Int) = it.drop(i - 1).next()
  }

  /** Avoid copying if source collection is already a view. */
  def from[E](it: Source[E]): IndexedSeqView[E] = it match {
    case it: IndexedSeqView[E] => it
    case it: IndexedSeq[E] => it.view
    case _ => IndexedSeqView.fromIterator(it.iterator())
  }

  def empty[A]: IndexedSeqView[A] = Empty

  /** The empty `IndexedSeqView` */
  case object Empty extends IndexedSeqView[Nothing] {
    override def length = 0
    override def apply(i: Int) = throw new NoSuchElementException("apply of empty indexed seq")
  }

  def newBuilder[A](): Builder[A, IndexedSeqView[A]] = ArrayBuffer.newBuilder[A]().mapResult(from)

  case class Take[A](underlying: IndexedSeqView[A], n: Int) extends IndexedSeqView[A] {
    private[this] val normN = n max 0
    def length = underlying.length min normN
    @throws[IndexOutOfBoundsException]
    override def apply(i: Int) = underlying.apply(i)
  }

  case class TakeRight[A](underlying: IndexedSeqView[A], n: Int) extends IndexedSeqView[A] {
    private[this] val delta = (underlying.length - (n max 0)) max 0
    def length = underlying.length - delta
    @throws[IndexOutOfBoundsException]
    override def apply(i: Int) = underlying.apply(i + delta)
  }

  case class Drop[A](underlying: IndexedSeqView[A], n: Int) extends IndexedSeqView[A] {
    protected val normN = n max 0
    def length = (underlying.length - normN) max 0
    @throws[IndexOutOfBoundsException]
    override def apply(i: Int) = underlying.apply(i + normN)
  }

  case class DropRight[A](underlying: IndexedSeqView[A], n: Int) extends IndexedSeqView[A] {
    private[this] val len = (underlying.length - (n max 0)) max 0
    def length = len
    @throws[IndexOutOfBoundsException]
    override def apply(i: Int) = underlying.apply(i)
  }

  case class Map[A, B](underlying: IndexedSeqView[A], f: A => B) extends IndexedSeqView[B] {
    def length = underlying.length
    @throws[IndexOutOfBoundsException]
    override def apply(n: Int) = f(underlying.apply(n))
  }

  case class Reverse[A](underlying: IndexedSeqView[A]) extends IndexedSeqView[A] {
    def length = underlying.length
    @throws[IndexOutOfBoundsException]
    override def apply(i: Int) = underlying.apply(length - 1 - i)
  }
}

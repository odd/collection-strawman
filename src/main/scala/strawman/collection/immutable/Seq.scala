package strawman
package collection
package immutable

import scala.{Any, Int, `inline`}

trait Seq[+A] extends Iterable[A]
                 with collection.Seq[A]
                 with SeqOps[A, Seq, Seq[A]]

trait SeqOps[+A, +CC[_], +C] extends collection.SeqOps[A, CC, C] {

  /** A copy of the $coll with an element prepended.
   *
   *  @param  elem   the prepended element
   *  @tparam B      the element type of the returned $coll.
   *  @return a new collection of type `Vector[B]` consisting of `value` followed
   *          by all elements of this $coll.
   *
   *    @inheritdoc
   *
   *    Also, the original $coll is not modified, so you will want to capture the result.
   *
   *    Example:
   *    {{{
   *      scala> val x = List(1)
   *      x: List[Int] = List(1)
   *
   *      scala> val y = 2 +: x
   *      y: List[Int] = List(2, 1)
   *
   *      scala> println(x)
   *      List(1)
   *    }}}
   *
   *    @return a new $coll consisting of `value` followed
   *            by all elements of this $coll.
   */
  def prepend[B >: A](elem: B): CC[B] = fromIterable(View.Prepend(elem, toIterable))

  /** Alias for `prepend`.
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    */
  @`inline` final def +: [B >: A](elem: B): CC[B] = prepend(elem)

  /** A copy of this $coll with an element appended.
   *
   *  @param  elem   the appended element
   *  @tparam B      the element type of the returned $coll.
   *  @return a new collection of type `CC[B]` consisting of
   *          all elements of this $coll followed by `value`.
   *
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   *
   *    Example:
   *    {{{
   *       scala> val a = List(1)
   *       a: List[Int] = List(1)
   *
   *       scala> val b = a :+ 2
   *       b: List[Int] = List(1, 2)
   *
   *       scala> println(a)
   *       List(1)
   *    }}}
   *
   *    @return a new $coll consisting of
   *            all elements of this $coll followed by `value`.
   */
  def append[B >: A](elem: B): CC[B] = fromIterable(View.Append(toIterable, elem))

  /** Alias for `appendAll` from here on down */
  @`inline` final override def concat[B >: A](suffix: collection.Iterable[B]) = appendAll(suffix)

  /** Alias for `append`
    *
    * Note that :-ending operators are right associative (see example).
    * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
    */
  @`inline` final def :+ [B >: A](elem: B): CC[B] = append(elem)

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix the traversable to append.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection of type `CC[B]` which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def appendAll[B >: A](suffix: collection.Iterable[B]): CC[B] = fromIterable(View.Concat(toIterable, suffix))

  /** Alias for `appendAll` */
  @`inline` final def :++ [B >: A](suffix: collection.Iterable[B]): CC[B] = appendAll(suffix)

  /** As with `:++`, returns a new collection containing the elements from the left operand followed by the
    *  elements from the right operand.
    *
    *  It differs from `:++` in that the right operand determines the type of
    *  the resulting collection rather than the left one.
    *  Mnemonic: the COLon is on the side of the new COLlection type.
    *
    *  @param prefix   the traversable to prepend.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new collection which contains all elements
    *                of `prefix` followed by all the elements of this $coll.
    *
    *  @usecase def prependAll[B](that: Iterable[B]): $Coll[B]
    *    @inheritdoc
    *
    *    Example:
    *    {{{
    *      scala> val x = List(1)
    *      x: List[Int] = List(1)
    *
    *      scala> val y = Vector(2)
    *      y: scala.collection.immutable.Vector[Int] = Vector(2)
    *
    *      scala> val z = x ++: y
    *      z: scala.collection.immutable.Vector[Int] = Vector(1, 2)
    *    }}}
    *
    *    @return       a new $coll which contains all elements of `prefix` followed
    *                  by all the elements of this $coll.
    */
  def prependAll[B >: A](prefix: collection.Iterable[B]): CC[B] = fromIterable(View.Concat(prefix, toIterable))

  /** Alias for `prependAll` */
  @`inline` final def ++: [B >: A](prefix: collection.Iterable[B]): CC[B] = prependAll(prefix)

  /** A copy of this $coll with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @tparam B        the element type of the returned $coll.
    *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    *
    *    @inheritdoc
    *
    *    @return a copy of this $coll with the element at position `index` replaced by `elem`.
    */
  def updated[B >: A](index: Int, elem: B): CC[B] = fromIterable(View.Updated(toIterable, index, elem))

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original $coll appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param  from     the index of the first replaced element
    *  @param  other    the replacement sequence
    *  @param  replaced the number of elements to drop in the original $coll
    *  @tparam B        the element type of the returned $coll.
    *  @return          a new $coll consisting of all elements of this $coll
    *                   except that `replaced` elements starting from `from` are replaced
    *                   by all the elements of `other`.
    */
  def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): CC[B] =
    fromIterable(new View.Patched(toIterable, from, other, replaced))

}

object Seq extends SeqFactory.Delegate[Seq](List)

/** Base trait for immutable indexed sequences that have efficient `apply` and `length` */
trait IndexedSeq[+A] extends Seq[A]
                        with collection.IndexedSeq[A]
                        with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](Vector)

/** Base trait for immutable indexed Seq operations */
trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends SeqOps[A, CC, C] with collection.IndexedSeqOps[A, CC, C]

/** Base trait for immutable linear sequences that have efficient `head` and `tail` */
trait LinearSeq[+A]
  extends Seq[A]
    with collection.LinearSeq[A]
    with LinearSeqOps[A, LinearSeq, LinearSeq[A]]

trait LinearSeqOps[+A, +CC[X] <: LinearSeq[X], +C <: LinearSeq[A]]
  extends SeqOps[A, CC, C]
    with collection.LinearSeqOps[A, CC, C]
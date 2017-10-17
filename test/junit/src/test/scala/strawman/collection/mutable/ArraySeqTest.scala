package strawman.collection.mutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.{List, ArraySeq}

@RunWith(classOf[JUnit4])
/* Test for scala/bug#8014 and ++ in general  */
class ArraySeqTest {
  val noArraySeq = ArraySeq.empty[Int]
  val xArraySeq = ArraySeq(1, 2, 3)
  val smallArraySeq = ArraySeq.range(0,3)
  val bigArraySeq = ArraySeq.range(0,64)
  val smsm = ArraySeq.tabulate(2 * smallArraySeq.length)(i => (i % smallArraySeq.length))
  val smbig = ArraySeq.tabulate(smallArraySeq.length + bigArraySeq.length)(i =>
    if (i < smallArraySeq.length) i else i - smallArraySeq.length
  )
  val bigsm = ArraySeq.tabulate(smallArraySeq.length + bigArraySeq.length)(i =>
    if (i < bigArraySeq.length) i else i - bigArraySeq.length
  )
  val bigbig = ArraySeq.tabulate(2 * bigArraySeq.length)(i => (i % bigArraySeq.length))


  val arraySeqs = List(noArraySeq, smallArraySeq, bigArraySeq)
  val ans = List(
    arraySeqs,
    List(smallArraySeq, smsm, smbig),
    List(bigArraySeq, bigsm, bigbig)
  )

  @Test
  def ArraySeqCat(): Unit = {
    val cats = arraySeqs.map(a => arraySeqs.map(a ++ _))
    assert( cats == ans )
  }

  @Test
  def arrayCat(): Unit = {
    val ars = arraySeqs.map(_.toArray)
    val cats = arraySeqs.map(a => ars.map(a ++ _))
    assert( cats == ans )
  }
}

package strawman.collection.mutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.{List, Spandex}

@RunWith(classOf[JUnit4])
/* Test for scala/bug#8014 and ++ in general  */
class SpandexTest {
  val noSpandex = Spandex.empty[Int]
  val xSpandex = Spandex(1, 2, 3)
  val smallSpandex = Spandex.range(0,3)
  val bigSpandex = Spandex.range(0,64)
  val smsm = Spandex.tabulate(2 * smallSpandex.length)(i => (i % smallSpandex.length))
  val smbig = Spandex.tabulate(smallSpandex.length + bigSpandex.length)(i =>
    if (i < smallSpandex.length) i else i - smallSpandex.length
  )
  val bigsm = Spandex.tabulate(smallSpandex.length + bigSpandex.length)(i =>
    if (i < bigSpandex.length) i else i - bigSpandex.length
  )
  val bigbig = Spandex.tabulate(2 * bigSpandex.length)(i => (i % bigSpandex.length))


  val spandexes = List(noSpandex, smallSpandex, bigSpandex)
  val ans = List(
    spandexes,
    List(smallSpandex, smsm, smbig),
    List(bigSpandex, bigsm, bigbig)
  )

  @Test
  def SpandexCat(): Unit = {
    val cats = spandexes.map(a => spandexes.map(a ++ _))
    assert( cats == ans )
  }

  @Test
  def arrayCat(): Unit = {
    val ars = spandexes.map(_.toArray)
    val cats = spandexes.map(a => ars.map(a ++ _))
    assert( cats == ans )
  }
}

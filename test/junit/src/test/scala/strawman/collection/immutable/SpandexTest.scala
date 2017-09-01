package strawman.collection.immutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class SpandexTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val v = Spandex(0) ++ Spandex(1 to 64: _*)

    assertEquals(Spandex(0, 1), v take 2)
    assertEquals(Spandex(63, 64), v takeRight 2)
    assertEquals(Spandex(2 to 64: _*), v drop 2)
    assertEquals(Spandex(0 to 62: _*), v dropRight 2)

    assertEquals(v, v take Int.MaxValue)
    assertEquals(v, v takeRight Int.MaxValue)
    assertEquals(Spandex.empty[Int], v drop Int.MaxValue)
    assertEquals(Spandex.empty[Int], v dropRight Int.MaxValue)

    assertEquals(Spandex.empty[Int], v take Int.MinValue)
    assertEquals(Spandex.empty[Int], v takeRight Int.MinValue)
    assertEquals(v, v drop Int.MinValue)
    assertEquals(v, v dropRight Int.MinValue)
  }
}

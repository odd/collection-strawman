package strawman.collection.immutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ArraySeqTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val v = ArraySeq(0) ++ ArraySeq(1 to 64: _*)

    assertEquals(ArraySeq(0, 1), v take 2)
    assertEquals(ArraySeq(63, 64), v takeRight 2)
    assertEquals(ArraySeq(2 to 64: _*), v drop 2)
    assertEquals(ArraySeq(0 to 62: _*), v dropRight 2)

    assertEquals(v, v take Int.MaxValue)
    assertEquals(v, v takeRight Int.MaxValue)
    assertEquals(ArraySeq.empty[Int], v drop Int.MaxValue)
    assertEquals(ArraySeq.empty[Int], v dropRight Int.MaxValue)

    assertEquals(ArraySeq.empty[Int], v take Int.MinValue)
    assertEquals(ArraySeq.empty[Int], v takeRight Int.MinValue)
    assertEquals(v, v drop Int.MinValue)
    assertEquals(v, v dropRight Int.MinValue)
  }
}

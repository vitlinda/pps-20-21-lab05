package u05lab

import org.junit.Assert._
import org.junit.Test
import u05lab.code.{Sequences}

class SequenceTest {

  @Test
  def testSequence() {
    assertEquals(Some(List(1, 2, 3)), Sequences.sequence(List[Option[Int]](Some(1), Some(2), Some(3))))
    assertEquals(None, Sequences.sequence(List[Option[Int]](Some(1), None, Some(3))))

  }

}

package u05lab.code

import org.junit.Assert._
import org.junit.Test

class ListsMyTest {

  @Test
  def testZipRight() {
    val l = List("a", "b", "c")

    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a",0), ("b",1),("c",2)), l.zipRight)
  }

  @Test
  def testPartition() ={
    import List._
    val l = 10 :: 20 :: 30 :: 40 :: nil // same as above
    assertEquals((cons(20,cons(30,cons(40,nil))), cons(10,nil)), l.partition(_>15))
  }
}
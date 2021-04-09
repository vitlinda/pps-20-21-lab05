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
    val l = 10 :: 20 :: 30 :: 40 :: nil
    assertEquals((cons(20, cons(30, cons(40, nil))), cons(10, nil)), l.partition(_>15))
  }

  @Test
  def testSpan() = {
    import List._
    val l = 10 :: 20 :: 30 :: 40 :: nil
    assertEquals((nil, cons(10, cons(20, cons(30, cons(40, nil))))), l.span(_>15))
    assertEquals((cons(10, nil), cons(20, cons(30, cons(40, nil)))), l.span(_<15))
  }

  @Test
  def testReduce(): Unit = {
    import List._
    val l = 10 :: 20 :: 30 :: 40 :: nil
    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduce(_+_))
    assertEquals(100, l.reduce(_+_))

    val l1 = 10 :: nil
    assertEquals(10, l1.reduce(_+_))
  }

  @Test
  def testTakeRight() = {
    import List._
    val l = 10 :: 20 :: 30 :: 40 :: nil
    assertEquals(cons(30, cons(40, nil)),l.takeRight(2))
  }

  @Test
  def testCollect() = {
    import List._
    val l = 30 :: 40 :: nil

//    val partialFunction: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
//      def apply(x: Int): Int = x + 1
//      def isDefinedAt(x: Int) = x > 20
//    }
    val partialFunction: PartialFunction[Int, Int] = {
      case e: Int if e > 20 => e + 1
    }
      assertEquals(cons(31, cons(41, nil)), l.collect(partialFunction))

  }
}
package u05lab.code

import java.util.concurrent.TimeUnit

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {

  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (!msg.isEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}

object CollectionsTest extends App {

  /* Linear sequences: List, ListBuffer */

  import scala.collection.mutable.{ListBuffer => MutableList}

  val mutableList: MutableList[Int] = MutableList()
  for (i <- 1 to 1000000) mutableList += i

  var list: scala.List[Int] = scala.List()
  for (i <- 1 to 1000000) list = i :: list

  import PerformanceUtils._

  assert(measure("mutable list reverse") {
    mutableList.reverse
  } > measure("list reverse") {
    list.reverse
  })
  assert(measure("mutable list append element to the end") {
    mutableList.append(1000001)
  } < measure("list append element to the end") {
    list :+ 1000001
  })
  assert(measure("mutable list last element") {
    mutableList.last
  } < measure("list last element") {
    list.last
  })
  assert(measure("mutable list remove last element") {
    mutableList - 1000001
  } > measure("list remove last element") {
    list = list.filter(_ != 1000001)
  })

  /* Indexed sequences: Vector, Array, ArrayBuffer */

  import scala.collection.mutable.{ArrayBuffer => MutableArray}

  var vector = (1 to 1000000).toVector
  var array = (1 to 1000000).toArray
  var mutableArray: ArrayBuffer[Int] = MutableArray()
  for (i <- 1 to 1000000) mutableArray += i

  measure("mutable array reverse") { mutableArray.reverse }
  measure("array reverse") { array.reverse }
  measure("mutable array append element to the end") { mutableArray.append(1000001) }
  measure("array append element to the end") { array :+ 1000001 }
  measure("mutable array last element") { mutableArray.last }
  measure("array last element") { array.last }
  measure("mutable array remove last element") { mutableArray - 1000001 }
  measure("array remove last element") { array = array.filter(_ != 1000001) }

  assert( measure("vector reverse") { vector.reverse } > measure("array reverse") { array.reverse })
  assert( measure("vector append element to the end") { vector :+ 1000001 } > measure("mutable array append element to the end") { mutableArray.append(1000001) })
  assert( measure("vector remove last element") { vector = vector.filter(_ != 1000001) } > measure("array remove last element") { array = array.filter(_ != 1000001) })


  /* Sets */

  import scala.collection.mutable.{Set => MutableSet}

  val mutableSet: MutableSet[Int] = MutableSet()
  for (i <- 1 to 1000000) mutableSet += i

  var set = Set[Int]()
  for (i <- 1 to 1000000) set = set + i

  assert( measure("mutableSet add element") { mutableSet += 1000001 } > measure("set add element") { set = set + 1000001 })
  assert( measure("mutableSet remove element") { mutableSet - 1000001 } >  measure("set remove element") { set = set.filter(_ != 1000001) } )


  /* Maps */

  import scala.collection.mutable.{Map => MutableMap}
  val mutableMap: MutableMap[String, Int] = MutableMap()
  for (i <- 1 to 1000000) mutableMap += i.toString -> i

  var map = Map[String, Int]()
  for (i <- 1 to 1000000) map = map + (i.toString -> i)

  measure("mutableMap add element") { mutableMap += "1000001" -> 1000001 }
  measure("map add element") { map = map + ("1000001" -> 1000001) }
  measure("mutableMap remove element") { mutableMap - "1000001" -> 1000001 }
  measure("map remove element") { map = map.filter(_._1 != "1000001") }
  measure("mutableMap remove element with filter") { map.filter(_._1 != "1000001") }



  /* Comparison */

  import PerformanceUtils._

  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector
  assert(measure("lst last") {
    lst.last
  } > measure("vec last") {
    vec.last
  })
}
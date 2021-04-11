package u05lab.code

import scala.collection.immutable.HashMap


trait ExamsManager {
  def createNewCall(call: String): Unit

  def addStudentResult(call: String, student: String, result: ExamResult): Unit

  def getAllStudentsFromCall(call: String): Set[String]

  def getEvaluationsMapFromCall(call: String): Map[String, Int]

  def getResultsMapFromStudent(student: String): Map[String, String]

  def getBestResultFromStudent(student: String): Option[Int]
}

object ExamsManager {
  def apply(): ExamsManager = ExamsManagerImpl()

  private case class ExamsManagerImpl() extends ExamsManager {
    private var map: Map[String, Map[String, ExamResult]] = HashMap()
    private def checkArgument(cond: Boolean): Unit = if(!cond) throw new IllegalArgumentException

    override def createNewCall(call: String): Unit = {
      checkArgument(!map.contains(call))
      map = map + (call -> Map())
    }

    override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
      checkArgument(map.contains(call))
      checkArgument(!map(call).contains(student))
      map = map + (call -> (map(call) + (student -> result)))
    }

    override def getAllStudentsFromCall(call: String): Set[String] = map(call).keySet

    override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
      map(call).filter(_._2.evaluation.isDefined).map(v => (v._1 -> v._2.evaluation.get))
    }

    override def getResultsMapFromStudent(student: String): Map[String, String] = {
      map.filter(p => p._2.contains(student)).map(e => (e._1 -> e._2(student).toString))
    }

    override def getBestResultFromStudent(student: String): Option[Int] = {
      val results = map.values.flatten.filter(_._1 == student).filter(_._2.evaluation.isDefined).map(_._2.evaluation).toList
      results match {
        case scala.Nil => None
        case _ => results.max
      }
    }

  }

}

trait Kind

object Kind {

  case object RETIRED extends Kind

  case object FAILED extends Kind

  case object SUCCEEDED extends Kind

}

trait ExamResult {
  def kind: Kind

  def evaluation: Option[Int]

  def cumLaude: Boolean
}

object ExamResult {
  def apply(kind: Kind, evaluation: Option[Int], cumLaude: Boolean): ExamResult = new ExamResultImpl(kind, evaluation, cumLaude)

  class ExamResultImpl(override val kind: Kind,
                       override val evaluation: Option[Int],
                       override val cumLaude: Boolean) extends ExamResult {
    override def toString: String = kind.toString
  }

  case class ExamResultSucceeded(override val kind: Kind,
                                 override val evaluation: Option[Int],
                                 override val cumLaude: Boolean) extends ExamResultImpl(kind, evaluation, cumLaude) {
    override def toString: String = super.toString + "(" + evaluation.get + ")"
  }

  case class ExamResultSucceededCumLaude(override val kind: Kind,
                                         override val evaluation: Option[Int],
                                         override val cumLaude: Boolean) extends ExamResultImpl(kind, evaluation, cumLaude) {
    override def toString: String = super.toString + "(" + evaluation.get + "L)"
  }

}

trait ExamResultFactory {
  def failed(): ExamResult

  def retired(): ExamResult

  def succeededCumLaude(): ExamResult

  def succeeded(evaluation: Int): ExamResult
}

class ExamResultFactoryImpl() extends ExamResultFactory {
  override def failed(): ExamResult = ExamResult(Kind.FAILED, None, false)

  override def retired(): ExamResult = ExamResult(Kind.RETIRED, None, false)

  import u05lab.code.ExamResult.ExamResultSucceededCumLaude

  override def succeededCumLaude(): ExamResult = ExamResultSucceededCumLaude(Kind.SUCCEEDED, Some(30), true)

  import u05lab.code.ExamResult.ExamResultSucceeded

  override def succeeded(evaluation: Int): ExamResult = {
    if (evaluation < 18 || evaluation > 30) throw new IllegalArgumentException
    ExamResultSucceeded(Kind.SUCCEEDED, Some(evaluation), false)
  }
}
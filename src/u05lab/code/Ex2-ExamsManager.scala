package u05lab.code

object ExamsManager {

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
  def apply(kind: Kind, evaluation: Option[Int], cumLaude: Boolean): ExamResult = ExamResultImpl(kind: Kind, evaluation, cumLaude)

  case class ExamResultImpl(override val kind: Kind,
                            override  val evaluation: Option[Int],
                            override val cumLaude: Boolean) extends ExamResult {
    override def toString: String = kind.toString
  }

  case class ExamResultSucceeded(override val kind: Kind,
                                 override  val evaluation: Option[Int],
                                 override val cumLaude: Boolean) extends ExamResult {
    override def toString: String = kind.toString + "(" + evaluation.get + ")"
  }

  case class ExamResultSucceededCumLaude(override val kind: Kind,
                                 override  val evaluation: Option[Int],
                                 override val cumLaude: Boolean) extends ExamResult {
    override def toString: String = kind.toString + "(" + evaluation.get + "L)"
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
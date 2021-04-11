package u05lab
import org.junit.Assert._
import org.junit.Test
import u05lab.code.{ExamResultFactoryImpl, ExamsManager, Kind}

import scala.collection.immutable.HashSet


class ExamsManagerTest {
  private val erf = new ExamResultFactoryImpl()
  private val em: ExamsManager = ExamsManager()

  // verifica base di ExamResultFactory
  @Test
  def testExamResultsBasicBehaviour() {
    // esame fallito, non c'è voto
    assertEquals(erf.failed().kind, Kind.FAILED);
    assertFalse(erf.failed().evaluation.isDefined)
    assertFalse(erf.failed().cumLaude);
    assertEquals(erf.failed().toString, "FAILED")

    // lo studente si è ritirato, non c'è voto
    assertEquals(erf.retired().kind, Kind.RETIRED);
    assertFalse(erf.retired().evaluation.isDefined)
    assertFalse(erf.retired().cumLaude);
    assertEquals(erf.retired().toString, "RETIRED")

    // 30L
    assertEquals(erf.succeededCumLaude().kind, Kind.SUCCEEDED)
    assertEquals(erf.succeededCumLaude().evaluation, Some(30))
    assertTrue(erf.succeededCumLaude().cumLaude)
    assertEquals(erf.succeededCumLaude().toString, "SUCCEEDED(30L)")

    // esame superato, ma non con lode
    assertEquals(erf.succeeded(28).kind, Kind.SUCCEEDED)
    assertEquals(erf.succeeded(28).evaluation, Some(28))
    assertFalse(erf.succeeded(28).cumLaude)
    assertEquals(erf.succeeded(28).toString, "SUCCEEDED(28)")
  }

  // verifica eccezione in ExamResultFactory
  @Test(expected = classOf[IllegalArgumentException])
  def optionalTestEvaluationCantBeGreaterThan30() {
    erf.succeeded(32)
  }

  // verifica eccezione in ExamResultFactory
  @Test(expected = classOf[IllegalArgumentException])
  def optionalTestEvaluationCantBeSmallerThan18() {
    erf.succeeded(17)
  }

  // metodo di creazione di una situazione di risultati in 3 appelli
  def prepareExams() {
    em.createNewCall("gennaio")
    em.createNewCall("febbraio")
    em.createNewCall("marzo")

    em.addStudentResult("gennaio", "rossi", erf.failed()) // rossi -> fallito
    em.addStudentResult("gennaio", "bianchi", erf.retired()) // bianchi -> ritirato
    em.addStudentResult("gennaio", "verdi", erf.succeeded(28)) // verdi -> 28
    em.addStudentResult("gennaio", "neri", erf.succeededCumLaude()) // neri -> 30L

    em.addStudentResult("febbraio", "rossi", erf.failed()) // etc..
    em.addStudentResult("febbraio", "bianchi", erf.succeeded(20))
    em.addStudentResult("febbraio", "verdi", erf.succeeded(30))

    em.addStudentResult("marzo", "rossi", erf.succeeded(25))
    em.addStudentResult("marzo", "bianchi", erf.succeeded(25))
    em.addStudentResult("marzo", "viola", erf.failed())
  }

  // verifica base della parte obbligatoria di ExamManager
  @Test
  def testExamsManagement() {
    this.prepareExams()

    // partecipanti agli appelli di gennaio e marzo
    assertEquals(em.getAllStudentsFromCall("gennaio"), HashSet("rossi","bianchi","verdi","neri"))
    assertEquals(em.getAllStudentsFromCall("marzo"), HashSet("rossi","bianchi","viola"))

    // promossi di gennaio con voto
    assertEquals(em.getEvaluationsMapFromCall("gennaio").size,2)
    assertEquals(em.getEvaluationsMapFromCall("gennaio").get("verdi"), Some(28))
    assertEquals(em.getEvaluationsMapFromCall("gennaio").get("neri"), Some(30))
    // promossi di febbraio con voto
    assertEquals(em.getEvaluationsMapFromCall("febbraio").size,2);
    assertEquals(em.getEvaluationsMapFromCall("febbraio").get("bianchi"), Some(20))
    assertEquals(em.getEvaluationsMapFromCall("febbraio").get("verdi"), Some(30))

    // tutti i risultati di rossi (attenzione ai toString!!)
    assertEquals(em.getResultsMapFromStudent("rossi").size,3)
    assertEquals(em.getResultsMapFromStudent("rossi")("gennaio"),"FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("febbraio"),"FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("marzo"),"SUCCEEDED(25)")

    // tutti i risultati di bianchi
    assertEquals(em.getResultsMapFromStudent("bianchi").size,3)
    assertEquals(em.getResultsMapFromStudent("bianchi")("gennaio"),"RETIRED")
    assertEquals(em.getResultsMapFromStudent("bianchi")("febbraio"),"SUCCEEDED(20)")
    assertEquals(em.getResultsMapFromStudent("bianchi")("marzo"),"SUCCEEDED(25)")

    // tutti i risultati di neri
    assertEquals(em.getResultsMapFromStudent("neri").size,1)
    assertEquals(em.getResultsMapFromStudent("neri")("gennaio"),"SUCCEEDED(30L)")
  }

  // verifica del metodo ExamManager.getBestResultFromStudent
  @Test
  def optionalTestExamsManagement() {
    this.prepareExams();
    // miglior voto acquisito da ogni studente, o vuoto..
    assertEquals(em.getBestResultFromStudent("rossi"), Some(25))
    assertEquals(em.getBestResultFromStudent("bianchi"), Some(25))
    assertEquals(em.getBestResultFromStudent("neri"), Some(30))
    assertEquals(em.getBestResultFromStudent("viola"), None)
  }


  @Test(expected = classOf[IllegalArgumentException])
  def optionalTestCantCreateACallTwice() {
    this.prepareExams()
    em.createNewCall("marzo")
  }

  @Test(expected = classOf[IllegalArgumentException])
  def optionalTestCantRegisterAnEvaluationTwice() {
    this.prepareExams()
    em.addStudentResult("gennaio", "verdi", erf.failed())
  }
}

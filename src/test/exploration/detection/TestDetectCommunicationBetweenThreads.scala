package exploration.detection

import ir._
import org.junit.Assert._
import org.junit.Test
import rewriting.rules.OpenCLRules

class TestDetectCommunicationBetweenThreads {

  import DetectCommunicationBetweenThreads._

  @Test
  def partialReduceWithReorderNoRace(): Unit = {

    // Communication between threads
    val f = gemvPartialReduceWithReorderNoRace

    val communicationHere = getCommunicationExpr(f)
    assertEquals(1, communicationHere.length)

    val implemented =
      communicationHere.map(implementCopyOnCommunication(f, _, OpenCLRules.localMemory))

    implemented.foreach(TypeChecker.apply)
  }

  @Test
  def introduceReuse(): Unit = {
    val communicationHere = getCommunicationExpr(gemvIntroduceReuse)
    assertEquals(0, communicationHere.length)
  }

  @Test
  def mmPlainTiling(): Unit = {
    val communicationHere = getCommunicationExpr(mmTiled)
    assertEquals(0, communicationHere.length)
  }

  @Test
  def mmTATiled1DBlocked(): Unit = {
    val communicationHere = getCommunicationExpr(mmTiled1DBlocked)
    assertEquals(0, communicationHere.length)
  }

  @Test
  def mmTATiled2DBlocked(): Unit = {
    val communicationHere = getCommunicationExpr(mmTiled2DBlocked)
    assertEquals(0, communicationHere.length)
  }
}

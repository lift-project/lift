package exploration.detection

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test
import rewriting.rules.OpenCLRules

class TestDetectCommunicationBetweenThreads {

  private val v_M_0 = Var("M", StartFromRange(1))
  private val v_N_1 = Var("N", StartFromRange(1))

  import DetectCommunicationBetweenThreads._

  @Test
  def partialReduceWithReorderNoRace(): Unit = {

    // Communication between threads
    val f = fun(
      ArrayType(ArrayType(Float, v_M_0), v_N_1),
      ArrayType(Float, v_M_0),
      ArrayType(Float, v_N_1),
      Float, Float,
      (p_0, p_1, p_2, p_3, p_4) =>
        FunCall(MapWrg(0)(fun((p_5) =>
          FunCall(Join(),
            FunCall(MapLcl(0)(fun((p_6) =>
              FunCall(toGlobal(fun((p_7) =>
                FunCall(MapSeq(fun((p_8) =>
                  FunCall(add,
                    FunCall(mult, p_8, p_3),
                    FunCall(mult,
                      FunCall(Get(1), p_5), p_4)))), p_7))),
                FunCall(ReduceSeq(fun((p_9, p_10) =>
                  FunCall(add, p_9, p_10))),
                  FunCall(idfloat, Value("0.0f", Float)), p_6)))),
              FunCall(Split(Cst(128)),
                FunCall(Join(),
                  FunCall(MapLcl(0)(fun((p_11) =>
                    FunCall(ReduceSeq(fun((p_12, p_13) =>
                      FunCall(add, p_12,
                        FunCall(mult,
                          FunCall(Get(0), p_13),
                          FunCall(Get(1), p_13))))),
                      FunCall(idfloat, Value("0.0f", Float)), p_11))),
                    FunCall(Split( v_M_0 * Pow(Cst(128), Cst(-1)) ),
                      FunCall(Gather(ReorderWithStride(128)),
                        FunCall(Zip(2), p_1,
                          FunCall(Get(0), p_5))))))))))),
          FunCall(Zip(2), p_0, p_2)))


    val communicationHere = getCommunicationExpr(f)
    assertEquals(1, communicationHere.length)

    val implemented =
      communicationHere.map(implementCopyOnCommunication(f, _, OpenCLRules.localMemory))

    implemented.foreach(TypeChecker.apply)
  }

  @Test
  def introduceReuse(): Unit = {
    val communicationHere = getCommunicationExpr(mmIntroduceReuse)
    assertEquals(0, communicationHere.length)
  }

  @Test
  def mmPlainTiling(): Unit = {
    val communicationHere = getCommunicationExpr(mmTiled)
    assertEquals(0, communicationHere.length)
  }

  @Test
  def mmTATiled2DBlocked(): Unit = {
    val communicationHere = getCommunicationExpr(mmTiled2DBlocked)
    assertEquals(0, communicationHere.length)
  }
}

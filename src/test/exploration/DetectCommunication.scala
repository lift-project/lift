package exploration

import ir._
import ir.ast._
import ir.view.{View, ViewMem}
import lift.arithmetic._
import opencl.generator.{NDRange, RangesAndCounts}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

class DetectCommunication {

  private val v_M_0 = Var("M", StartFromRange(1))
  private val v_N_1 = Var("N", StartFromRange(1))

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

    val copiedLambda = f
    val userFuns = Expr.visitWithState(Seq[FunCall]())(copiedLambda.body, {
      case (call@FunCall(_: UserFun, _*), seq) => seq :+ call
      case (call@FunCall(_: VectorizeUserFun, _*), seq) => seq :+ call
      case (_, seq) => seq
    })

    TypeChecker(copiedLambda)
    InferOpenCLAddressSpace(copiedLambda)
    RangesAndCounts(copiedLambda, NDRange(?, ?, ?), NDRange(?, ?, ?), collection.Map())
    OpenCLMemoryAllocator(copiedLambda)
    View(copiedLambda)
    UpdateContext(copiedLambda)

    val memVars = userFuns.map(_.mem.variable)

    val varsWithDataFlow = userFuns.map(_.args.filter(x => View.getSubViews(x.view).exists({
      case ViewMem(v, _) => memVars.contains(v) && userFuns.find(_.mem.variable == v).get.context.inMapLcl.reduce(_ || _)
      case _ => false
    }))).filter(_.nonEmpty).flatten.filterNot({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => true
      case _ => false
    })

    // Filter ones that have ViewAccess("l_id") o Join/Split/etc o ViewMap("l_id")
    // or simple approach MapLcl(...) o Split/Join/etc o MapLcl(...)

    println(varsWithDataFlow)

    // addIdAfterReduce / toLocal for last write where communication
    // similar to final write to global.
    println(f)
  }

}

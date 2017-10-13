package exploration.detection

import ir._
import ir.ast._
import ir.view._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Test

class DetectCommunicationBetweenThreads {

  private val v_M_0 = Var("M", StartFromRange(1))
  private val v_N_1 = Var("N", StartFromRange(1))

  def hasCommunication(expr: Expr): Boolean = {

    val testViews = View.getSubViews(expr.view)

    val dropTest = testViews.dropWhile({
      case ViewAccess(i, _, _) => !i.toString.startsWith("v_l_id")
      case _ => true
    })

    var accessCount = 0
    var mapCount = 0

    val takeTest = dropTest.takeWhile({
      case ViewAccess(i, _, _) if i.toString.startsWith("v_l_id") =>
        accessCount = accessCount + 1
        true
      case ViewMap(_, i, _) if i.toString.startsWith("v_l_id") =>
        mapCount = mapCount + 1
        accessCount != mapCount
      case _ => true
    })

    val communication = takeTest.collect({
      case ViewSplit(_, _, _) =>
      case ViewJoin(i, _, _) if i != Cst(1) =>
      case ViewReorder(_, _, _) =>
      case ViewAsVector(_, _, _) =>
      case ViewAsScalar(_, _, _) =>
      case ViewHead(_, _) =>
      case ViewTail(_, _) =>
      case ViewPad(_, _, _, _, _) =>
      case ViewSlide(_, _, _) =>
      case ViewFilter(_, _, _) =>
    }).nonEmpty

    val accessesBeforeJoin = takeTest.takeWhile({
      case ViewJoin(i, _, _) if i == Cst(1) => false
      case _ => true
    }).count({
      case ViewAccess(i, _, _) if i.toString.startsWith("v_l_id") => true
      case _ => false
    })

    val hasJoin = takeTest.collect({ case ViewJoin(i, _, _) if i == Cst(1) => }).nonEmpty
    val joinCommunication = hasJoin && accessesBeforeJoin > accessCount
    val tryToChangeAddressSpace = communication || joinCommunication
    tryToChangeAddressSpace
  }

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

    prepareLambda(f)

    val userFuns = Expr.visitWithState(Seq[FunCall]())(f.body, {
      case (call@FunCall(_: UserFun, _*), seq) => seq :+ call
      case (call@FunCall(_: VectorizeUserFun, _*), seq) => seq :+ call
      case (_, seq) => seq
    })

    val memVars = userFuns.map(_.mem.variable)

    val varsWithDataFlow = userFuns.map(uf =>
      uf.args.filter(arg =>
        View.getSubViews(arg.view).exists({
          case ViewMem(v, _) if memVars.contains(v) =>
            val value = userFuns.filter(uf2 => uf2.mem.variable == v)
            value.forall(_.context.inMapLcl.reduce(_ || _)) && !value.exists(_.eq(uf))
          case _ => false
        }))).filter(_.nonEmpty).flatten.filterNot({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => true
      case _ => false
    })

    println(varsWithDataFlow.filter(hasCommunication))

    // Filter ones that have ViewAccess("l_id") o Join/Split/etc o ViewMap("l_id")
    // or simple approach MapLcl(...) o Split/Join/etc o MapLcl(...)
    // Join of inner length 1 is fine if the inner length was produced by ReduceSeq



    // addIdAfterReduce / toLocal for last write where communication
    // similar to final write to global.
//    println(f)
  }

}

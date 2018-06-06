package exploration.detection

import ir._
import ir.ast._
import ir.view._
import lift.arithmetic._
import opencl.ir.pattern._
import rewriting.Rewrite
import rewriting.rules.{CopyRules, Rule}

object DetectCommunicationBetweenThreads {

  def getCommunicationExpr(f: Lambda): Seq[Expr] = {
    prepareLambda(f)
    val argsWithDataFlow = getCandidatesToCheck(f)

    // Filter ones that have ViewAccess("l_id") o Join/Split/etc o ViewMap("l_id")
    // or simple approach MapLcl(...) o Split/Join/etc o MapLcl(...)
    // Join of inner length 1 is fine if the inner length was produced by ReduceSeq
    argsWithDataFlow.filter(hasCommunication)
  }

  private def getCandidatesToCheck(lambda: Lambda) = {
    prepareLambda(lambda)

    val userFuns = Expr.visitWithState(Seq[FunCall]())(lambda.body, {
      case (call@FunCall(_: UserFun | _: VectorizeUserFun, _*), seq) => seq :+ call
      case (_, seq) => seq
    })

    val memVars = userFuns.map(_.mem.variable)

    val userFunsWoForceToTuple = userFuns.filterNot(call =>
      getUserFunName(call.f).startsWith("id")) // TODO: Better way to deal with forcing values into a tuple

    val varsWithDataFlow = userFunsWoForceToTuple.map(uf =>
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
    varsWithDataFlow
  }

  private def hasCommunication(expr: Expr): Boolean = {

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

  def implementCopyOnCommunication(lambda: Lambda, communicationArg: Expr, rule: Rule): Lambda = {

    val userFuns = rewriting.utils.Utils.collect(lambda.body, {
      case call@FunCall(_: UserFun | _: VectorizeUserFun, _*)
        if call.mem == communicationArg.mem =>
    })

    val noAccumulator = userFuns.filter(expr =>
      !lambda.body.contains({
        case FunCall(_: AbstractReduce, acc, _) if acc.contains({ case e if e.eq(expr) => })=>
      })
    )

    // Only one UserFunction is allowed to write, except in the case of accumulators
    assert(noAccumulator.length == 1)

    val copyThis = noAccumulator.head

    // Find outermost Reduce around `copyThis` if it exists
    val inReduce = rewriting.utils.Utils.collect(lambda.body, {
      case call@FunCall(reduce: AbstractReduce, _, _)
        if reduce.f.body.contains({ case e if e.eq(copyThis) => }) &&
          !call.context.inReduceSeq
      =>
    })

    if (inReduce.nonEmpty) {
      // in reduce, add id after reduce + implement + toLocal/toGlobal

      val reduce = inReduce.head
      val idAdded = Rewrite.applyRuleAt(reduce, CopyRules.addIdAfterReduce, reduce)
      TypeChecker(idAdded)
      val applyHere =
        idAdded match { case FunCall(MapSeq(Lambda1(_, id@FunCall(Id(), _))), _) => id }

      val idImplementation =
        Rewrite.applyRuleAt(applyHere, CopyRules.implementIdAsDeepCopy, applyHere)

      val idFun = rewriting.utils.Utils.collect(idImplementation, {
        case FunCall(uf: UserFun, _*) if uf.name.startsWith("id") => })

      val idImplemented = Expr.replace(idAdded, applyHere, idImplementation)
      val idImplementedLambda = FunDecl.replace(lambda, reduce, idImplemented)

      assert(idFun.length == 1)

      Rewrite.applyRuleAt(idImplementedLambda, idFun.head, rule)

    } else {
      // in map, add toLocal/toGlobal around copyThis
      Rewrite.applyRuleAt(lambda, copyThis, rule)
    }
  }
}

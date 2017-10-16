package exploration.detection

import exploration.MemoryMappingRewrite
import ir.ast._
import ir.view._
import lift.arithmetic.Var
import opencl.ir.OpenCLMemory.getAllMemoryVars
import opencl.ir.pattern._

object DetectReuseWithinThread {

  def findStrategicLocations(f: Lambda):  Seq[(Expr, Var)] = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForPrivate(f)
    getCandidates(strategicLocationsMarked)
  }

  def getCandidates(strategicLocationsMarked: Lambda): Seq[(Expr, Var)] = {
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getRuleLocationCandidates(strategicLocationsMarked, _))
    tryHere
  }


  private def getReuseCandidates(f: Lambda) = {
    val numDimensions = getNumDimensions(f)

    prepareLambda(f)

    val args = Expr.visitWithState(Seq[Expr]())(f.body, {
      case (call@FunCall(_: UserFun | _: VectorizeUserFun, args@_*), seq)
        if !getUserFunName(call.f).startsWith("id") && // TODO: Better way to deal with forcing values into a tuple
          call.context.inMapLcl.count(b => b) + call.context.inMapGlb.count(b => b) == numDimensions
      => seq ++ args
      case (_, seq) => seq
    }).distinct.diff(f.params).filter({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => false
      case _: Value => false
      case _ => true
    })

    args.filterNot(arg => getNumberOfPrivateAccesses(arg) >= getNumberOfSequentialDimensions(f, arg))
  }

  private def getRuleLocationCandidates(strategicLocationsMarked: Lambda, reuseExpr: Expr) = {
    val sourceView = View.getSubViews(reuseExpr.view).last

    sourceView match {
      case ViewMem(sourceVar, _) =>

        val numDimension = getNumDimensions(strategicLocationsMarked)

        // Find which "strategic" Id location(s) would copy the required variable and is suitable for local memory
        // TODO: Doesn't work for all reuse... Does it matter? Still gets what we care about
        Expr.visitWithState(Seq[(Expr, Var)]())(strategicLocationsMarked.body, {
          case (funCall@FunCall(Id(), _*), seq)
            if getAllMemoryVars(funCall.mem).contains(sourceVar) &&
              (funCall.context.inMapLcl.count(b => b) == numDimension
                || funCall.context.inMapGlb.count(b => b) == numDimension)
          =>
            seq :+ (funCall, sourceVar)
          case (_, seq) => seq
        })

      case _ => Seq()
    }
  }

  private def getNumberOfSequentialDimensions(f: Lambda, expr: Expr) = {

    val views = View.getSubViews(expr.view)

    Expr.visitWithState(0)(f.body, {
      case (FunCall(fp: ReduceSeq, acc, _), count)
        if containsExprButNotParallel(fp, expr) && !views.contains(acc.view) => count + 1
      case (FunCall(fp: MapSeq, _), count) if containsExprButNotParallel(fp, expr) => count + 1
      case (_, count) => count
    })
  }

  private def containsExprButNotParallel(fp: FunDecl with FPattern, expr: Expr) = {
    fp.f.body.contains({ case e if e eq expr => }) && // e contains expr
      !fp.f.body.contains({ case FunCall(MapLcl(_, _), _) => }) && // e doesn't contain parallel
      !fp.f.body.contains({ case FunCall(MapWrg(_, _), _) => }) &&
      !fp.f.body.contains({ case FunCall(MapGlb(_, _), _) => })
  }

  private def getNumberOfPrivateAccesses(expr: Expr) = {
    val views = View.getSubViews(expr.view)

    val sequentialMapViews = views.takeWhile({
      case ViewAccess(v, _, _) => v.toString.startsWith("v_i_")
      case ViewMap(_, v, _) => v.toString.startsWith("v_i_")
      case _ => true
    })

    val viewMaps = sequentialMapViews.count({
      case ViewMap(_, v, _) => v.toString.startsWith("v_i_")
      case _ => false
    })

    val viewAccesses = sequentialMapViews.count({
      case ViewAccess(v, _, _) => v.toString.startsWith("v_i_")
      case _ => false
    })

    viewAccesses - viewMaps
  }


}

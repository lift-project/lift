package exploration.detection

import exploration.MemoryMappingRewrite
import ir.ast._
import ir.view._
import lift.arithmetic.Var
import opencl.ir.pattern._

object DetectReuseWithinThread {

  def findStrategicLocations(f: Lambda):  Seq[(Expr, Var)] = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForPrivate(f)
    getCandidates(strategicLocationsMarked)
  }

  def getCandidates(strategicLocationsMarked: Lambda): Seq[(Expr, Var)] = {
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getLocationForCopyWithinThread(strategicLocationsMarked, _))
    tryHere
  }

  private def getReuseCandidates(f: Lambda) = {
    val numDimensions = getNumDimensions(f)

    def testNesting(call: FunCall) =
      call.context.inMapLcl.count(b => b) + call.context.inMapGlb.count(b => b) == numDimensions

    val args = getArgumentsAtSuitableLevel(f, testNesting)

    args.filterNot(arg => getNumberOfPrivateAccesses(arg) >= getNumberOfSequentialDimensions(f, arg))
  }

  private def getLocationForCopyWithinThread(lambda: Lambda, reuseExpr: Expr) = {

    val numDimension = getNumDimensions(lambda)

    def testContext(funCall: FunCall) =
      funCall.context.inMapLcl.count(b => b) == numDimension ||
        funCall.context.inMapGlb.count(b => b) == numDimension

    getLocationsForCopy(lambda, reuseExpr, testContext)
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

package exploration.detection

import exploration.MemoryMappingRewrite
import ir.ast._
import ir.view._
import lift.arithmetic._

object DetectReuseAcrossThreads {

  def findStrategicLocations(f: Lambda):  Seq[(Expr, Var)] = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForLocal(f)
    getCandidates(strategicLocationsMarked)
  }

  def getCandidates(strategicLocationsMarked: Lambda): Seq[(Expr, Var)] = {
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getLocationForCopyAcrossThreads(strategicLocationsMarked, _))
    tryHere
  }

  private def getReuseCandidates(f: Lambda) = {
    val numDimensions = getNumDimensions(f)

    def testNesting(call: FunCall) =
      call.context.inMapWrg.count(b => b) == numDimensions

    val args = getArgumentsAtSuitableLevel(f, testNesting)

    args.filterNot(getNumberOfLocalAccesses(_) >= numDimensions)
  }

  private def getLocationForCopyAcrossThreads(lambda: Lambda, reuseExpr: Expr) = {

    val numDimension = getNumDimensions(lambda)

    def testContext(funCall: FunCall) =
      !funCall.context.inMapLcl.reduce(_ || _) &&
        funCall.context.inMapWrg.count(b => b) == numDimension

    getLocationsForCopy(lambda, reuseExpr, testContext)
  }

  private def getNumberOfLocalAccesses(expr: Expr) = {
    val views = View.getSubViews(expr.view)

    val viewMaps = views.count({
      case ViewMap(_, v, _) => v.toString.contains("l_id")
      case _ => false
    })

    val viewAccesses = views.count({
      case ViewAccess(v, _, _) => v.toString.contains("l_id")
      case _ => false
    })

    viewAccesses - viewMaps
  }
}

package exploration.detection

import exploration.MemoryMappingRewrite
import ir.TypeChecker
import ir.ast._
import ir.view.{View, ViewAccess, ViewMap, ViewMem}
import lift.arithmetic._
import opencl.ir.OpenCLMemory.getAllMemoryVars
import opencl.ir.OpenCLMemoryCollection
import rewriting.Rewrite
import rewriting.rules.CopyRules

object DetectReuseAcrossThreads {

  def findStrategicLocations(f: Lambda) = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForLocal(f)
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getLocalMemoryCandidates(strategicLocationsMarked, _))

    (strategicLocationsMarked, tryHere)
  }

  def printStrategicLocalMemoryLocations(f: Lambda): Unit = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForLocal(f)
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getLocalMemoryCandidates(strategicLocationsMarked, _))

    val implementThese = createCombinations(tryHere)

    implementCombination(strategicLocationsMarked, implementThese(4))
  }

  def createCombinations(seq: Seq[(Expr, Var)]): Seq[Seq[(Expr, Var)]] = {
    val varToLocation = seq.groupBy(_._2)
    val allVariables = varToLocation.keys.toSeq

    val varCombinations =
      MemoryMappingRewrite.getCombinations(allVariables)

    varCombinations.flatMap(combination => {

      var optionsForVariableCombination = varToLocation(combination.head).map(Seq(_))

      combination.tail.foreach(v => {

        val oldOptions = optionsForVariableCombination

        optionsForVariableCombination =
          varToLocation(v).flatMap(z => oldOptions.map(_ :+ z))
      })

      optionsForVariableCombination
    })
  }

  def implementCombination(f: Lambda, combination: Seq[(Expr, Var)]): Lambda = {

    val locations = combination.groupBy(_._1)

    val simplifiedLocation = locations.map((pair) => (pair._1, pair._2.map(_._2)))

    // Order such that references won't change when implementing
    val order = Expr.visitLeftToRight(Seq[Expr]())(f.body, {
      case (expr, seq) if locations.keys.exists(_ eq expr) => seq :+ expr
      case (_, seq) => seq
    })

    val orderedLocations = order.map(expr => (expr, simplifiedLocation(expr)))

    // Implement one by one
    orderedLocations.foldLeft(f)((currentLambda, pair) =>
      implementReuse(currentLambda, pair._1, pair._2))
  }

  def implementReuse(f: Lambda, location: Expr, variables: Seq[Var]): Lambda = {

    // TODO: toAddressSpace
    if (variables.length == 1 && location.mem.variable == variables.head) {
      // implement deep copy
      Rewrite.applyRuleAt(f, location, CopyRules.implementIdAsDeepCopy)
    } else {
      // Assuming zips have been flattened, so only one level of Tuple

      val indices = variables.map(variable =>
        location.mem.asInstanceOf[OpenCLMemoryCollection].subMemories.indexWhere(_.variable == variable))

      val properResult = implementTuple(location, indices)

      FunDecl.replace(f, location, properResult)
    }
  }

  def implementTuple(expr: Expr, indices: Seq[Int]): Expr = {

    // Implement by one level until we get a tuple and then pick the components
    val oneLevel = CopyRules.implementOneLevelOfId.rewrite(expr)
    TypeChecker(oneLevel)

    oneLevel match {
      case FunCall(Tuple(_), args@_*) =>
        indices.foldLeft(oneLevel)((expr, index) =>
          Rewrite.applyRuleAt(expr, CopyRules.implementIdAsDeepCopy, args(index)))
      case FunCall(fp: FPattern, _) =>
        val newBody = implementTuple(fp.f.body, indices)
        Expr.replace(oneLevel, fp.f.body, newBody)
    }
  }

  private def getLocalMemoryCandidates(strategicLocationsMarked: Lambda, reuseExpr: Expr) = {
    val sourceView = View.getSubViews(reuseExpr.view).last

    sourceView match {
      case ViewMem(sourceVar, _) =>

        val numDimension = getNumDimensions(strategicLocationsMarked)

        // Find which "strategic" Id location(s) would copy the required variable and is suitable for local memory
        // TODO: Doesn't work for all reuse... Does it matter? Still gets what we care about
        Expr.visitWithState(Seq[(Expr, Var)]())(strategicLocationsMarked.body, {
          case (funCall@FunCall(Id(), _*), seq)
            if getAllMemoryVars(funCall.mem).contains(sourceVar) &&
              !funCall.context.inMapLcl.reduce(_ || _) &&
              funCall.context.inMapWrg.count(b => b) == numDimension
          =>
            seq :+ (funCall, sourceVar)
          case (_, seq) => seq
        })

      case _ => Seq()
    }
  }

  private def getReuseCandidates(f: Lambda) = {
    val numDimensions = getNumDimensions(f)

    prepareLambda(f)

    val args = Expr.visitWithState(Seq[Expr]())(f.body, {
      case (call@FunCall(_: UserFun | _: VectorizeUserFun, args@_*), seq)
        if call.context.inMapWrg.count(b => b) == numDimensions
      => seq ++ args
      case (_, seq) => seq
    }).distinct.diff(f.params).filter({
      case FunCall(_: UserFun | _: VectorizeUserFun, _*) => false
      case _: Value => false  // TODO: A copy will have been made here
      case _ => true
    })

    args.filterNot(getNumberOfLocalAccesses(_) >= numDimensions)
  }

  private def getNumberOfLocalAccesses(a: Expr) = {
    val views = View.getSubViews(a.view)

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

package exploration.detection

import exploration.MemoryMappingRewrite
import ir.ast._
import ir.view.{View, ViewAccess, ViewMap, ViewMem}
import lift.arithmetic._
import opencl.ir.OpenCLMemory.getAllMemoryVars
import opencl.ir.OpenCLMemoryCollection
import rewriting.Rewrite
import rewriting.rules.CopyRules

object DetectReuseAcrossThreads {

  def printStrategicLocalMemoryLocations(f: Lambda): Unit = {
    val strategicLocationsMarked = MemoryMappingRewrite.addIdsForLocal(f)
    val reuseCandidates = getReuseCandidates(strategicLocationsMarked)
    val tryHere = reuseCandidates.flatMap(getLocalMemoryCandidates(strategicLocationsMarked, _))

    val implementThese = createCombinations(tryHere)

    println(implementThese.mkString("\n\n"))

//    println(implementReuse(strategicLocationsMarked, tryHere.head._1, tryHere.head._2))

//    println
//    println(strategicLocationsMarked)
//    println(tryHere.mkString(", "))
//    println
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

  def implementReuse(f: Lambda, location: Expr, variable: Var): Lambda = {
    println(location.mem.variable)
    println(variable)
    // TODO: toAddressSpace
    if (location.mem.variable == variable) {
      // implement deep copy
      Rewrite.applyRuleAt(f, location, CopyRules.implementIdAsDeepCopy)
    } else {
      // Implement one level and pick the component that contains variable
      // Assuming it's a tuple. And Zips have been flattened, so only one level of Tuple, any second being a struct

      println(location.t)
      val index =
        location.mem.asInstanceOf[OpenCLMemoryCollection].subMemories.indexWhere(_.variable == variable)

      println(location.mem)

      val result = CopyRules.implementOneLevelOfId.rewrite(location)
      val properResult = result match {
        case FunCall(Tuple(_), args@_*) =>
          Rewrite.applyRuleAt(result, CopyRules.implementIdAsDeepCopy, args(index))
      }

      FunDecl.replace(f, location, properResult)

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

package dse

import ir.TypeChecker
import ir.ast.Lambda
import opencl.executor.{Compile, Execute}
import rewriting.Rewrite
import rewriting.macrorules.{MacroRules, ReuseRules, SlideTiling}
import rewriting.rules._


object Explorer {


  val HighLevelRules = List(
    ReuseRules.apply2DRegisterBlocking,
    ReuseRules.apply2DRegisterBlockingNoReorder,
    ReuseRules.apply1DRegisterBlocking,
    ReuseRules.tileMapMap,
    ReuseRules.finishTiling,
    MacroRules.partialReduceWithReorder,
    SlideTiling.tileStencils,
    MacroRules.interchange,
    ReuseRules.introduceReuseFromMap,
    Rules.splitJoin
  )

  val LowLevelRules = List(
    OpenCLRules.reduceSeq,
    OpenCLRules.reduceSeqUnroll,
    OpenCLRules.mapAtomLcl,
    OpenCLRules.localMemory,
    OpenCLRules.globalMemory,
    OpenCLRules.privateMemory,
    OpenCLRules.vectorize(4),
    OpenCLRules.vectorizeToAddressSpace(4),
    OpenCLRules.mapLcl(0),
    OpenCLRules.mapLcl(1),
    OpenCLRules.mapGlb(0),
    OpenCLRules.mapGlb(1),
    OpenCLRules.mapSeq,
    MacroRules.mapComposedWithReduceAsSequential,
    MacroRules.userFunCompositionToPrivate,
    SimplificationRules.removeEmptyMap,
    SimplificationRules.lambdaInlineParam,
    SimplificationRules.dropId,
    SimplificationRules.removeEmptyMap,
    CopyRules.addIdBeforeMapLcl,
    CopyRules.addIdForMapWrgParam,
    CopyRules.addIdAfterReduce,
    CopyRules.addIdForCurrentValueInReduce,
    CopyRules.addIdBeforeMapSeq,
    CopyRules.addIdValue,
    CopyRules.tupleToStruct,
    CopyRules.implementIdAsDeepCopy
  )

  /**
    *  working set :  12:2,0:0,21:0,26:0,4:6,10:0,11:0
    */


  val defaultRules = LowLevelRules ++ HighLevelRules


  def showRules( f0 : Lambda,
                 ruleToPrint : List[rewriting.rules.Rule]): Unit = {
    ruleToPrint.foreach(rule => {
      print( rule.toString + ": " + Rewrite.listAllPossibleRewritesForRules(f0, List(rule)).size + "\n")
    })
  }

  def applyRule( f0 : Lambda,
                 ruleName : rewriting.rules.Rule  ,
                 ruleIndex : Int,
                 ruleToPrint : List[rewriting.rules.Rule]): Lambda = {


    val first_rule = Rewrite.listAllPossibleRewritesForRules(f0, List(ruleName))(ruleIndex)
    val ret = Rewrite.applyRuleAt(f0, first_rule._2, first_rule._1)
    //print("\n\n== "  + ruleName.toString + " ==\n")
    //showRules(f0,ruleToPrint)
    //print("\n\n" + ret + "\n\n")
    ret
  }



  def explore (lambda : Lambda , parameters : List[List[Int]] , rules : List[Rule]): Lambda = {

    var f = List(lambda)

    for (index <- 0 to (parameters.size - 1)) {

      // Pick the rule
      //**************************

      val parameter = parameters(index)
      val rule_index = parameter(0)
      val rule_position = parameter(1)

      val selected_rule = rules(rule_index)
      val allRulesAt = Rewrite.listAllPossibleRewritesForRules(f(0), List(selected_rule))

      printf("Can apply %s in %d slot(s), picked %d.\n", selected_rule, allRulesAt.size, rule_position)

      if (allRulesAt.size == 0) { // TODO: Could continue anyway...
        printf("There is no way to go.\n")
        printf("DEPTH=%d\n", index)
        return
      }

      val point_index = (rule_position % allRulesAt.size)


      // Apply the rule
      //**************************

      try {
        val temporary = Explorer.applyRule(f(0), selected_rule, point_index, rules)
        TypeChecker(temporary)
        f = temporary :: f
      } catch {
        case _: Throwable => printf("RULE FAILED\n")
      }

    }


    // Now all the rules are applied, try compiling it.
    //**************************************************

    try {
      Compile(f(0))
    } catch {
      case _: Throwable => printf("DEPTH=%d\n", 0)
    }


    // And running it.
    //**************************************************


    f(0)
  }

}

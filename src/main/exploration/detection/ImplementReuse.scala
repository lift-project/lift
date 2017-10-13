package exploration.detection

import exploration.MemoryMappingRewrite
import ir.TypeChecker
import ir.ast._
import lift.arithmetic._
import opencl.ir.OpenCLMemoryCollection
import rewriting.Rewrite
import rewriting.rules.{CopyRules, Rule}

object ImplementReuse {

  def apply(lambda: Lambda, seq: Seq[(Expr, Var)], rule: Rule): Seq[Lambda] = {
    val combinations = createCombinations(seq)
    combinations.map(implementCombination(lambda, _, rule))
  }

  def createCombinations(seq: Seq[(Expr, Var)]): Seq[Seq[(Expr, Var)]] = {
    val varToLocation = seq.groupBy(_._2)
    val allVariables = varToLocation.keys.toSeq

    val varCombinations =
      MemoryMappingRewrite.getCombinations(allVariables).filterNot(_.isEmpty)

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

  def implementCombination(f: Lambda, combination: Seq[(Expr, Var)], rule: Rule): Lambda = {

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
      implementReuse(currentLambda, pair._1, pair._2, rule))
  }

  private def implementReuse(f: Lambda, location: Expr, variables: Seq[Var], rule: Rule): Lambda = {

    if (variables.length == 1 && location.mem.variable == variables.head) {

      implementNonTuple(rule, f, location)

    } else {
      // Assuming zips have been flattened, so only one level of Tuple and OpenCLMemoryCollection

      val indices = variables.map(variable =>
        location.mem.asInstanceOf[OpenCLMemoryCollection].subMemories.indexWhere(_.variable == variable))

      implementTuple(f, location, indices, rule)
    }
  }

  private def findId(copyAdded: Expr) = {
    Expr.visitWithState(None: Option[Expr])(copyAdded, {
      case (call@FunCall(uf: UserFun, _), _) if uf.name.startsWith("id") => Some(call)
      case (_, maybe) => maybe
    }).get
  }

  private def implementTuple(f: Lambda, expr: Expr, indices: Seq[Int], rule: Rule): Lambda = {

    // Implement by one level until we get a tuple and then pick the components
    val oneLevel = CopyRules.implementOneLevelOfId.rewrite(expr)
    val oneLevelLambda = FunDecl.replace(f, expr, oneLevel)
    TypeChecker(oneLevelLambda)

    oneLevel match {
      case FunCall(Tuple(_), args@_*) =>

        indices.foldLeft(oneLevelLambda)((currentLambda, index) => {
          val currentArg = args(index)
          implementNonTuple(rule, currentLambda, currentArg)
        })

      case FunCall(fp: FPattern, _) =>
        implementTuple(oneLevelLambda, fp.f.body, indices, rule)
    }
  }

  private def implementNonTuple(rule: Rule, currentLambda: Lambda, currentArg: Expr) = {
    val copyAdded = Rewrite.applyRuleAt(currentArg, CopyRules.implementIdAsDeepCopy, currentArg)

    val userFun = findId(copyAdded)

    val copyAddedLambda = FunDecl.replace(currentLambda, currentArg, copyAdded)

    Rewrite.applyRuleAt(copyAddedLambda, userFun, rule)
  }
}

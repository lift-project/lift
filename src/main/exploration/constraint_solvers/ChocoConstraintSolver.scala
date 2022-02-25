package exploration.constraint_solvers

import com.typesafe.scalalogging.Logger
import exploration.constraint_solvers.ChocoConstraintSolver._
import exploration._
import exploration.constraint_solvers.ConstraintSolver.{SolutionNotFound, getMinMax}
import lift.arithmetic._
import lift.arithmetic.ArithExpr.isInt
import org.chocosolver.solver.Model
import org.chocosolver.solver.constraints.Constraint
import org.chocosolver.solver.expression.continuous.arithmetic.CArExpression
import org.chocosolver.solver.expression.continuous.relational.CReExpression
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.expression.discrete.relational.ReExpression
import org.chocosolver.solver.search.limits.FailCounter
import org.chocosolver.solver.search.loop.lns.neighbors.RandomNeighborhood
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.variables.IntVar
import rewriting.RewriteParamWithOrdinalValues.EnumeratedVar

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.Random

class ChocoConstraintSolver(paramSpace: ParameterSpace,
                            val initialSeed: Int,
                            initialSolverState: Option[SolverState] = None) extends ConstraintSolver {
  private val logger = Logger(this.getClass)
  var exhausted: Boolean = false

  def getSolverDebugStr: String = model.toString

  val booleanOps = List("=", "!=", "<", ">", "<=", ">=")

  // TODO: deprecate
  val state: SolverState = initialSolverState.getOrElse(SolverState(ListBuffer()))

  var lastSolution: Option[ListMap[Var, Cst]] = None

  /* 1. Create a Model */
  implicit val model = new Model("conv_params")
  // Initialise parameters and constraints
  val (modelVars: List[IntVar], dependentModelVars: List[IntVar]) = initModel

  def initModel: (List[IntVar], List[IntVar]) = {
    // Temporarily enable the new factorisation method that can factorise sums with divisions -- this is needed
    // for translation of negative powers to Choco divisions
    val originalFactorizationMode = NewFactorizationOfSum.enabled
    NewFactorizationOfSum.enabled = true

    /* 2. Create variables */
    val modelVars: List[IntVar] =
      paramSpace.dependentParameters.map {
        case enumVar: EnumeratedVar =>
          model.intVar(enumVar.name, enumVar.enumValues.toArray)

        case aeVar: Var =>
          val (min, max) = getMinMax(aeVar, paramSpace.independentParameters, logger)
          model.intVar(aeVar.name, min, max)

        }.toList ++
        paramSpace.independentParameters.map(param =>
          model.intVar(param._1.name, param._2.evalInt, param._2.evalInt))
    //    model.intVar(aeVar.name, aeVar.range.min.evalInt, aeVar.range.max.evalInt)).toList

    val dependentModelVars = modelVars.take(paramSpace.dependentParameters.length)

    /* 3. Post constraints */
    paramSpace.constraints.foreach(liftConstraint => {

      liftConstraint.predicate match {
        /* Arithmetic constraint on divisibility that uses MOD */
        case ChocoPredicateStr(arithmConstraintOp) if booleanOps.contains(toChocoOp(arithmConstraintOp)) &&
          liftConstraint.params.head.isInstanceOf[lift.arithmetic.Mod] &&
          liftConstraint.params(1).isInstanceOf[Cst] =>
          val lhsMod: lift.arithmetic.Mod = liftConstraint.params.head.asInstanceOf[lift.arithmetic.Mod]

          new Constraint("aMod", new org.chocosolver.solver.constraints.ternary.PropModXYZ(
            toChocoArExpr(lhsMod.dividend).intVar,
            toChocoArExpr(lhsMod.divisor).intVar,
            model.intVar(liftConstraint.params(1).asInstanceOf[Cst].c.toInt)
          )).post()

        /* Arithmetic constraint */
        case ChocoPredicateStr(arithmConstraintOp) if booleanOps.contains(toChocoOp(arithmConstraintOp)) =>
          model.arithm(
            toChocoArExpr(liftConstraint.params.head).intVar,
            toChocoOp(arithmConstraintOp),
            toChocoArExpr(liftConstraint.params(1)).intVar).post()


        /* Implication constraint (DEPRECATED: never used anymore) */
//          case Left(implicationConstraint) if implicationConstraint.startsWith("ifThen") =>
//
//            val ifThenPattern = new Regex(
//              """ifThen\(args\((\d+?)\) (.+?) (\d)\)\(args\((\d+?)\) (.+?) (\d)\)""",
//              "ifClauseArgId", "ifClauseOp", "ifClauseRhs",
//              "thenClauseArgId", "thenClauseOp", "thenClauseRhs")
//
//            ifThenPattern.findFirstMatchIn(implicationConstraint) match {
//              case Some(m) =>
//                model.ifThen(
//                  /*if*/ model.arithm(
//                    toChocoArExpr(liftConstraint.params(m.group("ifClauseArgId").toInt)).intVar,
//                    toChocoOp(m.group("ifClauseOp")),
//                    toChocoArExpr(Cst(m.group("ifClauseRhs").toInt)).intVar),
//                  /*then*/ model.arithm(
//                    toChocoArExpr(liftConstraint.params(m.group("thenClauseArgId").toInt)).intVar,
//                    toChocoOp(m.group("thenClauseOp")),
//                    toChocoArExpr(Cst(m.group("thenClauseRhs").toInt)).intVar))
//
//              case _ =>
//                throw new IllegalArgumentException(s"Cannot parse the ifThen implication constraint: $liftConstraint")
//            }

        /* Enumeration constraint (DEPRECATED: never used anymore) */
//          case Left(enumConstraint) if enumConstraint.startsWith("enum") =>
//
//            val enumPatternHead = new Regex("""enum\(args\((\d+?)\)""", "argId")
//            val enumPatternTail = new Regex("""(, args\((\d+?)\))+?""", "", "argId")
//
//            enumPatternHead.findFirstMatchIn(enumConstraint) match {
//              case Some(m) =>
//                var argIds: List[Int] = List(m.group("argId").toInt)
//                argIds ++= enumPatternTail.findAllMatchIn(enumConstraint).map(_.group("argId").toInt)
//
//                model.or(argIds.map(i =>
//                  model.arithm(
//                    toChocoArExpr(liftConstraint.params.head).intVar,
//                    "=",
//                    toChocoArExpr(liftConstraint.params(i)).intVar)
//                ): _*).post()
//
//              case _ =>
//                throw new IllegalArgumentException(s"Cannot parse the enumeration constraint: $liftConstraint")
//            }

        /* A custom Scala function that will post the constraint itself */
        case ChocoPredicate(constraintPoster: ((Model, List[ArithExpr]) => Unit)) =>
          constraintPoster(model, liftConstraint.params)

        case ConstraintDSLPredicate(predicate) =>
          ChocoConstraintSolver.postHighLevelConstraint( predicate(liftConstraint.params) )
      }
//      } catch {
//            // A workaround for the integer overflow problem where even if the expression result doesn't overflow, a subexpression does.
//            // Only set mightOverflow for constraints where ignoring the constraint will not break lead to invalid candidates
//        case _: NegativeArraySizeException if liftConstraint.mightOverflow =>
//          logger.warn(s"Constraint ${liftConstraint.name} threw NegativeArraySizeException. Because it's a sign of " +
//            s"an int overflow and the constraint is marked as mightOverflow, the constraint is IGNORED.")
//        case e: Throwable => throw e
//      }
    })

    // Reset the factorization mode since we won't need to translate any more arithmetic expressions
    NewFactorizationOfSum.enabled = originalFactorizationMode

    /* 4. Specify a search strategy */
    // randomSearch chooses a random root combination, then changes one variable randomly for each solution
//    model.getSolver.setSearch(Search.randomSearch(modelVars.toArray, seed))
//    model.getSolver.setLNS(new RandomNeighborhood(dependentModelVars.toArray, 3, seed))
    //
//    model.getSolver.setRestartOnSolutions() // not frequent enough, goes into infinite loop upon solving
//    model.getSolver.setLubyRestart(2, new FailCounter(model, 1000), Int.MaxValue)
//    model.getSolver.setSearch(new org.chocosolver.solver.search.strategy.strategy.FullyRandom(modelVars.toArray, 8)) // TODO seed
//    model.getSolver.setSearch(org.chocosolver.solver.search.strategy.Search.activityBasedSearch(modelVars:_*))


    (modelVars, dependentModelVars)
  }

  def excludeCombinationOfValues(paramValues: List[Int],
                                 reducedLogging: Boolean = false): Unit = {
    if (!reducedLogging) logger.info("Excluding the last combination from the future search")
    // During debugging, the manual scheme might specify all parameters as independent, which would make the "and" below fail on an empty list
    if (dependentModelVars.nonEmpty)
      model.not(
        model.and(
          dependentModelVars.zip(paramValues).map(p => model.arithm(p._1, "=", model.intVar(p._2))): _*
        )).post()
  }


  def getNextRandomPoint(enforceSolutionUniqueness: Boolean,
                         parameterTypeName: String,
                         solutionIdx: Option[Int] = None,
                         reducedLogging: Boolean = false,
                         returnIndependentParamArgsToo: Boolean = false): ListMap[Var, Cst] = {
    val seed = new Random(if (state.getSeeds.nonEmpty) state.getSeeds.last else initialSeed).nextInt()
    val idxStr = solutionIdx match { case Some(idx) => s"$idx. "; case None => ""}
    logger.info(idxStr + s"Generating random " + parameterTypeName + f" parameter value combination using seed=$seed.")

    // Reset the model completely before each solution for reproducibility
    model.getSolver.hardReset()
//    model.getSolver.getMove.removeStrategy()
    model.getSolver.setLubyRestart(2, new FailCounter(model, 1000), Int.MaxValue)
//    model.getSolver.setRestartOnSolutions
    model.getSolver.setSearch(Search.randomSearch(modelVars.toArray, seed))
    model.getSolver.setLNS(new RandomNeighborhood(dependentModelVars.toArray, 3, seed))

    // Debugging Choco not returning
    val tryWithOldSolverFirst = false
    if (tryWithOldSolverFirst) {
      val homemadeSolver = new HomemadeConstraintSolver(paramSpace, verbose = true)

      val result = homemadeSolver.getNextRandomPoint(new Random(seed), paramSpace.independentParameters)
      println(result)
    }

    // Update the solver state
    state.updateSeed(seed)

    /* 5. Solve the problem */
    if (model.getSolver.solve()) {
      if (!reducedLogging) logger.info("Combination found.")

      val dependentTuningArgs = ListMap(
        paramSpace.dependentParameters.zip(dependentModelVars.map(modelVar => Cst(modelVar.getValue))): _*)

      lastSolution = Some(dependentTuningArgs)

      if (enforceSolutionUniqueness)
        excludeCombinationOfValues(dependentModelVars.map(_.getValue), reducedLogging = reducedLogging)

      val argsToPrint = dependentTuningArgs.map(p => p._1.name + "=" + p._2.c.toString).mkString(", ")

      // Disabling the extra check because ArithExpr cannot detect common denominators that are pow(-1):
      // (1/^c) * (1/^d) * (a + a*b + b + 1) is divisible by (1 /^ d ) * (b + 1), but ArithExpr can't evaluate it so
//      var failedConstraint: Option[String] = None
//      if (!paramSpace.constraints.forall(liftConstraint => {
//        val isValid = liftConstraint.isValid(paramSpace.independentParameters ++ dependentTuningArgs)
//
//        if (!isValid)
//          failedConstraint = Some(liftConstraint.comment)
//        isValid
//      }))
//        throw new IllegalStateException(
//          s"The generated combination did not pass constraints in the Lift format.\n" +
//            s"Failed constraint:\n$failedConstraint\nCombination:\n" + argsToPrint)

      (if (returnIndependentParamArgsToo) paramSpace.independentParameters else ListMap()) ++
        dependentTuningArgs

    } else {
      val msg = f"Cannot find a valid combination of " + parameterTypeName + f" param values"
      logger.info(msg)
      exhausted = true

      // Debugging: try to get a combination through the homemade solver, where we can see which constraints failed
      val announceFailedConstraints: Boolean = false

      if (announceFailedConstraints && parameterTypeName.equals("tune")) {
        val homemadeSolver = new HomemadeConstraintSolver(paramSpace, verbose = true)

        val result = homemadeSolver.getNextRandomPoint(new Random(seed), paramSpace.independentParameters)
        println(result)
      }

      throw SolutionNotFound(msg)
    }
  }
}

object ChocoConstraintSolver {
  private val logger = Logger(this.getClass)

  def postHighLevelConstraint(re: RelationalExpr)(implicit model: Model): Unit = {

    re match {
      case IfThen(ifExpr, thenExpr) =>
        model.ifThen(
          convert(ifExpr).fold(r => r.decompose(), c => c),
          convert(thenExpr).fold(r => r.decompose(), c => c))

      case r: RelationalExpr with FirstClassCitizen =>
        convert(r).fold(r => r.post(), c => c.post())
    }

    /**
     * Sticks to ReExpression while possible, and switches to Constraint when it encounters ops that are not
     * supported in ReExpr (xor, count)
     */
    def convert(r: RelationalExpr with FirstClassCitizen)(implicit model: Model): Either[ReExpression, Constraint] =
      r match {
        case Count(value, vars, lb, ub) =>
          Right(model.count(
            toChocoArExpr(value).intVar(),
            vars.map(v => getChocoVar(v)).toArray,
            model.intVar(lb, ub)))

        case Eq(left, right)  => Left(toChocoArExpr(left) eq toChocoArExpr(right))
        case Neq(left, right) => Left(toChocoArExpr(left) ne toChocoArExpr(right))
        case Ge(left, right)  => Left(toChocoArExpr(left) ge toChocoArExpr(right))
        case Le(left, right)  => Left(toChocoArExpr(left) le toChocoArExpr(right))

        case Not(expr) =>
          convert(expr) match {
            case Left(reExpression) => Left(reExpression.not())
            case Right(constraint) => Right(model.not(constraint))
          }

        case varArityLogisticExpr: VarArityLogisticExpr =>

          val convertedExprs = varArityLogisticExpr.exprs.map(convert)

          val logisticReExprOp: (ReExpression, ReExpression) => ReExpression =
            varArityLogisticExpr match {
              case _: And => (a, b) => a and b
              case _: Or => (a, b) => a or b
              case _: Xor => (a, b) => a xor b
            }

          val logisticCstrOp: (Constraint, Constraint) => Constraint =
            varArityLogisticExpr match {
              case _: And => (a, b) => model.and(a, b)
              case _: Or => (a, b) => model.or(a, b)
              case _: Xor => (a, b) => model.and( model.or(a, b), model.not(model.and(a, b)) )
            }

          convertedExprs.tail.foldLeft [Either[ReExpression, Constraint]] (convertedExprs.head) {
            case (Left(acc: ReExpression), Left(next: ReExpression))  => Left(logisticReExprOp(acc, next))
            case (Left(acc: ReExpression), Right(next: Constraint))   => Right(logisticCstrOp(acc.decompose(), next))
            case (Right(acc: Constraint), Left(next: ReExpression))   => Right(logisticCstrOp(acc, next.decompose()))
            case (Right(acc: Constraint), Right(next: Constraint))    => Right(logisticCstrOp(acc, next))
          }
      }

  }

  def getChocoVar(aeVar: Var)(implicit model: Model): IntVar = {
    model.getVars.find(_.getName == aeVar.name) match {
      case Some(chocoVar) => chocoVar.asIntVar
      case None => throw new IllegalArgumentException(
        s"ArithExpr variable $aeVar is not in the list of known model variables: " +
          model.getVars.map(_.getName).mkString("[", ",", "]"))
    }
  }

  def toChocoArExpr(ae: ArithExpr)(implicit model: Model): ArExpression = {

    def powToChocoProd(base: ArExpression, exponent: ArithExpr): ArExpression = {
      assert(exponent.isInstanceOf[Cst])
      assert(exponent.evalInt > 0)
      exponent match {
        case Cst(1) => base
        case _ =>  base.mul(powToChocoProd(base, exponent - Cst(1)))
      }
    }

    /**
     * Special case for a * b^(-e) -> it has to be converted to division first,
     * otherwise Choco evaluates b^(-e) as 0 in integer expressions.
     * Converts (a * b^(-e)) to (a / b^e).
     * @return (numerator and denominator, if any)
     */
    def prodToChocoProdOrDiv(factors: List[ArithExpr with SimplifiedExpr]): (ArExpression, Option[ArExpression]) = {
      val (numerators, denomerators) = factors.partition {
        case Pow(_, e) if ArithExpr.isSmaller(e, 0).getOrElse(false) => false
        case _ => true
      }
      val numeratorProd: ArExpression = numerators.foldLeft[ArExpression](model.intVar(1)){
        case (arExp: ArExpression, arithExpr: ArithExpr) => arExp.mul(convert(arithExpr))
      }
      if (denomerators.isEmpty) (numeratorProd, None)
      else {
        val denomeratorPowers: List[Pow] = denomerators.map(_.asInstanceOf[Pow])
        val denomeratorPowerProd: ArExpression = denomeratorPowers.
          // Convert negative powers to positive
          map(p => powToChocoProd(convert(p.b), Cst(-1) * p.e)).
          // Construct the product of powers
          reduce[ArExpression]{ case (ae1: ArExpression, ae2: ArExpression) => ae1.mul(ae2) }

//        numeratorProd.div(denomeratorPowerProd)
        (numeratorProd, Some(denomeratorPowerProd))
      }
    }
    /**
     * Assumes that variables are integers when converting ceilings
     */
    def convert(ae: ArithExpr): ArExpression = {
      // Rebuild ArithExpr with identity visitor to reset lazy vals used for simplification since we might be using
      // different simplification mode compared to when the expressions were first built
      // Example: if we built an expression (a/c^-1 + b/c^-1) with NewFactorizationOfSum disabled, then here we won't
      // be able to simplify it to c^-1 * (a + b). To solve this, we enable the NewFactorizationOfSum before calling
      // this function and reevaluate the expression after "resetting" it here.
      ae.visitAndRebuild(e => e) match {
        // Ceiling simplification if possible
        case CeilingFunction(ae) if isInt(ae) => convert(ae)

        // ceil(a/^b) = a/b + min(1, a % b)
        case CeilingFunction(Prod(factors)) if factors.exists {
          case Pow(_, e) if ArithExpr.isSmaller(e, 0).getOrElse(false) => true
          case _ => false } =>

          prodToChocoProdOrDiv(factors) match {
            case (numerator, Some(denominator)) => numerator.div(denominator).add(numerator.mod(denominator).min(1))
            case _ => throw new IllegalStateException()
          }

        // ceil(x + n) where n is int = ceil(x) + n
        case CeilingFunction(Sum(terms)) if terms.exists(isInt) =>
          val (intTerms, nonIntTerms) = terms.partition(isInt)
          // try
          convert(CeilingFunction(nonIntTerms.reduce(_ + _)) + intTerms.reduce(_ + _))

        case Cst(c)                     => model.intVar(c.toInt)
        case v@Var(_, _)                => getChocoVar(v)
        case IntDiv(numer, denom)       => convert(numer).div(convert(denom))
        case Pow(b, e) if ArithExpr.isSmaller(e, 0).getOrElse(false)
        => throw new NotImplementedError("Negative power translation to Choco is " +
          "currently only supported in products")
        // Choco doesn't seem to simplify expressions like these:
        case Pow(b, e) if e == Cst(1)   => convert(b)
        case Pow(b, e)                  => powToChocoProd(convert(b), e)

        case Prod(factors)              => prodToChocoProdOrDiv(factors) match {
          case (numerator, None) => numerator
          case (numerator, Some(denominator)) => numerator.div(denominator)
        }

        case Sum(terms)                 => terms.map(convert(_)).reduce((t1, t2) => t1.add(t2))
        case Mod(dividend, divisor)     => convert(dividend).mod(convert(divisor))
        case AbsFunction(e)             => convert(e).abs()
        case e =>
          throw new NotImplementedError(
            s"Arithmetic expression $e is not supported in converting ArithExpr to Choco Solver expression")
      }
    }

    convert(ae)
  }

  def toChocoOp(liftOp: String): String = {
    liftOp match {
      case "==" => "="
      case op => op
    }
  }

  object SyntacticSugar {

    implicit class ReExprFactory(thisExpr: ArExpression) {
      def ===(other: ArExpression): ReExpression = thisExpr eq other
      def !==(other: ArExpression): ReExpression = thisExpr ne other
      def >=(other: ArExpression): ReExpression = thisExpr ge other
      def <=(other: ArExpression): ReExpression = thisExpr le other
      def +(other: ArExpression): ArExpression = thisExpr add other
      def *(other: ArExpression): ArExpression = thisExpr mul other
      def /(other: ArExpression): ArExpression = thisExpr div other
      def %(other: ArExpression): ArExpression = thisExpr mod other
    }

    implicit class ArithExprWithSugar(thisAE: ArithExpr)(implicit model: Model) {
      def ===(other: ArExpression): ReExpression = toChoco(thisAE) === other
      def !==(other: ArExpression): ReExpression = toChoco(thisAE) !== other
      def >=(other: ArExpression): ReExpression = toChoco(thisAE) >= other
      def <=(other: ArExpression): ReExpression = toChoco(thisAE) <= other
      def +(other: ArExpression): ArExpression = toChoco(thisAE) + other
      def *(other: ArExpression): ArExpression = toChoco(thisAE) * other
      def /(other: ArExpression): ArExpression = toChoco(thisAE) / other
      def %(other: ArExpression): ArExpression = toChoco(thisAE) % other
    }
    implicit class CArExprWithSugar(thisExpr: CArExpression) {
      def >=(other: CArExpression): CReExpression = thisExpr ge other
      def <=(other: CArExpression): CReExpression = thisExpr le other
      def +(other: CArExpression): CArExpression = thisExpr add other
      def *(other: CArExpression): CArExpression = thisExpr mul other
    }

    implicit class ReExprWithSugar(thisExpr: ReExpression) {
      def or(cstr: Constraint)(implicit model: Model): Constraint = model.or(thisExpr.decompose(), cstr)
    }

    implicit def toChoco(ae: ArithExpr)(implicit model: Model): ArExpression = toChocoArExpr(ae)

    implicit def intToIntVar(v: Int)(implicit model: Model): IntVar = model.intVar(v)
    implicit def arExpressionToIntVar(arExpr: ArExpression): IntVar = arExpr.intVar

    // Requires the Ibex solver library
//    def realIntView(iVar: IntVar, precision: Double)(implicit model: Model): RealVar = model.realIntView(iVar, precision)

    implicit def reExprToConstraint(reExpr: ReExpression): Constraint = reExpr.decompose()

    implicit def reExprListToConstraints(reExprs: List[ReExpression]): List[Constraint] = reExprs.map(reExprToConstraint)

    def not(expr: ReExpression)(implicit model: Model): ReExpression = expr.not()
    def not(cstr: Constraint)(implicit model: Model): Constraint = model.not(cstr)

    def and(cstrs: Constraint*)(implicit model: Model): Constraint = model.and(cstrs: _*)
    def and(exprs: ReExpression*)(implicit model: Model): ReExpression =
      if (exprs.isEmpty) model.trueConstraint().reify() else exprs.head.and(exprs.tail: _*)

    def or(cstrs: Constraint*)(implicit model: Model): Constraint = model.or(cstrs: _*)
    def or(exprs: ReExpression*)(implicit model: Model): ReExpression =
      if (exprs.isEmpty) model.trueConstraint().reify() else exprs.head.or(exprs.tail: _*)

    def xor(exprs: ReExpression*)(implicit model: Model): ReExpression =
      if (exprs.isEmpty) model.trueConstraint().reify() else exprs.head.xor(exprs.tail: _*)

    def ifThen(ifCstr: Constraint, thenCstr: Constraint)(implicit model: Model): Unit = model.ifThen(ifCstr, thenCstr)

    def count(value: IntVar, vars: Seq[IntVar], limit: IntVar)(implicit model: Model): Constraint =
      model.count(value, vars.toArray, limit)
  }

  case class SolverState(private val seeds: ListBuffer[Int]) {
    def updateSeed(seed: Int): Unit = {
      seeds.append(seed)
    }

    def getSeeds: ListBuffer[Int] = seeds
  }
}
package exploration.constraint_solvers

import com.typesafe.scalalogging.Logger
import exploration.ParameterRewrite
import lift.arithmetic.NotEvaluableToIntException.NotEvaluableToInt
import lift.arithmetic.{Cst, NotEvaluableException, StartFromRange, Var}

import scala.collection.immutable.ListMap

trait ConstraintSolver {
  def getSolverDebugStr: String

  def getNextRandomPoint(enforceSolutionUniqueness: Boolean,
                         parameterTypeName: String,
                         solutionIdx: Option[Int] = None,
                         reducedLogging: Boolean = false,
                         returnIndependentParamArgsToo: Boolean = false): ListMap[Var, Cst]
}

object ConstraintSolver extends Enumeration {
  type Library = Value
  val Choco, Z3 = Value

  case class SolutionNotFound(msg: String) extends Exception(msg)

  def getMinMax(v: Var,
                independentParameters: ListMap[Var, Cst],
                logger: Logger): (Int, Int) = {
    if (independentParameters.contains(v))
      (independentParameters(v).evalInt, independentParameters(v).evalInt)
    else
      try {
        v.range match {
          case StartFromRange(start) => (start.evalInt, Integer.MAX_VALUE - 1) // -1 is a hack to pass Choco range checks
          case _ =>
            val loweredVar = ParameterRewrite.substitute(v, independentParameters)
            (loweredVar.min.evalInt,
              if (loweredVar.max.evalLong <= scala.Int.MaxValue)
                loweredVar.max.evalInt
              else
                Integer.MAX_VALUE - 1) // -1 is a hack to pass Choco range checks
        }
      } catch {
        case e @ NotEvaluableException.NotEvaluable =>
          logger.error(s"Cannot determine min-max of variable $v")
          throw e
        case e @ NotEvaluableToInt =>
          val loweredVar = ParameterRewrite.substitute(v, independentParameters).asInstanceOf[Var]
          logger.error(s"Cannot determine min-max of variable $v with a range of ${loweredVar.range}")
          throw e
      }
  }
}

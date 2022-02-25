package exploration.constraint_solvers

import com.typesafe.scalalogging.Logger
import exploration.ParameterRewrite.substitute
import exploration.{ParamConstraint, ParameterSpace}
import exploration.ParameterSpace.EmptySpace
import lift.arithmetic.NotEvaluableToIntException.NotEvaluableToInt
import lift.arithmetic.{Cst, Var}
import rewriting.RewriteParamWithOrdinalValues.EnumeratedVar
import utils.RangeValueGenerator

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.Random

class HomemadeConstraintSolver(paramSpace: ParameterSpace,
                               verbose: Boolean = false) {

  private val logger = Logger(this.getClass)

  var timer: mutable.Map[String, Long] = mutable.Map()

  def time[R](cmd: String, block: => R): R = {
    //    val t0 = System.nanoTime()
    val result = block    // call-by-name
    //    val t1 = System.nanoTime()
    //    if (timer.contains(cmd))
    //      timer += (cmd -> (timer(cmd) + t1 - t0))
    //    else
    //      timer += (cmd -> (t1 - t0))
    result
  }

  var constraintCounters: collection.mutable.Map[ParamConstraint, Int] = collection.mutable.Map(
    paramSpace.constraints.map(constraint => constraint -> 0): _*)
  var constraintsChecked: collection.mutable.Map[ParamConstraint, Boolean] = collection.mutable.Map(
    paramSpace.constraints.map(constraint => constraint -> false): _*)

  /**
   * Gets the next random point (a combination of values of all parameters) that satisfies all constraints
   */
  def getNextRandomPoint(random: Random,
                         independentValues: ListMap[Var, Cst] = ListMap[Var, Cst]()): Vector[Cst] = {
    logger.info("Generating a random point.")
    logger.info(s"Independent parameter values: ${independentValues.map(pair => pair._1.name + " = " +
      pair._2.c.toString).mkString(", ")}")

    val sortedParamsOfThisSpace = time("sortedParamsOfThisSpace", paramSpace.dependentParameters.sortWith((v1, v2) =>
      paramSpace.paramsSortedByValidationComplexity.indexOf(v1) <=
        paramSpace.paramsSortedByValidationComplexity.indexOf(v2)).toList)

    constraintsChecked = collection.mutable.Map(
      paramSpace.constraints.map(constraint => constraint -> false): _*)

    pickParamValues(sortedParamsOfThisSpace, independentValues, random) match {

      case Some(combination) =>
        val result = paramSpace.dependentParameters.
          map(param => sortedParamsOfThisSpace.indexOf(param)).
          // Restore original sorting before returning values
          map(sortedParamIdx => combination.drop(independentValues.size)(sortedParamIdx))

        logger.info(s"Generated the following point: ${paramSpace.dependentParameters.zip(result).map(pair => pair._1.name + " = " +
          pair._2.c.toString).mkString(", ")}")

        result
      case None =>
        logger.debug(timer.map(pair => pair._1 -> pair._2.toFloat / 1000000).mkString("\n"))
        if (verbose) {
          logger.debug("constraintCounter: ")
          logger.debug(constraintCounters.map(pair => pair._1.name + " = " + pair._2.toString).mkString("\n"))
        }
        throw EmptySpace(f"Could not generate a value for parameter space ${paramSpace.dependentParameters} " +
          f"with the chosen independent parameter values")
    }
  }



  /**
   * Randomly picks parameter value for the first parameter in params that satisfies all the constraints with
   * respect to the values of the parameters in partialCombination. Uses recursive backtracking to pick values that
   * allow for successful validation of remaining parameters in params.
   * @param params List of parameters whose values still need generating
   * @param random Random generator
   * @return A combination or None if no combination satisfies the constraints
   */
  private def pickParamValues(params: List[Var],
                              generatedParams: ListMap[Var, Cst],
                              random: Random): Option[List[Cst]] = {
    params match {
      case Nil =>
        // No parameters to process -> the combination is completed
        if (constraintsChecked.filter(_._2 == false).map(constraint => {
          if (verbose) logger.debug(f"Checking constraint ${constraint._1.name}")
          time("remainingIsValid", {
            val isValid = constraint._1.isValid(generatedParams)

            if (!isValid)
              constraintCounters(constraint._1) += 1
            isValid
          })
        }).forall(v => v)) {
          logger.debug(timer.map(pair => pair._1 -> pair._2.toFloat / 1000000).mkString("\n"))
          if (verbose) {
            logger.debug("-----------------------------DONE-----------------------------")
            logger.debug("constraintCounter: ")
            logger.debug(constraintCounters.map(pair => pair._1.name + " = " + pair._2.toString).mkString("\n"))
          }

          Some(generatedParams.values.toList)
        }
        else {
          if (verbose) {
            logger.debug(f"(none with chosen combination based on constraints that no dependent parameters refer to)")
            logger.debug("constraintCounter: ")
            logger.debug(constraintCounters.toList.sortBy(_._1.name).map(pair => pair._1.name + " = " + pair._2.toString).mkString("\n"))
          }
          None
        }

      case param :: remainingParams =>

        // Generate a random value for the current parameter
        val generatedValues: collection.mutable.Set[Cst] = collection.mutable.Set()

        val loweredParamRange = time("loweredParamRange",
          param.range.visitAndRebuild(substitute(_, generatedParams)))

        val paramRangeSize = param match {
          case enumVar: EnumeratedVar => enumVar.enumValues.size
          case _ =>
            time("paramRangeSize", try {
              RangeValueGenerator.rangeSize(loweredParamRange)
            } catch {
              case e @ NotEvaluableToInt =>
                Integer.MAX_VALUE - 1
            })
        }

        val loweredConstraintsPerParam: ListMap[ParamConstraint, ParamConstraint] =
          time("buildLoweredConstraints", if (paramSpace.constraintsPerParam.contains(param))
            paramSpace.constraintsPerParam(param).foldLeft(ListMap[ParamConstraint, ParamConstraint]()) {
              case (acc, constraint) => acc + (constraint -> constraint.lower(generatedParams))
            }
          else ListMap())

        // In addition to the stopping condition below, the loop will exit upon return of a valid combination
        while (generatedValues.size < paramRangeSize) {
          val newValue = time("getNewValue",
            ParameterSpace.getRandomValueInRange(param, random, loweredParamRange, paramRangeSize))
          // This might result in infinite loop if incorrect range is passed to the random generator
          if (!time("generatedValues.contains(newValue)", generatedValues.contains(newValue))) {
            // The value has not been evaluated before. Let's check it against the constraints
            time("generatedValues.add(newValue)", generatedValues.add(newValue))

            //          println(s"Picking a value for ${param.name}: ${newValue.c}")
            // If there are constraints, check them. Otherwise, just try completing the combination
            // Make sure all the values required by this constraint have been generated. If this is not so,
            // something has gone wrong with parameter sorting.
            if (!paramSpace.constraintsPerParam.contains(param) || {
              val p = time("filterConstraints", paramSpace.constraintsPerParam(param).
                // Only consider constraints which refer to the parameters whose values we have already generated
                filter(constraint =>
                  constraint.variables.forall(constraintParam => constraintParam == param ||
                    generatedParams.contains(constraintParam))))
              time("checkAllConstraints", {

                val (debugConstraints, nonDebugConstraints) = p.partition(constraint => constraint.name.startsWith("debug"))

                if (debugConstraints.map(constraint => {
                  time("checkGenerated", constraint.variables.foreach(constraintParam =>
                    if (!generatedParams.contains(constraintParam) && constraintParam != param)
                      throw new IllegalStateException(
                        s"The parameter ${constraintParam.name} referenced by the constraint " +
                          s"$constraint is not in the partially generated combination which should have been ensured by " +
                          s"sorting the parameters by validation complexity. Check the parameter sorting algorithm in " +
                          s"Constraints.")))

                  // Check that the constraint is valid with the values generated previously or now
                  val t = time("isvalid", loweredConstraintsPerParam(constraint).isValid(Map(param -> newValue)))

                  constraintsChecked(constraint) = true
                  t
                }).forall(_ == true)) {

                  nonDebugConstraints.map(constraint => {
                    time("checkGenerated", constraint.variables.foreach(constraintParam =>
                      if (!generatedParams.contains(constraintParam) && constraintParam != param)
                        throw new IllegalStateException(
                          s"The parameter ${constraintParam.name} referenced by the constraint " +
                            s"$constraint is not in the partially generated combination which should have been ensured by " +
                            s"sorting the parameters by validation complexity. Check the parameter sorting algorithm in " +
                            s"Constraints.")))

                    // Check that the constraint is valid with the values generated previously or now
                    val t = time("isvalid", loweredConstraintsPerParam(constraint).isValid(Map(param -> newValue)))

                    constraintsChecked(constraint) = true

                    if (!t)
                      constraintCounters(constraint) += 1
                    t
                  })

                } else Seq(false)

              })}.forall(_ == true)) {
              if (verbose) {
                logger.debug(generatedParams.toString)
                logger.debug("constraintCounter: ")
                logger.debug(constraintCounters.toList.sortBy(_._1.name).map(pair =>
                  pair._1.name + " = " + pair._2.toString + "\n" + pair._1.comment).mkString("\n"))
              }
              // Pass on the updated combination and try to generate the rest of the values
              pickParamValues(
                remainingParams,
                generatedParams + (param -> newValue),
                random) match {
                case Some(finalCombination) => return Some(finalCombination)
                case None =>
                // We couldn't find values for the remaining parameters using the current value for the current parameter
                // Try another value in the next iteration of the loop
              }
            }
          }
        }
        // If we haven't returned by this point, no value passed checks
        if (verbose) {
          logger.debug(f"(none on param ${param.name})")
          logger.debug("constraintCounter: ")
          logger.debug(constraintCounters.toList.sortBy(_._1.name).map(pair => pair._1.name + " = " + pair._2.toString).mkString("\n"))
        }
        None
    }
  }

  def validatePoints(values: Map[Var, Cst]): Boolean = paramSpace.constraints.forall(_.isValid(values))
}

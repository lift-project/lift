package exploration

import _root_.utils.RangeValueGenerator
import lift.arithmetic.{Cst, Var}

import scala.util.Random
import scala.collection.immutable.ListMap
import ParameterRewrite.substituteVars
import com.typesafe.scalalogging.Logger

/**
  * All information defining parameter space
  *
  * @param parameters unique parameters defining the space
  * @param constraints rules declaring what parameter value combinations are valid
  */
class ParameterSpace(val parameters: Vector[Var],
                     val constraints: ParamConstraints) {

  private val logger = Logger(this.getClass)

  var constraintCounters: collection.mutable.Map[ParamConstraint, Int] = collection.mutable.Map(
    constraints.constraints.map(constraint => constraint -> 0): _*)

  /**
    * Gets the next random point (a combination of values of all parameters) that satisfies all constraints
    */
  def getNextRandomPoint(random: Random,
                         independentValues: ListMap[Var, Cst] = ListMap[Var, Cst]()): Vector[Cst] = {
    logger.info("Generating a random point.")
    logger.info(s"Independent parameter values: ${independentValues.map(pair => pair._1.name + " = " +
      pair._2.c.toString).mkString(", ")}")
    val sortedParamsOfThisSpace = parameters.sortWith((v1, v2) =>
      constraints.paramsSortedByValidationComplexity.indexOf(v1) <=
        constraints.paramsSortedByValidationComplexity.indexOf(v2)).toList

    pickParamValues(sortedParamsOfThisSpace,
      independentValues.values.toList,
      independentValues.keys.toList.zipWithIndex.map { case (p, i) => p -> i }.toMap,
      random, substitutionTable = independentValues) match {

      case Some(combination) =>
        val result = parameters.
          map(param => sortedParamsOfThisSpace.indexOf(param)).
          // Restore original sorting before returning values
          map(sortedParamIdx => combination.drop(independentValues.size)(sortedParamIdx))

        logger.info(s"Generated the following point: ${parameters.zip(result).map(pair => pair._1.name + " = " +
          pair._2.c.toString).mkString(", ")}")

        result
      case None =>
        logger.error(s"Could not generate a value for parameter space $parameters with the chosen independent " +
          s"parameter values")
        throw new IllegalArgumentException(f"Could not generate a value for parameter space $parameters")
    }

    // TODO
//    def toString(): String = f"ParameterSpace("
  }


  /**
    * Randomly picks parameter value for the first parameter in params that satisfies all the constraints with
    * respect to the values of the parameters in partialCombination. Uses recursive backtracking to pick values that
    * allow for successful validation of remaining parameters in params.
    * @param params List of parameters whose values still need generating
    * @param partialCombination List of values that are already generated
    * @param paramsInCombination A map associating processed parameters with their values in partialCombination
    * @param random Random generator
    * @return A combination or None if no combination satisfies the constraints
    */
  def pickParamValues(params: List[Var],
                      partialCombination: List[Cst],
                      paramsInCombination: Map[Var, Int],
                      random: Random,
                      substitutionTable: Map[Var, Cst]): Option[List[Cst]] = params match {
    case Nil =>
      // No parameters to process -> the combination is completed
//      println("-----------------------------DONE-----------------------------")
//      println("constraintCounter: ")
//      println(constraintCounters.map(pair => pair._1.name + " = " + pair._2.toString).mkString("\n"))
      Some(partialCombination)

    case param :: remainingParams =>
      // Generate a random value for the current parameter
      val generatedValues: collection.mutable.Set[Cst] = collection.mutable.Set()
      val rangeSize = RangeValueGenerator.rangeSize(param.range.visitAndRebuild(substituteVars(_, substitutionTable)))

      // In addition to the stopping condition below, the loop will exit upon return of a valid combination
      while (generatedValues.size < rangeSize) {
        val newValue = ParameterSpace.getRandomValueInRange(param, random, substitutionTable)
        // This might generated infinite loop if incorrect range is passed to the random generator
        if (!generatedValues.contains(newValue)) {
          // The value has not been evaluated before. Let's check it against the constraints
          generatedValues.add(newValue)

//          println(s"Picking a value for ${param.name}: ${newValue.c}")
          // If there are constraints, check them. Otherwise, just try completing the combination
          // Make sure all the values required by this constraint have been generated. If this is not so,
          // something has gone wrong with parameter sorting.
          if (!constraints.constraintsPerParam.contains(param) ||
            constraints.constraintsPerParam(param).
              // Only consider constraints which refer to the parameters whose values we have already generated
              filter(constraint =>
              constraint.params.forall(constraintParam => constraintParam == param ||
                paramsInCombination.contains(constraintParam))).
              forall(constraint => {
                constraint.params.foreach(constraintParam =>
                  if (!paramsInCombination.contains(constraintParam) && constraintParam != param)
                    throw new IllegalStateException(
                      s"The parameter ${constraintParam.name} referenced by the constraint " +
                        s"$constraint is not in the partially generated combination which should have been ensured by " +
                        s"sorting the parameters by validation complexity. Check the parameter sorting algorithm in " +
                        s"Constraints."))

                // Check that the constraint is valid with the values generated previously or now
                val t = constraint.isValid(constraint.params.map(constraintParam =>
                  if (constraintParam == param) // Current param
                    newValue
                  else // One of the process params
                    partialCombination(paramsInCombination(constraintParam))))

                if (!t)
                  constraintCounters(constraint) += 1
                t
              })) {
//            println((paramsInCombination + (param -> partialCombination.length)).toSeq.sortBy(_._2))
//            println(partialCombination :+ newValue)
            // Pass on the updated combination and try to generate the rest of the values
            pickParamValues(
              remainingParams,
              partialCombination :+ newValue, // new value
              paramsInCombination + (param -> partialCombination.length), // new index
              random, substitutionTable) match {
              case Some(finalCombination) => return Some(finalCombination)
              case None =>
              // We couldn't find values for the remaining parameters using the current value for the current parameter
              // Try another value in the next iteration of the loop
            }
          }
        }
      }
      // If we haven't return by this point, no value passed checks
//      println("(none)")
//      println("constraintCounter: ")
//      println(constraintCounters.map(pair => pair._1.name + " = " + pair._2.toString).mkString("\n"))
      None
  }
}

object ParameterSpace {
  def getRandomValueInRange(param: Var, random: Random, substitutionTable: Map[Var, Cst]): Cst = {
    val loweredRange = param.range.visitAndRebuild(substituteVars(_, substitutionTable))

    val randomIndexInRange = random.nextInt(RangeValueGenerator.rangeSize(loweredRange))

    RangeValueGenerator.generateSingleValue(loweredRange, randomIndexInRange)
  }
}

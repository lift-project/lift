package exploration

import _root_.utils.RangeValueGenerator
import lift.arithmetic.{Cst, Var}

import scala.util.Random

/**
  * All information defining parameter space
  *
  * @param parameters unique parameters defining the space
  * @param constraints the rules declaring what parameter value combinations are valid
  */
class ParameterSpace(val parameters: Vector[Var],
                     val constraints: ParamConstraints) {

  /** Gets the next random point (a combination of values of all parameters) that satisfies all constraints
    */
  def getNextRandomPoint(random: Random): Vector[Cst] = pickParamValues(
    constraints.paramsSortedByValidationComplexity,
    List[Int](),
    Map[Var, Int](),
    random) match {
    case Some(combination) =>
      parameters.
        map(param => constraints.paramsSortedByValidationComplexity.indexOf(param)).
        // Restore original sorting before returning values
        map(sortedParamIdx => combination(sortedParamIdx)).
        map(intVal => Cst(intVal))
    case None =>
      throw new IllegalArgumentException(f"Could not generate a value for parameter space $this")
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
                      partialCombination: List[Int],
                      paramsInCombination: Map[Var, Int],
                      random: Random): Option[List[Int]] = params match {
    case Nil =>
      // No parameters to process -> the combination is completed
      Some(partialCombination)

    case param :: remainingParams =>
      // Generate a random value for the current parameter
      val generatedValues: collection.mutable.Set[Int] = collection.mutable.Set()
      val rangeSize = RangeValueGenerator.rangeSize(param.range)

      // In addition to the stopping condition below, the loop will exit upon return of a valid combination
      while (generatedValues.size < rangeSize) {
        val newValue = ParameterSpace.getRandomValueInRange(param, random)
        // This might generated infinite loop if incorrect range is passed to the random generator
        if (!generatedValues.contains(newValue)) {
          // The value has not been evaluated before. Let's check it against the constraints
          generatedValues.add(newValue)

          // If there are constraints, check them. Otherwise, just try completing the combination
          if (!constraints.constraintsPerParam.contains(param) ||
            constraints.constraintsPerParam(param).forall(constraint => {
              // Make sure all the values required by this constraint have been generated. If this is not so,
              // something has gone wrong with parameter sorting.
              constraint.params.foreach(constraintParam =>
                if (!paramsInCombination.contains(constraintParam) && constraintParam != param)
                  throw new IllegalStateException(
                    s"The parameter ${constraintParam.name} referenced by the constraint " +
                    s"$constraint is not in the partially generated combination which should have been ensured by " +
                    s"sorting the parameters by validation complexity. Check the parameter sorting algorithm in " +
                    s"Constraints."))

              // Check that the constraint is valid with the values generated previously or now
              constraint.isValid(constraint.params.map(constraintParam =>
                if (constraintParam == param) // Current param
                  newValue
                else // One of the process params
                  partialCombination(paramsInCombination(constraintParam))))
            })) {
            // Pass on the updated combination and try to generate the rest of the values
            pickParamValues(
              remainingParams,
              partialCombination :+ newValue, // new value
              paramsInCombination + (param -> partialCombination.length), // new index
              random) match {
              case Some(finalCombination) => return Some(finalCombination)
              case None =>
              // We couldn't find values for the remaining parameters using the current value for the current parameter
              // Try another value in the next iteration of the loop
            }
          }
        }
      }
      // If we haven't return by this point, no value passed checks
      None
  }
}

object ParameterSpace {
  def getRandomValueInRange(param: Var, random: Random): Int = {
    val randomIndexInRange = random.nextInt(RangeValueGenerator.rangeSize(param.range))

    RangeValueGenerator.generateSingleValue(param.range, randomIndexInRange)
  }
}

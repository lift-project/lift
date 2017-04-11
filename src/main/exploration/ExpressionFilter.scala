package exploration

import analysis.MemoryAmounts
import com.typesafe.scalalogging.Logger
import ir.ast._
import lift.arithmetic.{ArithExpr, Cst}
import opencl.generator.OpenCLGenerator.NDRange

object ExpressionFilter {

  private val logger = Logger(this.getClass)

  object Status extends Enumeration {
    type Status = Value
    val Success,
    TooMuchGlobalMemory,
    TooMuchPrivateMemory,
    TooMuchLocalMemory,
    NotEnoughWorkItems,
    TooManyWorkItems,
    NotEnoughWorkGroups,
    TooManyWorkGroups,
    NotEnoughParallelism,
    InternalException = Value
  }

  import exploration.ExpressionFilter.Status._

  def apply(
    local: ArithExpr,
    global: ArithExpr,
    searchParameters: SearchParameters
  ): Status =
    filterNDRanges(
      (Array(local, 1, 1), Array(global, 1, 1)),
      searchParameters
    )

  def apply(
    local1: ArithExpr, local2: ArithExpr,
    global1: ArithExpr, global2: ArithExpr,
    searchParameters: SearchParameters
  ): Status =
    filterNDRanges(
      (Array(local1, local2, 1), Array(global1, global2, 1)),
      searchParameters
    )

  def apply(
    local1: ArithExpr, local2: ArithExpr, local3: ArithExpr,
    global1: ArithExpr, global2: ArithExpr, global3: ArithExpr,
    searchParameters: SearchParameters
  ): Status =
    filterNDRanges(
      (Array(local1, local2, local3), Array(global1, global2, global3)),
      searchParameters
    )

  def filterNDRanges(
    ranges: (NDRange, NDRange),
    searchParameters: SearchParameters
  ): Status = {
    val local = ranges._1
    val global = ranges._2

    try {
      // Rule out obviously poor choices based on the grid size
      // - minimum size of the entire compute grid
      if (global.map(_.eval).product < searchParameters.minGridSize) {
        logger.debug("Not enough work-items in the grid")
        return NotEnoughWorkItems
      }

      if (local.forall(_.isEvaluable)) {

        // - minimum of work-items in a workgroup
        if (local.map(_.eval).product < searchParameters.minWorkItems) {
          logger.debug("Not enough work-items in a group")
          return NotEnoughWorkItems
        }

        // - maximum of work-items in a workgroup
        if (local.map(_.eval).product > searchParameters.maxWorkItems) {
          logger.debug("Too many work-items in a group")
          return TooManyWorkItems
        }

        val numWorkgroups =
          (global.map(_.eval) zip local.map(_.eval)).map(x => x._1 / x._2).product

        // - minimum number of workgroups
        if (numWorkgroups < searchParameters.minWorkgroups) {
          logger.debug("Not enough work-groups")
          return NotEnoughWorkGroups
        }

        // - maximum number of workgroups
        if (numWorkgroups > searchParameters.maxWorkgroups){
          logger.debug("Too many work-groups")
          return TooManyWorkGroups
        }

      }
      // All good...
      Success

    } catch {
      case t: Throwable =>
        logger.warn("Failed filtering", t)
        InternalException
      // TODO: Internal exceptions sound suspicious. Log to file...
    }
  }

  def apply(
    lambda: Lambda, ranges: (NDRange, NDRange),
    searchParameters: SearchParameters = SearchParameters.createDefault
  ): Status = {
    val local = ranges._1
    val global = ranges._2

    try {

      val memoryAmounts = MemoryAmounts(lambda, local, global)

      val privateMemories = memoryAmounts.getPrivateMemories
      val localMemories = memoryAmounts.getLocalMemories
      val globalMemories = memoryAmounts.getGlobalMemories

      // Check private memory usage and overflow
      val privateAllocSize = privateMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval

      if (privateAllocSize > searchParameters.maxPrivateMemory ||
        privateMemories.exists(_.mem.size.eval <= 0)) {
        logger.debug("Too much private memory")
        return TooMuchPrivateMemory
      }

      // Check local memory usage and overflow
      val localAllocSize = localMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval

      if (localAllocSize > searchParameters.maxLocalMemory ||
        localMemories.exists(_.mem.size.eval <= 0)) {
        logger.debug("Too much local memory")
        return TooMuchLocalMemory
      }

      // Check global memory overflow
      if (globalMemories.exists(_.mem.size.eval <= 0)) {
        logger.debug("Too much global memory")
        return TooMuchGlobalMemory
      }

      // in case of global-local size exploration, we already checked these before
      if (ParameterRewrite.exploreNDRange.value.isEmpty)
        filterNDRanges(ranges, searchParameters)
      else
        Success

    } catch {
      case t: Throwable =>
        logger.warn("Failed filtering", t)
        InternalException
      // TODO: Internal exceptions sound suspicious. Log to file...
    }
  }
}


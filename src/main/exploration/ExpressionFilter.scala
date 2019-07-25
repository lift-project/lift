package exploration

import analysis.MemoryAmounts
import com.typesafe.scalalogging.Logger
import ir.ast._
import opencl.generator.NDRange
import lift.arithmetic.{ArithExpr, Cst}

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
      (NDRange(local, 1, 1), NDRange(global, 1, 1)),
      searchParameters
    )

  def apply(
    local1: ArithExpr, local2: ArithExpr,
    global1: ArithExpr, global2: ArithExpr,
    searchParameters: SearchParameters
  ): Status =
    filterNDRanges(
      (NDRange(local1, local2, 1), NDRange(global1, global2, 1)),
      searchParameters
    )

  def apply(
    local1: ArithExpr, local2: ArithExpr, local3: ArithExpr,
    global1: ArithExpr, global2: ArithExpr, global3: ArithExpr,
    searchParameters: SearchParameters
  ): Status =
    filterNDRanges(
      (NDRange(local1, local2, local3), NDRange(global1, global2, global3)),
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
      if (global.numberOfWorkItems < searchParameters.minGlobalSize) {
        logger.debug(s"Not enough work-items in the grid (${global.numberOfWorkItems} - ${local.toString} ${global.toString})")
        return NotEnoughWorkItems
      }

      if (local.forall(_.isEvaluable)) {

        // - minimum of work-items in a workgroup
        if (local.numberOfWorkItems < searchParameters.minLocalSize) {
          logger.debug(s"Not enough work-items in a group (${local.numberOfWorkItems} - ${local.toString} ${global.toString})")
          return NotEnoughWorkItems
        }

        // - maximum of work-items in a workgroup
        if (local.numberOfWorkItems > searchParameters.maxLocalSize) {
          logger.debug(s"Too many work-items in a group (${local.numberOfWorkItems} - ${local.toString} ${global.toString})")
          return TooManyWorkItems
        }

            val numWorkgroups =
              NDRange.numberOfWorkgroups(global, local)

        // - minimum number of workgroups
        if (numWorkgroups < searchParameters.minWorkgroups) {
          logger.debug(s"Not enough work-groups ($numWorkgroups - ${local.toString} ${global.toString})")
          return NotEnoughWorkGroups
        }

        // - maximum number of workgroups
        if (numWorkgroups > searchParameters.maxWorkgroups){
          logger.debug(s"Too many work-groups ($numWorkgroups - ${local.toString} ${global.toString})")
          return TooManyWorkGroups
        }

      }
      // All good...
      Success

    } catch {
      case t: Throwable =>
        logger.warn("Failed filtering NDRanges", t)
        InternalException
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
        logger.debug(s"Too much private memory ($privateAllocSize)")
        return TooMuchPrivateMemory
      }

      // Check local memory usage and overflow
      val localAllocSize = localMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval

      if (localAllocSize > searchParameters.maxLocalMemory ||
        localMemories.exists(_.mem.size.eval <= 0)) {
        logger.debug(s"Too much local memory ($localAllocSize)")
        return TooMuchLocalMemory
      }

      // Check global memory overflow
      if (globalMemories.exists(_.mem.size.eval <= 0)) {
        logger.debug("Too much global memory")
        return TooMuchGlobalMemory
      }

      // in case of global-local size exploration, we already checked these before
      if (!SplitSlideRewrite.settings.parameterRewriteSettings.exploreNDRange)
        filterNDRanges(ranges, searchParameters)
      else
        Success

    } catch {
      case t: Throwable =>
        logger.warn("Failed filtering Expression", t)
        InternalException
    }
  }
}


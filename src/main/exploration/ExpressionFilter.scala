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

  def apply(local: ArithExpr, global: ArithExpr): Status = {
    filterNDRanges((NDRange(local, 1, 1), NDRange(global, 1, 1)))
  }

  def apply(local1: ArithExpr, local2: ArithExpr,
            global1: ArithExpr, global2: ArithExpr): Status = {
    filterNDRanges((NDRange(local1, local2, 1), NDRange(global1, global2, 1)))
  }

  def apply(local1: ArithExpr, local2: ArithExpr, local3: ArithExpr,
            global1: ArithExpr, global2: ArithExpr, global3: ArithExpr): Status = {
    filterNDRanges((NDRange(local1, local2, local3), NDRange(global1, global2, global3)))
  }
  def filterNDRanges(ranges: (NDRange, NDRange)): Status = {
    ranges match {
      case (local, global) =>
        try {
          // Rule out obviously poor choices based on the grid size
          // - minimum size of the entire compute grid
          if (global.numberOfWorkItems < SearchParameters.min_grid_size) {
            logger.debug("not enough work-items")
            return NotEnoughWorkItems
          }

          if (local.forall(_.isEvaluable)) {

            // - minimum of work-items in a workgroup
            if (local.numberOfWorkItems < SearchParameters.min_work_items) {
              logger.debug("not enough work-items (2)")
              return NotEnoughWorkItems
            }

            // - maximum of work-items in a workgroup
            if (local.numberOfWorkItems > 1024) {
              logger.debug("too many work-items")
              return TooManyWorkItems
            }

            val numWorkgroups = NDRange.numberOfWorkgroups(global, local)

            // - minimum number of workgroups
            if (numWorkgroups < SearchParameters.min_num_workgroups) {
              logger.debug("not enough work-groups")
              return NotEnoughWorkGroups
            }

            // - maximum number of workgroups
            if (numWorkgroups > SearchParameters.max_num_workgroups) {
              logger.debug("too many work-groups")
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
  }

  def apply(lambda: Lambda, ranges: (NDRange, NDRange)): Status = {
    ranges match {
      case (local, global) =>
        try {

          val memoryAmounts = MemoryAmounts(lambda, local, global)

          val privateMemories = memoryAmounts.getPrivateMemories
          val localMemories = memoryAmounts.getLocalMemories
          val globalMemories = memoryAmounts.getGlobalMemories

          // Check private memory usage and overflow
          val privateAllocSize = privateMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval
          if (privateAllocSize > SearchParameters.max_amount_private_memory ||
            privateMemories.exists(_.mem.size.eval <= 0)) {
            logger.debug("too much private memory")
            return TooMuchPrivateMemory
          }

          // Check local memory usage and overflow
          val localAllocSize = localMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval

          if (localAllocSize > 50000 || localMemories.exists(_.mem.size.eval <= 0)) {
            logger.debug("too much local memory")
            return TooMuchLocalMemory
          }

          // Check global memory overflow
          if (globalMemories.exists(_.mem.size.eval <= 0)) {
            logger.debug("too much global memory")
            return TooMuchGlobalMemory
          }

          // in case of global-local size exploration, we already checked these before
          if (!ParameterRewrite.exploreNDRange.value.isDefined)
            filterNDRanges(ranges)
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
}


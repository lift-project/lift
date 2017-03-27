package exploration

import analysis.MemoryAmounts
import lift.arithmetic.Cst
import com.typesafe.scalalogging.Logger
import ir.ast._
import opencl.generator.NDRange
import rewriting.InferNDRange

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

  def apply(lambda: Lambda): Status = {

    try {

      // Compute NDRange based on the parameters
      val (local, global) = InferNDRange(lambda)
      val memoryAmounts = MemoryAmounts(lambda, local, global)

      val privateMemories = memoryAmounts.getPrivateMemories
      val localMemories = memoryAmounts.getLocalMemories
      val globalMemories = memoryAmounts.getGlobalMemories

      // Check private memory usage and overflow
      val privateAllocSize = privateMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval
      if (privateAllocSize > SearchParameters.max_amount_private_memory ||
        privateMemories.exists(_.mem.size.eval <= 0)) {
        return TooMuchPrivateMemory
      }

      // Check local memory usage and overflow
      val localAllocSize = localMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval

      if (localAllocSize > 50000 || localMemories.exists(_.mem.size.eval <= 0))
        return TooMuchLocalMemory

      // Check global memory overflow
      if (globalMemories.exists(_.mem.size.eval <= 0))
        return TooMuchGlobalMemory

      // Rule out obviously poor choices based on the grid size
      // - minimum size of the entire compute grid
      if (global.numberOfWorkItems < SearchParameters.min_grid_size)
        return NotEnoughWorkItems

      if (local.forall(_.isEvaluable)) {

        // - minimum of work-items in a workgroup
        if (local.numberOfWorkItems < SearchParameters.min_work_items)
          return NotEnoughWorkItems

        // - maximum of work-items in a workgroup
        if (local.numberOfWorkItems > 1024)
          return TooManyWorkItems

        val numWorkgroups = NDRange.numberOfWorkgroups(global, local)

        // - minimum number of workgroups
        if (numWorkgroups < SearchParameters.min_num_workgroups)
          return NotEnoughWorkGroups

        // - maximum number of workgroups
        if (numWorkgroups > SearchParameters.max_num_workgroups)
          return TooManyWorkGroups

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


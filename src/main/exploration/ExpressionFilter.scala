package exploration

import apart.arithmetic.Cst
import rewriting.InferNDRange
import ir.{Memory, ScalarType}
import ir.ast._
import ir.view.View
import opencl.generator.{RangesAndCounts, OpenCLCodeGen}
import opencl.ir._

object ExpressionFilter {
  object Status extends Enumeration {
    type Status = Value
    val Success,
    TooMuchPrivateMemory,
    TooMuchLocalMemory,
    NotEnoughWorkItems,
    TooManyWorkItems,
    NotEnoughWorkGroups,
    TooManyWorkGroups,
    NotEnoughParallelism,
    InternalException = Value
  }

  import ExpressionFilter.Status._

  def apply(expr: Lambda): Status = {
    try {
      // Compute NDRange based on the parameters
      val (local, global) = InferNDRange(expr)
      val valueMap = ParameterRewrite.createValueMap(expr)

      // Allocate memory
      expr.params.foreach((p) => {
        p.t match {
          case _: ScalarType =>
            p.mem = OpenCLMemory.allocPrivateMemory(
              OpenCLMemory.getMaxSizeInBytes(p.t))
          case _ =>
            p.mem = OpenCLMemory.allocGlobalMemory(
              OpenCLMemory.getMaxSizeInBytes(p.t))
        }
        p.view = View(p.t, OpenCLCodeGen().toString(p.mem.variable))
      })

      RangesAndCounts(expr, local, global,valueMap)
      OpenCLMemoryAllocator.alloc(expr.body)

      // Get the allocated buffers
      val kernelMemory = TypedOpenCLMemory.get(expr.body, expr.params)
      val buffers = TypedOpenCLMemory.get(expr.body, expr.params, includePrivate = true)

      val valueMemories =
        Expr.visitWithState(Set[Memory]())(expr.body, (expr, set) =>
          expr match {
            case value: ir.ast.Value => set + value.mem
            case _ => set
          })

      val (_, privateMemories) =
        buffers.diff(kernelMemory).partition(m => valueMemories.contains(m.mem))

      // Check private memory usage and overflow
      val private_alloc_size = privateMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval
      if (private_alloc_size > SearchParameters.max_amount_private_memory ||
            privateMemories.exists(_.mem.size.eval <= 0)) {
        return TooMuchPrivateMemory
      }

      // Check local memory usage and overflow
      val localMemories = buffers.filter(_.mem.addressSpace == LocalMemory)
      val local_alloc_size = localMemories.map(_.mem.size).fold(Cst(0))(_ + _).eval

      if (local_alloc_size > 50000 || localMemories.exists(_.mem.size.eval <= 0))
        return TooMuchLocalMemory

      // Rule out obviously poor choices based on the grid size
      // - minimum size of the entire compute grid
      if (global.map(_.eval).product < SearchParameters.min_grid_size)
        return NotEnoughWorkItems

      if (local.forall(_.isEvaluable)) {

        // - minimum of work-items in a workgroup
        if (local.map(_.eval).product < SearchParameters.min_work_items)
          return NotEnoughWorkItems

        // - maximum of work-items in a workgroup
        if (local.map(_.eval).product > 1024)
          return TooManyWorkItems

        val numWorkgroups =
          (global.map(_.eval) zip local.map(_.eval)).map(x => x._1 / x._2).product

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
      case _: Throwable => InternalException
      // TODO: Internal exceptions sound suspicious. Log to file...
    }
  }
}


package gcoSearch
import exploration.InferNDRange
import ir.ast.Lambda
import opencl.executor.{Execute, Executor}
import opencl.ir._

object ExecutionHarness {
  object ErrorCounter {
    var local_mem = 0
    var priv_mem = 0
    var not_enough_wi = 0
    var not_enough_wg = 0
    var not_enough_rpt = 0
  }

  object Status extends Enumeration {
    type Status = Value
    val Success, Skipped, Avoided, ValidationError, ArithmeticsError, ExecutorError, UnknwownError = Value
  }
}

/** Quick execution harness to restart the executor on failure. */
class ExecutionHarness(gold: Array[Float]) {

  import ExecutionHarness.Status._

  // Function called on successful execution and cross validation
  def success(time: Double): (Status, Double) = (Success, time)

  // Error callback
  def failure(reason: Status): (Status, Double) = (reason, 0.0)

  // run the given lambda with the given dimensions and parameters
  def apply(cur_best: Double, expr: Lambda, values: Any*): (Status, Double) = {
    try {
      val (local, global) = InferNDRange(expr, values: _*)

      if (AppParams.print_stats) {
        val num_workgroups = (global.map(_.eval) zip local.map(_.eval)).map(x => x._1 / x._2).product

        val buffers = TypedOpenCLMemory.get(expr.body, expr.params, includePrivate = true)

        // filter private memory
        val private_buffers_size = buffers.filter(_.mem.addressSpace == PrivateMemory)
        val private_alloc_size = private_buffers_size.map(_.mem.size).reduce(_ + _).eval

        // filter local memory
        val local_buffers_size = buffers.filter(_.mem.addressSpace == LocalMemory)
        val local_alloc_size =
          if (local_buffers_size.nonEmpty) local_buffers_size.map(_.mem.size).reduce(_ + _).eval
          else 0

        println()
        println("Current run:")
        println("- local variables: " + private_buffers_size.map(x => OpenCLMemory.getMaxSizeInBytes(x.t)).reduce(_ + _).eval)
        println("- local size: " + local.map(_.eval).mkString(", "))
        println("- global size: " + global.map(_.eval).mkString(", "))
        println("- work item count: " + local.map(_.eval).product)
        println("- work group count: " + num_workgroups)
        println("- local allocation: " + local_alloc_size)
        println("- private allocation: " + private_alloc_size)
      }

      println(expr)

      // === Execution ===
      val (output: Array[Float], time) =
        Execute(local(0).eval, local(1).eval, global(0).eval, global(1).eval, (true, true)).evaluate(10, cur_best * 1.4f, expr, values: _*)

      if (time < 0) return failure(Avoided)

      // cross validation
      if (!AppParams.only_crossvalidate_better_solutions ||
        (AppParams.only_crossvalidate_better_solutions && time < cur_best)) {
        if (output.length != gold.length)
          failure(ValidationError)
        else {
          val passed = (output zip gold).forall(x => Math.abs(x._1 - x._2) < 0.001f * Math.max(Math.abs(x._1), Math.abs(x._2)))
          if (passed) success(time)
          else {
            val mismatch = (output zip gold).collect {
              case x if Math.abs(x._1 - x._2) >= 0.01f * Math.max(Math.abs(x._1), Math.abs(x._2)) => x
            }.toList
            println("Error: " + mismatch.size + " / " + gold.size + " mismatch")
            println("Avg Error: " + mismatch.map(x => Math.max(Math.abs(x._1), Math.abs(x._2)) / Math.min(Math.abs(x._1), Math.abs(x._2)) - 1).reduce(_ + _) / mismatch.size * 100.0f)
            println("Local size: " + local.mkString(", "))
            println("Global size: " + global.mkString(", "))
            println("Execution time: " + time)
            failure(ValidationError)
          }
        }
      } else success(time)
    } catch {
      case ea: Executor.ExecutorFailureException =>
        //ea.printStackTrace()
        println(expr)
        ea.consume()
        failure(ExecutorError)

      case e: Exception =>
        //e.printStackTrace()
        failure(ExecutorError)

      case e: Throwable =>
        //println(e)
        failure(ExecutorError)
    }
  }
}

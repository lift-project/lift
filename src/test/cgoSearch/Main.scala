package cgoSearch

import java.nio.file.{Files, Paths}

import apart.arithmetic.{ArithExpr, Cst}
import exploration.TestLowLevelRewrite
import ir.TypeChecker
import opencl.executor.{Eval, Executor}

import scala.collection.immutable.Map
import scala.io.Source

/**
 * This main currently runs a parameter space exploration over the serialized low level expressions.
 * It requires the lambdas/ and lowered/ folders with the generated index files.
 *
 * This version passes a transposed matrix to the kernel.
 */
object Main {
  def main(args: Array[String]) {
    Executor.loadLibrary()
    Executor.init()

    // Various counters
    var passed = 0
    var skipped = 0
    var avoided = 0
    var failed = 0
    var crashed = 0
    var best_time = Double.PositiveInfinity
    var all_times: List[Double] = List.empty
    var best_substitutions = Map[ArithExpr, ArithExpr]()

    // Prepare the input
    val mSize = SearchParameters.matrix_size
    val kSize = SearchParameters.matrix_size
    val nSize = SearchParameters.matrix_size

    println("Generating data")
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    // Kernel parameters
    val values = Seq(matrixA.transpose, matrixB)

    println("Computing gold solution")
    val gold = Executor.nativeMatrixMultiply(matrixA.flatten, matrixB.flatten, mSize, nSize, kSize)

    // create the execution engine
    val executor = new ExecutionHarness(gold)

    // list all the high level expression
    val all_files = Source.fromFile("lambdas/index").getLines().toList

    var expr_counter = 0
    List(all_files(24)).toList.foreach(filename => {
      var failure_guard = 0

      if (Files.exists(Paths.get(filename))) {
        expr_counter = expr_counter + 1
        println(s"Expression : $expr_counter / ${all_files.size}")

        val high_level_str = Source.fromFile(filename).getLines().mkString("\n").replace("idfloat", "id")
        val high_level_expr = Eval(high_level_str)
        val high_level_hash = filename.split("/").last

        TestLowLevelRewrite.replaceInputTypes(high_level_expr)

        TypeChecker(high_level_expr)

        val all_substitution_tables = ParameterSearch(high_level_expr)
        println(s"Found ${all_substitution_tables.size} valid parameter sets")


        if (Files.exists(Paths.get(s"lower/$high_level_hash/index"))
          && all_substitution_tables.size < 100000 && failure_guard < 10) {
          val low_level_expr_list = Source.fromFile(s"lower/$high_level_hash/index").getLines().toList

          val low_level_hash = filename.split("/").last
          var low_level_counter = 0
          println(s"Trying $low_level_hash from $high_level_hash")
          println(s"Found ${low_level_expr_list.size} low level expressions")

          low_level_expr_list.foreach(low_level_filename => {
            var counter = 0

            low_level_counter = low_level_counter + 1
            val low_level_str = Source.fromFile(low_level_filename).getLines().mkString("\n").replace("idfloat", "id")
            val low_level_factory = Eval.getMethod(low_level_str)

            println("Propagating parameters...")
            val potential_expressions = all_substitution_tables.par.map(st => {
              val params = st.map(a => a).toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
              try {
                val expr = low_level_factory(
                  Seq(Cst(SearchParameters.matrix_size),
                      Cst(SearchParameters.matrix_size),
                      Cst(SearchParameters.matrix_size)) ++ params)
                TypeChecker(expr)
                if(ExpressionFilter(expr) == ExpressionFilter.Status.Success)
                  Some((expr, st))
                else
                  None
                } catch {
                   case x => 
                   x.printStackTrace()
                   println(low_level_hash)
                   println(params.mkString("; "))
                   println(low_level_str)
                   println(SearchParameters.matrix_size)
                   System.exit(-1)
                   None
                }
            }).collect{ case Some(x) => x }.toList

            println(s"Found ${potential_expressions.size} / ${all_substitution_tables.size} filtered expressions")

            potential_expressions.foreach(expr => {
              if (failure_guard < 10) {
                //println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
                val (test, time) = executor(Math.min(best_time, 1000f), expr._1, values: _*)

                import ExecutionHarness.Status._
                test match {
                  case Success =>
                    passed = passed + 1
                    all_times = time :: all_times
                    if (time < best_time) {
                      best_time = time
                      best_substitutions = expr._2
                      println()
                      println(expr._1)
                    }

                  case Skipped =>
                    skipped = skipped + 1

                  case ValidationError =>
                    println()
                    println(expr._2.map(x => s"${x._1} -> ${x._2}").mkString("; "))
                    println(expr._1)
                    failed = failed + 1
                    failure_guard = failure_guard + 1

                  case Avoided =>
                    avoided = avoided + 1

                  case _ =>
                    println()
                    println(expr._2.map(x => s"${x._1} -> ${x._2}").mkString("; "))
                    println(expr._1)
                    crashed = crashed + 1
                }
                print(s"\r$expr_counter / ${all_files.size}; $low_level_counter / ${low_level_expr_list.size}; $counter / ${potential_expressions.size} " +
                  s"($passed passed, $skipped skipped, $avoided avoided, $failed failed, $crashed crashed) best = $best_time                   ")


              }
              counter = counter + 1
            })
          })
        }
      }
    })
  }
}


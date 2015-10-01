package exploration

import java.nio.file.{Files, Paths}

import apart.arithmetic.{ArithExpr, Cst, Var}
import exploration.TestLowLevelRewrite.ExecutionHarness
import cgoSearch.SearchParameters
import exploration.utils.Utils
import ir.ast.{FunCall, Split}
import ir.{ArrayType, TypeChecker}
import opencl.executor.{Eval, Executor}

import scala.collection.immutable.{Map => ScalaImmMap}
import scala.io.Source

object LowLevelExecutor {
  def main(args: Array[String]) {
    Executor.loadLibrary()
    Executor.init()

    // Step whatever: run everything

    var passed = 0
    var skipped = 0
    var avoided = 0
    var failed = 0
    var crashed = 0
    var best_time = Double.PositiveInfinity
    var all_times: List[Double] = List.empty
    var best_substitutions = Map[ArithExpr, ArithExpr]()

    val mSize = SearchParameters.matrix_size
    val kSize = SearchParameters.matrix_size
    val nSize = SearchParameters.matrix_size
    println("Generating data")
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    println("Computing gold solution")
    val gold = Executor.nativeMatrixMultiply(
      matrixA.flatten, matrixB.flatten, mSize, nSize, kSize)

    val executor = new ExecutionHarness(gold)
    val values = Seq(matrixA.transpose, matrixB)

    // list all the high level expression
    val all_files = Source.fromFile("lambdas/index").getLines().toList

    var expr_counter = 0
    util.Random.shuffle(all_files.toList).foreach(filename => {
        var failure_guard = 0

        if (Files.exists(Paths.get(filename))) {
          expr_counter = expr_counter + 1
          println(s"Expression : $expr_counter / ${all_files.size}")

          val high_level_str = Source.fromFile(filename).getLines().mkString("\n").replace("idfloat", "id")
          val high_level_expr = Eval(high_level_str)
          val high_level_hash = filename.split("/").last

          TestLowLevelRewrite.replaceInputTypes(high_level_expr)

          TypeChecker(high_level_expr)

          val tunableNodes = Utils.findTunableNodes(high_level_expr)

          val splits = tunableNodes.collect { case f@FunCall(Split(cs), x) => (cs, x.t.asInstanceOf[ArrayType].len) }

          def propagate(splits: List[(ArithExpr, ArithExpr)], m: ScalaImmMap[ArithExpr, ArithExpr]): List[(ArithExpr, ArithExpr)] = {
            splits.map((x) => (ArithExpr.substitute(x._1, m), ArithExpr.substitute(x._2, m)))
          }

          var all_substitution_tables: List[ScalaImmMap[ArithExpr, ArithExpr]] = List.empty

          println("Building substitution tables")

          // recursively build the substitution table.
          // It takes the first node to tune and recurse with all its possible values.
          def substitute(splits: List[(ArithExpr, ArithExpr)], substitutions: ScalaImmMap[ArithExpr, ArithExpr]): Unit = {

            if (splits.nonEmpty) {
              splits.head match {
                // If the stride is not set and the input length is constant, compute all divisors
                case (v: Var, Cst(len)) =>
                  (2 to len - 1).filter {
                    len % _ == 0
                  }.foreach(x => substitute(propagate(splits.tail, ScalaImmMap(v -> x)), substitutions + (v -> x)))

                // If the input AND the stride are already set, make sure they are multiple
                case (Cst(chunk), Cst(len)) if len % chunk == 0 =>
                  substitute(splits.tail, substitutions)

                // Otherwise we cannot set the parameter or the current combination is invalid
                case (x, y) => println(s"failed: $x, $y")
              }
            }
            else // if we propagated all the nodes, we have a (hopefully valid) substitution table
              all_substitution_tables = substitutions :: all_substitution_tables
          }

          // compute all the valid substitution tables
          substitute(splits, ScalaImmMap.empty)
          println(s"Found ${all_substitution_tables.size} valid parameter sets")


          if (Files.exists(Paths.get(s"lower/${high_level_hash}/index")) 
            && all_substitution_tables.size < 100000 && failure_guard < 10) {
            val low_level_expr_list = Source.fromFile(s"lower/${high_level_hash}/index").getLines().toList

            val low_level_hash = filename.split("/").last
            var low_level_counter = 0
            println(s"Trying $low_level_hash from $high_level_hash")

            low_level_expr_list.foreach(low_level_filename => {
              var counter = 0

              low_level_counter = low_level_counter + 1
              val low_level_str = Source.fromFile(low_level_filename).getLines().mkString("\n").replace("idfloat", "id")
              val low_level_factory = Eval.getMethod(low_level_str)

              all_substitution_tables.foreach(st => {
                if(failure_guard < 10) {
                  val params = st.map(a => a).toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
                  val expr = low_level_factory(Seq(Cst(512), Cst(512), Cst(512)) ++ params)
                  TypeChecker(expr)

                  //println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
                  val (test, time) = executor(Math.min(best_time, 1000f), expr, values: _*)

                  import ExecutionHarness.Status._
                  test match {
                    case Success =>
                      passed = passed + 1
                      all_times = time :: all_times
                      if (time < best_time) {
                        best_time = time
                        best_substitutions = st
                      }

                    case Skipped =>
                      skipped = skipped + 1

                    case ValidationError =>
                      println()
                      println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
                      println(expr)
                      failed = failed + 1
                      failure_guard = failure_guard + 1

                    case Avoided =>
                      avoided = avoided + 1

                    case _ =>
                      println()
                      println(st.map(x => s"${x._1} -> ${x._2}").mkString("; "))
                      println(expr)
                      crashed = crashed + 1
                  }
                  print(s"$expr_counter / ${all_files.size}; $low_level_counter / ${low_level_expr_list.size}; $counter / ${all_substitution_tables.size} "+
                    s"($passed passed, $skipped skipped, $avoided avoided, $failed failed, $crashed crashed) best = $best_time                   ")
                  println(s"local_mem = ${Counter.local_mem}, priv_mem = ${Counter.priv_mem}, not_enough_wi = ${Counter.not_enough_wi}, not_enough_wg = ${Counter.not_enough_wg}, not_enough_rpt = ${Counter.not_enough_rpt}")
  


                }
                counter = counter + 1
              })
            })
          }
        }
    })
  }
}

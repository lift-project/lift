package cgoSearch

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import apart.arithmetic.{ArithExpr, Var}
import exploration.utils.Utils
import ir.ast.Lambda
import ir.{Type, TypeChecker}
import opencl.executor.Eval

import scala.collection.immutable.Map
import scala.io.Source

/**
 * This main currently runs a parameter space exploration over the serialized low level expressions.
 * It requires the lambdas/ and lambdasLower/ folders with the generated index files.
 */
object GenerateOpenCL {
  var topFolder = "lambdas"

  def main(args: Array[String]) {

    if (args.nonEmpty)
      topFolder = args.head

    // list all the high level expression
    val all_files = Source.fromFile(s"$topFolder/index").getLines().toList
    val highLevelCount = all_files.size

    var expr_counter = 0
    all_files.foreach(filename => {

      if (Files.exists(Paths.get(filename))) {
        val high_level_hash = filename.split("/").last
        expr_counter = expr_counter + 1
        println(s"High-level expression : $expr_counter / $highLevelCount")

        val high_level_expr_orig = readLambdaFromFile(filename)

        val st = createValueMap(high_level_expr_orig)
        val sizesForFilter = st.values.toSeq
        val vars = Seq.fill(sizesForFilter.length)(Var(""))

        val high_level_expr = replaceInputTypes(high_level_expr_orig, st)

        TypeChecker(high_level_expr)

        val all_substitution_tables = ParameterSearch(high_level_expr)
        val substitutionCount = all_substitution_tables.size
        println(s"Found $substitutionCount valid parameter sets")

        val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
        if (Files.exists(Paths.get(loweredIndex))
          && substitutionCount < 800000 && substitutionCount != 0) {

          val low_level_expr_list = Source.fromFile(loweredIndex).getLines().toList

          val low_level_counter = new AtomicInteger()
          val lowLevelCount = low_level_expr_list.size
          println(s"Found $lowLevelCount low level expressions")

          low_level_expr_list.par.foreach(low_level_filename => {

            try {

              val low_level_hash = low_level_filename.split("/").last
              val low_level_str = readFromFile(low_level_filename)
              val low_level_factory = Eval.getMethod(low_level_str)

              println(s"Low-level expression ${low_level_counter.incrementAndGet()} / $lowLevelCount")

              println("Propagating parameters...")
              val potential_expressions = all_substitution_tables.map(st => {
                val params = st.map(a => a).toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
                try {
                  val expr = low_level_factory(sizesForFilter ++ params)
                  TypeChecker(expr)
                  if (ExpressionFilter(expr) == ExpressionFilter.Status.Success)
                    Some((low_level_factory(vars ++ params), params))
                  else
                    None
                } catch {
                  case x: ir.TypeException => None
                  case x: Throwable =>
                    x.printStackTrace()
                    println(low_level_hash)
                    println(params.mkString("; "))
                    println(low_level_str)
                    println(SearchParameters.matrix_size)
                    None
                }
              }).collect { case Some(x) => x }

              println(s"Found ${potential_expressions.size} / $substitutionCount filtered expressions")

              SaveOpenCL(topFolder, low_level_hash, high_level_hash, potential_expressions)
            } catch {
              case _: Throwable =>
              // TODO: Log all errors to a file, so they could be reproduced in case of bugs
              // Failed reading file or similar.
            }

          })
        }
      }
    })
  }

  def readFromFile(filename: String) =
    Source.fromFile(filename).getLines().mkString("\n")

  def readLambdaFromFile(filename: String) =
    Eval(readFromFile(filename))

  def createValueMap(lambda: Lambda): Map[ArithExpr, ArithExpr] = {
    val vars = lambda.params.flatMap(_.t.varList).distinct

    vars.foldLeft(Map[ArithExpr, ArithExpr]())((st, v) =>
      st.updated(v, SearchParameters.matrix_size))
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]): Lambda = {
    val tunable_nodes = Utils.findTunableNodes(lambda).reverse
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))
    Utils.quickAndDirtySubstitution(st, tunable_nodes, lambda)
  }

}

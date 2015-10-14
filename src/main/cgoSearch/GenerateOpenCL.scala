package cgoSearch

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import apart.arithmetic.{ArithExpr, Cst, Var}
import exploration.InferNDRange
import exploration.utils.Utils
import ir.ast.Lambda
import ir.{Type, TypeChecker}
import opencl.executor.Eval
import opencl.generator.OpenCLGenerator
import opencl.ir.TypedOpenCLMemory

import scala.collection.immutable.Map
import scala.io.Source

/**
 * This main currently runs a parameter space exploration over the serialized low level expressions.
 * It requires the lambdas/ and lambdasLower/ folders with the generated index files.
 */
object GenerateOpenCL {
  val generate_counter = new AtomicInteger()
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

        val high_level_str = readLambdaFromFile(filename)
        val high_level_expr = Eval(high_level_str)

        val st = createValueMap(high_level_expr)
        val sizesForFilter = st.values.toSeq
        val vars = Seq.fill(sizesForFilter.length)(Var(""))

        replaceInputTypes(high_level_expr, st)

        TypeChecker(high_level_expr)

        val all_substitution_tables = ParameterSearch(high_level_expr)
        val substitutionCount = all_substitution_tables.size
        println(s"Found $substitutionCount valid parameter sets")

        val loweredIndex = s"${topFolder}Lower/$high_level_hash/index"
        if (Files.exists(Paths.get(loweredIndex))
          && substitutionCount < 800000) {

          val low_level_expr_list = Source.fromFile(loweredIndex).getLines().toList

          val low_level_counter = new AtomicInteger()
          val lowLevelCount = low_level_expr_list.size
          println(s"Found $lowLevelCount low level expressions")

          low_level_expr_list.par.foreach(low_level_filename => {

            val low_level_hash = low_level_filename.split("/").last
            val low_level_str = readLambdaFromFile(low_level_filename)
            val low_level_factory = Eval.getMethod(low_level_str)

            println(s"Low-level expression ${low_level_counter.incrementAndGet()} / $lowLevelCount")

            println("Propagating parameters...")
            val potential_expressions = all_substitution_tables.map(st => {
              val params = st.map(a => a).toSeq.sortBy(_._1.toString.substring(3).toInt).map(_._2)
              try {
                val expr = low_level_factory(sizesForFilter ++ params)
                TypeChecker(expr)
                if(ExpressionFilter(expr) == ExpressionFilter.Status.Success)
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
            }).collect{ case Some(x) => x }

            println(s"Found ${potential_expressions.size} / $substitutionCount filtered expressions")

            dumpOpenCLToFiles(potential_expressions, low_level_hash, high_level_hash)
          })
        }
      }
    })
  }

  def readLambdaFromFile(filename: String) =
    Source.fromFile(filename).getLines().mkString("\n").replace("idfloat", "id")

  def createValueMap(lambda: Lambda): Map[ArithExpr, ArithExpr] = {
    val vars = lambda.params.flatMap(_.t.varList).distinct

    vars.foldLeft(Map[ArithExpr, ArithExpr]())((st, v) =>
      st.updated(v, SearchParameters.matrix_size))
  }

  def replaceInputTypes(lambda: Lambda, st: Map[ArithExpr, ArithExpr]) =
    lambda.params.foreach(p => p.t = Type.substitute(p.t, st))

  def dumpOpenCLToFiles(expressions: List[(Lambda, Seq[ArithExpr])],
                        lowLevelHash: String,
                        highLevelHash: String): Unit = {
    expressions.foreach(pair => {
      try {
        val lambda = pair._1
        val substitutionMap = pair._2

        val (local, global) = InferNDRange(lambda)
        val valueMap = createValueMap(lambda)

        val inNDRange = InferNDRange.substituteInNDRange(global, valueMap)
        val code = OpenCLGenerator.generate(lambda, local, inNDRange, valueMap)

        val kernel =
          s"""
             |// Substitutions: $substitutionMap
             |// Local sizes: ${local.map(_.eval).mkString(", ")}
             |// Global sizes: ${global.mkString(", ")}
             |// High-level hash: $highLevelHash
             |// Low-level hash: $lowLevelHash
             |// Input size: ${SearchParameters.matrix_size}
             |
        |$code
      """.stripMargin

        val variables = Utils.findVariables(kernel)
        val variablesReplacedInKernel = Utils.replaceVariableDeclarations(kernel, variables)

        val hash = Utils.Sha256Hash(variablesReplacedInKernel)
        val filename = hash + ".cl"

        val path = s"${topFolder}Cl/$lowLevelHash"

        val (_,globalBuffers) = OpenCLGenerator.getMemories(lambda)
        // FIXME(tlutz): some buffer sizes overflow


        // Dump only the code if the minimal amount of temporary global arrays doesn't overflow
        val min_map = getBufferSizes(1024, globalBuffers)

        if(min_map.forall(_ > 0)) {
          val v = GenerateOpenCL.generate_counter.incrementAndGet()
          if(v % 100 == 0) println(s"Generated $v source files")

          Utils.dumpToFile(variablesReplacedInKernel, filename, path)


          Seq(1024,2048,4096,8192,16384).foreach(i => {

            // Add to the CSV if there are no overflow
            val cur_temp_alloc = getBufferSizes(i, globalBuffers)

            if(cur_temp_alloc.forall(_ > 0)) {
              val fw = new java.io.FileWriter(s"$path/exec_$i.csv", true)
              fw.write(i + "," +
                global.map(substituteInputSizes(i, _)).mkString(",") + "," +
                local.map(substituteInputSizes(i, _)).mkString(",") +
                s",$hash," + (globalBuffers.length - 3) + "," +
                cur_temp_alloc.mkString(",") + "\n")
              fw.close()
            }
          })
        }
      } catch {
        case _: Throwable =>
      }
    })

  }

  def substituteInputSizes(size: Int, ae: ArithExpr) = {
    val subst = Map(ae.varList.map((_: ArithExpr, Cst(size): ArithExpr)).toSeq: _*)
    ArithExpr.substitute(ae, subst)
  }

  def getBufferSizes(inputSize: Int, globalBuffers: Array[TypedOpenCLMemory]) =
    globalBuffers.map(x => substituteInputSizes(inputSize, x.mem.size).eval)
}

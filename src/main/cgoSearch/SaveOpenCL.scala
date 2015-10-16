package cgoSearch

import java.io.FileWriter

import apart.arithmetic.{?, ArithExpr, Cst}
import exploration.InferNDRange
import exploration.utils.Utils
import ir.ast.Lambda
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator.{IllegalKernel, OpenCLGenerator}
import opencl.ir.TypedOpenCLMemory

object SaveOpenCL {
  def apply(topFolder: String, lowLevelHash: String, highLevelHash: String,
            expressions: List[(Lambda, Seq[ArithExpr])]) =
    (new SaveOpenCL(topFolder, lowLevelHash, highLevelHash))(expressions)
}

class SaveOpenCL(topFolder: String, lowLevelHash: String, highLevelHash: String) {

  private var local: NDRange = Array(?, ?, ?)
  private var global: NDRange = Array(?, ?, ?)

  def apply(expressions: List[(Lambda, Seq[ArithExpr])]): Unit = {
    expressions.foreach(processLambda)
  }

  private def processLambda(pair: (Lambda, Seq[ArithExpr])) = {
    try {
      val kernel = generateKernel(pair)
      dumpOpenCLToFiles(pair._1, kernel)
    } catch {
      case _:Throwable =>
    }
  }

  private def generateKernel(pair: (Lambda, Seq[ArithExpr])) = {
    val lambda = pair._1
    val substitutionMap = pair._2

    InferNDRange(lambda) match { case (l, g) => local = l; global = g}
    val valueMap = GenerateOpenCL.createValueMap(lambda)

    val globalSubstituted = InferNDRange.substituteInNDRange(global, valueMap)
    val code = OpenCLGenerator.generate(lambda, local, globalSubstituted, valueMap)

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
         |""".stripMargin

    Utils.findAndReplaceVariableNames(kernel)
  }

  private def dumpOpenCLToFiles(lambda: Lambda, kernel: String): Unit = {

    val hash = Utils.Sha256Hash(kernel)
    val filename = hash + ".cl"

    val path = s"${topFolder}Cl/$lowLevelHash"

    // FIXME(tlutz): some buffer sizes overflow
    val (_,globalBuffers) = OpenCLGenerator.getMemories(lambda)

    // Dump only the code if the minimal amount of temporary global arrays doesn't overflow
    val min_map = getBufferSizes(1024, globalBuffers)

    if (!min_map.forall(_ > 0))
      throw new IllegalKernel("Buffer size overflow")

    Utils.dumpToFile(kernel, filename, path)
    createCsv(hash, path, globalBuffers)
  }

  private def createCsv(hash: String, path: String, globalBuffers: Array[TypedOpenCLMemory]): Unit = {
    Seq(1024, 2048, 4096, 8192, 16384).foreach(i => {

      // Add to the CSV if there are no overflow
      val cur_temp_alloc = getBufferSizes(i, globalBuffers)

      if (cur_temp_alloc.forall(_ > 0)) {
        val fw = new FileWriter(s"$path/exec_$i.csv", true)
        fw.write(i + "," +
          global.map(substituteInputSizes(i, _)).mkString(",") + "," +
          local.map(substituteInputSizes(i, _)).mkString(",") +
          s",$hash," + (globalBuffers.length - 3) + "," +
          cur_temp_alloc.mkString(",") + "\n")
        fw.close()
      }
    })
  }

  private def substituteInputSizes(size: Int, ae: ArithExpr) = {
    val subst = Map(ae.varList.map((_: ArithExpr, Cst(size): ArithExpr)).toSeq: _*)
    ArithExpr.substitute(ae, subst)
  }

  private def getBufferSizes(inputSize: Int, globalBuffers: Array[TypedOpenCLMemory]) =
    globalBuffers.map(x => substituteInputSizes(inputSize, x.mem.size).eval)
}

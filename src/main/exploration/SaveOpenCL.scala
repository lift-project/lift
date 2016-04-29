package exploration

import java.io.FileWriter

import analysis.{AccessCounts, BarrierCounts, MemoryAmounts}
import apart.arithmetic.{?, ArithExpr, Cst}
import ir.ast.Lambda
import opencl.generator.OpenCLGenerator
import opencl.generator.OpenCLGenerator.NDRange
import opencl.ir.{GlobalMemory, LocalMemory, PrivateMemory, TypedOpenCLMemory}
import rewriting.InferNDRange
import rewriting.utils.Utils

object SaveOpenCL {
  def apply(topFolder: String, lowLevelHash: String, highLevelHash: String,
            expressions: List[(Lambda, Seq[ArithExpr])]) =
    (new SaveOpenCL(topFolder, lowLevelHash, highLevelHash))(expressions)
}

class SaveOpenCL(topFolder: String, lowLevelHash: String, highLevelHash: String) {

  private var local: NDRange = Array(?, ?, ?)
  private var global: NDRange = Array(?, ?, ?)

  val inputSizes = Seq(1024, 2048, 4096, 8192, 16384)

  def apply(expressions: List[(Lambda, Seq[ArithExpr])]): Seq[Option[String]] =
    expressions.map(processLambda)

  private def processLambda(pair: (Lambda, Seq[ArithExpr])) = {

    val fileWriter =
      new FileWriter(topFolder + "Cl/" + lowLevelHash + "/stats_1024.csv")
    fileWriter.write(statsHeader)
    fileWriter.close()

    try {
      val kernel = generateKernel(pair)
      dumpOpenCLToFiles(pair._1, kernel)
    } catch {
      case x: Throwable =>
        println(x)
        None
    }
  }

  private def generateKernel(pair: (Lambda, Seq[ArithExpr])) = {
    val lambda = pair._1
    val substitutionMap = pair._2

    InferNDRange(lambda) match { case (l, g) => local = l; global = g }
    val valueMap = ParameterRewrite.createValueMap(lambda)

    val globalSubstituted = InferNDRange.substituteInNDRange(global, valueMap)
    val code = OpenCLGenerator.generate(lambda, local, globalSubstituted, valueMap)

    val kernel =
      s"""
         |// Substitutions: $substitutionMap
         |// Local sizes: ${local.map(x => { try { x.eval } catch { case _: Throwable => x } }).mkString(", ")}
         |// Global sizes: ${global.mkString(", ")}
         |// High-level hash: $highLevelHash
         |// Low-level hash: $lowLevelHash
         |
         |$code
         |""".stripMargin

    Utils.findAndReplaceVariableNames(kernel)
  }

  private def dumpOpenCLToFiles(lambda: Lambda, kernel: String): Option[String] = {

    val hash = Utils.Sha256Hash(kernel)
    val filename = hash + ".cl"

    val path = s"${topFolder}Cl/$lowLevelHash"

    val (_, buffers) = OpenCLGenerator.getMemories(lambda)
    val (localBuffers, globalBuffers) = buffers.partition(_.mem.addressSpace == LocalMemory)

    val dumped = Utils.dumpToFile(kernel, filename, path)
    if (dumped) {
      createCsv(hash, path, lambda.params.length, globalBuffers, localBuffers)
      dumpStats(lambda, hash, path)
    }

    if (dumped) Some(hash) else None
  }

  private def createCsv(hash: String, path: String, numParams: Int,
                        globalBuffers: Array[TypedOpenCLMemory],
                        localBuffers: Array[TypedOpenCLMemory]): Unit = {
    inputSizes.foreach(i => {

      // Add to the CSV if there are no overflow
      val allBufferSizes = getBufferSizes(i, globalBuffers)
      val globalTempAlloc = allBufferSizes.drop(numParams + 1)
      val localTempAlloc = getBufferSizes(i, localBuffers)

      if (allBufferSizes.forall(_ > 0)) {
        val fileWriter = new FileWriter(s"$path/exec_$i.csv", true)
        fileWriter.write(i + "," +
          global.map(substituteInputSizes(i, _)).mkString(",") + "," +
          local.map(substituteInputSizes(i, _)).mkString(",") +
          s",$hash," + globalTempAlloc.length + "," +
          globalTempAlloc.mkString(",") +
          (if (globalTempAlloc.length == 0) "" else ",") +
          localTempAlloc.length +
          (if (localTempAlloc.length == 0) "" else ",") +
          localTempAlloc.mkString(",")+ "\n")
        fileWriter.close()
      }
    })
  }

  val statsHeader =
    "hash,size,globalSize0,globalSize1,globalSize2,localSize0,localSize1,localSize2," +
    "globalMemory,localMemory,privateMemory,globalStores,globalLoads," +
    "localStores,localLoads,privateStores,privateLoads,barriers\n"

  private def dumpStats(lambda: Lambda, hash: String, path: String): Unit = {

    val smallSize = inputSizes.head
    val exact = true

    val smallGlobalSizes = global.map(substituteInputSizes(smallSize, _))
    val smallLocalSizes = local.map(substituteInputSizes(smallSize, _))
    val valueMap = ParameterRewrite.createValueMap(lambda)

    val memoryAmounts = MemoryAmounts(lambda, smallLocalSizes, smallGlobalSizes, valueMap)
    val accessCounts = AccessCounts(lambda, smallLocalSizes, smallGlobalSizes, valueMap)
    val barrierCounts = BarrierCounts(lambda, smallLocalSizes, smallGlobalSizes, valueMap)

    val globalMemory = memoryAmounts.getGlobalMemoryUsed(exact).evalDbl
    val localMemory = memoryAmounts.getLocalMemoryUsed(exact).evalDbl
    val privateMemory = memoryAmounts.getPrivateMemoryUsed(exact).evalDbl

    val globalStores = accessCounts.storesToAddressSpace(GlobalMemory, exact).evalDbl
    val globalLoads = accessCounts.loadsToAddressSpace(GlobalMemory, exact).evalDbl
    val localStores = accessCounts.storesToAddressSpace(LocalMemory, exact).evalDbl
    val localLoads = accessCounts.loadsToAddressSpace(LocalMemory, exact).evalDbl
    val privateStores = accessCounts.storesToAddressSpace(PrivateMemory, exact).evalDbl
    val privateLoads = accessCounts.loadsToAddressSpace(PrivateMemory, exact).evalDbl

    val barriers = barrierCounts.getTotalCount(exact).evalDbl

    val string =
      s"$hash,$smallSize,${smallGlobalSizes.mkString(",")},${smallLocalSizes.mkString(",")}," +
      s"$globalMemory,$localMemory,$privateMemory,$globalStores,$globalLoads," +
      s"$localStores,$localLoads,$privateStores,$privateLoads,$barriers\n"

    val fileWriter = new FileWriter(s"$path/stats_$smallSize.csv", true)
    fileWriter.write(string)
    fileWriter.close()

  }

  private def substituteInputSizes(size: Int, ae: ArithExpr) = {
    val subst = Map(ae.varList.map((_: ArithExpr, Cst(size): ArithExpr)).toSeq: _*)
    ArithExpr.substitute(ae, subst)
  }

  private def getBufferSizes(inputSize: Int, globalBuffers: Array[TypedOpenCLMemory]) =
    globalBuffers.map(x => substituteInputSizes(inputSize, x.mem.size).eval)
}

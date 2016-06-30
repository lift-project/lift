package exploration

import java.io.FileWriter

import analysis._
import apart.arithmetic.{?, ArithExpr, Cst}
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import opencl.generator.OpenCLGenerator.NDRange
import opencl.generator.{IllegalKernel, OpenCLGenerator}
import opencl.ir._
import opencl.ir.ast._
import rewriting.InferNDRange
import rewriting.utils.Utils

import scala.sys.process._

object SaveOpenCL {
  def apply(topFolder: String, lowLevelHash: String, highLevelHash: String,
            expressions: List[(Lambda, Seq[ArithExpr])]) =
    (new SaveOpenCL(topFolder, lowLevelHash, highLevelHash))(expressions)
}

class SaveOpenCL(topFolder: String, lowLevelHash: String, highLevelHash: String) {

  private val logger = Logger(this.getClass)

  private var local: NDRange = Array(?, ?, ?)
  private var global: NDRange = Array(?, ?, ?)

  val inputSizes = Seq(512, 1024, 2048, 4096, 8192)

  def apply(expressions: List[(Lambda, Seq[ArithExpr])]): Seq[Option[String]] = {

    s"mkdir -p ${topFolder}Cl/$lowLevelHash".!

    val fileWriter =
      new FileWriter(topFolder + "Cl/" + lowLevelHash + "/stats_1024.csv")
    fileWriter.write(statsHeader)
    fileWriter.close()

    expressions.map(processLambda)
  }

  private def processLambda(pair: (Lambda, Seq[ArithExpr])) = {
    try {
      val kernel = generateKernel(pair)
      dumpOpenCLToFiles(pair._1, kernel)
    } catch {
      case _: IllegalKernel =>
        None
      case t: Throwable =>
        logger.warn(s"Failed compilation $highLevelHash (${pair._2.mkString(",")})", t)
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
      inputSizes.foreach(dumpStats(lambda, hash, path, _))
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

  private val statsHeader =
    "hash,size,globalSize0,globalSize1,globalSize2,localSize0,localSize1,localSize2," +
    "globalMemory,localMemory,privateMemory,globalStores,globalLoads," +
    "localStores,localLoads,privateStores,privateLoads,barriers," +
    "coalescedGlobalStores,coalescedGlobalLoads,vectorGlobalStores,vectorGlobalLoads," +
    "ifStatements,forStatements,add,mult,addMult,vecAddMult,dot\n"

  private def dumpStats(lambda: Lambda, hash: String, path: String, size: Int): Unit = {

    val string: String = getStatsString(lambda, hash, size)

    val fileWriter = new FileWriter(s"$path/stats_$size.csv", true)
    fileWriter.write(string)
    fileWriter.close()

  }

  def getStatsString(lambda: Lambda, hash: String, size: Int): String = {
    val exact = true

    val globalSizes = global.map(substituteInputSizes(size, _))
    val localSizes = local.map(substituteInputSizes(size, _))
    val valueMap = ParameterRewrite.createValueMap(lambda)

    val memoryAmounts = MemoryAmounts(lambda, localSizes, globalSizes, valueMap)
    val accessCounts = AccessCounts(lambda, localSizes, globalSizes, valueMap)
    val barrierCounts = BarrierCounts(lambda, localSizes, globalSizes, valueMap)
    val controlFlow = ControlFlow(lambda, localSizes, globalSizes, valueMap)
    val functionCounts = FunctionCounts(lambda, localSizes, globalSizes, valueMap)

    val globalMemory = memoryAmounts.getGlobalMemoryUsed(exact).evalDbl
    val localMemory = memoryAmounts.getLocalMemoryUsed(exact).evalDbl
    val privateMemory = memoryAmounts.getPrivateMemoryUsed(exact).evalDbl

    val globalStores = accessCounts.getStores(GlobalMemory, exact).evalDbl
    val globalLoads = accessCounts.getLoads(GlobalMemory, exact).evalDbl
    val localStores = accessCounts.getStores(LocalMemory, exact).evalDbl
    val localLoads = accessCounts.getLoads(LocalMemory, exact).evalDbl
    val privateStores = accessCounts.getStores(PrivateMemory, exact).evalDbl
    val privateLoads = accessCounts.getLoads(PrivateMemory, exact).evalDbl

    val barriers = barrierCounts.getTotalCount(exact).evalDbl

    val coalescedGlobalStores =
      accessCounts.getStores(GlobalMemory, CoalescedPattern, exact).evalDbl
    val coalescedGlobalLoads =
      accessCounts.getLoads(GlobalMemory, CoalescedPattern, exact).evalDbl

    val vectorGlobalStores =
      accessCounts.vectorStores(GlobalMemory, UnknownPattern, exact).evalDbl
    val vectorGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern, exact).evalDbl

    val ifStatements = controlFlow.getIfStatements(exact).evalDbl
    val forStatements = controlFlow.getForStatements(exact).evalDbl

    val addCount = functionCounts.getFunctionCount(add, exact).evalDbl
    val multCount = functionCounts.getFunctionCount(mult, exact).evalDbl
    val addMult = functionCounts.getAddMultCount(exact).evalDbl
    val vecAddMult = functionCounts.getVectorisedAddMultCount(exact).evalDbl
    val dotCount = functionCounts.getFunctionCount(dot, exact).evalDbl

    val string =
      s"$hash,$size,${globalSizes.mkString(",")},${localSizes.mkString(",")}," +
        s"$globalMemory,$localMemory,$privateMemory,$globalStores,$globalLoads," +
        s"$localStores,$localLoads,$privateStores,$privateLoads,$barriers," +
        s"$coalescedGlobalStores,$coalescedGlobalLoads,$vectorGlobalStores," +
        s"$vectorGlobalLoads,$ifStatements,$forStatements,$addCount,$multCount," +
        s"$addMult,$vecAddMult,$dotCount\n"
    string
  }

  private def substituteInputSizes(size: Int, ae: ArithExpr) = {
    val subst = Map(ae.varList.map((_: ArithExpr, Cst(size): ArithExpr)).toSeq: _*)
    ArithExpr.substitute(ae, subst)
  }

  private def getBufferSizes(inputSize: Int, globalBuffers: Array[TypedOpenCLMemory]) =
    globalBuffers.map(x => substituteInputSizes(inputSize, x.mem.size).eval)
}

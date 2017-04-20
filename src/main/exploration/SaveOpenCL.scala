package exploration

import java.io.FileWriter

import analysis._
import lift.arithmetic._
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import opencl.executor.Compile
import opencl.generator.NDRange
import opencl.generator.{IllegalKernel, OpenCLGenerator}
import opencl.ir._
import opencl.ir.ast._
import rewriting.InferNDRange
import rewriting.utils.Utils

import scala.sys.process._

object SaveOpenCL {
  def apply(topFolder: String, lowLevelHash: String,
            highLevelHash: String, settings: Settings,
            expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))]) =
    (new SaveOpenCL(topFolder, lowLevelHash, highLevelHash, settings))(expressions)
}

class SaveOpenCL(
  topFolder: String,
  lowLevelHash: String,
  highLevelHash: String,
  settings: Settings) {

  private val logger = Logger(this.getClass)

  var local: NDRange = NDRange(?, ?, ?)
  var global: NDRange = NDRange(?, ?, ?)
  private var sizeArgs: Seq[Var] = Seq()
  private var numSizes = 0

  val inputSizes = Seq(512, 1024, 2048)
  private var inputCombinations: Seq[Seq[ArithExpr]] = Seq()

  def apply(expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))]): Seq[Option[String]] = {

    prepare(expressions)

    s"mkdir -p ${topFolder}Cl/".!

    val fileWriter =
      new FileWriter(topFolder + "Cl/stats_header.csv")
    fileWriter.write(statsHeader)
    fileWriter.close()

    expressions.map(processLambda)
  }

  def prepare(expressions: Seq[(Lambda, Seq[ArithExpr], (NDRange, NDRange))]): Unit = {
    if (expressions.nonEmpty) {
      val lambda = expressions.head._1
      sizeArgs = lambda.getVarsInParams()
      numSizes = sizeArgs.length

      val combinations = settings.inputCombinations

      if (combinations.isDefined && combinations.get.head.length == numSizes)
        inputCombinations = combinations.get
      else
        inputCombinations = inputSizes.map(Seq.fill[ArithExpr](numSizes)(_))
    }
  }

  private def processLambda(tuple: (Lambda, Seq[ArithExpr], (NDRange, NDRange))) = {
    try {
      val kernel = generateKernel(tuple)
      dumpOpenCLToFiles(tuple, kernel)
    } catch {
      case _: IllegalKernel =>
        None
      case t: Throwable =>
        logger.warn(s"Failed compilation $highLevelHash/$lowLevelHash (${tuple._2.mkString(",")})", t)
        None
    }
  }

  private def generateKernel(tuple: (Lambda, Seq[ArithExpr], (NDRange, NDRange))) = {
    val lambda = tuple._1
    val substitutionMap = tuple._2
    val ranges = tuple._3

    if(ParameterRewrite.exploreNDRange.value.isDefined)
      ranges match { case (l, g) => local = l; global = g }
    else
      InferNDRange(lambda) match { case (l, g) => local = l; global = g }

    val code = if(ParameterRewrite.disableNDRangeInjection.value.isDefined)
      Compile(lambda)
    else
      Compile(lambda, local, global)

    val kernel =
      s"""
         |// Substitutions: $substitutionMap
         |// Local sizes: ${local.evaluated.toString}
         |// Global sizes: ${global.toString}
         |// High-level hash: $highLevelHash
         |// Low-level hash: $lowLevelHash
         |
         |$code
         |""".stripMargin

    Utils.findAndReplaceVariableNames(kernel)
  }

  private def dumpOpenCLToFiles(tuple: (Lambda, Seq[ArithExpr], (NDRange, NDRange)), kernel: String): Option[String] = {

    val lambda = tuple._1
    val rangeStrings = tuple._3 match {
      case (localSize, globalSize) =>
        (localSize.toString.replace(",","_"), globalSize.toString.replace(",", "_"))
    }
    val hash = lowLevelHash + "_" + tuple._2.mkString("_") + "_" + rangeStrings._1 + "_" + rangeStrings._2
    val filename = hash + ".cl"

    val path = s"${topFolder}Cl/$lowLevelHash"

    val (_, buffers) = OpenCLGenerator.getMemories(lambda)
    val (localBuffers, globalBuffers) = buffers.partition(_.mem.addressSpace == LocalMemory)

    val dumped = Utils.dumpToFile(kernel, filename, path)
    if (dumped) {
      createCsv(hash, path, lambda.params.length, globalBuffers, localBuffers)

      try {
        dumpStats(lambda, hash, path)
      } catch {
        case t: Throwable =>
          logger.warn(s"Failed to get stats: $highLevelHash/$lowLevelHash (${tuple._2.mkString(",")})", t)
      }
    }

    if (dumped) Some(hash) else None
  }

  private def createCsv(hash: String, path: String, numParams: Int,
                        globalBuffers: Array[TypedOpenCLMemory],
                        localBuffers: Array[TypedOpenCLMemory]): Unit = {

    inputCombinations.foreach(sizes => {

      val inputVarMapping: Map[ArithExpr, ArithExpr] = (sizeArgs, sizes).zipped.toMap

      val size = sizes.head

      // Add to the CSV if there are no overflow
      val allBufferSizes =
        globalBuffers.map(mem => ArithExpr.substitute(mem.mem.size, inputVarMapping).eval)
      val globalTempAlloc = allBufferSizes.drop(numParams + 1)
      val localTempAlloc =
        localBuffers.map(mem => ArithExpr.substitute(mem.mem.size, inputVarMapping).eval)

      if (allBufferSizes.forall(_ > 0)) {

        val sizeId = getSizeId(sizes)

        val fileWriter = new FileWriter(s"$path/exec_$sizeId.csv", true)

        fileWriter.write(size + "," +
          global.map(ArithExpr.substitute(_, inputVarMapping)).toString + "," +
          local.map(ArithExpr.substitute(_, inputVarMapping)).toString +
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

  def statsHeader =
    "hash," +
    "globalSize0,globalSize1,globalSize2,localSize0,localSize1,localSize2," +
    "globalMemory,localMemory,privateMemory,globalStores,globalLoads," +
    "localStores,localLoads,privateStores,privateLoads,barriers," +
    "coalescedGlobalStores,coalescedGlobalLoads,vectorGlobalStores,vectorGlobalLoads," +
    "ifStatements,forStatements,add,mult,addMult,vecAddMult,dot\n"

  private def dumpStats(lambda: Lambda, hash: String, path: String): Unit = {

    inputCombinations.foreach(sizes => {

      val string = getStatsString(lambda, hash, sizes)
      val sizeId = getSizeId(sizes)

      val fileWriter = new FileWriter(s"$path/stats_$sizeId.csv", true)
      fileWriter.write(string)
      fileWriter.close()
    })
  }

  private def getSizeId(sizes: Seq[ArithExpr]): String = {
    val sizeId =
      if (sizes.distinct.length == 1)
        sizes.head.toString
      else
        sizes.mkString("_")
    sizeId
  }

  def getStatsString(lambda: Lambda, hash: String, sizes: Seq[ArithExpr]): String = {

    val exact = true
    val inputVarMapping: Map[ArithExpr, ArithExpr] = (sizeArgs, sizes).zipped.toMap

    val globalSizes = global.map(ArithExpr.substitute(_, inputVarMapping))
    val localSizes = local.map(ArithExpr.substitute(_, inputVarMapping))

    val memoryAmounts = MemoryAmounts(lambda, localSizes, globalSizes, inputVarMapping)
    val accessCounts = AccessCounts(lambda, localSizes, globalSizes, inputVarMapping)
    val barrierCounts = BarrierCounts(lambda, localSizes, globalSizes, inputVarMapping)
    val controlFlow = ControlFlow(lambda, localSizes, globalSizes, inputVarMapping)
    val functionCounts = FunctionCounts(lambda, localSizes, globalSizes, inputVarMapping)

    val globalMemory = memoryAmounts.getGlobalMemoryUsed(exact).evalDouble
    val localMemory = memoryAmounts.getLocalMemoryUsed(exact).evalDouble
    val privateMemory = memoryAmounts.getPrivateMemoryUsed(exact).evalDouble

    val globalStores = accessCounts.getStores(GlobalMemory, exact).evalDouble
    val globalLoads = accessCounts.getLoads(GlobalMemory, exact).evalDouble
    val localStores = accessCounts.getStores(LocalMemory, exact).evalDouble
    val localLoads = accessCounts.getLoads(LocalMemory, exact).evalDouble
    val privateStores = accessCounts.getStores(PrivateMemory, exact).evalDouble
    val privateLoads = accessCounts.getLoads(PrivateMemory, exact).evalDouble

    val barriers = barrierCounts.getTotalCount(exact).evalDouble

    val coalescedGlobalStores =
      accessCounts.getStores(GlobalMemory, CoalescedPattern, exact).evalDouble
    val coalescedGlobalLoads =
      accessCounts.getLoads(GlobalMemory, CoalescedPattern, exact).evalDouble

    val vectorGlobalStores =
      accessCounts.vectorStores(GlobalMemory, UnknownPattern, exact).evalDouble
    val vectorGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern, exact).evalDouble

    val ifStatements = controlFlow.getIfStatements(exact).evalDouble
    val forStatements = controlFlow.getForStatements(exact).evalDouble

    val addCount = functionCounts.getFunctionCount(add, exact).evalDouble
    val multCount = functionCounts.getFunctionCount(mult, exact).evalDouble
    val addMult = functionCounts.getAddMultCount(exact).evalDouble
    val vecAddMult = functionCounts.getVectorisedAddMultCount(exact).evalDouble
    val dotCount = functionCounts.getFunctionCount(dot, exact).evalDouble

    val string =
      s"$hash,${globalSizes.toString},${localSizes.toString}," +
        s"$globalMemory,$localMemory,$privateMemory,$globalStores,$globalLoads," +
        s"$localStores,$localLoads,$privateStores,$privateLoads,$barriers," +
        s"$coalescedGlobalStores,$coalescedGlobalLoads,$vectorGlobalStores," +
        s"$vectorGlobalLoads,$ifStatements,$forStatements,$addCount,$multCount," +
        s"$addMult,$vecAddMult,$dotCount\n"

    string
  }

}

package exploration

import java.io.FileWriter

import analysis._
import lift.arithmetic._
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
  def apply(topFolder: String, lowLevelHash: String,
            highLevelHash: String, settings: Settings,
            expressions: List[(Lambda, Seq[ArithExpr])]) =
    (new SaveOpenCL(topFolder, lowLevelHash, highLevelHash, settings))(expressions)
}

class SaveOpenCL(
  topFolder: String,
  lowLevelHash: String,
  highLevelHash: String,
  settings: Settings) {

  private val logger = Logger(this.getClass)

  var local: NDRange = Array(?, ?, ?)
  var global: NDRange = Array(?, ?, ?)
  private var sizeArgs: Seq[Var] = Seq()
  private var numSizes = 0

  val inputSizes = Seq(512, 1024, 2048, 4096, 8192)
  private var inputCombinations: Seq[Seq[ArithExpr]] = Seq()

  def apply(expressions: List[(Lambda, Seq[ArithExpr])]): Seq[Option[String]] = {

    prepare(expressions)

    s"mkdir -p ${topFolder}Cl/".!

    val fileWriter =
      new FileWriter(topFolder + "Cl/stats_header.csv")
    fileWriter.write(statsHeader)
    fileWriter.close()

    expressions.map(processLambda)
  }

  def prepare(expressions: List[(Lambda, Seq[ArithExpr])]): Unit = {
    if (expressions.nonEmpty) {
      val lambda = expressions.head._1
      sizeArgs = lambda.params.flatMap(_.t.varList).sortBy(_.name).distinct
      numSizes = sizeArgs.length

      val combinations = settings.inputCombinations

      if (combinations.isDefined && combinations.get.head.length == numSizes)
        inputCombinations = combinations.get
      else
        inputCombinations = inputSizes.map(Seq.fill[ArithExpr](numSizes)(_))
    }
  }

  private def processLambda(pair: (Lambda, Seq[ArithExpr])) = {
    try {
      val kernel = generateKernel(pair)
      dumpOpenCLToFiles(pair, kernel)
    } catch {
      case _: IllegalKernel =>
        None
      case t: Throwable =>
        logger.warn(s"Failed compilation $highLevelHash/$lowLevelHash (${pair._2.mkString(",")})", t)
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

  private def dumpOpenCLToFiles(pair: (Lambda, Seq[ArithExpr]), kernel: String): Option[String] = {

    val lambda = pair._1
    val hash = lowLevelHash + "_" + pair._2.mkString("_")
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
          logger.warn(s"Failed to get stats: $highLevelHash/$lowLevelHash (${pair._2.mkString(",")})", t)
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
          global.map(ArithExpr.substitute(_, inputVarMapping)).mkString(",") + "," +
          local.map(ArithExpr.substitute(_, inputVarMapping)).mkString(",") +
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
    "hash," + (0 until numSizes).map("size" + _).mkString(",") +
    ",globalSize0,globalSize1,globalSize2,localSize0,localSize1,localSize2," +
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

    val valueMap = ParameterRewrite.createValueMap(lambda, sizes)

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
      s"$hash,${sizes.mkString(",")},${globalSizes.mkString(",")},${localSizes.mkString(",")}," +
        s"$globalMemory,$localMemory,$privateMemory,$globalStores,$globalLoads," +
        s"$localStores,$localLoads,$privateStores,$privateLoads,$barriers," +
        s"$coalescedGlobalStores,$coalescedGlobalLoads,$vectorGlobalStores," +
        s"$vectorGlobalLoads,$ifStatements,$forStatements,$addCount,$multCount," +
        s"$addMult,$vecAddMult,$dotCount\n"

    string
  }

}

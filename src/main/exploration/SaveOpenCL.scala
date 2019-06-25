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
import rewriting.utils.{DumpToFile, Utils}

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
        logger.warn(s"Failed compilation $highLevelHash/$lowLevelHash (${tuple._2.mkString(",")}), " +
          s"NDRanges: (${tuple._3._1.toString}, ${tuple._3._2.toString})", t)
        None
    }
  }

  private def generateKernel(tuple: (Lambda, Seq[ArithExpr], (NDRange, NDRange))) = {
    val lambda = tuple._1
    val substitutionMap = tuple._2
    val ranges = tuple._3

    if (SplitSlideRewrite.settings.parameterRewriteSettings.exploreNDRange)
      ranges match { case (l, g) => local = l; global = g }
    else
      InferNDRange(lambda) match { case (l, g) => local = l; global = g }

    val code = if (SplitSlideRewrite.settings.parameterRewriteSettings.disableNDRangeInjection)
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

    DumpToFile.findAndReplaceVariableNames(kernel)
  }

  private def dumpOpenCLToFiles(tuple: (Lambda, Seq[ArithExpr], (NDRange, NDRange)), kernel: String): Option[String] = {

    val lambda = tuple._1
    val rangeStrings = tuple._3 match {
      case (localSize, globalSize) =>
        (localSize.toString.replace(",","_"), globalSize.toString.replace(",", "_"))
    }
    var hash = lowLevelHash + "_" + tuple._2.mkString("_")

    // TODO: Also differentiate between injecting ranges on and off
    if (SplitSlideRewrite.settings.parameterRewriteSettings.exploreNDRange)
      hash += "_" + rangeStrings._1 + "_" + rangeStrings._2

    val filename = hash + ".cl"

    val path = s"${topFolder}Cl/$lowLevelHash"

    val (_, buffers) = OpenCLGenerator.getMemories(lambda)
    val (localBuffers, globalBuffers) = buffers.partition(_.mem.addressSpace == LocalMemory)

    val dumped = DumpToFile.dumpToFile(kernel, filename, path)
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
                        globalBuffers: Seq[TypedOpenCLMemory],
                        localBuffers: Seq[TypedOpenCLMemory]): Unit = {

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
          (if (globalTempAlloc.isEmpty) "" else ",") +
          localTempAlloc.length +
          (if (localTempAlloc.isEmpty) "" else ",") +
          localTempAlloc.mkString(",")+ "\n")

        fileWriter.close()
      }
    })
  }

  def statsHeader: String =
    "hash," +
    "globalSize0,globalSize1,globalSize2,localSize0,localSize1,localSize2," +
    "globalMemory,localMemory,privateMemory," +
    "globalStores,globalLoads," +
    "localStores,localLoads," +
    "privateStores,privateLoads," +
    "scalarCoalescedGlobalStores,scalarCoalescedGlobalLoads," +
    "vectorCoalescedGlobalStores,vectorCoalescedGlobalLoads," +
    "scalarGlobalStores,scalarGlobalLoads," +
    "vectorGlobalStores,vectorGlobalLoads," +
    "scalarCoalescedLocalStores,scalarCoalescedLocalLoads," +
    "vectorCoalescedLocalStores,vectorCoalescedLocalLoads," +
    "scalarLocalStores,scalarLocalLoads," +
    "vectorLocalStores,vectorLocalLoads," +
    "scalarPrivateStores,scalarPrivateLoads," +
    "vectorPrivateStores,vectorPrivateLoads," +
    "ifStatements,forStatements,forBranches,barriers," +
    "add,mult,addVec,multVec,addMult,addMultVec,dot,opCount\n"

  private def dumpStats(lambda: Lambda, hash: String, path: String): Unit = {

    inputCombinations.foreach(sizes => {

      val (stats, userfunCounts) = getStatsString(lambda, hash, sizes)
      val sizeId = getSizeId(sizes)

      val statsWriter = new FileWriter(s"$path/stats_$sizeId.csv", true)
      statsWriter.write(stats)
      statsWriter.close()

      val userFunWriter = new FileWriter(s"$path/userfuns_$sizeId.csv", true)
      userFunWriter.write(userfunCounts)
      userFunWriter.close()
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

  def getStatsString(lambda: Lambda, hash: String, sizes: Seq[ArithExpr]): (String, String) = {

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

    val scalarGlobalStores =
      accessCounts.scalarStores(GlobalMemory, UnknownPattern, exact).evalDouble
    val scalarGlobalLoads =
      accessCounts.scalarLoads(GlobalMemory, UnknownPattern, exact).evalDouble
    val scalarLocalStores =
      accessCounts.scalarStores(LocalMemory, UnknownPattern, exact).evalDouble
    val scalarLocalLoads =
      accessCounts.scalarLoads(LocalMemory, UnknownPattern, exact).evalDouble

    val privateScalarStores =
      accessCounts.scalarStores(PrivateMemory, UnknownPattern, exact).evalDouble
    val privateScalarLoads =
      accessCounts.scalarLoads(PrivateMemory, UnknownPattern, exact).evalDouble

    val barriers = barrierCounts.getTotalCount(exact).evalDouble

    val scalarCoalescedGlobalStores =
      accessCounts.scalarStores(GlobalMemory, CoalescedPattern, exact).evalDouble
    val scalarCoalescedGlobalLoads =
      accessCounts.scalarLoads(GlobalMemory, CoalescedPattern, exact).evalDouble

    val scalarCoalescedLocalStores =
      accessCounts.scalarStores(LocalMemory, CoalescedPattern, exact).evalDouble
    val scalarCoalescedLocalLoads =
      accessCounts.scalarLoads(LocalMemory, CoalescedPattern, exact).evalDouble

    val vectorCoalescedGlobalStores =
      accessCounts.vectorStores(GlobalMemory, CoalescedPattern, exact).evalDouble
    val vectorCoalescedGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, CoalescedPattern, exact).evalDouble

    val vectorCoalescedLocalStores =
      accessCounts.vectorStores(LocalMemory, CoalescedPattern, exact).evalDouble
    val vectorCoalescedLocalLoads =
      accessCounts.vectorLoads(LocalMemory, CoalescedPattern, exact).evalDouble

    val vectorGlobalStores =
      accessCounts.vectorStores(GlobalMemory, UnknownPattern, exact).evalDouble
    val vectorGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern, exact).evalDouble

    val vectorLocalStores =
      accessCounts.vectorStores(LocalMemory, UnknownPattern, exact).evalDouble
    val vectorLocalLoads =
      accessCounts.vectorLoads(LocalMemory, UnknownPattern, exact).evalDouble

    val vectorPrivateStores =
      accessCounts.vectorStores(PrivateMemory, UnknownPattern, exact).evalDouble
    val vectorPrivateLoads =
      accessCounts.vectorLoads(PrivateMemory, UnknownPattern, exact).evalDouble

    val globalStores =
      vectorGlobalStores + scalarGlobalStores +
      vectorCoalescedGlobalStores + scalarCoalescedGlobalStores
    val globalLoads =
      vectorGlobalLoads + scalarGlobalLoads +
      vectorCoalescedGlobalLoads + scalarCoalescedGlobalLoads

    val localStores =
      vectorLocalStores + scalarLocalStores +
      vectorCoalescedLocalStores + scalarCoalescedLocalStores
    val localLoads =
      vectorLocalLoads + scalarLocalLoads +
      vectorCoalescedLocalLoads + scalarCoalescedLocalLoads

    val privateStores = vectorPrivateStores + privateScalarStores
    val privateLoads = vectorPrivateLoads + privateScalarLoads

    val ifStatements = controlFlow.getIfStatements(exact).evalDouble
    val forStatements = controlFlow.getForStatements(exact).evalDouble
    val forBranches = controlFlow.getForBranches(exact).evalDouble

    val addScalarCount = functionCounts.getFunctionCount(add, exact).evalDouble
    val multScalarCount = functionCounts.getFunctionCount(mult, exact).evalDouble

    val addVecCount = functionCounts.getVectorisedCount(add, exact).evalDouble
    val multVecCount = functionCounts.getVectorisedCount(mult, exact).evalDouble

    val addMult = functionCounts.getAddMultCount(exact).evalDouble
    val vecAddMult = functionCounts.getVectorisedAddMultCount(exact).evalDouble
    val dotCount = functionCounts.getFunctionCount(dot, exact).evalDouble

    val opCount =
      addScalarCount + multScalarCount + addVecCount + multVecCount + dotCount

    val allFunctions = functionCounts.getFunctions

    val allUserFunctionCounts = hash + "," + allFunctions.map(function => {
      val count = functionCounts.getFunctionCount(function, exact).evalDouble
      val vectorisedCount = functionCounts.getVectorisedCount(function, exact).evalDouble
      s"$function,$count,$vectorisedCount"
    }).mkString(", ") + "\n"

    val string =
      s"$hash,${globalSizes.toString},${localSizes.toString}," +
        s"$globalMemory,$localMemory,$privateMemory," +
        s"$globalStores,$globalLoads," +
        s"$localStores,$localLoads," +
        s"$privateStores,$privateLoads," +
        s"$scalarCoalescedGlobalStores,$scalarCoalescedGlobalLoads," +
        s"$vectorCoalescedGlobalStores,$vectorCoalescedGlobalLoads," +
        s"$scalarGlobalStores,$scalarGlobalLoads," +
        s"$vectorGlobalStores,$vectorGlobalLoads," +
        s"$scalarCoalescedLocalStores,$scalarCoalescedLocalLoads," +
        s"$vectorCoalescedLocalStores,$vectorCoalescedLocalLoads," +
        s"$scalarLocalStores,$scalarLocalLoads," +
        s"$vectorLocalStores,$vectorLocalLoads," +
        s"$privateScalarStores,$privateScalarLoads," +
        s"$vectorPrivateStores,$vectorPrivateLoads," +
        s"$ifStatements,$forStatements,$forBranches,$barriers," +
        s"$addScalarCount,$multScalarCount," +
        s"$addVecCount,$multVecCount,$addMult,$vecAddMult,$dotCount,$opCount\n"

    (string, allUserFunctionCounts)
  }

}

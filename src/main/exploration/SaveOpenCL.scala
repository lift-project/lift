package exploration

import java.io.FileWriter

import analysis._
import lift.arithmetic._
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import opencl.executor.Compile
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

  val inputSizes = Seq(512, 1024, 2048)
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
      sizeArgs = lambda.getVarsInParams()
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

    val code = Compile(lambda, local, global)

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
    "hash," +
    "globalSize0,globalSize1,globalSize2,localSize0,localSize1,localSize2," +
    "globalMemory,localMemory,privateMemory,globalStores,globalLoads," +
    "localStores,localLoads,privateStores,privateLoads,barriers," +
    "coalescedGlobalStores,coalescedGlobalLoads,vectorGlobalStores,vectorGlobalLoads," +
    "vectorLocalStores,vectorLocalLoads,vectorPrivateStores,vectorPrivateLoads," +
    "scalarGlobalStores,scalarGlobalLoads,scalarLocalStores,scalarLocalLoads," +
    "scalarPrivateStores,scalarPrivateLoads," +
    "ifStatements,forStatements,add,mult,addVec,multVec,addMult,vecAddMult,dot,opCount\n"

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

    val globalMemory = memoryAmounts.getGlobalMemoryUsed(exact).evalDbl
    val localMemory = memoryAmounts.getLocalMemoryUsed(exact).evalDbl
    val privateMemory = memoryAmounts.getPrivateMemoryUsed(exact).evalDbl

    val globalScalarStores = accessCounts.getStores(GlobalMemory, exact).evalDbl
    val globalScalarLoads = accessCounts.getLoads(GlobalMemory, exact).evalDbl
    val localScalarStores = accessCounts.getStores(LocalMemory, exact).evalDbl
    val localScalarLoads = accessCounts.getLoads(LocalMemory, exact).evalDbl
    val privateScalarStores = accessCounts.getStores(PrivateMemory, exact).evalDbl
    val privateScalarLoads = accessCounts.getLoads(PrivateMemory, exact).evalDbl

    val barriers = barrierCounts.getTotalCount(exact).evalDbl

    val coalescedGlobalStores =
      accessCounts.getStores(GlobalMemory, CoalescedPattern, exact).evalDbl
    val coalescedGlobalLoads =
      accessCounts.getLoads(GlobalMemory, CoalescedPattern, exact).evalDbl

    val vectorGlobalStores =
      accessCounts.vectorStores(GlobalMemory, UnknownPattern, exact).evalDbl
    val vectorGlobalLoads =
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern, exact).evalDbl

    val vectorLocalStores =
      accessCounts.vectorStores(LocalMemory, UnknownPattern, exact).evalDbl
    val vectorLocalLoads =
      accessCounts.vectorLoads(LocalMemory, UnknownPattern, exact).evalDbl

    val vectorPrivateStores =
      accessCounts.vectorStores(PrivateMemory, UnknownPattern, exact).evalDbl
    val vectorPrivateLoads =
      accessCounts.vectorLoads(PrivateMemory, UnknownPattern, exact).evalDbl

    val globalStores = vectorGlobalStores + globalScalarStores
    val globalLoads = vectorGlobalLoads + globalScalarLoads

    val localStores = vectorLocalStores + localScalarStores
    val localLoads = vectorLocalLoads + localScalarLoads

    val privateStores = vectorPrivateStores + privateScalarStores
    val privateLoads = vectorPrivateLoads + privateScalarLoads

    val ifStatements = controlFlow.getIfStatements(exact).evalDbl
    val forStatements = controlFlow.getForStatements(exact).evalDbl

    val addScalarCount = functionCounts.getFunctionCount(add, exact).evalDbl
    val multScalarCount = functionCounts.getFunctionCount(mult, exact).evalDbl

    val addVecCount = functionCounts.getVectorisedCount(add, exact).evalDbl
    val multVecCount = functionCounts.getVectorisedCount(mult, exact).evalDbl

    val addMult = functionCounts.getAddMultCount(exact).evalDbl
    val vecAddMult = functionCounts.getVectorisedAddMultCount(exact).evalDbl
    val dotCount = functionCounts.getFunctionCount(dot, exact).evalDbl

    val opCount =
      addScalarCount + multScalarCount + addVecCount + multVecCount + dotCount

    val string =
      s"$hash,${globalSizes.mkString(",")},${localSizes.mkString(",")}," +
        s"$globalMemory,$localMemory,$privateMemory,$globalStores,$globalLoads," +
        s"$localStores,$localLoads,$privateStores,$privateLoads,$barriers," +
        s"$coalescedGlobalStores,$coalescedGlobalLoads,$vectorGlobalStores," +
        s"$vectorGlobalLoads," +
        s"$vectorLocalStores,$vectorLocalLoads,$vectorPrivateStores,$vectorPrivateLoads," +
        s"$globalScalarStores,$globalScalarLoads,$localScalarStores,$localScalarLoads," +
        s"$privateScalarStores,$privateScalarLoads," +
        s"$ifStatements,$forStatements,$addScalarCount,$multScalarCount," +
        s"$addVecCount,$multVecCount,$addMult,$vecAddMult,$dotCount,$opCount\n"

    string
  }

}

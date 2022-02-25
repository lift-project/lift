package analysis

import java.io.{File, FileWriter}

import analysis.CacheLinesAccessed.DeviceConfig
import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import opencl.executor.Eval

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex


object BenchmarkCacheUsageEfficiency {
  private val logger = Logger(this.getClass)

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000 + "ms")
    result
  }


  val maliConfig: DeviceConfig = DeviceConfig( // cache size = 524288 bytes = 524 KB = 8192 lines
    nCores = 12,
    nParQuadsPerCore = 3, // 12*3*4 = 144 simultaneous threads*/
    cacheLineSizeInBytes = 64, // 512 bits
    localMemIsGlobal = true
  )

//  val expLabel = "outofbounds_fix"
//  val generateDate = "20.01.2021_18.13.34"
//  val rootDirPath = new File("c:/Users/Naums/hipeac20experiments/generated_files")
//  val allFilesDirPath = new File(rootDirPath.getAbsolutePath + s"/generated_files_${expLabel}_${generateDate}")
//  val jobDirs: mutable.Map[Int, File] = mutable.Map()
//  val layerToBenchmark = 8
//  val measurementsFilePath = new File(s"c:/Users/Naums/hipeac20experiments/parsed_timings/vgg_${expLabel}_params_and_results.csv")
//  val cacheLinesCsvFilePath = new File(s"c:/Users/Naums/hipeac20experiments/parsed_timings/vgg_${expLabel}_cache_lines.csv")
//  val overwriteOldMetrics = false
//  val generatedInJobs = true

  val generateLabel = "autotune" // direct!
  val runLabel = "direct_autotune_0"
  val generateDate = "09.11.2021_17.41.51"

//  val generateLabel = "debug_direct_for_gemm" ///mnt/c/Users/Naums/hipeac20experiments/generated_files/generated_files_debug_direct_for_gemm_09.11.2021_13.49.37/8/0/kernel_21625.cl
//  val runLabel = "debug_gemm_6" // gemm!
//  val generateDate = "09.11.2021_13.49.37"

  val rootDirPath = new File("c:/Users/Naums/hipeac20experiments/generated_files")
  val jobFilesDirPath = new File(rootDirPath.getAbsolutePath + s"/generated_files_${generateLabel}_${generateDate}")
  val jobDirs: mutable.Map[Int, File] = mutable.Map()
  val layerToBenchmark = 8
  val measurementsFilePath = new File(s"c:/Users/Naums/hipeac20experiments/parsed_timings/vgg_${runLabel}_params_and_results.csv")
  val cacheLinesCsvFilePath = new File(s"c:/Users/Naums/hipeac20experiments/parsed_timings/vgg_${runLabel}_cache_lines.csv")
  val overwriteOldMetrics = false
  val generatedInJobs = false

  def main(args: Array[String]): Unit = {

    if (overwriteOldMetrics) {
      if (cacheLinesCsvFilePath.exists())
        cacheLinesCsvFilePath.delete()
    }

    def updateOldMetrics(): Set[(Int, Int, Int)] =
      if (!cacheLinesCsvFilePath.exists()) {
        Set()
      } else {
        logger.info(s"Reading data from $cacheLinesCsvFilePath")
        readCsv()
      }

    var oldMetrics: Set[(Int, Int, Int)] = updateOldMetrics()

    logger.info(s"Saving data to $cacheLinesCsvFilePath")

    case class LambdaInfo(lambdaFilePath: String,
                          lambdaStr: String,
                          jobIdx: Int,
                          layerIdx: Int,
                          tunePointIdx: Int,
                          infoFromKernel:  Option[((Int, Int, Int), (Int, Int, Int), String)])
    val batchSize = 8
    val batchOfLambdas: ListBuffer[LambdaInfo] = ListBuffer()
    for {
      jobDir <- convLambdaPrintouts()
      (lambdaFilePath, lambdaStr) <- jobDir

//      if jobIdx == 143
//      if layerIdx == 8
//      if tunePointIdx == 146
    } {
      if (batchOfLambdas.size < batchSize) {
        val (jobIdx, layerIdx, tunePointIdx) = getLayerTunePointIds(lambdaFilePath)

        if (!oldMetrics.contains((jobIdx, layerIdx, tunePointIdx))){ // && tunePointIdx == 1250) {

          val infoFromKernel = convLambdaNDranges(jobIdx, layerIdx, tunePointIdx)

          if (infoFromKernel.isDefined) {
            batchOfLambdas += LambdaInfo(lambdaFilePath, lambdaStr, jobIdx, layerIdx, tunePointIdx, infoFromKernel)

          } else {
            recordToCsv(jobIdx, layerIdx, tunePointIdx, None)
          }
        }
      } else {
        val batchResult = time {
          batchOfLambdas.toList.par.map { i =>
            logger.info("Analysing the following lambda/kernel:")
            logger.info(s"${i.lambdaFilePath}")

//            val measuredRuntime = getMeasuredRuntimeMedian(i.jobIdx, i.layerIdx, i.tunePointIdx) match {
//              case Some(v) => v
//              case None => throw new IllegalArgumentException()
//            }
//            logger.info(s"Measured runtime median: $measuredRuntime")

            val (localSizes, globalSizes, kernelFilePath) = i.infoFromKernel.get
            logger.info(s"$kernelFilePath")
            logger.info(s"localSizes: $localSizes globalSizes: $globalSizes")

            val lambda = Eval(i.lambdaStr)
            println("Eval completed")

            val cacheLinesAccessed = CacheLinesAccessed(lambda, localSizes, globalSizes, maliConfig, verbose = true)
            logger.info("\n\n")
            (cacheLinesAccessed, localSizes, globalSizes)
          }
        }
        time {
          batchOfLambdas.zip(batchResult).foreach {
            case (i, (cacheLinesAccessed, localSizes, globalSizes)) =>

              recordToCsv(i.jobIdx, i.layerIdx, i.tunePointIdx, Some((cacheLinesAccessed, localSizes, globalSizes)))
          }
          batchOfLambdas.clear()
          oldMetrics = updateOldMetrics()
        }}
    }
  }

  def convLambdaPrintouts(): Iterator[Iterator[(String, String)]] = {
    if (!(jobFilesDirPath.exists && jobFilesDirPath.isDirectory))
      throw new IllegalArgumentException()

    for {
      jobLambdaDir <- (
        if (generatedInJobs) {
          jobFilesDirPath.listFiles.filter(f =>
            f.isDirectory && f.getName.startsWith("generated_lambdas_")
          ).iterator } else {
          List(new File(rootDirPath.getAbsolutePath + s"/generated_lambdas_${generateLabel}_${generateDate}"))
        })
      if jobLambdaDir.listFiles.exists(f =>
        f.isDirectory && f.getName.equals(layerToBenchmark.toString))
    } yield {

      val layerLambdasDirPath = jobLambdaDir.listFiles.find(f =>
        f.isDirectory && f.getName.equals(layerToBenchmark.toString)
      ).get

      for {
        tunePointLambdaDir <- layerLambdasDirPath.listFiles.filter(_.isDirectory).iterator

        lambdaFile = tunePointLambdaDir.listFiles.toList.
          find(_.getName.equals("conv_concrete.scala"))

        if {
          if (lambdaFile.isEmpty) {
            println("WARNING: " + s"No conv_concrete.scala in ${tunePointLambdaDir.getPath}")
            false
          } else true }
      } yield {
        val src = Source.fromFile(lambdaFile.get.getPath)
        // Repair lambda strings generated before the fix of the ScalaPrinter (Zip() => Zip(n))
        val lambdaStr = src.mkString.replace("FunCall(Zip(), ", "Zip(")
        src.close()

        (lambdaFile.get.getPath, lambdaStr)
      }
    }
  }

  def getLayerTunePointIds(lambdaFilePath: String): (Int, Int, Int) = {
    if (generatedInJobs) {
      val lambdaFilePathPattern = new Regex(
        s"\\\\generated_lambdas_${generateLabel}" + """_([0-9]+)_.*\\([0-9]+)\\([0-9]+)\\conv_concrete.scala""",
        "jobId", "layerId", "tunePointId")

      lambdaFilePathPattern.findFirstMatchIn(lambdaFilePath) match {
        case None =>
          throw new IllegalStateException(lambdaFilePath)
        case Some(fpMatch) =>
          (fpMatch.group("jobId").toInt,
            fpMatch.group("layerId").toInt,
            fpMatch.group("tunePointId").toInt)
      }
    } else {
      val lambdaFilePathPattern = new Regex(
        s"\\\\generated_lambdas_${generateLabel}" + """_.*\\([0-9]+)\\([0-9]+)\\conv_concrete.scala""",
        "layerId", "tunePointId")

      lambdaFilePathPattern.findFirstMatchIn(lambdaFilePath) match {
        case None =>
          throw new IllegalStateException(lambdaFilePath)
        case Some(fpMatch) =>
          (0,
            fpMatch.group("layerId").toInt,
            fpMatch.group("tunePointId").toInt)
      }
    }
  }

  val cachedUnprocessedRuntimes: mutable.Map[(Int, Int), ListBuffer[Float]] = mutable.Map()

  val runtimesFile: BufferedSource = io.Source.fromFile(measurementsFilePath, "utf-8")
  val runtimesFileIterator: Iterator[String] = runtimesFile.getLines

  val (layerColIdx, tunepointColIdx, totalRuntimeColIdx) = {
    val line = runtimesFileIterator.next()
    val header: Array[String] = line.split(",").map(_.trim)
    (header.indexOf("layer"), header.indexOf("tunepoint"), header.indexOf("total_runtime"))
  }
  assert(layerColIdx != -1 && tunepointColIdx != -1 && totalRuntimeColIdx != -1)

  @tailrec
  def getMeasuredRuntimeMedian(jobIdx: Int, layerIdx: Int, tunePointIdx: Int): Option[Float] = {
    val idx = (layerIdx, tunePointIdx)

    if (!runtimesFileIterator.hasNext) None
    else {
      val line = runtimesFileIterator.next()
      var values = line.split(",").map(_.trim)
      val runtimes: ListBuffer[Float] = ListBuffer()

      while ({
        val isTarget = isTargetMeasurement(values, jobIdx, layerIdx, tunePointIdx)
        if (isTarget)
          runtimes.append(parseRuntime(values(totalRuntimeColIdx)))
        isTarget && runtimesFileIterator.hasNext
      }) {
        val line = runtimesFileIterator.next()
        values = line.split(",").map(_.trim)
      }

      if (cachedUnprocessedRuntimes.contains(idx)) {
        runtimes.append(cachedUnprocessedRuntimes(idx): _*)
        cachedUnprocessedRuntimes.remove(idx)
      }

      if (cachedUnprocessedRuntimes.contains(idx))
        cachedUnprocessedRuntimes(idx).append(parseRuntime(values(totalRuntimeColIdx)))
      else
        cachedUnprocessedRuntimes += (idx -> ListBuffer(parseRuntime(values(totalRuntimeColIdx))))

      if (runtimes.nonEmpty) {
        //        Some(runtimes.sum / runtimes.size)
        Some(runtimes.sortWith(_ < _).drop(runtimes.length / 2).head)
      } else
        getMeasuredRuntimeMedian(jobIdx, layerIdx, tunePointIdx)
    }
  }

  def parseRuntime(runtimeStr: String): Float =
    if (!runtimeStr.equals("")) runtimeStr.toFloat
    else Float.PositiveInfinity

  def isTargetMeasurement(values: Array[String],
                          jobIdx: Int,
                          layerIdx: Int,
                          tunePointIdx: Int): Boolean = {
//    println((jobIdx, layerIdx, tunePointIdx))
//    println(values)
    values(layerColIdx).toInt == layerIdx && {
      val jobAndTuneIdxStr = values(tunepointColIdx)
      val (jobIdxStr, tuneIdxStr) = (
        jobAndTuneIdxStr.split("_")(0),
        jobAndTuneIdxStr.split("_")(1))

      jobIdxStr.toInt == jobIdx &&
      tuneIdxStr.toInt == tunePointIdx

    }
  }

  def convLambdaNDranges(jobId: Int, layerId: Int, tunePoint: Int): Option[((Int, Int, Int), (Int, Int, Int), String)] = {

    val kernelDirPath = jobDirs.get(jobId) match {
      case Some(jobDir) => jobDir
      case None if generatedInJobs =>
        val jobDir = jobFilesDirPath.listFiles.find(f =>
          f.isDirectory && f.getName.startsWith(s"generated_files_${generateLabel}_${jobId}_")) match {
          case Some(path) => path
          case None =>
            throw new IllegalStateException()
        }
        jobDirs += (jobId -> jobDir)
        jobDir

      case None if !generatedInJobs =>
        jobDirs += (jobId -> jobFilesDirPath)
        jobFilesDirPath
    }

    val tunePointKernelDir = new File(kernelDirPath.getAbsolutePath + f"/$layerId/$tunePoint")

    if (!(tunePointKernelDir.exists() && tunePointKernelDir.isDirectory))
      throw new IllegalStateException()

    tunePointKernelDir.listFiles.toList.
      find(f => f.getName.startsWith("kernel_") && f.getName.endsWith(".cl")) match {
      case None =>
        println("WARNING: " + s"No kernel file in ${tunePointKernelDir.getPath}")
        None
      case Some(kernelFile) =>
        val src = Source.fromFile(kernelFile.getPath)
        val kernel = src.mkString
        src.close()

        val kernelHeaderPattern = new Regex(
          """// localSizes\(0\) -> ([0-9]+),\n""" +
            """// localSizes\(1\) -> ([0-9]+),\n""" +
            """// localSizes\(2\) -> ([0-9]+),\n""" +
            """// nWrgs\(0\) -> ([0-9]+),\n""" +
            """// nWrgs\(1\) -> ([0-9]+),\n""" +
            """// nWrgs\(2\) -> ([0-9]+)""", "l0", "l1", "l2", "wrg0", "wrg1", "wrg2")

        kernelHeaderPattern.findFirstMatchIn(kernel) match {
          case None =>
            println("WARNING: " + s"Cannot match header in ${tunePointKernelDir.getPath}")
            None
          case Some(kMatch) =>
            val (l0, l1, l2) = (kMatch.group("l0").toInt, kMatch.group("l1").toInt, kMatch.group("l2").toInt)

            Some(((l0, l1, l2), (
              l0 * kMatch.group("wrg0").toInt,
              l1 * kMatch.group("wrg1").toInt,
              l2 * kMatch.group("wrg2").toInt),
              kernelFile.getPath))
        }
    }
  }

  def readCsv(): Set[(Int, Int, Int)] = {
    if (!cacheLinesCsvFilePath.exists())
      Set()
    else {
      val bufferedSource = io.Source.fromFile(cacheLinesCsvFilePath)
      bufferedSource.getLines.toList.tail.map(line =>
        line.split(",").slice(0, 3).map(_.trim.toInt) match {
          case Array(j: Int, l: Int, t: Int) => (j, l, t)
          case _ => throw new IllegalStateException()
        }
      ).toSet
    }

  }

  def recordToCsv(jobIdx: Int,
                  layerIdx: Int,
                  tunePointIdx: Int,
                  features: Option[(Long, (Int, Int, Int), (Int, Int, Int))]): Unit = {
    if (!cacheLinesCsvFilePath.exists()) {
      val fileWriter = new FileWriter(cacheLinesCsvFilePath.getPath, true)
      fileWriter.write("job,layer,tunePoint,runtime,cacheLines,l0,l1,l2,g0,g1,g2\n")
      fileWriter.close()
    }
    val fileWriter = new FileWriter(cacheLinesCsvFilePath.getPath, true)

    features match {

      case Some((cacheLines, localSizes, globalSizes)) =>
        fileWriter.write("%d,%d,%d,nan,%d,%d,%d,%d,%d,%d,%d\n".format(jobIdx, layerIdx, tunePointIdx, cacheLines,
          localSizes._1, localSizes._2, localSizes._3,
          globalSizes._1, globalSizes._2, globalSizes._3))

      case None =>
        fileWriter.write("%d,%d,%d,nan,nan,nan,nan,nan,nan,nan,nan\n".format(jobIdx, layerIdx, tunePointIdx))
    }
    fileWriter.close()
  }
}

package patterns

import exploration.ParameterRewrite.substituteVars
import ir.ast.{FunDecl, Lambda, Map}
import lift.arithmetic.{Cst, Var}
import opencl.executor.Executor
import org.junit.Assert.assertEquals
import patterns.nn.conv.ConvStencil3D
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DRewriteParams, ConvStencil3DTuneParams}
import patterns.nn.utils.Utils.slidingOutputSize
import rewriting.rules.{NeuralNetRules, OpenCLRules, Rule}

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

package object nn {
  trait LayerParams {
    val paramVector: Vector[Var]
  }
  abstract class LayerConfig extends LayerParams
  abstract class LayerTuneParams extends LayerParams
  abstract class LayerRewriteParams extends LayerParams

  abstract class LayerExpression(layerConfig: LayerConfig,
                                 tuneParams: LayerTuneParams,
                                 rewriteParams: ConvStencil3DRewriteParams)

  trait LayerExpressionFactory {
    // TODO: generalize types
    def apply(layerConfig: ConvStencil3DLayerConfig,
              tuneParams: ConvStencil3DTuneParams,
              rewriteParams: ConvStencil3DRewriteParams,
              fuseLambdas: Boolean, shareKernels: Boolean): Seq[Lambda]
  }

  type Array2D[T] = Array[Array[T]]
  type Array3D[T] = Array[Array[Array[T]]]
  type Array4D[T] = Array[Array[Array[Array[T]]]]
  type Array5D[T] = Array[Array[Array[Array[Array[T]]]]]
  type Array6D[T] = Array[Array[Array[Array[Array[Array[T]]]]]]
  type Array7D[T] = Array[Array[Array[Array[Array[Array[Array[T]]]]]]]



  def group(arr1d: Array[Float], shape: (Int, Int)): Array2D[Float] = {
    val arr2d = Array.fill[Array[Float]](shape._1)(
      Array.fill[Float](shape._2)(0))
    for (i <- 0 until shape._1; j <- 0 until shape._2) {
      arr2d(i)(j) = arr1d(i * shape._2 + j)
    }
    arr2d
  }

  def group(arr1d: Array[Float], shape: (Int, Int, Int)): Array3D[Float] = {
    val arr3d = Array.fill[Array2D[Float]](shape._1)(
      Array.fill[Array[Float]](shape._2)(
        Array.fill[Float](shape._3)(0)))
    for (i <- 0 until shape._1; j <- 0 until shape._2; k <- 0 until shape._3) {
      arr3d(i)(j)(k) = arr1d(i * shape._2 * shape._3 + j * shape._3 + k)
    }
    arr3d
  }

  def group(arr1d: Array[Float], shape: (Int, Int, Int, Int)): Array4D[Float] = {
    val arr5d = Array.fill[Array3D[Float]](shape._1)(
      Array.fill[Array2D[Float]](shape._2)(
        Array.fill[Array[Float]](shape._3)(
          Array.fill[Float](shape._4)(0))))
    for (i <- 0 until shape._1; j <- 0 until shape._2; k <- 0 until shape._3;
         l <- 0 until shape._4) {
      arr5d(i)(j)(k)(l) = arr1d(i * shape._2 * shape._3 * shape._4 +
        j * shape._3 * shape._4 + k * shape._4 + l)
    }
    arr5d
  }

  def group(arr1d: Array[Float], shape: (Int, Int, Int, Int, Int)): Array5D[Float] = {
    val arr5d = Array.fill[Array4D[Float]](shape._1)(
      Array.fill[Array3D[Float]](shape._2)(
        Array.fill[Array2D[Float]](shape._3)(
          Array.fill[Array[Float]](shape._4)(
            Array.fill[Float](shape._5)(0)))))
    for (i <- 0 until shape._1; j <- 0 until shape._2; k <- 0 until shape._3;
         l <- 0 until shape._4; m <- 0 until shape._5) {
      arr5d(i)(j)(k)(l)(m) = arr1d(i * shape._2 * shape._3 * shape._4 * shape._5 +
        j * shape._3 * shape._4 * shape._5 + k * shape._4 * shape._5 + l * shape._5 + m)
    }
    arr5d
  }

  def group(arr1d: Array[Float], shape: (Int, Int, Int, Int, Int, Int)): Array6D[Float] = {
    val arr6d = Array.fill[Array5D[Float]](shape._1)(
      Array.fill[Array4D[Float]](shape._2)(
        Array.fill[Array3D[Float]](shape._3)(
          Array.fill[Array2D[Float]](shape._4)(
            Array.fill[Array[Float]](shape._5)(
              Array.fill[Float](shape._6)(0))))))
    for (i <- 0 until shape._1; j <- 0 until shape._2; k <- 0 until shape._3;
         l <- 0 until shape._4; m <- 0 until shape._5; n <- 0 until shape._6) {
      arr6d(i)(j)(k)(l)(m)(n) = arr1d(i * shape._2 * shape._3 * shape._4 * shape._5 * shape._6 +
        j * shape._3 * shape._4 * shape._5 * shape._6 + k * shape._4 * shape._5 * shape._6 +
        l * shape._5 * shape._6 + m * shape._6 + n)
    }
    arr6d
  }

  def validateResults(layerFactory: ConvStencil3D,
                      layerConfigVars: ConvStencil3DLayerConfig,
                      tuneParamVars: ConvStencil3DTuneParams,
                      substitutionTable: collection.immutable.Map[Var, Cst],
                      liftResult: Array[Float],
                      gold: Array[Array[Array[Array[Float]]]]) = {
    //    val outputWidthHeight = slidingOutputSize(
    //      substituteVars(layerFactory.paddedInputWidthHeight, substitutionTable),
    //      substitutionTable(layerConfigVars.kernelWidthHeight),
    //      substitutionTable(layerConfigVars.kernelStride)).evalInt
    val outputWidthHeight = slidingOutputSize(
      substitutionTable(tuneParamVars.tileWidthHeight),
      substitutionTable(layerConfigVars.kernelWidthHeight),
      substitutionTable(layerConfigVars.kernelStride)).evalInt *
      slidingOutputSize(
        substitutionTable(layerConfigVars.inputWidthHeight) + 2 * substitutionTable(layerConfigVars.padFunc),
        substitutionTable(tuneParamVars.tileWidthHeight),
        substitutionTable(tuneParamVars.tileWidthHeight) -
          (substitutionTable(layerConfigVars.kernelWidthHeight) -
            substitutionTable(layerConfigVars.kernelStride))).evalInt

    val result = patterns.nn.group(liftResult, (substitutionTable(layerConfigVars.nInputs).evalInt,
      substitutionTable(layerConfigVars.kernelChannels).evalInt,
      outputWidthHeight, outputWidthHeight))

    for {(input, inputIdx) <- result.zip(gold).zipWithIndex
         (kernelChannel, kernelIdx) <- input._1.zip(input._2).zipWithIndex
         (row, rowIdx) <- kernelChannel._1.zip(kernelChannel._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex} {
      assertEquals(
        f"result($inputIdx)($kernelIdx)($rowIdx)($elIdx) != " +
          f"gold($inputIdx)($kernelIdx)($rowIdx)($elIdx)",
        el._1, el._2, 0.1)
    }
  }



  case class TuningPoint(tunePointIdx: Int,
                         values: scala.Vector[Cst],
                         rewriteRules: ListMap[String, Rule])

  case class LayerPoints(layerConfigIdx: Int,
                         layerConfig: scala.Vector[Cst],
                         tuningPoints: ListBuffer[TuningPoint])



  def parseCSV(csvPath: String): collection.mutable.Map[Int, LayerPoints] = {

    val params = List(
      "inputChannels", "inputWidthHeight", "padFunc",
      "kernelChannels", "kernelWidthHeight", "kernelStride",
      "nKernelsPerWrg", "nWindowsPerThread", "seqOpsPerThread", "tileWidthHeight", "padOptTotal",
      "reduceSeqUnroll", "vectorise4",
      "l00", "l01", "l02", "g00", "g01", "g02",
      "layer_idx", "tune_point_idx",
      "predicted_rank")

    val bufferedFile = io.Source.fromFile(csvPath)
    val lineIterator = bufferedFile.getLines
    val header = lineIterator.next().split(",").map(_.trim)
    val paramColumns: collection.immutable.Map[String, Int] = params.map(param => param -> header.indexOf(param)).toMap

    val result: collection.mutable.Map[Int, LayerPoints] = collection.mutable.Map[Int, LayerPoints]()

    for (line <- lineIterator) {
      val values = line.split(",").map(_.trim)
      val layerConfigIdx = values(paramColumns("layer_idx")).toInt

      // Add layer
      if (!result.contains(layerConfigIdx))
        result.put(
          layerConfigIdx, LayerPoints(
            layerConfigIdx = layerConfigIdx,
            layerConfig = Vector(
              /* nInputs */ Cst(1),
              /* inputWidthHeight */ Cst(values(paramColumns("inputWidthHeight")).toInt),
              /* inputChannels */ Cst(values(paramColumns("inputChannels")).toInt),
              /* kernelWidthHeight */ Cst(values(paramColumns("kernelWidthHeight")).toInt),
              /* kernelChannels */ Cst(values(paramColumns("kernelChannels")).toInt),
              /* kernelStride */ Cst(values(paramColumns("kernelStride")).toInt),
              /* padFunc */ Cst(values(paramColumns("padFunc")).toInt)),
            tuningPoints = ListBuffer[TuningPoint]()))

      // Add tuning point
      result(layerConfigIdx).tuningPoints += TuningPoint(
        tunePointIdx = values(paramColumns("tune_point_idx")).toInt,
        values = scala.Vector(
          /* tileWidthHeight */ Cst(values(paramColumns("tileWidthHeight")).toInt),
          /* nKernelsPerWrg */ Cst(values(paramColumns("nKernelsPerWrg")).toInt),
          /* seqOpsPerThread */ Cst(values(paramColumns("seqOpsPerThread")).toInt),
          /* nWindowsPerThread */ Cst(values(paramColumns("nWindowsPerThread")).toInt),
          /* padOptTotal */ Cst(values(paramColumns("padOptTotal")).toInt)
        ),
        rewriteRules = {
          var rules: ListBuffer[(String, Rule)] = ListBuffer[(String, Rule)]()

          if (values(paramColumns("reduceSeqUnroll")) == "1")
            rules += (("reduceSeqUnroll", OpenCLRules.reduceSeqUnroll))

          if (values(paramColumns("vectorise4")) == "1")
            rules += (("vectorise4", NeuralNetRules.StencilRules.vectorise4))

          ListMap(rules: _*)
        }
      )
    }

    result
  }

}

package patterns.nn

import exploration.ParameterRewrite
import ir.{ArrayType, Type}
import ir.ast.{PadConstant, Value}
import lift.arithmetic.{Cst, Var}
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir.{Float, id}
import org.junit.Assert.assertEquals
import org.junit.{Test}
import patterns.nn.conv.ConvStencil3D
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DRewriteParams, ConvStencil3DTuneParams}
import patterns.nn.utils.Utils.slidingOutputSize

import scala.util.Random

class TestPadding {
  def AT = ArrayType // alias
  type AT = ArrayType // alias

  val layerConfigVars = new ConvStencil3DLayerConfig()
  val tuneParamVars = new ConvStencil3DTuneParams()
  val rewriteParamVars = new ConvStencil3DRewriteParams()

  @Test
  def testPadding(): Unit = {

    // TODO: this is a long test (20 seconds on integrated Intel GPU, worth reducing it or moving to the long tests)
    val layerConfig: List[Cst] = List(
      /* The biggest VGG layer conv1_2 */
      1,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
      224,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
      64,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
      3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
      64,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
      3,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
      1//padFunc = Var("padFunc", RangeAdd(0, 1, 1)))
    ).map(Cst(_))

    val tuneParams: List[Cst] = List(
      12,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
      1,//val vectorLen: Var = Var("vectorLen"),
      1,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
      288,//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
      169//padOptTotal
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val rewriteParams: List[Cst] = List(
      Cst(1)
    )

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams) ++
        rewriteParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(rewriteParams)).toMap


    /* Non-padded X */
    val nonpaddedInputWidthHeight = substitutionTable(layerConfigVars.inputWidthHeight)

    val nonpaddedXType = AT(AT(AT(AT(Float,
      layerConfigVars.inputChannels),
      nonpaddedInputWidthHeight),
      nonpaddedInputWidthHeight),
      layerConfigVars.nInputs)

    val X: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.nInputs).evalInt,
      nonpaddedInputWidthHeight.evalInt,
      nonpaddedInputWidthHeight.evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())

    /* Padded X */
    val paddedInputWidthHeight = nonpaddedInputWidthHeight + Cst(2) * substitutionTable(layerConfigVars.padFunc) +
      substitutionTable(tuneParamVars.padOptTotal)

    val paddedXType = AT(AT(AT(AT(Float,
      layerConfigVars.inputChannels),
      paddedInputWidthHeight),
      paddedInputWidthHeight),
      layerConfigVars.nInputs)


    val padFactory = new patterns.nn.pad.PadConv(layerConfigVars, tuneParamVars, factory,
      originalSize = layerConfigVars.inputWidthHeight,
      originalType = nonpaddedXType,
      padFunc = substitutionTable(layerConfigVars.padFunc),
      padOptTotal = substitutionTable(tuneParamVars.padOptTotal),
      newType = paddedXType)

    val paddingLambda = ParameterRewrite(padFactory(), substitutionTable)

    val paddingLambdaNDRanges = padFactory.paddingLambdaNDRanges(substitutionTable)


    val (output, _) = Execute(paddingLambdaNDRanges._1, paddingLambdaNDRanges._2, (false, false))[Array[Float]](
      paddingLambda, X)

    val liftResult = patterns.nn.group(output,
      (substitutionTable(layerConfigVars.nInputs).evalInt,
        paddedInputWidthHeight.evalInt,
        paddedInputWidthHeight.evalInt,
        substitutionTable(layerConfigVars.inputChannels).evalInt))


    val gold = PadConstant(0, 0, Value("0", opencl.ir.Float)).eval2d(X,
      padFunc = substitutionTable(layerConfigVars.padFunc).evalInt,
      padOptTotal = substitutionTable(tuneParamVars.padOptTotal).evalInt)


    for {(input, inputIdx) <- liftResult.zip(gold).zipWithIndex
         (row, rowIdx) <- input._1.zip(input._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex
         (ch, chIdx) <- el._1.zip(el._2).zipWithIndex}
      assertEquals(
        f"liftResult($inputIdx)($rowIdx)($elIdx)($chIdx) != " +
          f"gold($inputIdx)($rowIdx)($elIdx)($chIdx)",
        ch._1, ch._2, 0.01)
    println("done")
  }


  @Test
  def testDepadding(): Unit = {

    // TODO: this is a long test (20 seconds on integrated Intel GPU, worth reducing it or moving to the long tests)
    val layerConfig: List[Cst] = List(
      /* The biggest VGG layer conv1_2 */
      1,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
      224,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
      64,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
      3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
      64,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
      3,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
      1//padFunc = Var("padFunc", RangeAdd(0, 1, 1)))
    ).map(Cst(_))

    val tuneParams: List[Cst] = List(
      12,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
      1,//val vectorLen: Var = Var("vectorLen"),
      1,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
      288,//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
      169//padOptTotal
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val rewriteParams: List[Cst] = List(
      Cst(1)
    )

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams) ++
        rewriteParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(rewriteParams)).toMap


    /* Non-padded X */
    val paddedOutputWidthHeight = slidingOutputSize(
        substitutionTable(layerConfigVars.inputWidthHeight) + 2 * substitutionTable(layerConfigVars.padFunc) +
          substitutionTable(tuneParamVars.padOptTotal),
        substitutionTable(layerConfigVars.kernelChannels),
        substitutionTable(layerConfigVars.kernelStride))


    val paddedYType = AT(AT(AT(AT(Float,
      paddedOutputWidthHeight),
      paddedOutputWidthHeight),
      substitutionTable(layerConfigVars.kernelChannels)),
      substitutionTable(layerConfigVars.nInputs))

    val paddedYTypeLengths = Type.getLengths(paddedYType).dropRight(1).map(_.evalInt)

    val paddedY: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      paddedYTypeLengths(0), paddedYTypeLengths(1), paddedYTypeLengths(2), paddedYTypeLengths(3))(
      (_, _, _, _) => Random.nextFloat())

    /* Padded X */
    val depaddedOutputWidthHeight = slidingOutputSize(
      substitutionTable(layerConfigVars.inputWidthHeight) + 2 * substitutionTable(layerConfigVars.padFunc),
      substitutionTable(layerConfigVars.kernelChannels),
      substitutionTable(layerConfigVars.kernelStride))

    val depadSize = paddedOutputWidthHeight - depaddedOutputWidthHeight

    val depaddedYType = AT(AT(AT(AT(Float,
      depaddedOutputWidthHeight),
      depaddedOutputWidthHeight),
      substitutionTable(layerConfigVars.kernelChannels)),
      substitutionTable(layerConfigVars.nInputs))

    val depadFactory = new patterns.nn.pad.DepadConv(layerConfigVars, tuneParamVars, factory,
      originalSize = paddedOutputWidthHeight, originalType = paddedYType,
      depadSize = depadSize, newType = depaddedYType)

    val depaddingLambda = ParameterRewrite(depadFactory(), substitutionTable)

    val depaddingLambdaNDRanges = depadFactory.depaddingLambdaNDRanges(substitutionTable)


    val (output, _) = Execute(depaddingLambdaNDRanges._1, depaddingLambdaNDRanges._2, (false, false))[Array[Float]](
      depaddingLambda, paddedY)

    val liftResult = patterns.nn.group(output,
      (substitutionTable(layerConfigVars.nInputs).evalInt,
        depaddedOutputWidthHeight.evalInt,
        depaddedOutputWidthHeight.evalInt,
        substitutionTable(layerConfigVars.inputChannels).evalInt))


    val gold = PadConstant(0, 0, Value("0", opencl.ir.Float)).evalDepad2d(paddedY,
      substitutionTable(tuneParamVars.padOptTotal).evalInt)


    for {(input, inputIdx) <- liftResult.zip(gold).zipWithIndex
         (row, rowIdx) <- input._1.zip(input._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex
         (ch, chIdx) <- el._1.zip(el._2).zipWithIndex}
      assertEquals(
        f"liftResult($inputIdx)($rowIdx)($elIdx)($chIdx) != " +
          f"gold($inputIdx)($rowIdx)($elIdx)($chIdx)",
        ch._1, ch._2, 0.01)
    println("done")
  }
}

object TestPadding extends TestWithExecutor

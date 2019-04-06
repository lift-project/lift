package patterns.nn

import exploration.ParameterRewrite
import exploration.ParameterRewrite.substituteVars
import ir.ast.{PadConstant, Value}
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.executor.{Execute, Executor}
import org.junit.Assert.assertEquals
import org.junit.{BeforeClass, Test}
import patterns.nn.conv.ConvStencil3D
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}
import opencl.ir.id

import scala.util.Random

class TestPadding {

  val layerConfigVars = new ConvStencil3DLayerConfig()
  val tuneParamVars = new ConvStencil3DTuneParams()

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
      169//padOpt
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars)
    factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams)).toMap

    val paddedInputWidthHeight = ArithExpr.substitute(factory.paddedInputWidthHeight.get, substitutionTable.toMap)

    val X: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.nInputs).evalInt,
      substitutionTable(layerConfigVars.inputWidthHeight).evalInt,
      substitutionTable(layerConfigVars.inputWidthHeight).evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())

    val padFactory = new patterns.nn.pad.PadConv(layerConfigVars, tuneParamVars, factory,
      layerConfigVars.inputWidthHeight,
      substitutionTable(layerConfigVars.padFunc) + substitutionTable(tuneParamVars.padOpt))

    val paddingLambda = ParameterRewrite(padFactory(),
      substitutionTable)

    val paddingLambdaNDRanges = padFactory.paddingLambdaNDRanges(substitutionTable)


    val (output, _) = Execute(paddingLambdaNDRanges._1, paddingLambdaNDRanges._2, (false, false))[Array[Float]](paddingLambda, X)

    val liftResult = nn.group(output,
      (substitutionTable(layerConfigVars.nInputs).evalInt,
        paddedInputWidthHeight.evalInt,
        paddedInputWidthHeight.evalInt,
        substitutionTable(layerConfigVars.inputChannels).evalInt))


    val gold = PadConstant(0, 0, Value("0", opencl.ir.Float)).eval2d(X,
      substitutionTable(layerConfigVars.padFunc).evalInt +
        substitutionTable(tuneParamVars.padOpt).evalInt)


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
      169//padOpt
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars)
    factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams)).toMap

    val paddedInputWidthHeight = ArithExpr.substitute(factory.paddedInputWidthHeight.get, substitutionTable.toMap)

    val nonPaddedX: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.nInputs).evalInt,
      substitutionTable(layerConfigVars.inputWidthHeight).evalInt + 2 * substitutionTable(layerConfigVars.padFunc).evalInt,
      substitutionTable(layerConfigVars.inputWidthHeight).evalInt + 2 * substitutionTable(layerConfigVars.padFunc).evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())

    val paddedX = PadConstant(0, 0, Value("0", opencl.ir.Float)).eval2d(nonPaddedX,
      substitutionTable(tuneParamVars.padOpt).evalInt)

    val depadFactory = new patterns.nn.pad.PadConv(layerConfigVars, tuneParamVars, factory, paddedInputWidthHeight,
      Cst(-1) * substitutionTable(tuneParamVars.padOpt))

    val depaddingLambda = ParameterRewrite(depadFactory(), substitutionTable)

    val depaddingLambdaNDRanges = depadFactory.paddingLambdaNDRanges(substitutionTable)


    val (output, _) = Execute(depaddingLambdaNDRanges._1, depaddingLambdaNDRanges._2, (false, false))[Array[Float]](
      depaddingLambda, paddedX)

    val liftResult = nn.group(output,
      (substitutionTable(layerConfigVars.nInputs).evalInt,
        substitutionTable(layerConfigVars.inputWidthHeight).evalInt + 2 * substitutionTable(layerConfigVars.padFunc).evalInt,
        substitutionTable(layerConfigVars.inputWidthHeight).evalInt + 2 * substitutionTable(layerConfigVars.padFunc).evalInt,
        substitutionTable(layerConfigVars.inputChannels).evalInt))


    val gold = PadConstant(0, 0, Value("0", opencl.ir.Float)).evalDepad2d(paddedX,
      (-1) * substitutionTable(tuneParamVars.padOpt).evalInt)


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

object TestPadding {
  @BeforeClass
  def before(): Unit = {
    Executor.loadAndInit()
    Random.setSeed(0)
  }
}

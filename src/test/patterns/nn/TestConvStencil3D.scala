package patterns.nn

import exploration.ParameterRewrite
import exploration.ParameterRewrite.substituteVars
import ir.Type
import lift.arithmetic.{ArithExpr, Cst, Var}
import opencl.executor.{Compile, Execute, TestWithExecutor}
import opencl.ir.id
import org.junit.Assert.assertEquals
import org.junit.{Ignore, Test}
import patterns.nn.conv.ConvStencil3D
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DRewriteParams, ConvStencil3DTuneParams}
import patterns.nn.utils.Utils.slidingOutputSize

import scala.util.Random

class TestConvStencil3D {
  val example_X: Array[Array[Array[Array[Float]]]] = /*[n_inputs_SV][input_ydim_SV][input_xdim_SV][in_channels_SV]*/
    Array(
      Array(
        Array(Array(0.0f, 0.0f), Array(1.0f, 1.0f), Array(2.0f, 2.0f), Array(3.0f, 3.0f),
          Array(4.0f, 4.0f), Array(5.0f, 5.0f), Array(6.0f, 6.0f), Array(7.0f, 7.0f)),
        Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
          Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
        Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
          Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
        Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
          Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
        Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
          Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
        Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
          Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
        Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
          Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
        Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
          Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f))),
      Array(
        Array(Array(0.0f, 0.0f),   Array(1.0f, 1.0f),   Array(2.0f, 2.0f),   Array(3.0f, 3.0f),
          Array(4.0f, 4.0f),   Array(5.0f, 5.0f),   Array(6.0f, 6.0f),  Array(7.0f, 7.0f)),
        Array(Array(8.0f, 8.0f),   Array(9.0f, 9.0f),   Array(10.0f, 10.0f), Array(11.0f, 11.0f),
          Array(12.0f, 12.0f), Array(13.0f, 13.0f), Array(14.0f, 14.0f), Array(15.0f, 15.0f)),
        Array(Array(16.0f, 16.0f), Array(17.0f, 17.0f), Array(18.0f, 18.0f), Array(19.0f, 19.0f),
          Array(20.0f, 20.0f), Array(21.0f, 21.0f), Array(22.0f, 22.0f), Array(23.0f, 23.0f)),
        Array(Array(24.0f, 24.0f), Array(25.0f, 25.0f), Array(26.0f, 26.0f), Array(27.0f, 27.0f),
          Array(28.0f, 28.0f), Array(29.0f, 29.0f), Array(30.0f, 30.0f), Array(31.0f, 31.0f)),
        Array(Array(32.0f, 32.0f), Array(33.0f, 33.0f), Array(34.0f, 34.0f), Array(35.0f, 35.0f),
          Array(36.0f, 36.0f), Array(37.0f, 37.0f), Array(38.0f, 38.0f), Array(39.0f, 39.0f)),
        Array(Array(40.0f, 40.0f), Array(41.0f, 41.0f), Array(42.0f, 42.0f), Array(43.0f, 43.0f),
          Array(44.0f, 44.0f), Array(45.0f, 45.0f), Array(46.0f, 46.0f), Array(47.0f, 47.0f)),
        Array(Array(48.0f, 48.0f), Array(49.0f, 49.0f), Array(50.0f, 50.0f), Array(51.0f, 51.0f),
          Array(52.0f, 52.0f), Array(53.0f, 53.0f), Array(54.0f, 54.0f), Array(55.0f, 55.0f)),
        Array(Array(56.0f, 56.0f), Array(57.0f, 57.0f), Array(58.0f, 58.0f), Array(59.0f, 59.0f),
          Array(60.0f, 60.0f), Array(61.0f, 61.0f), Array(62.0f, 62.0f), Array(63.0f, 63.0f))))

  val example_B: Array[Float] = Array(0.0f, 1.0f, 2.0f)

  val example_K: Array[Array[Array[Array[Float]]]] = /*[out_channels_SV][kernel_ydim_SV][kernel_xdim_SV][in_channels_SV]*/
    Array(
      Array(
        Array(Array(1f, 0f), Array(3f, 0f), Array(5f, 0f)),
        Array(Array(7f, 0f), Array(9f, 0f), Array(11f, 0f)),
        Array(Array(13f, 0f), Array(15f, 0f), Array(17f, 0f))),
      Array(
        Array(Array(0f, 1f), Array(0f, 3f), Array(0f, 5f)),
        Array(Array(0f, 7f), Array(0f, 9f), Array(0f, 11f)),
        Array(Array(0f, 13f), Array(0f, 15f), Array(0f, 17f))),
      Array(
        Array(Array(1f, 0f), Array(3f, 0f), Array(5f, 0f)),
        Array(Array(7f, 0f), Array(9f, 0f), Array(11f, 0f)),
        Array(Array(13f, 0f), Array(15f, 0f), Array(17f, 0f))))

  val goldExample: Array[Array[Array[Array[Float]]]] = //[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)]
    Array(
      Array(
        Array(
          Array(1029f, 1110f, 1191f, 1272f, 1353f, 1434f),
          Array(1677f, 1758f, 1839f, 1920f, 2001f, 2082f),
          Array(2325f, 2406f, 2487f, 2568f, 2649f, 2730f),
          Array(2973f, 3054f, 3135f, 3216f, 3297f, 3378f),
          Array(3621f, 3702f, 3783f, 3864f, 3945f, 4026f),
          Array(4269f, 4350f, 4431f, 4512f, 4593f, 4674)),
        Array(
          Array(1030f, 1111f, 1192f, 1273f, 1354f, 1435f),
          Array(1678f, 1759f, 1840f, 1921f, 2002f, 2083f),
          Array(2326f, 2407f, 2488f, 2569f, 2650f, 2731f),
          Array(2974f, 3055f, 3136f, 3217f, 3298f, 3379f),
          Array(3622f, 3703f, 3784f, 3865f, 3946f, 4027f),
          Array(4270f, 4351f, 4432f, 4513f, 4594f, 4675)),
        Array(
          Array(1031f, 1112f, 1193f, 1274f, 1355f, 1436f),
          Array(1679f, 1760f, 1841f, 1922f, 2003f, 2084f),
          Array(2327f, 2408f, 2489f, 2570f, 2651f, 2732f),
          Array(2975f, 3056f, 3137f, 3218f, 3299f, 3380f),
          Array(3623f, 3704f, 3785f, 3866f, 3947f, 4028f),
          Array(4271f, 4352f, 4433f, 4514f, 4595f, 4676))),
      Array(
        Array(
          Array(1029f, 1110f, 1191f, 1272f, 1353f, 1434f),
          Array(1677f, 1758f, 1839f, 1920f, 2001f, 2082f),
          Array(2325f, 2406f, 2487f, 2568f, 2649f, 2730f),
          Array(2973f, 3054f, 3135f, 3216f, 3297f, 3378f),
          Array(3621f, 3702f, 3783f, 3864f, 3945f, 4026f),
          Array(4269f, 4350f, 4431f, 4512f, 4593f, 4674)),
        Array(
          Array(1030f, 1111f, 1192f, 1273f, 1354f, 1435f),
          Array(1678f, 1759f, 1840f, 1921f, 2002f, 2083f),
          Array(2326f, 2407f, 2488f, 2569f, 2650f, 2731f),
          Array(2974f, 3055f, 3136f, 3217f, 3298f, 3379f),
          Array(3622f, 3703f, 3784f, 3865f, 3946f, 4027f),
          Array(4270f, 4351f, 4432f, 4513f, 4594f, 4675)),
        Array(
          Array(1031f, 1112f, 1193f, 1274f, 1355f, 1436f),
          Array(1679f, 1760f, 1841f, 1922f, 2003f, 2084f),
          Array(2327f, 2408f, 2489f, 2570f, 2651f, 2732f),
          Array(2975f, 3056f, 3137f, 3218f, 3299f, 3380f),
          Array(3623f, 3704f, 3785f, 3866f, 3947f, 4028f),
          Array(4271f, 4352f, 4433f, 4514f, 4595f, 4676))))

  val layerConfigVars = new ConvStencil3DLayerConfig()
  val layerConfigExample: List[Cst] = List(
    2,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
    8,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
    2,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
    3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
    3,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
    1,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
    0//padFunc = Var("padWidthHeight", RangeAdd(0, 1, 1)))
  ).map(Cst(_))

  val tuneParamVars = new ConvStencil3DTuneParams()
  val tuneParamsExample: List[Cst] = List(
    8,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
    1,//val vectorLen: Var = Var("vectorLen"),
    1,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
    1,//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
    0//padOptTotal
    //val coalesce: Boolean = false,
    //val unrollReduce: Boolean = false)
  ).map(Cst(_))

  val rewriteParamVars = new ConvStencil3DRewriteParams()
  val rewriteParamVarsExamples: List[Cst] = List(
    Cst(1)
  )

  val substitutionTableExample: Map[Var, Cst] =
    (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfigExample) ++
      tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParamsExample) ++
      rewriteParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(rewriteParamVarsExamples)).toMap

  @Ignore
  @Test
  def testScalaConvSanityCheck(): Unit = {
    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    val lambdas = factory.apply(id)

    val result: Array[Array[Array[Array[Float]]]] =
      factory.eval(example_K, example_B,
        example_X, substitutionTableExample)

    //[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)]
    for {(input, inputIdx) <- result.zip(goldExample).zipWithIndex
         (kernelChannel, kernelIdx) <- input._1.zip(input._2).zipWithIndex
         (row, rowIdx) <- kernelChannel._1.zip(kernelChannel._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex}
      assertEquals(
        f"result($inputIdx)($kernelIdx)($rowIdx)($elIdx) != " +
        f"gold($inputIdx)($kernelIdx)($rowIdx)($elIdx)",
        el._1, el._2, 0)
  }


  @Ignore
  @Test
  def testBothLsSanityCheck(): Unit = {
    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    val lambdas = factory.apply(id)

    val concreteLambda1 = ParameterRewrite(lambdas.head, substitutionTableExample)
    val concreteLambda2 = ParameterRewrite(lambdas(1), substitutionTableExample)

    val partReducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(factory.partReducedXType, substitutionTableExample.toMap)).dropRight(1).map(_.evalInt)

    val (output1, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda1, example_K, example_X)
    val (output2, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda2, example_B,
      patterns.nn.group(output1, (partReducedXTypeLengths(0), partReducedXTypeLengths(1), partReducedXTypeLengths(2),
        partReducedXTypeLengths(3), partReducedXTypeLengths(4))))

    validateResults(
      factory,
      layerConfigVars,
      tuneParamVars,
      substitutionTableExample,
      output2,
      goldExample)
  }


  @Ignore
  @Test
  def test1stLSanityCheck(): Unit = {
    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    val lambdas = factory.apply(id)

    val concreteLambda1 = ParameterRewrite(lambdas.head, substitutionTableExample)
    val concreteLambda2 = ParameterRewrite(lambdas(1), substitutionTableExample)

    val partReducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(factory.partReducedXType,
        substitutionTableExample.asInstanceOf[Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val (output1, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda1, example_K, example_X)

    val output1ND: Array5D[Float] = patterns.nn.group(output1,
      (partReducedXTypeLengths(0), partReducedXTypeLengths(1), partReducedXTypeLengths(2),
      partReducedXTypeLengths(3), partReducedXTypeLengths(4)))

    val result = factory.evalFinalLambda(example_B, output1ND, substitutionTableExample)

    val outputWidthHeight = slidingOutputSize(
      substituteVars(factory.paddedInputWidthHeight, substitutionTableExample),
      substitutionTableExample(layerConfigVars.kernelWidthHeight),
      substitutionTableExample(layerConfigVars.kernelStride)).evalInt

    for {(input, inputIdx) <- result.zip(goldExample).zipWithIndex
         (kernelChannel, kernelIdx) <- input._1.zip(input._2).zipWithIndex
         (row, rowIdx) <- kernelChannel._1.zip(kernelChannel._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex}
      assertEquals(
        f"result($inputIdx)($kernelIdx)($rowIdx)($elIdx) != " +
          f"gold($inputIdx)($kernelIdx)($rowIdx)($elIdx)",
        el._1, el._2, 0)
  }



  def test1stLAgainstScalaVersion(layerConfig: List[Cst], tuneParams: List[Cst], rewriteParams: List[Cst]): Unit = {
    Random.setSeed(0)

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    val lambdas = factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams) ++
        rewriteParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(rewriteParams)).toMap

    val (k, b, x) = patterns.nn.conv.generateTestData(factory, layerConfigVars, substitutionTable,
      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt)

//    val K: Array4D[Float] = Array.tabulate(
//      substitutionTable(layerConfigVars.kernelChannels).evalInt,
//      substitutionTable(layerConfigVars.kernelWidthHeight).evalInt,
//      substitutionTable(layerConfigVars.kernelWidthHeight).evalInt,
//      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())
//    val B: Array[Float] = Array.tabulate(
//      substitutionTable(layerConfigVars.kernelChannels).evalInt)(_ => Random.nextFloat())
//    val X: Array4D[Float] = Array.tabulate(
//      substitutionTable(layerConfigVars.nInputs).evalInt,
//      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
//      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
//      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())


    val concreteLambda1 = ParameterRewrite(lambdas.head, substitutionTable)
//    val concreteLambda2 = ParameterRewrite(lambdas(1), substitutionTable)

    val partReducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(factory.partReducedXType,
        substitutionTable.asInstanceOf[Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val kernel1 = Compile(concreteLambda1)
    val (output1, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda1, k, x)

    val output1ND = patterns.nn.group(output1, (partReducedXTypeLengths(0), partReducedXTypeLengths(1), partReducedXTypeLengths(2),
      partReducedXTypeLengths(3), partReducedXTypeLengths(4)))

    val result = factory.evalFinalLambda(b, output1ND, substitutionTable)

    val outputWidthHeight = slidingOutputSize(
      substitutionTable(tuneParamVars.tileWidthHeight),
      substitutionTable(layerConfigVars.kernelWidthHeight),
      substitutionTable(layerConfigVars.kernelStride)).evalInt *
      slidingOutputSize(
        substituteVars(factory.paddedInputWidthHeight, substitutionTable),
        substitutionTable(tuneParamVars.tileWidthHeight),
        substitutionTable(tuneParamVars.tileWidthHeight) -
          (substitutionTable(layerConfigVars.kernelWidthHeight) -
            substitutionTable(layerConfigVars.kernelStride))).evalInt

//    val result = patterns.nn.group(output2, (substitutionTable(layerConfigVars.nInputs).evalInt,
//      substitutionTable(layerConfigVars.kernelChannels).evalInt,
//      outputWidthHeight, outputWidthHeight))

    val gold = factory.eval(k, b, x, substitutionTable)

    for {(input, inputIdx) <- result.zip(gold).zipWithIndex
         (kernelChannel, kernelIdx) <- input._1.zip(input._2).zipWithIndex
         (row, rowIdx) <- kernelChannel._1.zip(kernelChannel._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex}
      assertEquals(
        f"result($inputIdx)($kernelIdx)($rowIdx)($elIdx) != " +
          f"gold($inputIdx)($kernelIdx)($rowIdx)($elIdx)",
        el._1, el._2, 0.01)
    println("done")
  }


  def testBothLsAgainstScalaVersion(layerConfig: List[Cst], tuneParams: List[Cst], rewriteParams: List[Cst]): Unit = {
    Random.setSeed(0)

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    val lambdas = factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams) ++
        rewriteParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(rewriteParams)).toMap


    val K: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.kernelChannels).evalInt,
      substitutionTable(layerConfigVars.kernelWidthHeight).evalInt,
      substitutionTable(layerConfigVars.kernelWidthHeight).evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())
    val B: Array[Float] = Array.tabulate(
      substitutionTable(layerConfigVars.kernelChannels).evalInt)(_ => Random.nextFloat())
    val X: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.nInputs).evalInt,
      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())


    val concreteLambda1 = ParameterRewrite(lambdas.head, substitutionTable)
    val concreteLambda2 = ParameterRewrite(lambdas(1), substitutionTable)

    val partReducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(factory.partReducedXType,
        substitutionTable.asInstanceOf[Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val kernel1 = Compile(concreteLambda1)
    val (output1, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda1, K, X)

    val output1ND = patterns.nn.group(output1, (partReducedXTypeLengths(0), partReducedXTypeLengths(1), partReducedXTypeLengths(2),
      partReducedXTypeLengths(3), partReducedXTypeLengths(4)))
    val kernel2 = Compile(concreteLambda2)
    val (output2, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda2, B, output1ND)

    val outputWidthHeight = slidingOutputSize(
      substitutionTable(tuneParamVars.tileWidthHeight),
      substitutionTable(layerConfigVars.kernelWidthHeight),
      substitutionTable(layerConfigVars.kernelStride)).evalInt *
      slidingOutputSize(
        substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
        substitutionTable(tuneParamVars.tileWidthHeight),
        substitutionTable(tuneParamVars.tileWidthHeight) -
          (substitutionTable(layerConfigVars.kernelWidthHeight) -
            substitutionTable(layerConfigVars.kernelStride))).evalInt


    val gold = factory.eval(K, B, X, substitutionTable)

    validateResults(
      factory,
      layerConfigVars,
      tuneParamVars,
      substitutionTable,
      output2,
      gold)

    println("done")
  }


  def testBothLsAndScalaAgainstScalaVersion(layerConfig: List[Cst], tuneParams: List[Cst], rewriteParams: List[Cst]):
  Unit = {
    Random.setSeed(0)

    val factory = new ConvStencil3D(layerConfigVars, tuneParamVars, rewriteParamVars, false, false)
    val lambdas = factory.apply(id)

    val substitutionTable: Map[Var, Cst] =
      (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
        tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams) ++
        rewriteParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(rewriteParams)).toMap


    val K: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.kernelChannels).evalInt,
      substitutionTable(layerConfigVars.kernelWidthHeight).evalInt,
      substitutionTable(layerConfigVars.kernelWidthHeight).evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())
    val B: Array[Float] = Array.tabulate(
      substitutionTable(layerConfigVars.kernelChannels).evalInt)(_ => Random.nextFloat())
    val X: Array[Array[Array[Array[Float]]]] = Array.tabulate(
      substitutionTable(layerConfigVars.nInputs).evalInt,
      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
      substituteVars(factory.paddedInputWidthHeight, substitutionTable).evalInt,
      substitutionTable(layerConfigVars.inputChannels).evalInt)((_, _, _, _) => Random.nextFloat())


    val concreteLambda1 = ParameterRewrite(lambdas.head, substitutionTable)
    val concreteLambda2 = ParameterRewrite(lambdas(1), substitutionTable)

    val partReducedXTypeLengths: Seq[Int] = Type.getLengths(
      Type.substitute(factory.partReducedXType,
        substitutionTable.asInstanceOf[Map[ArithExpr, ArithExpr]])).dropRight(1).map(_.evalInt)

    val kernel1 = Compile(concreteLambda1)
    val (output1, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda1, K, X)

    val output1ND = patterns.nn.group(output1, (partReducedXTypeLengths(0), partReducedXTypeLengths(1), partReducedXTypeLengths(2),
      partReducedXTypeLengths(3), partReducedXTypeLengths(4)))
    val kernel2 = Compile(concreteLambda2)
    val (output2, _) = Execute(1, 1, 1, 32, 32, 32, (false, false))[Array[Float]](concreteLambda2, B, output1ND)


    val gold = factory.eval(K, B, X, substitutionTable)

    val scalaResult = factory.evalFinalLambda(B, output1ND, substitutionTable)

    validateResults(
      factory,
      layerConfigVars,
      tuneParamVars,
      substitutionTable,
      output2,
      scalaResult)

    validateResults(
      factory,
      layerConfigVars,
      tuneParamVars,
      substitutionTable,
      output2,
      gold)

    println("done")
  }


  @Ignore
  @Test
  def test1stLOnBiggerExample(): Unit = {
    // TODO: this is a long test (20 seconds on integrated Intel GPU, worth reducing it or moving to the long tests)
    val layerConfig: List[Cst] = List(
      /* The biggest VGG layer conv1_2 */
      1,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
      224,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
      64,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
      3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
      64,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
      3,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
      2//padFunc = Var("padFunc", RangeAdd(0, 1, 1)))
    ).map(Cst(_))

    val tuneParams: List[Cst] = List(
      226,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
      1,//val vectorLen: Var = Var("vectorLen"),
      8,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
      32,//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
      0//padOptTotal
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val rewriteParams: List[Cst] = List(
      Cst(1)
    )

    test1stLAgainstScalaVersion(layerConfig, tuneParams, rewriteParams)
  }

  @Ignore
  @Test
  def testBothLsAndScalaOnBiggerExample(): Unit = {
    // TODO: this is a long test (20 seconds on integrated Intel GPU, worth reducing it or moving to the long tests)
    val layerConfig: List[Cst] = List(
      /* The biggest VGG layer conv1_2 */
      1,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
      224,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
      64,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
      3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
      64,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
      3,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
      2//padFunc = Var("padFunc", RangeAdd(0, 1, 1)))
    ).map(Cst(_))

    val tuneParams: List[Cst] = List(
      226,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
      1,//val vectorLen: Var = Var("vectorLen"),
      8,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
      32,//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
      0//padOptTotal
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val rewriteParams: List[Cst] = List(
      Cst(1)
    )

    testBothLsAndScalaAgainstScalaVersion(layerConfig, tuneParams, rewriteParams)
  }

  @Ignore
  @Test
  def testBothLsOnBiggerExample(): Unit = {
    // TODO: this is a long test (20 seconds on integrated Intel GPU, worth reducing it or moving to the long tests)
    val layerConfig: List[Cst] = List(
      /* The biggest VGG layer conv1_2 */
      1,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
      224,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
      64,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
      3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
      64,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
      3,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
      2//padFunc = Var("padFunc", RangeAdd(0, 1, 1)))
    ).map(Cst(_))

    val tuneParams: List[Cst] = List(
      226,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
      1,//val vectorLen: Var = Var("vectorLen"),
      8,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
      32,//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
      0//padOptTotal
      // val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val rewriteParams: List[Cst] = List(
      Cst(1)
    )

    testBothLsAgainstScalaVersion(layerConfig, tuneParams, rewriteParams)
  }
}

object TestConvStencil3D extends TestWithExecutor
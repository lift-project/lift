package patterns.nn

import lift.arithmetic.{Cst, Var}
import org.junit.Test
import org.junit.Assert.{assertArrayEquals, assertEquals}
import patterns.nn.conv.ConvStencil3D
import patterns.nn.conv.ConvStencil3D.{ConvStencil3DLayerConfig, ConvStencil3DTuneParams}

class TestConvStencil3D {

  val input_X: Array[Array[Array[Array[Float]]]] = /*[n_inputs_SV][input_ydim_SV][input_xdim_SV][in_channels_SV]*/
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

  val input_B: Array[Float] = Array(0.0f, 1.0f, 2.0f)

  val input_K: Array[Array[Array[Array[Float]]]] = /*[out_channels_SV][kernel_ydim_SV][kernel_xdim_SV][in_channels_SV]*/
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

  val gold: Array[Array[Array[Array[Float]]]] = //[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)]
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


  @Test
  def testScalaConv(): Unit = {
    val layerConfigVars = new ConvStencil3DLayerConfig()
    val layerConfig: List[Cst] = List(
      2,//nInputs = Var("nInputs", RangeMul(1, 256, mul = 2)),
      8,//inputWidthHeight = Var("inputWidthHeight", RangeAdd(2, 224, step = 2)),
      2,//inputChannels = Var("inputChannels", RangeMul(1, 2048, 2)),
      3,//kernelWidthHeight = Var("kernelWidthHeight", RangeAdd(1, 16, 1)),
      3,//kernelChannels = Var("kernelChannels", RangeMul(1, 2048, mul = 2)),
      1,//kernelStride = Var("kernelStride", RangeAdd(1, 4, 1)),
      0//padWidthHeight = Var("padWidthHeight", RangeAdd(0, 1, 1)))
    ).map(Cst(_))

    val tuneParamVars = new ConvStencil3DTuneParams()
    val tuneParams: List[Cst] = List(
      8,//val tileWidthHeight: Var = Var("tileWidthHeight"),//(kernelWidthHeightTmp - kernelStrideTmp) + tileStrideTmp,
      1,//val vectorLen: Var = Var("vectorLen"),
      1,//val nKernelsPerWrg: Var = Var("nKernelsPerWrg"),
      1//val seqOpsPerThread: Var = Var("seqOpsPerThread"),
      //val coalesce: Boolean = false,
      //val unrollReduce: Boolean = false)
    ).map(Cst(_))

    val substitutionTable: Map[Var, Cst] = (layerConfigVars.paramVector.filter(_.isInstanceOf[Var]).zip(layerConfig) ++
      tuneParamVars.paramVector.filter(_.isInstanceOf[Var]).zip(tuneParams)).toMap

    val result: Array[Array[Array[Array[Float]]]] =
      new ConvStencil3D(layerConfigVars, tuneParamVars).eval(input_K, input_B, Array(input_X), substitutionTable)(0)
    //[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)]
    for {(input, inputIdx) <- result.zip(gold).zipWithIndex
         (kernelChannel, kernelIdx) <- input._1.zip(input._2).zipWithIndex
         (row, rowIdx) <- kernelChannel._1.zip(kernelChannel._2).zipWithIndex
         (el, elIdx) <- row._1.zip(row._2).zipWithIndex}
      assertEquals(
        f"result($inputIdx)($kernelIdx)($rowIdx)($elIdx) != " +
        f"gold($inputIdx)($kernelIdx)($rowIdx)($elIdx)",
        el._1, el._2, 0)
  }

}

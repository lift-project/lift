package opencl.generator.stencil

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Ignore, Test}
import rewriting.SimplifyAndFuse

import scala.collection.immutable
import scala.io.Source
import scala.util.Random

object TestStencilRodinia {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

object HotSpotConstants {

  val localDimX = 8
  val localDimY = 8
  val localDimZ = 4

  val t_chip = 0.0005f
  val chip_height = 0.016f
  val chip_width = 0.016f

  val FACTOR_CHIP = 0.5f
  val SPEC_HEAT_SI = 1750000
  val K_SI = 100
  val MAX_PD = 3000000
  val PRECISION = 0.001f

  val dx = chip_height / localDimX
  val dy = chip_width / localDimY
  val dz = t_chip / localDimZ

  val Cap = FACTOR_CHIP * SPEC_HEAT_SI * t_chip * dx * dy
  val Rx = dy / (2.0f * K_SI * t_chip * dx)
  val Ry = dx / (2.0f * K_SI * t_chip * dy)
  val Rz = dz / (K_SI * dx * dy)

  val max_slope = MAX_PD / (FACTOR_CHIP * t_chip * SPEC_HEAT_SI)
  val dt = PRECISION / max_slope

  val stepDivCap = dt / Cap
  val ce = stepDivCap / Rx
  val cn = stepDivCap / Ry
  val ct = stepDivCap / Rz
  val cw = ce
  val cs = cn
  val cb = ct

  val cc = 1.0f - (2.0f * ce + 2.0f * cn + 3.0f * ct)




}

class TestStencilRodinia {

  /* **********************************************************
     RODINIA HOTSPOT
 ***********************************************************/
  def test = UserFun("hotspot", Array("power", "top", "bottom", "left", "right", "center"), """
      |#define MAX_PD  (3.0e6)
      |#define PRECISION   0.001
      |#define SPEC_HEAT_SI 1.75e6
      |#define K_SI 100
      |#define FACTOR_CHIP 0.5
      |
      |    /* chip parameters  */
      |    const float t_chip = 0.0005f;
      |    const float chip_height = 0.016f;
      |    const float chip_width = 0.016f;
      |    /* ambient temperature, assuming no package at all  */
      |    const float amb_temp = 80.0f;
      |
      |    float row = 512.0f;
      |    float col = 512.0f;
      |
      |    float grid_height = chip_height / row;
      |    float grid_width = chip_width / col;
      |
      |    float Cap = FACTOR_CHIP * SPEC_HEAT_SI * t_chip * grid_width * grid_height;
      |    float Rx = grid_width / (2.0 * K_SI * t_chip * grid_height);
      |    float Ry = grid_height / (2.0 * K_SI * t_chip * grid_width);
      |    float Rz = t_chip / (K_SI * grid_height * grid_width);
      |
      |    float max_slope = MAX_PD / (FACTOR_CHIP * t_chip * SPEC_HEAT_SI);
      |    float stepl = PRECISION / max_slope;
      |
      |    float step_div_Cap=stepl/Cap;
      |    float Rx_1=1/Rx;
      |    float Ry_1=1/Ry;
      |    float Rz_1=1/Rz;
      |
      |    return center +
      |       step_div_Cap * (power + (bottom + top - 2.0f * center) * Ry_1 +
      |               (right + left - 2.0f * center) * Rx_1 + (amb_temp - center) * Rz_1);
      |
    """.stripMargin, Seq(Float, Float, Float, Float, Float, Float), Float)


  @Test def rodiniaHotspot(): Unit = {

    LongTestsEnabled()
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    // p = powerValue, t = heatNbh; userfun has to compute:
    // out = t[0,0] + c(p + y(t[-1,0] + t[1,0] - 2t[0,0]) +
    //                      x(t[0,-1] + t[0,1] - 2t[0,0]) +
    //                      z(a - t[0,0]));
    // a, c, x, y, z are constants
    val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)

    // the two outermost dimensions of A and B have to be the same
    // zip matrices elementwise

    // create 16x16 heat tile and 14x14 power tile
    val createTiles = \((heat, power) =>
      Zip2D(Slide2D(16, 14) o Pad2D(1, 1, Pad.Boundary.Clamp) $ heat,
        Slide2D(14, 14) $ power)
    )

    // load into local memory and prepare data for single work-item
    // < < coeff, heat> :: [9] , power >
    val prepareData = \((coeff, tiles) =>
      Zip2D(
        // first component;
        Map(Map(\(heatNbh =>
          Zip(coeff, Join() $ heatNbh)
        ))) o Slide2D(3, 1) o
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._0,
        // second component
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._1)
    )

    // how to compute output using required data:
    val stencil = fun(requiredData => {
      val coeffHeatTuple = requiredData._0
      val powerValue = requiredData._1

      toGlobal(MapSeq(id)) o
        MapSeq(\(x => addAmbientTemp(powerValue, x))) o
        ReduceSeqUnroll(\((acc, next) =>
          multAndSumUp(acc, next._0, next._1)), 0.0f) $ coeffHeatTuple
    })

    val rodinia = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      //ArrayType(ArrayType(Float, 8204), 8204),
      //ArrayType(ArrayType(Float, 8204), 8204),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(\(tiles =>
          MapLcl(1)(MapLcl(0)(stencil)) o prepareData(coeff) $ tiles)
        )) $ createTiles(heat, power)

      }
    )
    /*
    val stencil = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(
          fun(tiles => {
            val powerTile = Get(tiles, 0)
            val heatTile = Get(tiles, 1)
            MapLcl(1)(MapLcl(0)(
              fun(nbhs => {
                val powerValue = Get(nbhs, 0) // Float
                val heatNbh = Get(nbhs, 1)    // [[Float]_3]_3
                toGlobal(MapSeq(id)) o
                  MapSeq( \(x => addAmbientTemp(powerValue, x))) o
                  ReduceSeqUnroll(\((acc, next) =>
                    multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() $ heatNbh, coeff)
              })
            )) o
              Split(14) $
              Zip(Join() o toLocal(MapLcl(1)(MapLcl(0)(id))) $ powerTile,
                  Join() o Slide2D(3,1,3,1) o
                           toLocal(MapLcl(1)(MapLcl(0)(id))) $ heatTile)
          })
        )) o
          Split(74) $
          Zip(Join() o Slide2D(14,14) $ power,
              Join() o Slide2D(16,14) o
                Pad2D(1,1,Pad.Boundary.Wrap) $ heat) //actually its mirror
      }
    )
    */

    val heat = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    val power = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    //val heat = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    //val power = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    val x = 0.1f
    val y = 0.1f
    val z = 1024000
    val c = 1.068e-7f
    val coeff = Array(0, c * y, 0, c * x, c * (-2 * y - 2 * x - z + 1), c * x, 0, c * y, 0)

    val (output: Array[Float], runtime) = Execute(16, 16, 1184, 1184, (true, true))(rodinia, heat, power, coeff)
    //val (output: Array[Float], runtime) = Execute(16,16, 9376, 9376, (true, true))(rodinia, heat, power, coeff)
  }

  /* **********************************************************
      HOTSPOT 3D RODINIA
  ***********************************************************/
  @Test def rodiniaHotspot3D(): Unit = {
    LongTestsEnabled()
    //val hotspot = UserFun("hotspot", "tuple", "{ return tuple_0; }", TupleType(Float, ArrayType(ArrayType(Float, 3),3)), Float)
    // segfaults
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      ArrayType(Float, 27),
      (input, weights) => {
        MapSeq(MapGlb(1)(MapGlb(0)(\(nbh =>

          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $
            Zip(Join() o Join() $ nbh,
              weights))
        ))) o Slide3D(3, 1) o Pad3D(1, 1, 1, Pad.Boundary.MirrorUnsafe) $ input
      }
    )

    // testing
    val input = Array.tabulate(8, 512, 512) { (i, j, k) => Random.nextFloat() }
    val weights = Array.tabulate(27) { (i) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 1, (true, true))(stencil, input, weights)
  }

  // rodinia 3d opt1
  @Test def rodiniaHotspot3DOpt1(): Unit = {
    val zip3d = \((A, B, C) =>
      Map(Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2)))) o Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2))) $ Zip(A, B, C)
    )

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)(\(nbh =>
          toGlobal(MapSeq(\(acc => add(acc, nbh._2)))) o
            MapSeq(\(acc => add(acc, nbh._1))) o
            ReduceSeqUnroll(add, 0.0f) o Join() o Join() $ nbh._0)
        ))) $ zip3d(
          Slide3D(3, 1, 3, 1, 1, 1) o Pad3D(1, 1, 0, Pad.Boundary.Clamp) $ input,
          input, // leftshift z - upper element
          input) // rightshift z - lower element
      }
    )

    val input = Array.fill(8)(Array.fill(512)(Array.fill(512)(1.0f)))
    val weights = Array.fill(3)(Array.fill(3)(1.0f))
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 8, (true, true))(stencil, input)
  }

  @Test def rodiniaHotspot3DLocalMemory(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapWrg(1)(MapWrg(0)(\(tiles =>
          MapSeq(MapLcl(1)(MapLcl(0)(\(nbh =>
            toGlobal(MapSeq(id)) o
              ReduceSeq(add, 0.0f) o Join() o Join() $ nbh)))) o
            Slide3D(3, 1) o
            toLocal(MapSeq(MapLcl(1)(MapLcl(0)(id)))) $ tiles)
        ))) o Slide3D(66, 64, 6, 4, 10, 10) o Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ input
      }
    )

    // testing
    val input = Array.tabulate(8, 512, 512) { (i, j, k) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 1, (true, true))(stencil, input)
  }

  @Test
  def RodiniaHotSpot3DWithData(): Unit = {

    val tempInput = StencilDataArrays.inputTempHotspot3D.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray
    val powerInput = StencilDataArrays.inputPowerHotspot3D.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray.sliding(HotSpotConstants.localDimX, HotSpotConstants.localDimY).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val calculateHotspot = UserFun("calculateHotspot", Array("tInC", "cc", "tInN", "cn", "tInS", "cs", "tInE", "ce", "tInW", "cw", "tInT", "ct", "tInB", "cb", "stepDivCap", "pInC", "amb_temp"),
      "{ return  tInC*cc + tInN*cn + tInS*cs + tInE*ce + tInW*cw + tInT*ct + tInB*cb + stepDivCap * pInC + ct*amb_temp; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val rodiniaHotSpot3D =
      fun(ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      Float,
      Float,
      Float,
      Float,
      Float,
      Float,
      Float,
      Float,
      (temp, power, ce, cw, cn, cs, ct, cb, cc, stepDivCap) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun((m) => {

          val amb_temp = 80.0f
          val ct_amb_temp = fun(x => mult(x, ct)) $ amb_temp

          val tInC = Get(m, 1).at(1).at(1).at(1)
          val tIncCC = toPrivate(fun(x => mult(x, cc))) $ tInC

          val tInW = Get(m, 1).at(0).at(1).at(1)
          val tIncW = toPrivate(fun(x => mult(x, cw))) $ tInW

          val tInN = Get(m, 1).at(1).at(0).at(1)
          val tIncN = toPrivate(fun(x => mult(x, cn))) $ tInN

          val tInB = Get(m, 1).at(1).at(1).at(0)
          val tIncB = toPrivate(fun(x => mult(x, cb))) $ tInB

          val tInT = Get(m, 1).at(1).at(1).at(2)
          val tIncT = toPrivate(fun(x => mult(x, ct))) $ tInT

          val tInS = Get(m, 1).at(1).at(2).at(1)
          val tIncS = toPrivate(fun(x => mult(x, cs))) $ tInS

          val tInE = Get(m, 1).at(2).at(1).at(1)
          val tIncE = toPrivate(fun(x => mult(x, ce))) $ tInE

          val pInc = Get(m, 0)
          val pcSDC = toPrivate(fun(x => mult(x, stepDivCap))) $ pInc

          toGlobal(id) o
            toPrivate(fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp))) $ tInC
        })))
        ) $ Zip3D(power, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) o Pad3D(1, 1, 1, Pad.Boundary.MirrorUnsafe) $ temp)
      })


    val newLambda = SimplifyAndFuse(rodiniaHotSpot3D)
    val source = Compile(newLambda, 32, 4, 2, 512, 512, 8, immutable.Map())

    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,newLambda, tempInput, powerInput, HotSpotConstants.ce,HotSpotConstants.cw,HotSpotConstants.cn,HotSpotConstants.cs,HotSpotConstants.ct,HotSpotConstants.cb,HotSpotConstants.cc,HotSpotConstants.stepDivCap)

    if(StencilUtilities.printOutput)
    {
      StencilUtilities.printOriginalAndOutput3DSame(tempInput, output)
    }

//    assertArrayEquals(StencilDataArrays.compareDataHotspot3D, output, 0.1f)

  }
  @Ignore
  @Test
  def RodiniaSRAD1(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    //val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    // the benchmark is indexed by colum, so we have to go through this rigamarole
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val calculateG2 = UserFun("calculateG2", Array("dN", "dS", "dE", "dW", "jC"),
      "{ return  (dN*dN + dS*dS + dW*dW + dE*dE) / (jC * jC); }", Seq(Float, Float, Float, Float, Float), Float)

    val calculateL = UserFun("calculateL", Array("dN", "dS", "dE", "dW", "jC"),
      "{ return  (dN + dS + dW + dE ) / jC; }", Seq(Float, Float, Float, Float, Float), Float)

    val calculateNum = UserFun("calculateNum", Array("g2", "l"),
      "{ return (0.5*g2) - ((1.0/16.0)*(l*l)); }", Seq(Float, Float), Float)

    val calculateDen = UserFun("calculateDen", Array("L"),
      "{ return 1 + (0.25*L) ; }", Seq(Float), Float)

    val calculateQsqr = UserFun("calculateQsqr", Array("num", "den"),
      "{ return  num/(den*den); }", Seq(Float, Float), Float)

    val calculateDen2 = UserFun("calculateDen2", Array("qsqr", "q0sqr"),
      "{ return (qsqr-q0sqr) / (q0sqr * (1+q0sqr)); }", Seq(Float, Float), Float)

    val calculateCoeff = UserFun("calculateCoeff", Array("den"),
      "{ return 1.0 / (1.0+den); }", Seq(Float), Float)

    val saturateCoeff = UserFun("saturateCoeff", Array("coeff"),
      "{ if(coeff > 1) { return 1.0f; } else if(coeff < 0 ) { return 0.0f; } else { return coeff;} }", Seq(Float), Float)

    val idxF = UserFun("idxF", Array("i", "j", "m", "n"), "{ return i+502*j; }",
      Seq(Int, Int, Int, Int), Int)

    val at = ArrayType(ArrayType(Int, n), m)

    val sradKernel1 = fun(
      ArrayType(ArrayType(Float, m), n),
      (image) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {

          val q0sqr = 0.053787220269f // this value is dependent on data set size !!

          val Jc = m.at(1).at(1)

          val JW = m.at(1).at(0)
          val JN = m.at(0).at(1)
          val JS = m.at(2).at(1)
          val JE = m.at(1).at(2)

          val DW = toPrivate(fun(x => subtract(x, Jc))) $ JW
          val DN = toPrivate(fun(x => subtract(x, Jc))) $ JN
          val DS = toPrivate(fun(x => subtract(x, Jc))) $ JS
          val DE = toPrivate(fun(x => subtract(x, Jc))) $ JE
          val G2 = fun(x => calculateG2(x, DS, DW, DE, Jc)) $ DN
          val L = fun(x => calculateL(x, DS, DW, DE, Jc)) $ DN
          val Num = fun(x => calculateNum(x, L)) $ G2
          val Den = fun(x => calculateDen(x)) $ L
          val Qsqr = fun(x => calculateQsqr(x, Den)) $ Num
          val Den2 = fun(x => calculateDen2(x, q0sqr)) $ Qsqr
          val Coeff = fun(x => calculateCoeff(x)) $ Den2
          val SCoeff = toPrivate(fun(x => saturateCoeff(x))) $ Coeff

          //          val getIdx = Array2DFromUserFunGenerator(idxF, ArrayType(ArrayType(Float, m),n))

          toGlobal(id) $ SCoeff
        }))
        ) o Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image
      })

    val newLambda = SimplifyAndFuse(sradKernel1)
    val source = Compile(newLambda)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D)

    // undo the transpose
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(imageValues2D, outputRemixed2.flatten, Nrows)
    }

    assertArrayEquals(coefficientValues, outputRemixed2.flatten, StencilUtilities.stencilDelta)

  }


  @Ignore
  @Test
  def RodiniaSRAD1Consolidated(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    //val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    // the benchmark is indexed by colum, so we have to go through this rigamarole
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val q0sqr = 0.053787220269f // this value is dependent on data set size !!

    val calculateScoeff = UserFun("calculateScoeff", Array("dN", "dS", "dE", "dW", "jC", "q0sqr"),
      "{ float g2 = (dN*dN + dS*dS + dW*dW + dE*dE) / (jC * jC);" +
        "float l = (dN + dS + dW + dE ) / jC; " +
        "float num = (0.5*g2) - ((1.0/16.0)*(l*l)); " +
        "float  den = 1 + (0.25*l);" +
        "float qsqr = num/(den*den); " +
        "den = (qsqr-q0sqr) / (q0sqr * (1+q0sqr));" +
        "float coeff = 1.0 / (1.0+den); " +
        " if(coeff > 1) { return 1.0f; } else if(coeff < 0 ) { return 0.0f; } else { return coeff;}  }", Seq(Float, Float, Float, Float, Float, Float), Float)

    val sradKernel1 = fun(
      ArrayType(ArrayType(Float, m), n),
      Float,
      (image, q0sqr) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {


          val Jc = m.at(1).at(1)

          val JW = m.at(1).at(0)
          val JN = m.at(0).at(1)
          val JS = m.at(2).at(1)
          val JE = m.at(1).at(2)

          val DW = toPrivate(fun(x => subtract(x, Jc))) $ JW
          val DN = toPrivate(fun(x => subtract(x, Jc))) $ JN
          val DS = toPrivate(fun(x => subtract(x, Jc))) $ JS
          val DE = toPrivate(fun(x => subtract(x, Jc))) $ JE

          val scoeff = toPrivate(fun(x => calculateScoeff(x, DS, DE, DW, Jc, q0sqr))) $ DN

          toGlobal(id) $ scoeff
        }))
        ) o Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image
      })

    val newLambda = SimplifyAndFuse(sradKernel1)
    val source = Compile(newLambda)
    println(source)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D,q0sqr)

    // undo the transpose
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(imageValues2D, outputRemixed2.flatten, Nrows)
    }

    assertArrayEquals(coefficientValues, outputRemixed2.flatten, StencilUtilities.stencilDelta)
  }

  @Ignore
  @Test
  def RodiniaSRAD2CalculateOnTheFly(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    // val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray
    //val coeffValues2D = coefficientValues.sliding(Ncols,Ncols).toArray
    val coeffValues2D1 = coefficientValues.sliding(Nrows, Nrows).toArray
    val coeffValues2D = coeffValues2D1.transpose

    val finalImageCompareFile = dir + "imagecompare.txt"
    val finalImageCompareValues: Array[Float] = Source.fromFile(finalImageCompareFile).getLines().toList.map(x => x.toFloat).toArray
    val finalImageValues2D = finalImageCompareValues.sliding(Ncols, Ncols).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val calculateDiv = UserFun("calculateDiv", Array("dN", "dS", "dW", "dE", "orgDn", "orgDs", "orgDw", "orgDe"),
      "{ return  (dN*orgDn + dS*orgDs + dW*orgDw + dE*orgDe) ; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val calculateImageUpdate = UserFun("calculateImageUpdate", Array("img", "div"),
      "{ return img + 0.125 * div; }", Seq(Float, Float), Float)

    val sradKernel2 = fun(
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      (image, coeff) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {

          val imageNBH = Get(m, 0)
          val coeffNBH = Get(m, 1)

          val imageC = imageNBH.at(1).at(1)
          val coeffC = coeffNBH.at(1).at(1)

          val JW = imageNBH.at(1).at(0)
          val JN = imageNBH.at(0).at(1)
          val JS = imageNBH.at(2).at(1)
          val JE = imageNBH.at(1).at(2)

          val orgDW = toPrivate(fun(x => subtract(x, imageC))) $ JW
          val orgDN = toPrivate(fun(x => subtract(x, imageC))) $ JN
          val orgDS = toPrivate(fun(x => subtract(x, imageC))) $ JS
          val orgDE = toPrivate(fun(x => subtract(x, imageC))) $ JE

          val newDW = coeffC
          val newDN = coeffC
          val newDS = coeffNBH.at(2).at(1)
          val newDE = coeffNBH.at(1).at(2)

          val div = toPrivate(fun(x => calculateDiv(x, newDS, newDW, newDE, orgDN, orgDS, orgDW, orgDE))) $ newDN
          val newImg = toPrivate(fun(x => calculateImageUpdate(x, div))) $ imageC

          toGlobal(id) $ newImg
        }))
        ) $ Zip2D(Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image, Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ coeff)
      })

    val newLambda = SimplifyAndFuse(sradKernel2)
    val source = Compile(newLambda)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D, coeffValues2D)
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.print2DArray(coeffValues2D)
      println("**** OUTPUT ******")
      StencilUtilities.print2DArray(outputRemixed2)
      println("*** EXPECTED ***")
      StencilUtilities.print2DArray(finalImageValues2D)
    }
    assertArrayEquals(finalImageCompareValues, outputRemixed2.flatten, StencilUtilities.stencilDelta)

  }

  @Ignore
  @Test
  def RodiniaSRAD2CalculatePullInValues(): Unit = {
    val dir = "../../src/test/opencl/generator/stencil/resources/" // should be a better option

    val Nrows = 502
    val Ncols = 458

    val imageBeforeFile = dir + "imagebefore.txt"
    val imageValues: Array[Float] = Source.fromFile(imageBeforeFile).getLines().toList.map(x => x.toFloat).toArray
    // val imageValues2D = imageValues.sliding(Ncols,Ncols).toArray
    val imageValues2D1 = imageValues.sliding(Nrows, Nrows).toArray
    val imageValues2D = imageValues2D1.transpose

    val coeffBeforeSecondFile = dir + "cbeforesecond.txt"
    val coefficientValues: Array[Float] = Source.fromFile(coeffBeforeSecondFile).getLines().toList.map(x => x.toFloat).toArray
    //val coeffValues2D = coefficientValues.sliding(Ncols,Ncols).toArray
    val coeffValues2D1 = coefficientValues.sliding(Nrows, Nrows).toArray
    val coeffValues2D = coeffValues2D1.transpose

    val dDNFile = dir + "dDN.txt"
    val dDNValues: Array[Float] = Source.fromFile(dDNFile).getLines().toList.map(x => x.toFloat).toArray
    val DNValues2D1 = dDNValues.sliding(Nrows, Nrows).toArray
    val DNValues2D = DNValues2D1.transpose

    val dDSFile = dir + "dDS.txt"
    val dDSValues: Array[Float] = Source.fromFile(dDSFile).getLines().toList.map(x => x.toFloat).toArray
    val DSValues2D1 = dDSValues.sliding(Nrows, Nrows).toArray
    val DSValues2D = DSValues2D1.transpose

    val dDEFile = dir + "dDE.txt"
    val dDEValues: Array[Float] = Source.fromFile(dDEFile).getLines().toList.map(x => x.toFloat).toArray
    val DEValues2D1 = dDEValues.sliding(Nrows, Nrows).toArray
    val DEValues2D = DEValues2D1.transpose

    val dDWFile = dir + "dDW.txt"
    val dDWValues: Array[Float] = Source.fromFile(dDWFile).getLines().toList.map(x => x.toFloat).toArray
    val DWValues2D1 = dDWValues.sliding(Nrows, Nrows).toArray
    val DWValues2D = DWValues2D1.transpose

    val finalImageCompareFile = dir + "imagecompare.txt"
    val finalImageCompareValues: Array[Float] = Source.fromFile(finalImageCompareFile).getLines().toList.map(x => x.toFloat).toArray
    val finalImageValues2D = finalImageCompareValues.sliding(Ncols, Ncols).toArray

    val m = SizeVar("M")
    val n = SizeVar("N")

    val calculateDiv = UserFun("calculateDiv", Array("dN", "dS", "dW", "dE", "orgDn", "orgDs", "orgDw", "orgDe"),
      "{ return  (dN*orgDn + dS*orgDs + dW*orgDw + dE*orgDe) ; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val calculateImageUpdate = UserFun("calculateImageUpdate", Array("img", "div"),
      "{ return img + 0.125 * div; }", Seq(Float, Float), Float)

    val sradKernel2 = fun(
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      ArrayType(ArrayType(Float, m), n),
      (image, coeff, DN, DS, DE, DW) => {
        MapGlb(1)(MapGlb(0)(fun((m) => {

          val imageNBH = Get(m, 0)
          val coeffNBH = Get(m, 1)

          val imageC = imageNBH.at(1).at(1)
          val coeffC = coeffNBH.at(1).at(1)

          val newDW = coeffC
          val newDN = coeffC
          val newDS = coeffNBH.at(2).at(1)
          val newDE = coeffNBH.at(1).at(2)

          val orgDN = Get(m, 2)
          val orgDS = Get(m, 3)
          val orgDE = Get(m, 4)
          val orgDW = Get(m, 5)

          val div = toPrivate(fun(x => calculateDiv(x, newDS, newDW, newDE, orgDN, orgDS, orgDW, orgDE))) $ newDN
          val newImg = toPrivate(fun(x => calculateImageUpdate(x, div))) $ imageC

          toGlobal(id) $ newImg
        }))
        ) $ Zip2D(Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ image, Slide2D(3, 1) o Pad2D(1, 1, Pad.Boundary.MirrorUnsafe) $ coeff, DN, DS, DE, DW)
      })

    val newLambda = SimplifyAndFuse(sradKernel2)
    val source = Compile(newLambda)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (true, true))(source, newLambda, imageValues2D, coeffValues2D, DNValues2D, DSValues2D, DEValues2D, DWValues2D)
    val outputRemixed = output.sliding(Ncols, Ncols).toArray
    val outputRemixed2 = outputRemixed.transpose

    if (StencilUtilities.printOutput) {
      StencilUtilities.print2DArray(coeffValues2D)
      println("**** OUTPUT ******")
      StencilUtilities.print2DArray(outputRemixed2)
      println("*** EXPECTED ***")
      StencilUtilities.print2DArray(finalImageValues2D)
    }
    assertArrayEquals(finalImageCompareValues, outputRemixed2.flatten, .002f)

  }

}

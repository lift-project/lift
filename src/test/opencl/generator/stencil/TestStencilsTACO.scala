package opencl.generator.stencil

import java.io.PrintWriter

import ir.ArrayType
import ir.ast.{Get, Pad, Pad3D, Slide3D, UserFun, Zip3D, fun, _}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.InlineStructs
import opencl.generator.stencil.acoustic.{RoomConstants, StencilUtilities}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeqSlide, toGlobal, toPrivate}
import org.junit.Assert._
import org.junit._
import rewriting.SimplifyAndFuse

object TestStencilsTACO extends TestWithExecutor

/**
  * Benchmarks used in TACO paper submission 2019
  **/

class TestStencilsTACO {

  val outputdir = "/home/reese/workspace/taco_kernels/raw_files/"
  val ext = ".cl"
  val printToFile = false

  val delta = 0.1f

  val m = SizeVar("M")
  val n = SizeVar("N")
  val o = SizeVar("O")

  val localDimX = 20
  val localDimY = 15
  val localDimZ = 12

  val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)

  /** * Hotspot3D ***/

  val calculateHotspot = UserFun("calculateHotspot", Array("tInC", "cc", "tInN", "cn", "tInS", "cs", "tInE", "ce", "tInW", "cw", "tInT", "ct", "tInB", "cb", "stepDivCap", "pInC", "amb_temp"),
    "{ return  tInC*cc + tInN*cn + tInS*cs + tInE*ce + tInW*cw + tInT*ct + tInB*cb + stepDivCap * pInC + ct*amb_temp; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  def rodinia(m: Param) = {
    val ce = 0.03413332998752593994f
    val cw = 0.03413332998752593994f
    val cn = 0.03413332998752593994f
    val cs = 0.03413332998752593994f
    val ct = 0.00053333328105509281f
    val cb = 0.00053333328105509281f
    val cc = 0.86186665296554565430f
    val stepDivCap = 0.34133329987525939941f

    val amb_temp = 80.0f
    val ct_amb_temp = toPrivate(fun(x => mult(x, ct))) $ amb_temp

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

    fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp)) $ tInC

  }

  def rodiniaMSS(m: Param) = {
    val ce = 0.03413332998752593994f
    val cw = 0.03413332998752593994f
    val cn = 0.03413332998752593994f
    val cs = 0.03413332998752593994f
    val ct = 0.00053333328105509281f
    val cb = 0.00053333328105509281f
    val cc = 0.86186665296554565430f
    val stepDivCap = 0.34133329987525939941f

    val amb_temp = 80.0f
    val ct_amb_temp = toPrivate(fun(x => mult(x, ct))) $ amb_temp

    val tInC = Get(m.at(1).at(1).at(1), 1)
    val tIncCC = toPrivate(fun(x => mult(x, cc))) $ tInC

    val tInW = Get(m.at(0).at(1).at(1), 1)
    val tIncW = toPrivate(fun(x => mult(x, cw))) $ tInW

    val tInN = Get(m.at(1).at(0).at(1), 1)
    val tIncN = toPrivate(fun(x => mult(x, cn))) $ tInN

    val tInB = Get(m.at(1).at(1).at(0), 1)
    val tIncB = toPrivate(fun(x => mult(x, cb))) $ tInB

    val tInT = Get(m.at(1).at(1).at(2), 1)
    val tIncT = toPrivate(fun(x => mult(x, ct))) $ tInT

    val tInS = Get(m.at(1).at(2).at(1), 1)
    val tIncS = toPrivate(fun(x => mult(x, cs))) $ tInS

    val tInE = Get(m.at(2).at(1).at(1), 1)
    val tIncE = toPrivate(fun(x => mult(x, ce))) $ tInE

    val pInc = Get(m.at(1).at(1).at(1), 0)
    val pcSDC = toPrivate(fun(x => mult(x, stepDivCap))) $ pInc

   toGlobal(id) o toPrivate(fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp))) $ tInC

  }

  @Test
  def MSSHotSpot3D(): Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val calculateHotspot = UserFun("calculateHotspot", Array("tInC", "cc", "tInN", "cn", "tInS", "cs", "tInE", "ce", "tInW", "cw", "tInT", "ct", "tInB", "cb", "stepDivCap", "pInC", "amb_temp"),
      "{ return  tInC*cc + tInN*cn + tInS*cs + tInE*ce + tInW*cw + tInT*ct + tInB*cb + stepDivCap * pInC + ct*amb_temp; }", Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val size = 3
    val step = 1

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
       (temp, power) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun((m) => {
          rodinia(m)
        })))
        ) $ Zip3D(power, Slide3D(size, step) o Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ temp)
      })

    val stencilMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      (temp, power) => {
          TransposeW() o Map(TransposeW()) o TransposeW() o
          MapGlb(0)(MapGlb(1)(fun(x => {
            toGlobal(MapSeqSlide(fun((m) => {
              rodiniaMSS(m)
            }), size, step))
          } o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ Zip3D(Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ power, /*Slide3D(size,step) o */ Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ temp)
      })

    val lambda = SimplifyAndFuse(stencil)
    val source = Compile(lambda)//, NDRange(32,4,2), NDRange(n,m,1))

    val mssLambda = SimplifyAndFuse(stencilMSS)
    val sourceMSS = Compile(mssLambda)//, NDRange(32,4,2), NDRange(n,m,1))

    val orgFile = outputdir+"hotspot3D-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"hotspot3D-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

     val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](stencil, data, data)
     val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](stencilMSS, data, data)

     assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  /** * Acoustic ***/

  val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }", Seq(Int, Int, Int, Int, Int, Int), Int)
  val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int, Float, Float), Float)

  val SR = 441.0f
  val alpha = 0.005f
  val c = 344.0f
  val NF = 4410
  val k = 1 / SR
  val h = Math.sqrt(3.0f) * c * k
  val lambda = c * k / h

  val loss1 = 1.0f / (1.0f + lambda * alpha)
  val loss2 = 1.0f - lambda * alpha

  val l2: Float = ((c * c * k * k) / (h * h)).toFloat
  val cf1: Array[Float] = Array(loss1.toFloat, 1.0f)
  val cf21: Array[Float] = Array(loss2.toFloat, 1.0f)

  val idIF = UserFun("idIF", "x", "{ return (float)(x*1.0); }", Int, Float)

  def acoustic(m: Param) = {

    val cf = toPrivate(fun(x => getCF(x, cf1(0), cf1(1)))) $ Get(m.at(1).at(1).at(1), 2)
    val cf2 = toPrivate(fun(x => getCF(x, cf21(0), cf21(1)))) $ Get(m.at(1).at(1).at(1), 2)
    val maskedValStencil = l2

    val `tile[1][1][1]` = Get(m.at(1).at(1).at(1), 1)

    val `tile[0][1][1]` = Get(m.at(0).at(1).at(1), 1)
    val `tile[1][0][1]` = Get(m.at(1).at(0).at(1), 1)
    val `tile[1][1][0]` = Get(m.at(1).at(1).at(0), 1)
    val `tile[1][1][2]` = Get(m.at(1).at(1).at(2), 1)
    val `tile[1][2][1]` = Get(m.at(1).at(2).at(1), 1)
    val `tile[2][1][1]` = Get(m.at(2).at(1).at(1), 1)

    val stencil = toPrivate(fun(x => add(x, `tile[0][1][1]`))) o
      toPrivate(fun(x => add(x, `tile[1][0][1]`))) o
      toPrivate(fun(x => add(x, `tile[1][1][0]`))) o
      toPrivate(fun(x => add(x, `tile[1][1][2]`))) o
      toPrivate(fun(x => add(x, `tile[1][2][1]`))) $ `tile[2][1][1]`

    val valueMat1 = Get(m.at(1).at(1).at(1), 0)
    val valueMask = toPrivate(idIF) $ Get(m.at(1).at(1).at(1), 2)

    toGlobal(id) o toPrivate(fun(x => mult(x, cf))) o toPrivate(addTuple) $
      Tuple(toPrivate(fun(x => mult(x, `tile[1][1][1]`))) o toPrivate(fun(x => subtract(2.0f, x))) o toPrivate(fun(x => mult(x, RoomConstants.l2))) $ valueMask,
        toPrivate(subtractTuple) $ Tuple(
          toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
          toPrivate(fun(x => mult(x, cf2))) $ valueMat1))

  }

  @Test
  def MSSAcoustic3D(): Unit = {

    LongTestsEnabled()

    val ISflag = InlineStructs()
    InlineStructs(true)

    val arraySigmno2 = ArrayType(ArrayType(ArrayType(Int, m + 2), n + 2), o + 2)

    val size = 3
    val step = 1

    val aStencil =
      fun(
        ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
        ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
        (mat1, mat2) => {
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            acoustic(m)
          })))
          ) o Slide3D(size, step) $ Zip3D( mat1, mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno2))
        })

    val aStencilMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
      ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
      (mat1, mat2) => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          MapGlb(0)(MapGlb(1)(fun(x => {
            toGlobal(MapSeqSlide(fun((m) => {
              acoustic(m)
            }), size, step))
          } o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ Zip3D(mat1, mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno2))
      })

    val lambda = SimplifyAndFuse(aStencil)
    val source = Compile(lambda)//, NDRange(32,4,2), NDRange(n,m,1))

    val mssLambda = SimplifyAndFuse(aStencilMSS)
    val sourceMSS = Compile(mssLambda)//, NDRange(32,4,2), NDRange(n,m,1))

    val orgFile = outputdir+"acoustic-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"acoustic-MSS-"+m+"-"+"n"+"-"+o+ext

   if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](aStencil, data, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](aStencilMSS, data, data)

    assertArrayEquals(output_MSS, output_org, delta)

    InlineStructs(ISflag)

  }

  @Test
  def MSSAcoustic3DCGO(): Unit = {

    LongTestsEnabled()

    val ISflag = InlineStructs()
    InlineStructs(true)

    val arraySigmno2 = ArrayType(ArrayType(ArrayType(Int, m + 2), n + 2), o + 2)

    val size = 3
    val step = 1

    val aStencil =
      fun(
        ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
        ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
        (mat1, mat2) => {
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            acoustic(m)
          })))
          ) o Slide3D(size, step) $ Zip3D( mat1, mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno2))    // no padding
        })

    val aStencilMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
      ArrayType(ArrayType(ArrayType(Float, m+2),n+2),o+2),
      (mat1, mat2) => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          MapGlb(0)(MapGlb(1)(fun(x => {
            toGlobal(MapSeqSlide(fun((m) => {
              acoustic(m)
            }), size, step))
          } o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ Zip3D(mat1, mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno2)) // no padding
      })

    val lambda = SimplifyAndFuse(aStencil)
    val source = Compile(lambda)//, NDRange(32,4,2), NDRange(n,m,1))

    val mssLambda = SimplifyAndFuse(aStencilMSS)
    val sourceMSS = Compile(mssLambda)//, NDRange(32,4,2), NDRange(n,m,1))

    val orgFile = outputdir+"acoustic-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"acoustic-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](aStencil, data, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](aStencilMSS, data, data)

    assertArrayEquals(output_MSS, output_org, delta)

    InlineStructs(ISflag)

  }

  /**
    * PPCG
    **/

  def vonNeumann7pt(x: Param) = {
    val N = x.at(1).at(0).at(1)
    val S = x.at(1).at(2).at(1)
    val W = x.at(1).at(1).at(0)
    val E = x.at(1).at(1).at(2)
    val C = x.at(1).at(1).at(1)
    val F = x.at(0).at(1).at(1)
    val B = x.at(2).at(1).at(1)
    (N, S, W, E, F, B, C)
  }

  def jacobi7ptW = UserFun("jacobi", Array("C", "N", "S", "E", "W", "F", "B"),
    """return 0.161f * E + 0.162f * W +
      0.163f * S + 0.164f * N +
      0.165f * B + 0.166f * F -
      1.67f * C;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def j3d7ptMSS: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

             toGlobal(id) o toPrivate(fun(x =>
              jacobi7ptW(x, n, s, e, w, f, b))) $ c

        })))) o Slide3D(size,step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

              toGlobal(id) o toPrivate(fun(x =>
             jacobi7ptW(x, n, s, e, w, f, b))) $ c


          }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })
    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d7pt-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d7pt-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](source, originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](sourceMSS,lambdaMSS, data)

    StencilUtilities.print1DArrayAs3DArray(output_org,localDimX,localDimY,localDimZ)
    StencilUtilities.print1DArrayAs3DArray(output_MSS,localDimX,localDimY,localDimZ)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def heat = UserFun("heat", Array("C", "S", "N", "E", "W", "B", "F"),
    """return 0.125f * (B - 2.0f * C + F) +
      |       0.125f * (S - 2.0f * C + N) +
      |       0.125f * (E - 2.0f * C + W) + C;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def heat3dMSS: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

            toGlobal(id) o toPrivate(fun(x =>
              heat(x, n, s, e, w, f, b))) $ c

          })))) o Slide3D(size,step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

              toGlobal(id) o toPrivate(fun(x =>
                heat(x, n, s, e, w, f, b))) $ c

            }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })

    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"heat3d-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"heat3d-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def jacobi13 = UserFun("jacobi13", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C"),
    """return 0.083f * EE + 0.083f * E + 0.083f * W + 0.083f * WW +
      |       0.083f * SS + 0.083f * S + 0.083f * N + 0.083f * NN +
      |       0.083f * BB + 0.083f * B + 0.083f * F + 0.083f * FF -
      |       0.996f * C;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def jacobi13ptMSS: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(2)))) o
          Map(Scatter(Shift(2))) o
          Scatter(Shift(2)) o
          Pad3D(2, 2, 2, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            //              z     y     x
            val ee = nbh.at(2).at(2).at(4)
            val e = nbh.at(2).at(2).at(3)
            val w = nbh.at(2).at(2).at(1)
            val ww = nbh.at(2).at(2).at(0)
            val ss = nbh.at(2).at(4).at(2)
            val s = nbh.at(2).at(3).at(2)
            val n = nbh.at(2).at(1).at(2)
            val nn = nbh.at(2).at(0).at(2)
            val bb = nbh.at(4).at(2).at(2)
            val b = nbh.at(3).at(2).at(2)
            val f = nbh.at(1).at(2).at(2)
            val ff = nbh.at(0).at(2).at(2)
            val c = nbh.at(2).at(2).at(2)

            toGlobal(id) o toPrivate(fun(x =>
             jacobi13(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

          })))) o Slide3D(5, 1) $ input
      })

  val lambdaMSS = fun(
    ArrayType(ArrayType(ArrayType(Float, m), n), o),
    input => {
      TransposeW() o Map(TransposeW()) o TransposeW() o
        Map(Map(Scatter(Shift(2)))) o
        Map(Scatter(Shift(2))) o
        Scatter(Shift(2)) o
        Pad3D(2, 2, 2, Pad.Boundary.Clamp) o
        MapGlb(0)(MapGlb(1)(fun( x => {
          toGlobal(MapSeqSlide(fun(nbh => {

            val ee = nbh.at(2).at(2).at(4)
            val e = nbh.at(2).at(2).at(3)
            val w = nbh.at(2).at(2).at(1)
            val ww = nbh.at(2).at(2).at(0)
            val ss = nbh.at(2).at(4).at(2)
            val s = nbh.at(2).at(3).at(2)
            val n = nbh.at(2).at(1).at(2)
            val nn = nbh.at(2).at(0).at(2)
            val bb = nbh.at(4).at(2).at(2)
            val b = nbh.at(3).at(2).at(2)
            val f = nbh.at(1).at(2).at(2)
            val ff = nbh.at(0).at(2).at(2)
            val c = nbh.at(2).at(2).at(2)

           toGlobal(id) o toPrivate(fun(x =>
             jacobi13(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

          }) ,5,1))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(5,1) o Map(Transpose()) o Transpose() $ input
    })

  val source = Compile(originalLambda)

  val sourceMSS = Compile(lambdaMSS)

   val orgFile = outputdir+"j3d13pt-original-"+m+"-"+"n"+"-"+o+ext
   val mssFile = outputdir+"j3d13pt-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }


  val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](source,originalLambda, data)
  val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](sourceMSS,lambdaMSS, data)

  StencilUtilities.print1DArray(output_org)

  assertArrayEquals(output_org, output_MSS, delta)

  InlineStructs(ISflag)

  }

  def poisson = UserFun("poisson", Array("C", "N", "S", "E", "W", "F", "B",
    "FN", "BN", "FS", "BS", "FW", "BW", "NW", "SW", "FE", "BE", "NE", "SE"),
    """return 2.666f * C - 0.166f * (F + B + N + S + E + W) -
      |       0.0833f * (FN + BN + FS + BS + FW + BW +
      |                  NW + SW + FE + BE + NE + SE);""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
      Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def poisson3dMSS: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            //              z     y     x
            val c  = nbh.at(1).at(1).at(1)
            val n  = nbh.at(1).at(0).at(1)
            val s  = nbh.at(1).at(2).at(1)
            val e  = nbh.at(1).at(1).at(2)
            val w  = nbh.at(1).at(1).at(0)
            val f  = nbh.at(0).at(1).at(1)
            val b  = nbh.at(2).at(1).at(1)
            val fn = nbh.at(0).at(0).at(1)
            val bn = nbh.at(2).at(0).at(1)
            val fs = nbh.at(0).at(2).at(1)
            val bs = nbh.at(2).at(2).at(1)
            val fw = nbh.at(0).at(1).at(0)
            val bw = nbh.at(2).at(1).at(0)
            val nw = nbh.at(1).at(0).at(0)
            val sw = nbh.at(1).at(2).at(0)
            val fe = nbh.at(0).at(1).at(2)
            val be = nbh.at(2).at(1).at(2)
            val ne = nbh.at(1).at(0).at(2)
            val se = nbh.at(1).at(2).at(2)


            toGlobal(id) o toPrivate(fun(x =>
              poisson(x, n, s, e, w, f, b, fn, bn, fs, bs, fw, bw, nw, sw, fe, be, ne, se))) $ c

          })))) o Slide3D(size,step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {
              //              z     y     x
              val c  = nbh.at(1).at(1).at(1)
              val n  = nbh.at(1).at(0).at(1)
              val s  = nbh.at(1).at(2).at(1)
              val e  = nbh.at(1).at(1).at(2)
              val w  = nbh.at(1).at(1).at(0)
              val f  = nbh.at(0).at(1).at(1)
              val b  = nbh.at(2).at(1).at(1)
              val fn = nbh.at(0).at(0).at(1)
              val bn = nbh.at(2).at(0).at(1)
              val fs = nbh.at(0).at(2).at(1)
              val bs = nbh.at(2).at(2).at(1)
              val fw = nbh.at(0).at(1).at(0)
              val bw = nbh.at(2).at(1).at(0)
              val nw = nbh.at(1).at(0).at(0)
              val sw = nbh.at(1).at(2).at(0)
              val fe = nbh.at(0).at(1).at(2)
              val be = nbh.at(2).at(1).at(2)
              val ne = nbh.at(1).at(0).at(2)
              val se = nbh.at(1).at(2).at(2)


              toGlobal(id) o toPrivate(fun(x =>
                poisson(x, n, s, e, w, f, b, fn, bn, fs, bs, fw, bw, nw, sw, fe, be, ne, se))) $ c

            }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })

    val lambda = SimplifyAndFuse(originalLambda)
    val source = Compile(lambda)

    val mssLambda = SimplifyAndFuse(lambdaMSS)
    val sourceMSS = Compile(mssLambda)

    val orgFile = outputdir+"poisson-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"poisson-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }



    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def jacobi27 = UserFun("jacobi27", Array("FNW", "FN", "FNE", "FW", "F", "FE", "FSW", "FS", "FSE",
    "NW", "N", "NE", "W", "C", "E", "SW", "S", "SE",
    "BNW", "BN", "BNE", "BW", "B", "BE", "BSW", "BS", "BSE"),
    """return (0.5 * FNW + 0.7 * FN + 0.9 * FNE +
      |        1.2 * FW + 1.5 * F + 1.2 * FE +
      |        0.9 * FSW + 0.7 * FS + 0.5 * FSE +
      |        0.51 * NW + 0.71 * N + 0.91 * NE +
      |        1.21 * W + 1.51 * C + 1.21 * E +
      |        0.91 * SW + 0.71 * S + 0.51 * SE +
      |        0.52 * BNW + 0.72 * BN + 0.92 * BNE +
      |        1.22 * BW + 1.52 * B + 1.22 * BE +
      |        0.92 * BSW + 0.72 * BS + 0.52 * BSE) / 159;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
      Float, Float, Float, Float, Float, Float, Float, Float, Float,
      Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  def calculate27ptStencil(nbh: Param) =
  {
    //              z     y     x
    val fnw = nbh.at(0).at(0).at(0)
    val fn  = nbh.at(0).at(0).at(1)
    val fne = nbh.at(0).at(0).at(2)
    val fw  = nbh.at(0).at(1).at(0)
    val f   = nbh.at(0).at(1).at(1)
    val fe  = nbh.at(0).at(1).at(2)
    val fsw = nbh.at(0).at(2).at(0)
    val fs  = nbh.at(0).at(2).at(1)
    val fse = nbh.at(0).at(2).at(2)

    val nw  = nbh.at(1).at(0).at(0)
    val n   = nbh.at(1).at(0).at(1)
    val ne  = nbh.at(1).at(0).at(2)
    val w   = nbh.at(1).at(1).at(0)
    val c   = nbh.at(1).at(1).at(1)
    val e   = nbh.at(1).at(1).at(2)
    val sw  = nbh.at(1).at(2).at(0)
    val s   = nbh.at(1).at(2).at(1)
    val se  = nbh.at(1).at(2).at(2)

    val bnw = nbh.at(2).at(0).at(0)
    val bn  = nbh.at(2).at(0).at(1)
    val bne = nbh.at(2).at(0).at(2)
    val bw  = nbh.at(2).at(1).at(0)
    val b   = nbh.at(2).at(1).at(1)
    val be  = nbh.at(2).at(1).at(2)
    val bsw = nbh.at(2).at(2).at(0)
    val bs  = nbh.at(2).at(2).at(1)
    val bse = nbh.at(2).at(2).at(2)

    toGlobal(id) o toPrivate(fun(x =>
      jacobi27(x, fn, fne, fw, f, fe, fsw, fs, fse,
        nw, n, ne, w, c, e, sw, s, se,
        bnw, bn, bne, bw, b, be, bsw, bs, bse))) $ fnw

  }

  @Test
  def jacobi27MSS: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            calculate27ptStencil(nbh)

          })))) o Slide3D(size, step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              calculate27ptStencil(nbh)

            }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })

    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d27pt-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d27pt-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }


    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, 1.0f)

    InlineStructs(ISflag)

  }

  /** leggy 3 **/

  def jacobi21 = UserFun("jacobi21", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C","EEE","WWW","SSS","NNN","BBB","FFF"),
    """return 0.083f * EE + 0.083f * E + 0.083f * W + 0.083f * WW +
      |       0.083f * SS + 0.083f * S + 0.083f * N + 0.083f * NN +
      |       0.083f * BB + 0.083f * B + 0.083f * F + 0.083f * FF -
      |       0.083f * EEE + 0.083f * WWW +
      |       0.083f * SSS + 0.083f * NNN +
      |       0.083f * BBB + 0.083f * FFF -
      |       0.996f * C;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def jacobi21ptMSS: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(3)))) o
          Map(Scatter(Shift(3))) o
          Scatter(Shift(3)) o
          Pad3D(3, 3, 3, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            //              z     y     x
            val ee = nbh.at(3).at(3).at(5)
            val eee = nbh.at(3).at(3).at(6)
            val e = nbh.at(3).at(3).at(4)
            val w = nbh.at(3).at(3).at(2)
            val ww = nbh.at(3).at(3).at(1)
            val www = nbh.at(3).at(3).at(0)
            val ss = nbh.at(3).at(5).at(3)
            val sss = nbh.at(3).at(6).at(3)
            val s = nbh.at(3).at(4).at(3)
            val n = nbh.at(3).at(2).at(3)
            val nn = nbh.at(3).at(1).at(3)
            val nnn = nbh.at(3).at(0).at(3)
            val bb = nbh.at(5).at(3).at(3)
            val bbb = nbh.at(6).at(3).at(3)
            val b = nbh.at(4).at(3).at(3)
            val f = nbh.at(2).at(3).at(3)
            val ff = nbh.at(1).at(3).at(3)
            val fff = nbh.at(0).at(3).at(3)
            val c = nbh.at(3).at(3).at(3)

            toGlobal(id) o toPrivate(fun(x =>
              jacobi21(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c,eee,www,sss,nnn,bbb,fff))) $ ee

          })))) o Slide3D(7, 1) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(3)))) o
          Map(Scatter(Shift(3))) o
          Scatter(Shift(3)) o
          Pad3D(3, 3, 3, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              //              z     y     x
              val ee = nbh.at(3).at(3).at(5)
              val eee = nbh.at(3).at(3).at(6)
              val e = nbh.at(3).at(3).at(4)
              val w = nbh.at(3).at(3).at(2)
              val ww = nbh.at(3).at(3).at(1)
              val www = nbh.at(3).at(3).at(0)
              val ss = nbh.at(3).at(5).at(3)
              val sss = nbh.at(3).at(6).at(3)
              val s = nbh.at(3).at(4).at(3)
              val n = nbh.at(3).at(2).at(3)
              val nn = nbh.at(3).at(1).at(3)
              val nnn = nbh.at(3).at(0).at(3)
              val bb = nbh.at(5).at(3).at(3)
              val bbb = nbh.at(6).at(3).at(3)
              val b = nbh.at(4).at(3).at(3)
              val f = nbh.at(2).at(3).at(3)
              val ff = nbh.at(1).at(3).at(3)
              val fff = nbh.at(0).at(3).at(3)
              val c = nbh.at(3).at(3).at(3)

              toGlobal(id) o toPrivate(fun(x =>
                jacobi21(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c,eee,www,sss,nnn,bbb,fff))) $ ee

            }) ,7,1))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(7,1) o Map(Transpose()) o Transpose() $ input
      })

    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d21pt-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d21pt-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](source,originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](sourceMSS,lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  /** just jacobis, no weights + **/

  def jacobi7ptPlus = UserFun("jacobi7Plus", Array("C", "N", "S", "E", "W", "F", "B"),
    """return C+N+S+E+W+F+B;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def j3d7ptMSSPlus: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

            toGlobal(id) o toPrivate(fun(x =>
              jacobi7ptPlus(x, n, s, e, w, f, b))) $ c

          })))) o Slide3D(size,step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

              toGlobal(id) o toPrivate(fun(x =>
                jacobi7ptPlus(x, n, s, e, w, f, b))) $ c

            }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })
    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d7ptplus-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d7ptplus-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile) {
      new PrintWriter(orgFile) {
        try {
          write(source)
        } finally {
          close
        }
      }
      new PrintWriter(mssFile) {
        try {
          write(sourceMSS)
        } finally {
          close
        }
      }
    }
    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](source, originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](sourceMSS,lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def jacobi13Plus = UserFun("jacobi13Plus", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C"),
    """return  EE +  E +  W +  WW +
      |        SS +  S +  N +  NN +
      |        BB +  B +  F +  FF +
      |        C;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def jacobi13ptMSSPlus: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(2)))) o
          Map(Scatter(Shift(2))) o
          Scatter(Shift(2)) o
          Pad3D(2, 2, 2, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            //              z     y     x
            val ee = nbh.at(2).at(2).at(4)
            val e = nbh.at(2).at(2).at(3)
            val w = nbh.at(2).at(2).at(1)
            val ww = nbh.at(2).at(2).at(0)
            val ss = nbh.at(2).at(4).at(2)
            val s = nbh.at(2).at(3).at(2)
            val n = nbh.at(2).at(1).at(2)
            val nn = nbh.at(2).at(0).at(2)
            val bb = nbh.at(4).at(2).at(2)
            val b = nbh.at(3).at(2).at(2)
            val f = nbh.at(1).at(2).at(2)
            val ff = nbh.at(0).at(2).at(2)
            val c = nbh.at(2).at(2).at(2)

            toGlobal(id) o toPrivate(fun(x =>
              jacobi13Plus(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

          })))) o Slide3D(5, 1) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(2)))) o
          Map(Scatter(Shift(2))) o
          Scatter(Shift(2)) o
          Pad3D(2, 2, 2, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              val ee = nbh.at(2).at(2).at(4)
              val e = nbh.at(2).at(2).at(3)
              val w = nbh.at(2).at(2).at(1)
              val ww = nbh.at(2).at(2).at(0)
              val ss = nbh.at(2).at(4).at(2)
              val s = nbh.at(2).at(3).at(2)
              val n = nbh.at(2).at(1).at(2)
              val nn = nbh.at(2).at(0).at(2)
              val bb = nbh.at(4).at(2).at(2)
              val b = nbh.at(3).at(2).at(2)
              val f = nbh.at(1).at(2).at(2)
              val ff = nbh.at(0).at(2).at(2)
              val c = nbh.at(2).at(2).at(2)

              toGlobal(id) o toPrivate(fun(x =>
                jacobi13Plus(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

            }) ,5,1))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(5,1) o Map(Transpose()) o Transpose() $ input
      })

    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d13ptplus-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d13ptplus-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile) {
      new PrintWriter(orgFile) {
        try {
          write(source)
        } finally {
          close
        }
      }
      new PrintWriter(mssFile) {
        try {
          write(sourceMSS)
        } finally {
          close
        }
      }
    }
    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](source,originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](sourceMSS,lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def j3d19ptPlus = UserFun("j3d19ptPlus", Array("C", "N", "S", "E", "W", "F", "B",
    "FN", "BN", "FS", "BS", "FW", "BW", "NW", "SW", "FE", "BE", "NE", "SE"),
    """return C +  (F + B + N + S + E + W) +
      |        (FN + BN + FS + BS + FW + BW +
      |                  NW + SW + FE + BE + NE + SE);""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
      Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def j3d19ptMSSPlus: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            //              z     y     x
            val c  = nbh.at(1).at(1).at(1)
            val n  = nbh.at(1).at(0).at(1)
            val s  = nbh.at(1).at(2).at(1)
            val e  = nbh.at(1).at(1).at(2)
            val w  = nbh.at(1).at(1).at(0)
            val f  = nbh.at(0).at(1).at(1)
            val b  = nbh.at(2).at(1).at(1)
            val fn = nbh.at(0).at(0).at(1)
            val bn = nbh.at(2).at(0).at(1)
            val fs = nbh.at(0).at(2).at(1)
            val bs = nbh.at(2).at(2).at(1)
            val fw = nbh.at(0).at(1).at(0)
            val bw = nbh.at(2).at(1).at(0)
            val nw = nbh.at(1).at(0).at(0)
            val sw = nbh.at(1).at(2).at(0)
            val fe = nbh.at(0).at(1).at(2)
            val be = nbh.at(2).at(1).at(2)
            val ne = nbh.at(1).at(0).at(2)
            val se = nbh.at(1).at(2).at(2)


            toGlobal(id) o toPrivate(fun(x =>
              j3d19ptPlus(x, n, s, e, w, f, b, fn, bn, fs, bs, fw, bw, nw, sw, fe, be, ne, se))) $ c

          })))) o Slide3D(size,step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {
              //              z     y     x
              val c  = nbh.at(1).at(1).at(1)
              val n  = nbh.at(1).at(0).at(1)
              val s  = nbh.at(1).at(2).at(1)
              val e  = nbh.at(1).at(1).at(2)
              val w  = nbh.at(1).at(1).at(0)
              val f  = nbh.at(0).at(1).at(1)
              val b  = nbh.at(2).at(1).at(1)
              val fn = nbh.at(0).at(0).at(1)
              val bn = nbh.at(2).at(0).at(1)
              val fs = nbh.at(0).at(2).at(1)
              val bs = nbh.at(2).at(2).at(1)
              val fw = nbh.at(0).at(1).at(0)
              val bw = nbh.at(2).at(1).at(0)
              val nw = nbh.at(1).at(0).at(0)
              val sw = nbh.at(1).at(2).at(0)
              val fe = nbh.at(0).at(1).at(2)
              val be = nbh.at(2).at(1).at(2)
              val ne = nbh.at(1).at(0).at(2)
              val se = nbh.at(1).at(2).at(2)


              toGlobal(id) o toPrivate(fun(x =>
                j3d19ptPlus(x, n, s, e, w, f, b, fn, bn, fs, bs, fw, bw, nw, sw, fe, be, ne, se))) $ c

            }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })

    val lambda = SimplifyAndFuse(originalLambda)
    val source = Compile(lambda)

    val mssLambda = SimplifyAndFuse(lambdaMSS)
    val sourceMSS = Compile(mssLambda)

    val orgFile = outputdir+"j3d19ptplus-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d19ptplus-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile) {
      new PrintWriter(orgFile) {
        try {
          write(source)
        } finally {
          close
        }
      }
      new PrintWriter(mssFile) {
        try {
          write(sourceMSS)
        } finally {
          close
        }
      }
    }
    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def jacobi27Plus = UserFun("jacobi27Plus", Array("FNW", "FN", "FNE", "FW", "F", "FE", "FSW", "FS", "FSE",
    "NW", "N", "NE", "W", "C", "E", "SW", "S", "SE",
    "BNW", "BN", "BNE", "BW", "B", "BE", "BSW", "BS", "BSE"),
    """return ( FNW +  FN +  FNE +
      |         FW +  F +  FE +
      |         FSW +  FS +  FSE +
      |         NW +  N +  NE +
      |         W +  C +  E +
      |         SW +  S +  SE +
      |         BNW +  BN +  BNE +
      |         BW +  B +  BE +
      |         BSW +  BS +  BSE) ;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
      Float, Float, Float, Float, Float, Float, Float, Float, Float,
      Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  def calculate27ptStencilPlus(nbh: Param) =
  {
    //              z     y     x
    val fnw = nbh.at(0).at(0).at(0)
    val fn  = nbh.at(0).at(0).at(1)
    val fne = nbh.at(0).at(0).at(2)
    val fw  = nbh.at(0).at(1).at(0)
    val f   = nbh.at(0).at(1).at(1)
    val fe  = nbh.at(0).at(1).at(2)
    val fsw = nbh.at(0).at(2).at(0)
    val fs  = nbh.at(0).at(2).at(1)
    val fse = nbh.at(0).at(2).at(2)

    val nw  = nbh.at(1).at(0).at(0)
    val n   = nbh.at(1).at(0).at(1)
    val ne  = nbh.at(1).at(0).at(2)
    val w   = nbh.at(1).at(1).at(0)
    val c   = nbh.at(1).at(1).at(1)
    val e   = nbh.at(1).at(1).at(2)
    val sw  = nbh.at(1).at(2).at(0)
    val s   = nbh.at(1).at(2).at(1)
    val se  = nbh.at(1).at(2).at(2)

    val bnw = nbh.at(2).at(0).at(0)
    val bn  = nbh.at(2).at(0).at(1)
    val bne = nbh.at(2).at(0).at(2)
    val bw  = nbh.at(2).at(1).at(0)
    val b   = nbh.at(2).at(1).at(1)
    val be  = nbh.at(2).at(1).at(2)
    val bsw = nbh.at(2).at(2).at(0)
    val bs  = nbh.at(2).at(2).at(1)
    val bse = nbh.at(2).at(2).at(2)

    toGlobal(id) o toPrivate(fun(x =>
      jacobi27Plus(x, fn, fne, fw, f, fe, fsw, fs, fse,
        nw, n, ne, w, c, e, sw, s, se,
        bnw, bn, bne, bw, b, be, bsw, bs, bse))) $ fnw

  }

  @Test
  def jacobi27MSSPlus: Unit = {

    val ISflag = InlineStructs()
    InlineStructs(true)

    val size = 3
    val step = 1


    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            calculate27ptStencilPlus(nbh)

          })))) o Slide3D(size, step) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              calculate27ptStencilPlus(nbh)

            }) ,size,step))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ input
      })


    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d27ptplus-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d27ptplus-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile) {
      new PrintWriter(orgFile) {
        try {
          write(source)
        } finally {
          close
        }
      }
      new PrintWriter(mssFile) {
        try {
          write(sourceMSS)
        } finally {
          close
        }
      }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

  def jacobi21Plus = UserFun("jacobi21Plus", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C","EEE","WWW","SSS","NNN","BBB","FFF"),
    """return  EE +  E +  W +  WW +
      |        SS +  S +  N +  NN +
      |        BB +  B +  F +  FF +
      |        EEE +  WWW +
      |        SSS +  NNN +
      |        BBB +  FFF +
      |        C;""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

  @Test
  def jacobi21ptMSSPlus: Unit = {

    val ISflag = InlineStructs()
    //    InlineStructs(true)

    val originalLambda = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        Map(Map(Scatter(Shift(3)))) o
          Map(Scatter(Shift(3))) o
          Scatter(Shift(3)) o
          Pad3D(3, 3, 3, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(nbh => {

            //              z     y     x
            val ee = nbh.at(3).at(3).at(5)
            val eee = nbh.at(3).at(3).at(6)
            val e = nbh.at(3).at(3).at(4)
            val w = nbh.at(3).at(3).at(2)
            val ww = nbh.at(3).at(3).at(1)
            val www = nbh.at(3).at(3).at(0)
            val ss = nbh.at(3).at(5).at(3)
            val sss = nbh.at(3).at(6).at(3)
            val s = nbh.at(3).at(4).at(3)
            val n = nbh.at(3).at(2).at(3)
            val nn = nbh.at(3).at(1).at(3)
            val nnn = nbh.at(3).at(0).at(3)
            val bb = nbh.at(5).at(3).at(3)
            val bbb = nbh.at(6).at(3).at(3)
            val b = nbh.at(4).at(3).at(3)
            val f = nbh.at(2).at(3).at(3)
            val ff = nbh.at(1).at(3).at(3)
            val fff = nbh.at(0).at(3).at(3)
            val c = nbh.at(3).at(3).at(3)

            toGlobal(id) o toPrivate(fun(x =>
              jacobi21(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c,eee,www,sss,nnn,bbb,fff))) $ ee

          })))) o Slide3D(7, 1) $ input
      })

    val lambdaMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      input => {
        TransposeW() o Map(TransposeW()) o TransposeW() o
          Map(Map(Scatter(Shift(3)))) o
          Map(Scatter(Shift(3))) o
          Scatter(Shift(3)) o
          Pad3D(3, 3, 3, Pad.Boundary.Clamp) o
          MapGlb(0)(MapGlb(1)(fun( x => {
            toGlobal(MapSeqSlide(fun(nbh => {

              //              z     y     x
              val ee = nbh.at(3).at(3).at(5)
              val eee = nbh.at(3).at(3).at(6)
              val e = nbh.at(3).at(3).at(4)
              val w = nbh.at(3).at(3).at(2)
              val ww = nbh.at(3).at(3).at(1)
              val www = nbh.at(3).at(3).at(0)
              val ss = nbh.at(3).at(5).at(3)
              val sss = nbh.at(3).at(6).at(3)
              val s = nbh.at(3).at(4).at(3)
              val n = nbh.at(3).at(2).at(3)
              val nn = nbh.at(3).at(1).at(3)
              val nnn = nbh.at(3).at(0).at(3)
              val bb = nbh.at(5).at(3).at(3)
              val bbb = nbh.at(6).at(3).at(3)
              val b = nbh.at(4).at(3).at(3)
              val f = nbh.at(2).at(3).at(3)
              val ff = nbh.at(1).at(3).at(3)
              val fff = nbh.at(0).at(3).at(3)
              val c = nbh.at(3).at(3).at(3)

              toGlobal(id) o toPrivate(fun(x =>
                jacobi21(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c,eee,www,sss,nnn,bbb,fff))) $ ee

            }) ,7,1))} o Transpose() o Map(Transpose()) $ x))) o Transpose() o Slide2D(7,1) o Map(Transpose()) o Transpose() $ input
      })

    val source = Compile(originalLambda)

    val sourceMSS = Compile(lambdaMSS)

    val orgFile = outputdir+"j3d21ptplus-original-"+m+"-"+"n"+"-"+o+ext
    val mssFile = outputdir+"j3d21ptplus-MSS-"+m+"-"+"n"+"-"+o+ext

    if(printToFile)
    {
      new PrintWriter(orgFile) { try {write(source)} finally {close} }
      new PrintWriter(mssFile) { try {write(sourceMSS)} finally {close} }
    }

    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](source,originalLambda, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](sourceMSS,lambdaMSS, data)

    assertArrayEquals(output_org, output_MSS, delta)

    InlineStructs(ISflag)

  }

}

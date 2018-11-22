package opencl.generator.stencil

import ir.ArrayType
import ir.ast.{Get, Pad, Pad3D, Slide3D, UserFun, Zip3D, fun, _}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.{BoundaryUtilities, RoomConstants, StencilUtilities}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeqSlide, toGlobal, toPrivate}
import org.junit.Assert._
import org.junit.Test

object TestStencilsTACO extends TestWithExecutor

/**
  * Benchmarks used in TACO paper submission 2018
  */
class TestStencilsTACO {

  val delta = 0.1f

  val m = SizeVar("M")
  val n = SizeVar("N")
  val o = SizeVar("O")

  val localDimX = 8
  val localDimY = 6
  val localDimZ = 4

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

//    fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp)) $ tInC

    toGlobal(id) $ tInS

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
    val ct_amb_temp = fun(x => mult(x, ct)) $ amb_temp

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

   // fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp)) $ tInC

    toGlobal(id) $ tInS

  }


  @Test
  def originalHotSpot3D(): Unit = {

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

  }

  /**
    * TODO fix this test!!
    */
  @Test
  def MSSHotSpot3D(): Unit = {

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

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
        Map(TransposeW()) o TransposeW() o Map(TransposeW()) o
          MapGlb(0)(MapGlb(1)(fun(x => {
            toGlobal(MapSeqSlide(fun((m) => {
              rodiniaMSS(m)
            }), size, step))
          } o Transpose() o Map(Transpose()) $ x))) o Slide2D(size, step)  o Map(Transpose())  o Transpose() o Map(Transpose())  $ Zip3D(Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ power, /*Slide3D(size,step) o */ Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ temp)
      })

    /*
    val mssLambda = SimplifyAndFuse(stencilMSS)
    val source = Compile(mssLambda)//, NDRange(32,4,2), NDRange(n,m,1))
    println(source)
*/
    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](stencil, data, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](stencilMSS, data, data)

    assertArrayEquals(output_org, output_MSS, delta)

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

    val save = toPrivate(fun(x => mult(x, `tile[1][1][1]`))) o toPrivate(fun(x => subtract(2.0f, x))) o toPrivate(fun(x => mult(x, RoomConstants.l2))) $ valueMask

    val ret = toGlobal(id) o toPrivate(fun(x => mult(x, cf))) o toPrivate(addTuple) $
      Tuple(save,
        toPrivate(subtractTuple) $ Tuple(
          toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
          toPrivate(fun(x => mult(x, cf2))) $ valueMat1))

    toGlobal(id) $ valueMask
  }

  def acousticMSS(m: Param) = {

    val cf = toPrivate(fun(x => getCF(x, RoomConstants.cf(0), RoomConstants.cf(1)))) $ Get(m.at(1).at(1).at(1), 2)
    val cf2 = toPrivate(fun(x => getCF(x, RoomConstants.cf2(0), RoomConstants.cf2(1)))) $ Get(m.at(1).at(1).at(1), 2)
    val maskedValStencil = RoomConstants.l2
    val valueMat1 = Get(m.at(1).at(1).at(1), 0)
    val valueMask = toPrivate(BoundaryUtilities.idIF) $ Get(m.at(1).at(1).at(1), 2)

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
      toPrivate(fun(x => add(x, `tile[1][1][2]`))) o
      toPrivate(fun(x => add(x, `tile[1][2][1]`))) $ `tile[2][1][1]`

    val save = toPrivate(fun(x => mult(x, `tile[1][1][1]`))) o toPrivate(fun(x => subtract(2.0f, x))) o toPrivate(fun(x => mult(x, RoomConstants.l2))) $ valueMask

    val ret = toGlobal(id) o toPrivate(fun(x => mult(x, cf))) o toPrivate(addTuple) $
      Tuple(save,
        toPrivate(subtractTuple) $ Tuple(
          toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
          toPrivate(fun(x => mult(x, cf2))) $ valueMat1))
    toGlobal(id) $ valueMask

  }

  @Test
  def originalAcoustic3D(): Unit = {

    val arraySig = ArrayType(ArrayType(ArrayType(Int, m), n), o)

    val size = 3
    val step = 1

    val acousticStencil =
      fun(
        ArrayType(ArrayType(ArrayType(Float, m), n), o),
        ArrayType(ArrayType(ArrayType(Float, m + 2), n + 2), o + 2),
        (mat1, mat2) => {
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            acoustic(m)
          })))
          ) $ Zip3D(mat1, Slide3D(size, step) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
        })

  }

  @Test
  def MSSAcoustic3D(): Unit = {

    val arraySigmno = ArrayType(ArrayType(ArrayType(Int, m), n), o)
    val arraySigmno2 = ArrayType(ArrayType(ArrayType(Int, m + 2), n + 2), o + 2)
    val arraySigonm = ArrayType(ArrayType(ArrayType(Int, o), n), m)
    val arraySigonm2 = ArrayType(ArrayType(ArrayType(Int, o + 2), n + 2), m + 2)

    val size = 3
    val step = 1

    val aStencil =
      fun(
        ArrayType(ArrayType(ArrayType(Float, m),n),o),
        ArrayType(ArrayType(ArrayType(Float, m),n),o),
        (mat1, mat2) => {
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            acoustic(m)
          })))
            /*) $ Zip3D(mat1, Slide3D(size,step) o PadConstant3D(1,1,1,0.0f) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigonm))*/
            // this should work, but doesn't, etc
          ) o Slide3D(size, step) $ Zip3D(PadConstant3D(1, 1, 1, 0.0f) $ mat1, PadConstant3D(1, 1, 1, 0.0f) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno2))
        })

    val aStencilMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, m),n),o),
      ArrayType(ArrayType(ArrayType(Float, m),n),o),
      (mat1, mat2) => {
        /*        Map(TransposeW()) o TransposeW() o Map(TransposeW()) o*/
        MapGlb(0)(MapGlb(1)(fun(x => {
          toGlobal(MapSeqSlide(fun((m) => {
            acousticMSS(m)
          }), size, step))
        } o Transpose() o Map(Transpose()) $ x))) o Slide2D(size, step) /* o Map(Transpose())  o Transpose() o Map(Transpose())*/ $ Zip3D(PadConstant3D(1, 1, 1, 0.0f) $ mat1, PadConstant3D(1, 1, 1, 0.0f) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno2))
      })
    /*
    val mssLambda = SimplifyAndFuse(aStencilMSS)
    val source = Compile(mssLambda)//, NDRange(32,4,2), NDRange(n,m,1))
    println(source)
*/
    val (output_org: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](aStencil, data, data)
    val (output_MSS: Array[Float], _) = Execute(2, 2, 2, 2, 2, 2, (true, true))[Array[Float]](aStencilMSS, data, data)

    assertArrayEquals(output_MSS, output_org, delta)

  }


  /**
    * TODO Add MSS tests for the PPCG 3D stencils !!
    */

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

  @Test def j3d7pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("C", "N", "S", "E", "W", "F", "B"),
      """return 0.161f * E + 0.162f * W +
      0.163f * S + 0.164f * N +
      0.165f * B + 0.166f * F -
      1.67f * C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

                      toGlobal(id) o toPrivate(λ(x =>
                        jacobi(x, n, s, e, w, f, b))) $ c

          })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test def heat3d: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)

    def heat = UserFun("heat", Array("C", "S", "N", "E", "W", "B", "F"),
      """return 0.125f * (B - 2.0f * C + F) +
        |       0.125f * (S - 2.0f * C + N) +
        |       0.125f * (E - 2.0f * C + W) + C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1, 1, 1, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

            val (n, s, w, e, f, b, c) = vonNeumann7pt(nbh)

            toGlobal(id) o toPrivate(λ(x =>
              heat(x, n, s, e, w, f, b))) $ c

          })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }


  @Test def j3d13pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("EE", "E", "W", "WW", "SS", "S", "N", "NN", "BB", "B", "F", "FF", "C"),
      """return 0.083f * EE + 0.083f * E + 0.083f * W + 0.083f * WW +
        |       0.083f * SS + 0.083f * S + 0.083f * N + 0.083f * NN +
        |       0.083f * BB + 0.083f * B + 0.083f * F + 0.083f * FF -
        |       0.996f * C;""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(Shift(2)))) o
          Map(Scatter(Shift(2))) o
          Scatter(Shift(2)) o
          Pad3D(2, 2, 2, Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

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

            toGlobal(id) o toPrivate(λ(x =>
              jacobi(x, e, w, ww, ss, s, n, nn, bb, b, f, ff, c))) $ ee

          })))) o Slide3D(5, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }
  @Test def poisson3d: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def poisson = UserFun("jacobi", Array("C", "N", "S", "E", "W", "F", "B",
      "FN", "BN", "FS", "BS", "FW", "BW", "NW", "SW", "FE", "BE", "NE", "SE"),
      """return 2.666f * C - 0.166f * (F + B + N + S + E + W) -
        |       0.0833f * (FN + BN + FS + BS + FW + BW +
        |                  NW + SW + FE + BE + NE + SE);""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, Float, Float,
        Float, Float, Float, Float, Float, Float, Float, Float, Float, Float), Float)

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

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


            toGlobal(id) o toPrivate(λ(x =>
              poisson(x, n, s, e, w, f, b, fn, bn, fs, bs, fw, bw, nw, sw, fe, be, ne, se))) $ c

          })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }


  /**
   * TODO where is heat3d??
   */

  @Test def j3d27pt: Unit = {
    val M = 512
    val N = 512
    val O = 512

    // [X-1][][] = F(ront) [X+1][][] = B(ack)
    // [][X-1][] = N(orth) [][X+1][] = S(outh)
    // [][][X-1] = W(est)  [][][X+1] = E(ast)
    def jacobi = UserFun("jacobi", Array("FNW", "FN", "FNE", "FW", "F", "FE", "FSW", "FS", "FSE",
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

    val lambda = λ(
      ArrayType(ArrayType(ArrayType(Float, M), N), O),
      input => {
        Map(Map(Scatter(Shift(1)))) o
          Map(Scatter(Shift(1))) o
          Scatter(Shift(1)) o
          Pad3D(1,1,1,Pad.Boundary.Clamp) o
          MapGlb(2)(MapGlb(1)(MapGlb(0)(λ(nbh => {

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

            toGlobal(id) o toPrivate(λ(x =>
              jacobi(x, fn, fne, fw, f, fe, fsw, fs, fse,
                nw, n, ne, w, c, e, sw, s, se,
                bnw, bn, bne, bw, b, be, bsw, bs, bse))) $ fnw

          })))) o Slide3D(3, 1) $ input
      })

    val kernel = Compile(lambda)
    println(kernel)
  }
}

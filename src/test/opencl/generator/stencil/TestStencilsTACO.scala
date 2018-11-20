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
import rewriting.SimplifyAndFuse

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

  /*** Hotspot3D ***/

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
    val ct_amb_temp = fun(x => mult(x, ct)) $ amb_temp

    val tInC = Get(m.at(1).at(1).at(1),1)
    val tIncCC = toPrivate(fun(x => mult(x, cc))) $ tInC

    val tInW = Get(m.at(0).at(1).at(1),1)
    val tIncW = toPrivate(fun(x => mult(x, cw))) $ tInW

    val tInN = Get(m.at(1).at(0).at(1),1)
    val tIncN = toPrivate(fun(x => mult(x, cn))) $ tInN

    val tInB = Get(m.at(1).at(1).at(0),1)
    val tIncB = toPrivate(fun(x => mult(x, cb))) $ tInB

    val tInT = Get(m.at(1).at(1).at(2),1)
    val tIncT = toPrivate(fun(x => mult(x, ct))) $ tInT

    val tInS = Get(m.at(1).at(2).at(1),1)

    val tIncS = toPrivate(fun(x => mult(x, cs))) $ tInS

    val tInE = Get(m.at(2).at(1).at(1),1)
    val tIncE = toPrivate(fun(x => mult(x, ce))) $ tInE

    val pInc = Get(m.at(1).at(1).at(1),0)
    val pcSDC = toPrivate(fun(x => mult(x, stepDivCap))) $ pInc

    fun(x => calculateHotspot(x, cc, tInN, cn, tInS, cs, tInE, ce, tInW, cw, tInT, ct, tInB, cb, stepDivCap, pInc, amb_temp)) $ tInC

  }


  @Test
  def originalHotSpot3D(): Unit =
  {

     val size = 3
     val step = 1

     val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      (temp, power) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun((m) => {
          rodinia(m)
        })))
        ) $ Zip3D(power, Slide3D(size,step) o Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ temp)
      })

  }

  @Test
  def MSSHotSpot3D(): Unit =
  {

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
        ) $ Zip3D(power, Slide3D(size,step) o Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ temp)
      })

    val stencilMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, o), n), m),
      ArrayType(ArrayType(ArrayType(Float, o), n), m),
      (temp, power) => {
        Map(TransposeW()) o TransposeW() o Map(TransposeW()) o
        MapGlb(0)(MapGlb(1)(fun (x => { toGlobal(MapSeqSlide(fun((m) => {
          rodiniaMSS(m)
        }),size,step)) } o Transpose() o Map(Transpose()) $ x ))) o  Slide2D(size,step) o Map(Transpose())  o Transpose() o Map(Transpose()) $ Zip3D( Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ power, /*Slide3D(size,step) o */ Pad3D(1, 1, 1, Pad.Boundary.Clamp) $  temp)
      })


    val mssLambda = SimplifyAndFuse(stencilMSS)
    val source = Compile(mssLambda)//, NDRange(32,4,2), NDRange(n,m,1))
    println(source)

    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](stencil, data,data)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](stencilMSS, data,data)

    assertArrayEquals(output_MSS, output_org, delta)

  }

  /*** Acoustic ***/

  val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }", Seq(Int,Int,Int,Int,Int,Int), Int)
  val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int,Float,Float), Float)

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

  def acoustic(m: Param) =
  {

          val cf = toPrivate( fun(x => getCF(x,cf1(0), cf1(1))) ) $ Get(m,2)
          val cf2 = toPrivate( fun(x => getCF(x,cf21(0), cf21(1))) ) $ Get(m,2)
          val maskedValStencil = l2

          val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

          val `tile[0][1][1]` = Get(m,1).at(0).at(1).at(1)
          val `tile[1][0][1]` = Get(m,1).at(1).at(0).at(1)
          val `tile[1][1][0]` = Get(m,1).at(1).at(1).at(0)
          val `tile[1][1][2]` = Get(m,1).at(1).at(1).at(2)
          val `tile[1][2][1]` = Get(m,1).at(1).at(2).at(1)
          val `tile[2][1][1]` = Get(m,1).at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          val valueMat1 = Get(m,0)
          val valueMask = toPrivate(idIF) $ Get(m,2)

          toGlobal(id) o toPrivate(fun( x => mult(x,cf))) o toPrivate(addTuple) $
            Tuple(toPrivate(multTuple) $ Tuple(toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,l2))) $ valueMask, `tile[1][1][1]`),
              toPrivate(subtractTuple) $ Tuple(
                toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
                toPrivate(fun(x => mult(x,cf2))) $ valueMat1))
  }

  def acousticMSS(m: Param) =
  {

    val cf = toPrivate( fun(x => getCF(x,RoomConstants.cf(0), RoomConstants.cf(1))) ) $ Get(m.at(1).at(1).at(1),2)
    val cf2 = toPrivate( fun(x => getCF(x,RoomConstants.cf2(0), RoomConstants.cf2(1))) ) $ Get(m.at(1).at(1).at(1),2)
    val maskedValStencil = RoomConstants.l2
    val valueMat1 = Get(m.at(1).at(1).at(1),0)
    val valueMask = toPrivate(BoundaryUtilities.idIF) $ Get(m.at(1).at(1).at(1),2)

    val `tile[1][1][1]` = Get(m.at(1).at(1).at(1),1)

    val `tile[0][1][1]` = Get(m.at(0).at(1).at(1),1)
    val `tile[1][0][1]` = Get(m.at(1).at(0).at(1),1)
    val `tile[1][1][0]` = Get(m.at(1).at(1).at(0),1)
    val `tile[1][1][2]` = Get(m.at(1).at(1).at(2),1)
    val `tile[1][2][1]` = Get(m.at(1).at(2).at(1),1)
    val `tile[2][1][1]` = Get(m.at(2).at(1).at(1),1)

    val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
      toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
      toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
      toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
      toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
      toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

    val save = toPrivate(fun( x => mult(x,`tile[1][1][1]`))) o toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,RoomConstants.l2))) $ valueMask

    toGlobal(id) o toPrivate(fun( x => mult(x,cf))) o toPrivate(addTuple) $
      Tuple(save,
        toPrivate(subtractTuple) $ Tuple(
          toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
          toPrivate(fun(x => mult(x,cf2))) $ valueMat1))

  }

  @Test
  def originalAcoustic3D(): Unit =
  {

    val arraySig = ArrayType(ArrayType(ArrayType(Int, m), n), o)

    val size = 3
    val step = 1

    val acousticStencil =
    fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m+2), n+2), o+2),
      (mat1, mat2) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            acoustic(m)
        })))
        ) $ Zip3D(mat1, Slide3D(size,step) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
      })

  }

  @Test
  def MSSAcoustic3D(): Unit =
  {

    val arraySigmno = ArrayType(ArrayType(ArrayType(Int, m), n), o)
    val arraySigonm = ArrayType(ArrayType(ArrayType(Int, o), n), m)

    val size = 3
    val step = 1

    val aStencil =
      fun(
        ArrayType(ArrayType(ArrayType(Float, m), n), o),
        ArrayType(ArrayType(ArrayType(Float, m), n), o),
        (mat1, mat2) => {
          MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            acoustic(m)
          })))
          ) $ Zip3D(mat1, Slide3D(size,step) o PadConstant3D(1,1,1,0.0f) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySigmno))
        })

    val aStencilMSS = fun(
      ArrayType(ArrayType(ArrayType(Float, o), n), m),
      ArrayType(ArrayType(ArrayType(Float, o), n), m),
      (mat1, mat2) => {
        Map(TransposeW()) o TransposeW() o Map(TransposeW()) o
          MapGlb(0)(MapGlb(1)(fun (x => { toGlobal(MapSeqSlide(fun((m) => {
            acousticMSS(m)
          }),size,step)) } o Transpose() o Map(Transpose()) $ x ))) o  Slide2D(size,step) o Map(Transpose())  o Transpose() o Map(Transpose()) $ Zip3D( PadConstant3D(1,1,1,0.0f) $ mat1, PadConstant3D(1,1,1,0.0f) $ mat2, PadConstant3D(1,1,1,0.0f) $ Array3DFromUserFunGenerator(getNumNeighbours, arraySigonm))
      })

    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](aStencil, data,data)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](aStencilMSS, data,data)

    assertArrayEquals(output_MSS, output_org, delta)

  }


  /**
    * TODO Add tests for the PPCG 3D stencils !!
    */

}

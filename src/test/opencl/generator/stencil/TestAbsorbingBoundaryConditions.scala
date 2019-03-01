package opencl.generator.stencil

import ir.ArrayTypeWSWC
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.{AcousticComparisonArrays, BoundaryUtilities, RoomConstants, StencilUtilities}
import opencl.ir._
import opencl.ir.pattern._
import org.junit._
import org.junit.Assert._
import rewriting.SimplifyAndFuse

import scala.language.implicitConversions

object TestAbsorbingBoundaryConditions extends TestWithExecutor

object ABCConstants {

  val SR = 441.0f
  val alpha = 0.005f
  val c = 344.0f
  val NF = 4410
  val k = 1 / SR
  val h = Math.sqrt(3.0f) * c * k
  val lambda = c * k / h

  val loss1 = 1.0f / (1.0f + lambda * alpha)
  val loss2 = 1.0f - lambda * alpha

  val l2 = ((c * c * k * k) / (h * h)).toFloat
  val cf = Array(loss1.toFloat, 1.0f)
  val cf2 = Array(loss2.toFloat, 1.0f)

}

object ABCStencilHelpers{

  val O = 2 + SizeVar("O")
  val N = 2 + SizeVar("N")
  val M = 2 + SizeVar("M")

  def original1DStencil(size: Int, step: Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      MapGlb(0)(
        fun(neighbours => {
          toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp, 0.0f) $ neighbours
        })) o Slide(size, step) $ input
  )

  def stencil1D(a: Int, b: Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      toGlobal(MapSeqSlide(MapSeqUnroll(id) o ReduceSeqUnroll(absAndSumUp, 0.0f), a, b)) $ input
  )
}

class TestAbsorbingBoundaryConditions
{

  /** 1D **/
  // do stencil, access just boundary points
  @Test
  def joinMainStencilAndBoundary1D(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val bL = values(0)
    val bR = values(size-1)
    val bAdded = bL + bR
    val padValue = 0
    val padLR = Array.fill(1)(padValue.toFloat)
    val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR
    val gold = paddedValues.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).map(z => z*bAdded)

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        MapGlb(0)(fun(neighbourhood => {

//          val main = toPrivate(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          val main = PrintType()  o toPrivate(MapSeqUnroll(id)) o /* toPrivate(id) o*/ ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          val stencil = main.at(0)
          val boundaryL = PrintType() o toPrivate(id) $ input.at(0)
          val boundaryR = toPrivate(id) $ input.at(N-1)

          val returnValue =  toGlobal(id) o toPrivate(fun(x => mult(x,stencil))) o
            toPrivate(fun(x => add(x,boundaryL))) $ boundaryR

           returnValue

          })) o Slide(a,b) o PadConstant(1,1,0.0f) $ input // Zip( , 0.0f) // ArrayFromUserFunGenerator(0,ArrayTypeWSWC(Float,size+2)), ArrayFromValue(input.at(N-1),ArrayTypeWSWC(Float,size+2)))
      }
    )
//    println(Compile(stencil1D(3,1)))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

//    StencilUtilities.print1DArray(values)
//    StencilUtilities.print1DArray(gold)
//    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }


  @Test
  def test3DAsymMaskStencilWithAtAndArrayGen(): Unit = {

    val compareData = AcousticComparisonArrays.test3DAsymMaskStencilComparisonData8x6x10

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
      "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }", Seq(Int,Int,Int,Int,Int,Int), Int)

    val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int,Float,Float), Float)


    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val dx = 256
    val dy = 256
    val dz = 202

    val arraySig = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, dx), dy), dz)

    val lambdaNeighAt = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dx), dy), dz),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dx+2), dy+2), dz+2),
      (mat1, mat2) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)(fun(m => {

          val cf = toPrivate( fun(x => getCF(x,RoomConstants.cf(0), RoomConstants.cf(1))) ) $ Get(m,2)
          val cf2 = toPrivate( fun(x => getCF(x,RoomConstants.cf2(0), RoomConstants.cf2(1))) ) $ Get(m,2)
          val maskedValStencil = RoomConstants.l2

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
          val valueMask = toPrivate(BoundaryUtilities.idIF) $ Get(m,2)

          toGlobal(id) o toPrivate(fun( x => mult(x,cf))) o toPrivate(addTuple) $
            Tuple(toPrivate(multTuple) $ Tuple(toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,RoomConstants.l2))) $ valueMask, `tile[1][1][1]`),
              toPrivate(subtractTuple) $ Tuple(
                toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
                toPrivate(fun(x => mult(x,cf2))) $ valueMat1))

        })))) o PrintType() $ Zip3D(mat1, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
      })

    val newLambda = SimplifyAndFuse(lambdaNeighAt)
    //val source = Compile(newLambda, 64,4,2,dx,dy,dz, immutable.Map())
    val source = Compile(newLambda)
    println(source)

}


}





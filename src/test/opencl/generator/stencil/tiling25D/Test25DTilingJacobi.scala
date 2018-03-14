package opencl.generator.stencil.tiling25D

import opencl.executor.TestWithExecutor
import ir.ArrayTypeWSWC
import ir.ast.{Get, Slide, Zip, fun, _}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.{BoundaryUtilities, RoomConstants, StencilUtilities}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._
import rewriting.SimplifyAndFuse

import scala.collection.parallel
import scala.collection.parallel.immutable

/**
  */

object Test25DTilingJacobi extends TestWithExecutor

object JacobiHelperFunctions
{

  val O = SizeVar("O")
  val N = SizeVar("N")
  val M = SizeVar("M")

  def original7ptJacobi(a: Int, b: Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
    (mat) => {
      (MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

        val `tile[0][0][0]` = m.at(0).at(0).at(0)

        val `tile[1][1][1]` = m.at(1).at(1).at(1)
        val `tile[0][1][1]` = m.at(0).at(1).at(1)
        val `tile[1][0][1]` = m.at(1).at(0).at(1)
        val `tile[1][1][0]` = m.at(1).at(1).at(0)
        val `tile[1][1][2]` = m.at(1).at(1).at(2)
        val `tile[1][2][1]` = m.at(1).at(2).at(1)
        val `tile[2][1][1]` = m.at(2).at(1).at(1)

        val stencil =  fun(x => add(x,`tile[0][1][1]`)) o
          fun(x => add(x,`tile[1][1][1]`)) o
          fun(x => add(x,`tile[1][0][1]`)) o
          fun(x => add(x,`tile[1][1][0]`)) o
          fun(x => add(x,`tile[1][1][2]`)) o
          fun(x => add(x,`tile[1][2][1]`)) $ `tile[2][1][1]`

        toGlobal(id) $ stencil
      })))) o Slide3D(a,b) o PadConstant3D(1,1,1,0.0f)  $ mat)
    })

  def MSS7ptJacobi(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
    (input) =>
      MapGlb(1)(MapGlb(0)(fun(x => {
        toGlobal(MapSeqSlide(
          fun(m => {

            val `tile[0][0][0]` = m.at(0).at(0).at(0)

            val `tile[1][1][1]` = m.at(1).at(1).at(1)
            val `tile[0][1][1]` = m.at(0).at(1).at(1)
            val `tile[1][0][1]` = m.at(1).at(0).at(1)
            val `tile[1][1][0]` = m.at(1).at(1).at(0)
            val `tile[1][1][2]` = m.at(1).at(1).at(2)
            val `tile[1][2][1]` = m.at(1).at(2).at(1)
            val `tile[2][1][1]` = m.at(2).at(1).at(1)

            val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`)) o
              fun(x => add(x,`tile[1][1][1]`)) o
              fun(x => add(x,`tile[1][0][1]`)) o
              fun(x => add(x,`tile[1][1][0]`)) o
              fun(x => add(x,`tile[1][1][2]`)) o
              fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

            toGlobal(id) $ stencil
            //toGlobal(id) $ `tile[0][0][0]`

          }), a,b)) o Transpose() o Map(Transpose()) $ x }))) o Slide2D(a,b) o PadConstant3D(1,1,1,0.0f) $ input
  )

}


class Test25DTilingJacobi {

  @Test
  def jacobi7ptMSSComparison(): Unit = {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    /*
    val localDimX = 510
    val localDimY = 510
    val localDimZ = 62
    */

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val Nx = localDimX
    val Ny = localDimY
    val Nz = localDimZ


    val orgLambda = SimplifyAndFuse(JacobiHelperFunctions.original7ptJacobi(slidesize,slidestep))
    //val sourceOrg = Compile(orgLambda)//, NDRange(32,4,2), NDRange(n,m,1))
    val sourceOrg = Compile(orgLambda)
    println(sourceOrg)


    val lambdaMSS = SimplifyAndFuse(JacobiHelperFunctions.MSS7ptJacobi(slidesize,slidestep))
    val sourceMSS = Compile(lambdaMSS)//, NDRange(32,4,2), NDRange(n,m,1))
    //val sourceMSS = Compile(lambdaMSS,64,4,1,Nx,Ny,Nz, immutable.Map())
    println(sourceMSS)

    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](JacobiHelperFunctions.original7ptJacobi(slidesize,slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](JacobiHelperFunctions.MSS7ptJacobi(slidesize,slidestep), data)


    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

    /*
    StencilUtilities.print1DArrayAs3DArray(output_org,Nx,Ny,Nz)
    StencilUtilities.printOriginalAndOutput3DSame(dataUp,output_org)
    println("*******************************")
    StencilUtilities.print1DArray(output_org)
    //StencilUtilities.print1DArrayAs3DArray(output_MSS,Nx,Ny,Nz)
    */
  }

}

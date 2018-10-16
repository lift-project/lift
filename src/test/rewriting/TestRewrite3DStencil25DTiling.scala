package rewriting

import ir.ArrayTypeWSWC
import ir.ast.{Slide2D, Slide3D, Transpose, fun, _}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern.{MapGlb, toPrivate, _}
import org.junit.Assert._
import org.junit._

object TestRewrite3DStencil25DTiling extends TestWithExecutor

class TestRewrite3DStencil25DTiling
{

  @Test
  def stencil3DJacobiMSSComparisons(): Unit = {

    val localDimX = 10
    val localDimY = 10
    val localDimZ = 6

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPaddingInOrder(localDimX, localDimY, localDimZ)


    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")


    def jacobi3D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2), n+2), m+2),
      (mat) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[1][1][1]` = m.at(1).at(1).at(1)
          val `tile[0][1][1]` = m.at(0).at(1).at(1)
          val `tile[1][0][1]` = m.at(1).at(0).at(1)
          val `tile[1][1][0]` = m.at(1).at(1).at(0)
          val `tile[1][1][2]` = m.at(1).at(1).at(2)
          val `tile[1][2][1]` = m.at(1).at(2).at(1)
          val `tile[2][1][1]` = m.at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          toGlobal(id) $ stencil

        })))) o Slide3D(a,b) $ mat
      })

    def jacobi3Dmapseqslide(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2),n+2),m+2),
      (mat) =>
        MapGlb(1)(MapGlb(0)( fun (x => {
          toGlobal(MapSeqSlide(fun(m => {

            val `tile[1][1][1]` = m.at(1).at(1).at(1)
            val `tile[0][1][1]` = m.at(0).at(1).at(1)
            val `tile[1][0][1]` = m.at(1).at(0).at(1)
            val `tile[1][1][0]` = m.at(1).at(1).at(0)
            val `tile[1][1][2]` = m.at(1).at(1).at(2)
            val `tile[1][2][1]` = m.at(1).at(2).at(1)
            val `tile[2][1][1]` = m.at(2).at(1).at(1)

            val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
              toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
              toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
              toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
              toPrivate(fun(x => add(x,`tile[1][1][1]`))) o
              toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

            toGlobal(id) $ stencil

          }),a,b))  } o Transpose() o Map(Transpose()) $ x

        ))) o Slide2D(a,b) $ mat)


    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3D(slidesize,slidestep), stencilarrpadded3D)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3Dmapseqslide(slidesize,slidestep), stencilarrpadded3D)

    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

  }

  @Test
  def stencil3DJacobiMSSComparisonsWithRewriteRule(): Unit = {

    val localDimX = 10
    val localDimY = 10
    val localDimZ = 6

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPaddingInOrder(localDimX, localDimY, localDimZ)


    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")


    def jacobi3D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2), n+2), m+2),
      (mat) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[1][1][1]` = m.at(1).at(1).at(1)
          val `tile[0][1][1]` = m.at(0).at(1).at(1)
          val `tile[1][0][1]` = m.at(1).at(0).at(1)
          val `tile[1][1][0]` = m.at(1).at(1).at(0)
          val `tile[1][1][2]` = m.at(1).at(1).at(2)
          val `tile[1][2][1]` = m.at(1).at(2).at(1)
          val `tile[2][1][1]` = m.at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          toGlobal(id) $ stencil

        })))) o Slide3D(a,b) $ mat
      })

    def jacobi3DMapSeqSlideWithRule(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2),n+2),m+2),
      (mat) => mat)


    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3D(slidesize,slidestep), stencilarrpadded3D)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3DMapSeqSlideWithRule(slidesize,slidestep), stencilarrpadded3D)

    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

  }

  @Test
  def stencil3DJacobiComparisonsCoalescedWithPadConstant(): Unit = {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPaddingInOrder(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val Nx = localDimX
    val Ny = localDimY
    val Nz = localDimZ

    def jacobi3D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, Nx),Ny),Nz),
      (mat) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[1][1][1]` = m.at(1).at(1).at(1)
          val `tile[0][1][1]` = m.at(0).at(1).at(1)
          val `tile[1][0][1]` = m.at(1).at(0).at(1)
          val `tile[1][1][0]` = m.at(1).at(1).at(0)
          val `tile[1][1][2]` = m.at(1).at(1).at(2)
          val `tile[1][2][1]` = m.at(1).at(2).at(1)
          val `tile[2][1][1]` = m.at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          toGlobal(id)  $ stencil

        })))) o Slide3D(a,b) o PadConstant3D(1,1,1,0.0f) $ mat
      })

    def jacobi3DMapSeqSlide(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, Nx),Ny),Nz),
      (mat) =>
        Map(TransposeW()) o TransposeW() o Map(TransposeW()) o
          MapGlb(0)(MapGlb(1)( fun (x => {
            toGlobal(MapSeqSlide(fun(m => {

              val `tile[1][1][1]` = m.at(1).at(1).at(1)
              val `tile[0][1][1]` = m.at(0).at(1).at(1)
              val `tile[1][0][1]` = m.at(1).at(0).at(1)
              val `tile[1][1][0]` = m.at(1).at(1).at(0)
              val `tile[1][1][2]` = m.at(1).at(1).at(2)
              val `tile[1][2][1]` = m.at(1).at(2).at(1)
              val `tile[2][1][1]` = m.at(2).at(1).at(1)

              val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
                toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
                toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
                toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
                toPrivate(fun(x => add(x,`tile[1][1][1]`))) o
                toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

              toGlobal(id) $ stencil

            }),a,b))  } o Transpose() o Map(Transpose()) $ x
          ))) o Slide2D(a,b) o Map(Transpose())  o Transpose() o Map(Transpose()) o PadConstant3D(1,1,1,0.0f) $ mat)



    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3D(slidesize,slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3DMapSeqSlide(slidesize,slidestep), data)


    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

  }

  @Test
  def stencil3DJacobiComparisonsCoalescedWithPadConstantWithRewriteRule(): Unit = {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 4

    val slidesize = 3
    val slidestep = 1

    val data = StencilUtilities.createDataFloat3DInOrder(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPaddingInOrder(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val Nx = localDimX
    val Ny = localDimY
    val Nz = localDimZ

    def jacobi3D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, Nx),Ny),Nz),
      (mat) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[1][1][1]` = m.at(1).at(1).at(1)
          val `tile[0][1][1]` = m.at(0).at(1).at(1)
          val `tile[1][0][1]` = m.at(1).at(0).at(1)
          val `tile[1][1][0]` = m.at(1).at(1).at(0)
          val `tile[1][1][2]` = m.at(1).at(1).at(2)
          val `tile[1][2][1]` = m.at(1).at(2).at(1)
          val `tile[2][1][1]` = m.at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          toGlobal(id)  $ stencil

        })))) o Slide3D(a,b) o PadConstant3D(1,1,1,0.0f) $ mat
      })

    def jacobi3DMapSeqSlideWithRewriteRule(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, Nx),Ny),Nz),
      (mat) => mat)

    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3D(slidesize,slidestep), data)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3DMapSeqSlideWithRewriteRule(slidesize,slidestep), data)


    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)

  }

}

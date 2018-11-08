package rewriting

import ir.ast.debug.PrintType
import ir.ast.{Slide2D, Slide3D, Transpose, fun, _}
import ir.{ArrayType, ArrayTypeWSWC}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.MapSeqSlideHelpers
import opencl.generator.stencil.acoustic.{BoundaryUtilities, RoomConstants, StencilUtilities}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, toPrivate, _}
import org.junit.Assert._
import org.junit._
import rewriting.macrorules.MapSeqSlideRewrite
import rewriting.rules.Rules
import rewriting.utils.NumberExpression

object TestRewriteStencil25DTiling extends TestWithExecutor

class TestRewriteStencil25DTiling
{

  val O = 2 + SizeVar("O")
  val N = 2 + SizeVar("N")
  val M = 2 + SizeVar("M")

  def original1DStencil(size: Int, step: Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      MapSeq(
        fun(neighbours => {
          toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbours
        } )) o Slide(size,step) $ input
  )

  def stencil1D(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      toGlobal(MapSeqSlide(MapSeqUnroll(id) o ReduceSeqUnroll(absAndSumUp,0.0f), a,b)) $ input
  )

  def original2DStencil(size: Int, step: Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (input) => {
      MapSeq(
        MapSeq(fun(neighbours => {
          toGlobal(MapSeqUnroll(id)) o
            ReduceSeq(add, 0.0f) o Join() $ neighbours
        }))
      ) o Slide2D(size,step) $ input
    })


  def stencil2D(size: Int, step :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (input) =>
      Map(fun(x => {
        toGlobal(MapSeqSlide(MapSeq(id) o ReduceSeq(add, 0.0f) o Join(), size, step)) o Transpose() $ x
      })) o Slide(size,step)  $ input
  )

  def jacobi2D(m: Param) = {
    val `tile[1][1]` = m.at(1).at(1)
    val `tile[0][1]` = m.at(0).at(1)
    val `tile[1][0]` = m.at(1).at(0)
    val `tile[1][2]` = m.at(1).at(2)
    val `tile[2][1]` = m.at(2).at(1)

    val stencil = toPrivate(fun(x => add(x, `tile[1][1]`))) o
      toPrivate(fun(x => add(x, `tile[0][1]`))) o
      toPrivate(fun(x => add(x, `tile[1][0]`))) o
      toPrivate(fun(x => add(x, `tile[1][2]`))) $ `tile[2][1]`

    toGlobal(id) $ stencil
  }

  def jacobi3D(m: Param) = {
    val `tile[1][1][1]` = m.at(1).at(1).at(1)
    val `tile[0][1][1]` = m.at(0).at(1).at(1)
    val `tile[1][0][1]` = m.at(1).at(0).at(1)
    val `tile[1][1][0]` = m.at(1).at(1).at(0)
    val `tile[1][1][2]` = m.at(1).at(1).at(2)
    val `tile[1][2][1]` = m.at(1).at(2).at(1)
    val `tile[2][1][1]` = m.at(2).at(1).at(1)

    val stencil = toPrivate(fun(x => add(x, `tile[0][1][1]`))) o
      toPrivate(fun(x => add(x, `tile[1][0][1]`))) o
      toPrivate(fun(x => add(x, `tile[1][1][0]`))) o
      toPrivate(fun(x => add(x, `tile[1][1][2]`))) o
      toPrivate(fun(x => add(x, `tile[1][1][1]`))) o
      toPrivate(fun(x => add(x, `tile[1][2][1]`))) $ `tile[2][1][1]`

    toGlobal(id) $ stencil
  }


  def userFun3D(m: Param) = {

    val `tile[1][1][1]L` = Get(m.at(1).at(1).at(1),0)

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
      toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
      toPrivate(fun(x => add(x,`tile[1][1][1]L`))) o
      toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

    toGlobal(id) $ stencil

  }


  /** 1D **/
  @Test
  def test1DStencilRewrite(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 100
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    /*
    DotPrinter.withNumbering("/home/reese/scratch/","MSSrewrite",original1DStencil(3,1),true)
    println(NumberExpression.breadthFirst(original1DStencil(slidesize,slidestep)).mkString("\n\n"))
    */

    val rewriteStencil1D : Lambda1 = Rewrite.applyRuleAtId(original1DStencil(slidesize,slidestep),0,Rules.mapSeqSlide)
//    println(rewriteStencil1D)

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](MapSeqSlideHelpers.stencil1D(slidesize, slidestep), values)
    val (rewrite_output: Array[Float], _) = Execute(2, 2)[Array[Float]](rewriteStencil1D, values)
    assertArrayEquals(gold, output, 0.1f)
    assertArrayEquals(rewrite_output, output, 0.1f)

  }

  /** 2D **/

  @Test
  def test2DStencilRewriteSeq(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def original2DStencil(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) => {
        MapSeq(
          MapSeq(fun(m => {
            jacobi2D(m)
          }))) o Slide2D(size,step) $ input
      })

    def jacobi2DHighLevel(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      (mat) => {
        MapSeq(MapSeq(fun(m => jacobi2D(m)))) o
          // Slide2D
          Map(Transpose()) o Slide(a, b) o Map(Slide(a, b)) $ mat
      })

    def jacobi2DMapSeqSlideHighLevel(size: Int, step :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapSeq(fun(x => {
          toGlobal(MapSeqSlide( (fun(m => jacobi2D(m))), size, step)) o  Transpose()  $ x
        })) o Slide(size,step)   $ input
    )

    /*
    //    DotPrinter.withNumbering("/home/reese/scratch/","MSS2rewrite",jacobi2DHighLevel(slidesize,slidestep),true)
    //    DotPrinter.withNumbering("/home/reese/scratch/","2DMapSeqSlide",jacobi2DMapSeqSlideHighLevel(slidesize,slidestep),true)
        println(NumberExpression.breadthFirst(original2DStencil(slidesize,slidestep)).mkString("\n\n"))
     */

    val rewriteStencil2D = Rewrite.applyRuleAtId(original2DStencil(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide2DSeq)

    /*
    println(original2DStencil(slidesize,slidestep))
    println(rewriteStencil2D)
    println(jacobi2DMapSeqSlideHighLevel(slidesize,slidestep))
    */

    val (output: Array[Float], _) = Execute(2,2)[Array[Float]](original2DStencil(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)[Array[Float]](jacobi2DMapSeqSlideHighLevel(slidesize,slidestep), values)
    val (rewrite_output: Array[Float], _) = Execute(2,2)[Array[Float]](rewriteStencil2D,values)

    assertArrayEquals(output, gold, 0.1f)
    assertArrayEquals(output, rewrite_output, 0.1f)

  }

  @Test
  def test2DStencilRewrite(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def original2DStencil(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) => {
        Map(
          MapSeq(fun(m => {
            jacobi2D(m)
          }))) o Slide2D(size,step) $ input
      })

    def jacobi2DHighLevel(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      (mat) => {
        MapSeq(MapSeq(fun(m => jacobi2D(m)))) o
          // Slide2D
          Map(Transpose()) o Slide(a, b) o Map(Slide(a, b)) $ mat
      })

    def jacobi2DMapSeqSlideHighLevel(size: Int, step :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapSeq(fun(x => {
          toGlobal(MapSeqSlide( (fun(m => jacobi2D(m))), size, step)) o  Transpose()  $ x
        })) o Slide(size,step)   $ input
    )


    //    DotPrinter.withNumbering("/home/reese/scratch/","MSS2rewrite",jacobi2DHighLevel(slidesize,slidestep),true)
    //    DotPrinter.withNumbering("/home/reese/scratch/","2DMapSeqSlide",jacobi2DMapSeqSlideHighLevel(slidesize,slidestep),true)
    println(NumberExpression.breadthFirst(original2DStencil(slidesize,slidestep)).mkString("\n\n"))
    val rewriteStencil2D = Rewrite.applyRuleAtId(original2DStencil/*jacobi2DHighLevel*/(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide2D)
    println(rewriteStencil2D)

    //    Compile(rewriteStencil2D)

    /*
    */

    val (original: Array[Float], _) = Execute(2,2)[Array[Float]](original2DStencil(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)[Array[Float]](jacobi2DHighLevel(slidesize,slidestep), values)
    val (output: Array[Float], _) = Execute(2,2)[Array[Float]](jacobi2DMapSeqSlideHighLevel(slidesize,slidestep), values)

    assertArrayEquals(original, gold, 0.1f)
    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def testStencil3DSeq(): Unit = {

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


    def jacobi3Dlambda(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2), n+2), m+2),
      (mat) => {
        MapSeq(MapSeq(MapSeq(fun(m => jacobi3D(m))))) o
          Slide3D(a,b) $ mat
      })

    def jacobi3DHighLevel(a: Int, b: Int) = fun(
      ArrayType(ArrayType(ArrayType(Float, o+2), n+2), m+2),
      (mat) => {
        MapSeq(MapSeq(MapSeq(fun(m => jacobi3D(m))))) o
          // Slide3D
          Map(Map(Transpose()) o Transpose()) o
          Slide(a, b) o Map(Map(Transpose()) o Slide(a, b) o Map(Slide(a, b))) $ mat
      })

    def jacobi3DmapseqslideHighLevel(a : Int, b : Int) = fun(
      ArrayType(ArrayType(ArrayType(Float, o+2),n+2),m+2),
      mat =>
        MapSeq(MapSeq( fun(x => {
          MapSeqSlide( fun(m => jacobi3D(m)), a,b)
        } o Transpose() o Map(Transpose()) $ x

        ))) o
          // Slide2D
          Map(Transpose()) o Slide(a, b) o Map(Slide(a, b)) $ mat)


    def jacobi3Dmapseqslide(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2),n+2),m+2),
      (mat) =>
        MapGlb(1)(MapGlb(0)( fun (x => {
          toGlobal(MapSeqSlide(fun(m => {
            jacobi3D(m)
          }),a,b))  } o Transpose() o Map(Transpose()) $ x

        ))) o Slide2D(a,b) $ mat)

    /*
    //DotPrinter.withNumbering("/home/reese/scratch/","MSS3rewrite",jacobi3Dlambda(slidesize,slidestep),true)
    //DotPrinter.withNumbering("/home/reese/scratch/","3DMapSeqSlide",jacobi3DMapSeqSlideHighLevel(slidesize,slidestep),true)
    println(NumberExpression.breadthFirst(jacobi3Dlambda(slidesize,slidestep)).mkString("\n\n"))
    */

    val rewriteStencil3D = Rewrite.applyRuleAtId(jacobi3Dlambda(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSlideNDSeq)
    //val rewriteStencil3D = Rewrite.applyRuleAtId(jacobi3DHighLevel(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSeq)
    println(rewriteStencil3D)
    println(jacobi3DmapseqslideHighLevel(slidesize,slidestep))

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3Dlambda(slidesize,slidestep), stencilarrpadded3D)
    val (output_rewrite: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](rewriteStencil3D, stencilarrpadded3D)

    /*
    val (output_HL: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3DHighLevel(slidesize,slidestep), stencilarrpadded3D)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3Dmapseqslide(slidesize,slidestep), stencilarrpadded3D)
    */

    assertArrayEquals(output_rewrite, output, StencilUtilities.stencilDelta)

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

    def jacobi3Dlambda(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, Nx),Ny),Nz),
      (mat) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {
            jacobi3D(m)
        })))) o Slide3D(a,b) o PadConstant3D(1,1,1,0.0f) $ mat
      })

    def jacobi3DMapSeqSlide(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, Nx),Ny),Nz),
      (mat) =>
        Map(TransposeW()) o TransposeW() o Map(TransposeW()) o
          MapGlb(0)(MapGlb(1)( fun (x => {
            toGlobal(MapSeqSlide(fun(m => {
              jacobi3D(m)
            }),a,b))  } o Transpose() o Map(Transpose()) $ x
          ))) o Slide2D(a,b) o Map(Transpose())  o Transpose() o Map(Transpose()) o PadConstant3D(1,1,1,0.0f) $ mat)

    val rewriteStencil3D = Rewrite.applyRuleAtId(jacobi3Dlambda(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSlideNDSeq)
    //val rewriteStencil3D = Rewrite.applyRuleAtId(jacobi3DHighLevel(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSeq)
    println(rewriteStencil3D)
    println(jacobi3DMapSeqSlide(slidesize,slidestep))


    val (output_org: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3Dlambda(slidesize,slidestep), data)
    val (output_rewrite: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](rewriteStencil3D, data)
    val (output_MSS: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3DMapSeqSlide(slidesize,slidestep), data)


    assertArrayEquals(output_MSS, output_org, StencilUtilities.stencilDelta)
    assertArrayEquals(output_rewrite, output_org, StencilUtilities.stencilDelta)

  }

  @Test
  def zippedInputs3DRewriteRule(): Unit = {

    val size = 12
    val slidesize = 3
    val slidestep = 1

    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }
    val values2 = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size*2 + k*.5 + 1).toFloat }

    val N = SizeVar("N") + 2
    val M = SizeVar("M") + 2
    val O = SizeVar("O") + 2

    def lambda3D(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),O),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),O),
      (mat1,mat2) =>
        MapSeq(MapSeq(MapSeq(
          fun( m => {
               userFun3D(m)
          })))) o Slide3D(size, step) $ Zip3D(mat1,mat2)
    )

    val lambda3DMSS = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      (mat1, mat2) =>
        MapSeq(MapSeq(fun(x => {
          toGlobal(MapSeqSlide(fun(m => {
              userFun3D(m)
          }),slidesize,slidestep)) o Transpose() o Map(Transpose()) } $ x )))
          o Slide2D(slidesize,slidestep)  $ Zip3D(mat1, mat2)
    )

    val rewriteStencil3D = Rewrite.applyRuleAtId(lambda3D(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSlideNDSeq)
    //val rewriteStencil3D = Rewrite.applyRuleAtId(jacobi3DHighLevel(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSeq)
    println(rewriteStencil3D)
    println(lambda3DMSS(slidesize,slidestep))

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](lambda3D(slidesize,slidestep),values,values2)
    val (output_mss: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](lambda3DMSS,values,values2)
    val (output_rewrite: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](rewriteStencil3D,values,values2)

    assertArrayEquals(output_mss, output, StencilUtilities.stencilDelta)
    assertArrayEquals(output_rewrite, output, StencilUtilities.stencilDelta)

  }

  @Test
  def roomCodeMapSeqSlideRewriteRule(): Unit = {

    val size = 12
    val slidesize = 3
    val slidestep = 1

    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }
    val values2 = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size*2 + k*.5 + 1).toFloat }

    val localDimX = 12
    val localDimY = 10
    val localDimZ = 8

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPaddingInOrder(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val N = SizeVar("N") + 2
    val M = SizeVar("M") + 2
    val O = SizeVar("O") + 2

    val arraySig0 = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O), N), M)
    val arraySig2 = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O+2), N+2), M+2)


    val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
      "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return (float)count; }", Seq(Int,Int,Int,Int,Int,Int), Float)

    val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int,Float,Float), Float)

    def original3DStencil(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2),N+2),M+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2),N+2),M+2),
      (mat1,mat2) =>
        MapGlb(2)(MapGlb(1)(MapGlb(0)(
          fun( m => {

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

            toGlobal(id) o toPrivate(fun( x => mult(x,cf))) o toPrivate(addTuple) $
              Tuple(toPrivate(multTuple) $ Tuple(toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,RoomConstants.l2))) $ valueMask, `tile[1][1][1]`),
                toPrivate(subtractTuple) $ Tuple(
                  toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
                  toPrivate(fun(x => mult(x,cf2))) $ valueMat1))

          })))) o Slide3D(size, step) $ Zip3D(mat1,mat2,Array3DFromUserFunGenerator(getNumNeighbours, arraySig2))
    )

    val lambda3D = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2), N+2), M+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2), N+2), M+2),
      (mat1, mat2) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide(fun(m => {

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

          }),slidesize,slidestep)) o Transpose() o Map(Transpose()) } $ x )))
          o PrintType() o Slide2D(slidesize,slidestep)  $ Zip3D(mat1, mat2,Array3DFromUserFunGenerator(getNumNeighbours, arraySig2))
    )

    /*
    //print n' compare
    println(Compile(original3DStencil(slidesize,slidestep)))
    println(Compile(lambda3D))
    */

    val (outputOrg: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original3DStencil(slidesize,slidestep),stencilarrpadded3D, stencilarrpadded3D)
    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](lambda3D,stencilarrpadded3D, stencilarrpadded3D)

    assertArrayEquals(output, outputOrg, StencilUtilities.stencilDelta)

  }

}

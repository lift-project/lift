package rewriting
import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.MapSeqSlideHelpers
import opencl.generator.stencil.acoustic._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import rewriting.macrorules.MapSeqSlideRewrite
import rewriting.rules.{OpenCLRules, Rules}

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

  val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int,Float,Float), Float)

  def acoustic(m: Param) = {
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

    toGlobal(id) o toPrivate(fun(x => mult(x, cf))) o toPrivate(addTuple) $
      Tuple(save,
        toPrivate(subtractTuple) $ Tuple(
          toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
          toPrivate(fun(x => mult(x, cf2))) $ valueMat1))
  }

  /** 1D **/

  @Test
  def test1DStencilRewriteSeq(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 100
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val rewriteStencil1D : Lambda1 = Rewrite.applyRuleAtId(original1DStencil(slidesize,slidestep),0,Rules.mapSeqSlideSeq)

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

    val rewriteStencil2D = Rewrite.applyRuleAtId(original2DStencil(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide2DSeq)

    val (output: Array[Float], _) = Execute(2,2)[Array[Float]](original2DStencil(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)[Array[Float]](jacobi2DMapSeqSlideHighLevel(slidesize,slidestep), values)
    val (rewrite_output: Array[Float], _) = Execute(2,2)[Array[Float]](rewriteStencil2D,values)

    assertArrayEquals(output, gold, 0.1f)
    assertArrayEquals(output, rewrite_output, 0.1f)

  }


  @Test
  def test2DStencilRewriteReplaceMaps(): Unit = {

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
          Map(fun(m => {
            jacobi2D(m)
          }))) o Slide2D(size,step) $ input
      })

    def rewrite2DStencilCompare(size: Int, step :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide( (fun(m => jacobi2D(m))), size, step)) o  Transpose()  $ x
        })) o Slide(size,step)   $ input
    )

    val firstMapRewriteStencil2D = Rewrite.applyRuleAtId(original2DStencil(slidesize,slidestep),9,OpenCLRules.mapSeq)

    val rewriteStencil2D = Rewrite.applyRuleAtId(firstMapRewriteStencil2D,0,MapSeqSlideRewrite.mapSeqSlide2D)

    val secondMapRewriteStencil2D = Rewrite.applyRuleAtId(rewriteStencil2D,0,OpenCLRules.mapGlb)

    val (gold: Array[Float], _) = Execute(2,2)[Array[Float]](rewrite2DStencilCompare(slidesize,slidestep), values)
    val (rewrite_output: Array[Float], _) = Execute(2,2)[Array[Float]](secondMapRewriteStencil2D,values)

    assertArrayEquals(rewrite_output, gold, 0.1f)

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

    def jacobi3Dmapseqslide(a : Int, b : Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, o+2),n+2),m+2),
      (mat) =>
        MapGlb(1)(MapGlb(0)( fun (x => {
          toGlobal(MapSeqSlide(fun(m => {
            jacobi3D(m)
          }),a,b))  } o Transpose() o Map(Transpose()) $ x

        ))) o Slide2D(a,b) $ mat)

    val rewriteStencil3D = Rewrite.applyRuleAtId(jacobi3Dlambda(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSlideNDSeq)

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](jacobi3Dlambda(slidesize,slidestep), stencilarrpadded3D)
    val (output_rewrite: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](rewriteStencil3D, stencilarrpadded3D)

    assertArrayEquals(output_rewrite, output, StencilUtilities.stencilDelta)

  }

  @Test
  def zippedInputs3DRewriteRuleSeq(): Unit = {

    val size = 12
    val slidesize = 3
    val slidestep = 1

    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }
    val values2 = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size*2 + k*.5 + 1).toFloat }

    val N = SizeVar("N") + 2
    val M = SizeVar("M") + 2
    val O = SizeVar("O") + 2

    def lambda3D(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M),N),O),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M),N),O),
      (mat1,mat2) =>
        MapSeq(MapSeq(MapSeq(
          fun( m => {
               userFun3D(m)
          })))) o Slide3D(size, step) $ Zip3D(mat1,mat2)
    )

    val lambda3DMSS = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), O),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), O),
      (mat1, mat2) =>
        TransposeW() o Map(TransposeW()) o TransposeW() o
        MapSeq(MapSeq(fun(x => {
          toGlobal(MapSeqSlide(fun(m => {
              userFun3D(m)
          }),slidesize,slidestep)) o Transpose() o Map(Transpose()) } $ x )))
          o Transpose() o Slide2D(slidesize,slidestep) o Map(Transpose()) o Transpose() $ Zip3D(mat1, mat2)
    )

    val rewriteStencil3D = Rewrite.applyRuleAtId(lambda3D(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSlideNDSeq)

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](lambda3D(slidesize,slidestep),values,values2)
    val (output_mss: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](lambda3DMSS,values,values2)
    val (output_rewrite: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](rewriteStencil3D,values,values2)

    assertArrayEquals(output_mss, output, StencilUtilities.stencilDelta)
    assertArrayEquals(output_rewrite, output_mss, StencilUtilities.stencilDelta)

  }

  @Test
  def roomCodeMapSeqSlideRewriteRuleSeq(): Unit = {

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

    val arraySig = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O+2), N+2), M+2)

    val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
      "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return (float)count; }", Seq(Int,Int,Int,Int,Int,Int), Float)


    def original3D(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2),N+2),M+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2),N+2),M+2),
      (mat1,mat2) =>
        MapSeq(MapSeq(MapSeq(
          fun( m => {
            acoustic(m)
          })))) o Slide3D(size, step) $ Zip3D(mat1,mat2,Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
    )

    val lambda3DMSS = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2), N+2), M+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2), N+2), M+2),
      (mat1, mat2) =>
        TransposeW() o Map(TransposeW()) o TransposeW() o
        MapSeq(MapSeq(fun(x => {
          toGlobal(MapSeqSlide(fun(m => {
            acoustic(m)
          }),slidesize,slidestep)) o Transpose() o Map(Transpose()) } $ x )))
          o Transpose() o Slide2D(slidesize,slidestep) o Map(Transpose()) o Transpose() $ Zip3D(mat1, mat2,Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
    )

    val rewriteStencil3D = Rewrite.applyRuleAtId(original3D(slidesize,slidestep),0,MapSeqSlideRewrite.mapSeqSlide3DSlideNDSeq)

    val (outputOrg: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original3D(slidesize,slidestep),stencilarrpadded3D, stencilarrpadded3D)
    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](rewriteStencil3D,stencilarrpadded3D, stencilarrpadded3D)

    assertArrayEquals(output, outputOrg, StencilUtilities.stencilDelta)

  }

  @Test
  def test3DAcousticStencilRewriteReplaceMaps(): Unit = {

    val size = 12
    val slidesize = 3
    val slidestep = 1

    val localDimX = 4
    val localDimY = 6
    val localDimZ = 4

    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPaddingInOrder(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val N = SizeVar("N") + 2
    val M = SizeVar("M") + 2
    val O = SizeVar("O") + 2

    val arraySig0 = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, O+2), N+2), M+2)

    val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
      "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return (float)count; }", Seq(Int,Int,Int,Int,Int,Int), Float)

    def original3DStencil(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2),N+2),M+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2),N+2),M+2),
      (mat1,mat2) =>
        Map(Map(Map(
          fun( m => {
            acoustic(m)
          })))) o Slide3D(size, step) $ Zip3D( mat1, mat2,Array3DFromUserFunGenerator(getNumNeighbours, arraySig0))
    )

    def rewrite3DStencilCompare(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2), N+2), M+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O+2), N+2), M+2),
      (mat1, mat2) =>
       Map(TransposeW()) o TransposeW() o Map(TransposeW()) o
        MapGlb(0)(MapGlb(1)(fun(x => {
          toGlobal(MapSeqSlide(fun(m => {
            acoustic(m)
          }),slidesize,slidestep)) o Transpose() o Map(Transpose()) } $ x )))
           o Transpose() o Slide2D(size,step) o Map(Transpose()) o Transpose() $ Zip3D( mat1, mat2,Array3DFromUserFunGenerator(getNumNeighbours, arraySig0))
    )


    val firstMapRewriteStencil3D = Rewrite.applyRuleAtId(original3DStencil(slidesize,slidestep),49,OpenCLRules.mapSeq)

    val rewriteStencil3D = Rewrite.applyRuleAtId(firstMapRewriteStencil3D,0,MapSeqSlideRewrite.mapSeqSlide3DSlideND)

    val secondMapRewriteStencil3D = Rewrite.applyRuleAtId(rewriteStencil3D,38,OpenCLRules.mapGlb(1))

    val thirdMapRewriteStencil3D = Rewrite.applyRuleAtId(secondMapRewriteStencil3D,3,OpenCLRules.mapGlb(0))

    val (gold: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))[Array[Float]](rewrite3DStencilCompare(slidesize,slidestep), stencilarrpadded3D,stencilarrOther3D)
    val (rewrite_output: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))[Array[Float]](thirdMapRewriteStencil3D,stencilarrpadded3D,stencilarrOther3D)

    assertArrayEquals(rewrite_output, gold, 0.1f)

  }

}

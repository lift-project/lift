package opencl.generator.stencil

import apart.arithmetic.{SizeVar, StartFromRange, Var}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestStencil2D {
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

class TestStencil2D extends TestStencil {

  /* **********************************************************
       SLIDE 2D o PAD 2D
   ***********************************************************/
  def create2DPadSlideLambda(boundary: BoundaryFun,
                             size: Int, step: Int,
                             left: Int, right: Int): Lambda1 = {
    fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqUnroll(MapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(size, step) o Pad2D(left, right, boundary) $ domain
      }
    )
  }

  def runCombinedPadGroupTest(size: Int, step: Int,
                              left: Int, right: Int,
                              boundary: BoundaryFun,
                              scalaBoundary: (Int, Int) => Int,
                              data: Array[Array[Float]] = data2D): Unit = {
    val gold = Utils.scalaGenerate2DNeighbours(data, size, step, size, step, left, right, left, right, scalaBoundary)
    val goldFlat = gold.flatten.flatten.flatten

    val lambda = create2DPadSlideLambda(boundary, size, step, left, right)
    val (output: Array[Float], runtime) = Execute(data.length, data.length)(lambda, data)

    assertArrayEquals(goldFlat, output, 0.1f)
  }

  @Test def groupClampPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = Utils.scalaClamp

    runCombinedPadGroupTest(3, 1, 1, 1, boundary, scalaBoundary)
  }

  @Test def groupBigClampPaddedData2D(): Unit = {
    val data2D = Array.tabulate(10, 10) { (i, j) => i * 10.0f + j }
    val boundary = Pad.Boundary.Clamp
    val scalaBoundary = Utils.scalaClamp

    runCombinedPadGroupTest(5, 1, 2, 2, boundary, scalaBoundary, data2D)
  }

  @Test def groupMirrorPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Mirror
    val scalaBoundary = Utils.scalaMirror

    runCombinedPadGroupTest(3, 1, 1, 1, boundary, scalaBoundary)
  }

  @Test def groupWrapPaddedData2D(): Unit = {
    val boundary = Pad.Boundary.Wrap
    val scalaBoundary = Utils.scalaWrap

    runCombinedPadGroupTest(3, 1, 1, 1, boundary, scalaBoundary)
  }

  /* **********************************************************
       2D STENCILS
   ***********************************************************/
  def createSimple2DStencil(size: Int, step: Int,
                            left: Int, right: Int,
                            weights: Array[Float],
                            boundary: BoundaryFun,
                            fromRange: Int): Lambda2 = {
    createSimple2DStencil(size, step, size, step, left, right, left, right, weights, boundary, fromRange)
  }

  def createSimple2DStencil(size1: Int, step1: Int,
                            size2: Int, step2: Int,
                            top: Int, bottom: Int,
                            left: Int, right: Int,
                            weights: Array[Float],
                            boundary: BoundaryFun,
                            fromRange: Int): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(fromRange))), Var("M", StartFromRange(fromRange))),
      ArrayType(Float, weights.length),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(size1, step1, size2, step2) o Pad2D(top, bottom, left, right, boundary) $ matrix
      })
  }

  def run2DStencil(stencil: Lambda2,
                   size1: Int, step1: Int,
                   size2: Int, step2: Int,
                   top: Int, bottom: Int,
                   left: Int, right: Int,
                   weights: Array[Float],
                   name: String,
                   scalaBoundary: (Int, Int) => Int): Unit = {
    try {
      //val (width, height, input) = readInputImage(lenaPGM)
      // be carefull when choosing small input size because of 'StartsFromRange(100)'
      val width = randomData2D(0).length
      val height = randomData2D.length

      // change input used here
      val input = randomData2D

      val (output: Array[Float], runtime) = Execute(1, 1, width, height, (false, false))(stencil, input, weights)
      println("Runtime: " + runtime)

      //savePGM(name, outputLocation, output.grouped(width).toArray)

      val gold = Utils.scalaCompute2DStencil(input, size1, step1, size2, step2, top, bottom, left, right, weights, scalaBoundary)
      assertArrayEquals(gold, output, 0.1f)

    } catch {
      case x: Exception => x.printStackTrace()
    }
  }

  def run2DStencil(stencil: Lambda2,
                   size: Int, step: Int,
                   left: Int, right: Int,
                   weights: Array[Float],
                   name: String,
                   scalaBoundary: (Int, Int) => Int): Unit = {
    run2DStencil(stencil, size, step, size, step, left, right, left, right, weights, name, scalaBoundary)
  }

  val gaussWeights = Array(
      0.08f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)

  @Test def gaussianBlur(): Unit = {
    val stencil = createSimple2DStencil(3, 1, 1, 1, gaussWeights, BOUNDARY, 2)
    run2DStencil(stencil, 3, 1, 1, 1, gaussWeights, "gauss.pgm", SCALABOUNDARY)
  }

  @Test def gaussianBlur25PointStencil(): Unit = {
    val weights = Array(
      1, 4, 7, 4, 1,
      4, 16, 26, 16, 4,
      7, 26, 41, 26, 7,
      4, 16, 26, 16, 4,
      1, 4, 7, 4, 1).map(_ * 0.004219409282700422f)
    val stencil = createSimple2DStencil(5, 1, 2, 2, weights, BOUNDARY, 3)
    run2DStencil(stencil, 5, 1, 2, 2, weights, "gauss25.pgm", SCALABOUNDARY)
  }

  @Test def blurX3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(1, 1, 3, 1, 0, 0, 1, 1, weights, Pad.Boundary.Wrap, 2)
    run2DStencil(stencil, 1, 1, 3, 1, 0, 0, 1, 1, weights, "notUsed", Utils.scalaWrap)
  }

  @Test def blurY3Point(): Unit = {
    val weights = Array.fill[Float](3)(1.0f)
    val stencil = createSimple2DStencil(3, 1, 1, 1, 1, 1, 0, 0, weights, Pad.Boundary.Wrap, 2)
    run2DStencil(stencil, 3, 1, 1, 1, 1, 1, 0, 0, weights, "notUsed", Utils.scalaWrap)
  }

  /* **********************************************************
     TILING 2D
 ***********************************************************/
  def createTiled2DStencil(size: Int, step: Int,
                           tileSize: Int, tileStep: Int,
                           left: Int, right: Int,
                           weights: Array[Float],
                           boundary: Pad.BoundaryFun): Lambda = {
    createTiled2DStencil(size, step, size, step,
      tileSize, tileStep, tileSize, tileStep,
      left, right, left, right,
      weights, boundary)
  }

  def createTiled2DStencil(size1: Int, step1: Int,
                           size2: Int, step2: Int,
                           tileSize1: Int, tileStep1: Int,
                           tileSize2: Int, tileStep2: Int,
                           top: Int, bottom: Int,
                           left: Int, right: Int,
                           weights: Array[Float],
                           boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, weights.length),
    (matrix, weights) => {
      Untile() o MapWrg(1)(MapWrg(0)(fun(tile =>

        MapLcl(1)(MapLcl(0)(
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })

        )) o Slide2D(size1, step1, size2, step2) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o Slide2D(tileSize1, tileStep1, tileSize2, tileStep2) o Pad2D(top, bottom, left, right, boundary) $ matrix
    }
  )

  def createCopyTilesLambda(size: Int, step: Int,
                            left: Int, right: Int,
                            boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
    ArrayType(Float, 9),
    (matrix, weights) => {
      MapWrg(1)(MapWrg(0)(fun(tile =>
        toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile

      ))) o Slide2D(size, step) o Pad2D(left, right, boundary) $ matrix
    }
  )

  @Test def copyTilesIdentity(): Unit = {
    val data2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j }
    val tiled: Lambda = createCopyTilesLambda(4, 2, 1, 1, BOUNDARY)

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaGenerate2DNeighbours(data2D, 4, 2, 4, 2, 1, 1, 1, 1, SCALABOUNDARY).flatten.flatten.flatten

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiling2DBiggerTiles(): Unit = {
    val data2D = Array.tabulate(1024, 1024) { (i, j) => i * 24.0f + j }
    val tiled: Lambda = createTiled2DStencil(3, 1, 10, 8, 1, 1, gaussWeights, BOUNDARY)
    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, (false, false))(tiled, data2D, gaussWeights)
    val gold = Utils.scalaCompute2DStencil(data2D, 3, 1, 3, 1, 1, 1, 1, 1, gaussWeights, SCALABOUNDARY)

    assertArrayEquals(gold, output, 0.1f)
  }

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Test def tiled2D9PointStencil(): Unit = {
    val tiled: Lambda = createTiled2DStencil(3, 1, 4, 2, 1, 1, gaussWeights, BOUNDARY)
    run2DStencil(tiled, 3, 1, 1, 1, gaussWeights, "notUsed", SCALABOUNDARY)
  }

  def createTiled2DStencilWithTiledLoading(size1: Int, step1: Int,
                                           size2: Int, step2: Int,
                                           tileSize1: Int, tileStep1: Int,
                                           tileSize2: Int, tileStep2: Int,
                                           top: Int, bottom: Int,
                                           left: Int, right: Int,
                                           weights: Array[Float],
                                           boundary: Pad.BoundaryFun): Lambda = fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, weights.length),
    (matrix, weights) => {
      Untile() o MapWrg(1)(MapWrg(0)(fun(tile =>

        MapLcl(1)(MapLcl(0)(
          //MapSeq(MapSeq((toGlobal(id))))
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })

          // create stencil neighbourhoods
        )) o Slide2D(size1, step1, size2, step2) o Map(Join()) o

          // load chunks to local memory
          toLocal(MapLcl(1)(MapSeqUnroll(MapLcl(0)(id)))) $ tile

        // spliting tile into chunks
      ))) o Map(Map(Map(Split(8)))) o
        // creating tiles
        Slide2D(tileSize1, tileStep1, tileSize2, tileStep2) o
        Pad2D(top, bottom, left, right, boundary) $ matrix
    }
  )

  // be carefull when choosing small input size because of 'StartsFromRange(100)'
  @Ignore // falsely classified as not valid because of barriers
  @Test def tiledBlurXTiledLoading(): Unit = {
    val weights = Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1).map(_.toFloat)
    val tiled: Lambda = createTiled2DStencilWithTiledLoading(1, 1, 17, 1, 1, 1, 24, 8, 0, 0, 8, 8, weights, Pad.Boundary.Clamp)
    run2DStencil(tiled, 1, 1, 17, 1, 0, 0, 8, 8, weights, "notUsed", Utils.scalaClamp)
  }

  @Ignore // todo compN(p, i) = p^i, but map^i = map o ... o map does not work yet
  @Test def alternative2DStencilExpression(): Unit = {
    val clamp = Pad.Boundary.Clamp

    // apply primitive in X and Y Dimension
    val dim2: (FunDecl) => Lambda = (primitive: FunDecl) =>
      primitive o Map(primitive)

    // compose primitive n-times
    def compN: (FunDecl, Int) => FunDecl = (primitive: FunDecl, i: Int) =>
      if (i > 1)
        primitive o compN(primitive, i-1)
      else
        primitive

    def dimN: (FunDecl, Int) => FunDecl = (primitive: FunDecl, dim: Int) =>
      if (dim > 1)
        // should be `Map` inside the compN
        dimN(primitive, dim-1) o compN(Map.asInstanceOf[FunDecl], dim-1) o primitive
      else
        primitive

    // apply 2D boundary handling
    //val boundary = (size: Int, b: Pad.BoundaryFun) => dim2(Pad(size,size,b))
    val boundary = (size: Int, b: Pad.BoundaryFun) => dimN(Pad(size,size,b), 2)

    // create 2D neighborhoods
    //val nbh = (size: Int) => Map(Transpose()) o dim2(Slide(size, size-2))
    val nbh = (size: Int) => Map(Transpose()) o dimN(Slide(size, size-2), 2)

    // 2D stencil function
    val f = toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()


    val lambda = fun(
      ArrayType(ArrayType(Float, SizeVar("N")), SizeVar("M")),
      (input) => {
        MapGlb(1)(MapGlb(0)(f)) o nbh(3) o boundary(1, clamp) $ input
      })

    val lambda2 = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        MapGlb(1)(id) o compN(Pad(1,1,clamp), 3) $ input
      })

    /*
    val input = Array.tabulate(512, 512) { (i,j) => scala.util.Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(16, 16, 512, 512, (false, false))(lambda, input)
    val weights = Array.tabulate(9) { i => 1.0f}
    val gold = Utils.scalaCompute2DStencil(input, 3,1,3,1, 1,1,1,1, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.1f)
    */
  }

  @Test
  def stencil2DTilingRewriteIdentities(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val n = 3
    val s = 1
    val u = 6
    val v = 4

    // use these values to take powers of two as valid inputs for slide (pad is not used here)
    val input = Array.tabulate(34, 34) { (i, j) => i * 34.0f + j }

    // use abbr. notation because of long expressions
    // *=map (as in BMF), J=join, T=transpose, S_ab = slide a b
    // (0): *T o S_ns o *S_ns
    val gold = Map(Transpose()) o Slide(n,s) o Map(Slide(n,s))

    // 2D tiling
    // J o J o *T o **Slide2d_ns o Slide2d_uv
    // val desired = Map(Join()) o Join() o Map(Transpose()) o Map(Map(Slide2D(n,s))) o Slide2D(u,v)
    //
    // = *J o J o *T o ***T o **S_ns o ***S_ns o *T o S_uv o *S_uv
    val desired = Map(Join()) o Join() o Map(Transpose()) o Map(Map(Map(Transpose()))) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o
      Map(Transpose()) o Slide(n,s) o Map(Slide(n,s))

    // (1): *T o J o *S_ns o S_uv o *S_ns
    val f1 = Map(Transpose()) o Join() o Map(Slide(n,s)) o Slide(u,v) o Map(Slide(n,s))

    // (2): J o **T o *S_ns o S_uv o *S_ns
    val f2 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Slide(u,v) o Map(Slide(n,s))

    // (3): tile first S_ns -> does not lead to anything yet. see (6) for other try
    // J o **T o *(J o *S_ns o S_uv) o S_uv o *S_ns
    val f3 = Join() o Map(Map(Transpose())) o Map(Join() o Map(Slide(n,s)) o
      Slide(u,v)) o Slide(u,v) o Map(Slide(n,s))

    // (4): J o **T o *J o **S_ns o *S_uv o S_uv o *S_ns
    val f4 = Join() o Map(Map(Transpose())) o Map(Join()) o Map(Map(Slide(n,s))) o
      Map(Slide(u,v)) o Slide(u,v) o Map(Slide(n,s))

    // (5): J o *J o ***T o **S_ns o *S_uv o S_uv o *S_ns
    val f5 = Join() o Map(Join()) o Map(Map(Map(Transpose()))) o Map(Map(Slide(n,s))) o
      Map(Slide(u,v)) o Slide(u,v) o Map(Slide(n,s))


    // (6): Try tiling other S_ns from (2)
    // J o **T o *S_ns o S_uv o *(J o *S_ns o S_uv)
    val f6 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Slide(u,v) o Map(Join() o Map(Slide(n,s)) o Slide(u,v))

    // (7): J o **T o *S_ns o S_uv o *J o **S_ns o *S_uv
    val f7 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Slide(u,v) o Map(Join()) o
      Map(Map(Slide(n,s))) o Map(Slide(u,v))

    // (8): J o **T o *S_ns o **J o S_uv o **S_ns o *S_uv
    val f8 = Join() o Map(Map(Transpose())) o Map(Slide(n,s)) o Map(Map(Join())) o
      Slide(u,v) o Map(Map(Slide(n,s))) o Map(Slide(u,v))

    // (9): J o **T o ***J o *S_ns o S_uv o **S_ns o *S_uv
    val f9 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o Slide(u,v) o
      Map(Map(Slide(n,s))) o Map(Slide(u,v))

    // (10): J o **T o ***J o *S_ns o ***S_ns o S_uv o *S_uv
    val f10 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Map(Map(Slide(n,s)))) o Slide(u,v) o Map(Slide(u,v))

    // (11): J o **T o ***J o *S_ns o ***S_ns o *(T o T) o S_uv o *S_uv
    val f11 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Map(Map(Slide(n,s)))) o Map(Transpose() o Transpose()) o Slide(u,v) o Map(Slide(u,v))

    // (12): J o **T o ***J o *S_ns o ***S_ns o *T o *T o S_uv o *S_uv
    val f12 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o Map(Transpose()) o Slide(u,v) o Map(Slide(u,v))

    // (13): J o **T o ***J o *S_ns o *T o ***S_ns o *T o S_uv o *S_uv
    val f13 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Slide(n,s)) o
      Map(Transpose()) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o Slide(u,v) o Map(Slide(u,v))

    // (14): J o **T o ***J o **T o *T o **S_ns_ o ***S_ns o *T o S_uv o *S_uv
    val f14 = Join() o Map(Map(Transpose())) o Map(Map(Map(Join()))) o Map(Map(Transpose())) o
      Map(Transpose()) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o
      Slide(u,v) o Map(Slide(u,v))

    // (15): J o **J o ***T o **T o **T o *T o **S_ns o ***S_ns o *T o S_uv o *S_uv
    val f15 = Join() o Map(Map(Join())) o Map(Map(Map(Transpose()))) o
      Map(Map(Transpose())) o Map(Map(Transpose())) o // they cancel out
      Map(Transpose()) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o
      Slide(u,v) o Map(Slide(u,v))

    // (16): J o **J o ***T o *T o **S_ns o ***S_ns o *T o S_uv o *S_uv
    val f16 = Join() o Map(Map(Join())) o Map(Map(Map(Transpose()))) o
      Map(Transpose()) o Map(Map(Slide(n,s))) o Map(Map(Map(Slide(n,s)))) o Map(Transpose()) o
      Slide(u,v) o Map(Slide(u,v))

    def lambda(f: Lambda): Lambda1 = fun(
      ArrayType(ArrayType(Float, M), N),
      input =>
        MapGlb(1)(MapGlb(0)(MapSeq(MapSeq(id)))) o f $ input
    )

    val (outGold: Array[Float], runtime) = Execute(1,1,32,32,(false,false))(lambda(gold), input)
    val (outDesired: Array[Float], runtime0) = Execute(1,1,32,32,(false,false))(lambda(desired), input)
    val (outF1: Array[Float], runtime1) = Execute(1,1,32,32,(false,false))(lambda(f1), input)
    val (outF2: Array[Float], runtime2) = Execute(1,1,32,32,(false,false))(lambda(f2), input)
    val (outF3: Array[Float], runtime3) = Execute(1,1,32,32,(false,false))(lambda(f3), input)
    val (outF4: Array[Float], runtime4) = Execute(1,1,32,32,(false,false))(lambda(f4), input)
    val (outF5: Array[Float], runtime5) = Execute(1,1,32,32,(false,false))(lambda(f5), input)
    val (outF6: Array[Float], runtime6) = Execute(1,1,32,32,(false,false))(lambda(f6), input)
    val (outF7: Array[Float], runtime7) = Execute(1,1,32,32,(false,false))(lambda(f7), input)
    val (outF8: Array[Float], runtime8) = Execute(1,1,32,32,(false,false))(lambda(f8), input)
    val (outF9: Array[Float], runtime9) = Execute(1,1,32,32,(false,false))(lambda(f9), input)
    val (outF10: Array[Float], runtime10) = Execute(1,1,32,32,(false,false))(lambda(f10), input)
    val (outF11: Array[Float], runtime11) = Execute(1,1,32,32,(false,false))(lambda(f11), input)
    val (outF12: Array[Float], runtime12) = Execute(1,1,32,32,(false,false))(lambda(f12), input)
    val (outF13: Array[Float], runtime13) = Execute(1,1,32,32,(false,false))(lambda(f13), input)
    val (outF14: Array[Float], runtime14) = Execute(1,1,32,32,(false,false))(lambda(f14), input)
    val (outF15: Array[Float], runtime15) = Execute(1,1,32,32,(false,false))(lambda(f15), input)
    val (outF16: Array[Float], runtime16) = Execute(1,1,32,32,(false,false))(lambda(f16), input)

    assertArrayEquals(outGold, outDesired, 0.1f)
    assertArrayEquals(outGold, outF1, 0.1f)
    assertArrayEquals(outGold, outF2, 0.1f)
    assertArrayEquals(outGold, outF3, 0.1f)
    assertArrayEquals(outGold, outF4, 0.1f)
    assertArrayEquals(outGold, outF5, 0.1f)
    assertArrayEquals(outGold, outF6, 0.1f)
    assertArrayEquals(outGold, outF7, 0.1f)
    assertArrayEquals(outGold, outF8, 0.1f)
    assertArrayEquals(outGold, outF9, 0.1f)
    assertArrayEquals(outGold, outF10, 0.1f)
    assertArrayEquals(outGold, outF11, 0.1f)
    assertArrayEquals(outGold, outF12, 0.1f)
    assertArrayEquals(outGold, outF13, 0.1f)
    assertArrayEquals(outGold, outF14, 0.1f)
    assertArrayEquals(outGold, outF15, 0.1f)
    assertArrayEquals(outGold, outF16, 0.1f)
  }

  @Test
  def stencil2DTilingLocalMemIdentities(): Unit = {
    // stencil shape
    val n = 3
    val s = 1
    // tile size
    val u = 6
    val v = 4
    // pad parameters
    val l = 1
    val r = 1
    val b = Pad.Boundary.Clamp

    // use these values to take powers of two as valid inputs for slide (pad is not used here)
    val input = Array.tabulate(32, 32) { (i, j) => i * 32.0f + j }
    // stencil function: nbh:[3][3] -> [1]
    val f = toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()

    def lambda(f: Lambda) = {
      fun(
        ArrayType(ArrayType(Float,SizeVar("M")), SizeVar("N")),
        input => f $ input
      )
    }

    // use shorthand notation
    val P = Pad(l,r,b)
    val T = Transpose()
    val T_w = TransposeW()
    val J = Join()
    val S_ns = Slide(n,s)
    val S_uv = Slide(u,v)
    def *(f: Lambda) = Map(f)
    def **(f: Lambda) = Map(Map(f))
    def ***(f: Lambda) = Map(Map(Map((f))))

    val gold = MapGlb(1)(MapGlb(0)(f)) o                  // (C) apply stencil function
      Map(Transpose()) o Slide(n,s) o Map(Slide(n,s)) o   // (B) 2d neighborhood creation
        Pad(l,r,b) o Map(Pad(l,r,b))                      // (A) 2d padding

    // same as gold but short
    val goldShort = MapGlb(1)(MapGlb(0)(f)) o             // (C)
      *(T) o S_ns o *(S_ns) o                             // (B)
        P o *(P)                                          // (A)

    // introduce 2d tiles
    val f1 = MapGlb(1)(MapGlb(0)(f)) o                                            // (C)
      *(J) o J o *(T) o ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o   // (B) (tiling inclusive)
        P o *(P)                                                                  // (A)

    // todo proof rewrite from f1 to f2/f3

    // move maps forward to exploit more levels of parallelism
    // what is untile doing??
    val f2 = /* *(J) o J o *(T) o */ Untile() o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)))) o // (C) using workgroups
      ***(T) o **(S_ns) o ***(S_ns) o *(T) o S_uv o *(S_uv) o                                   // (B)
        P o *(P)                                                                                // (A)

    // using TransposeW instead of Transpose after stencil computation does the job
    val f3 = *(J) o J o *(T_w) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)))) o  // (C)
      ***(T) o **(S_ns) o ***(S_ns) o         // (%)                              // (B) Create neighborhoods in tiles
        *(T) o S_uv o *(S_uv) o                                                   // (B) Create tiles
          P o *(P)                                                                // (A)

    // fuse the expressions (%) from above with the MapWrg's
    val f4 = *(J) o J o *(T_w) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)) o  // (C)
      *(T) o S_ns o *(S_ns))) o                                                 // (B) Create neighborhoods in tiles
        *(T) o S_uv o *(S_uv) o                                                 // (B) Create tiles
          P o *(P)                                                              // (A)

    // load tiles to local memory
    val f5 = *(J) o J o *(T_w) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)) o // (C)
      *(T) o S_ns o *(S_ns) o                 // Slide2D n s                   // (B.3) Create neighborhoods in tiles
        toLocal(MapLcl(1)(MapLcl(0)(id))))) o // whole line = id               // (B.2) Load tiles to local memory
        *(T) o S_uv o *(S_uv) o               // Slide2D u v                   // (B.1) Create tiles
          P o *(P)                                                             // (A)

    // try to replace T_w with T by moving it in front of computation
    // also choose J o **J instead of *J o J
    val f6 = J o **(J) o MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)(f)) o *(T) o  // (C)
      *(T) o S_ns o *(S_ns) o                 // Slide2D n s                   // (B.3)
        toLocal(MapLcl(1)(MapLcl(0)(id))))) o // whole line = id               // (B.2)
        *(T) o S_uv o *(S_uv) o               // Slide2D u v                   // (B.1)
          P o *(P)                                                             // (A)

    val (outGold: Array[Float], runtime) = Execute(1,1,32,32,(false,false))(lambda(gold), input)
    val (outGoldShort: Array[Float], runtimeShort) = Execute(1,1,32,32,(false,false))(lambda(goldShort), input)
    val (outF1: Array[Float], runtime1) = Execute(1,1,32,32,(false,false))(lambda(f1), input)
    val (outF2: Array[Float], runtime2) = Execute(1,1,32,32,(false,false))(lambda(f2), input)
    val (outF3: Array[Float], runtime3) = Execute(1,1,32,32,(false,false))(lambda(f3), input)
    val (outF4: Array[Float], runtime4) = Execute(1,1,32,32,(false,false))(lambda(f4), input)
    val (outF5: Array[Float], runtime5) = Execute(1,1,32,32,(false,false))(lambda(f5), input)
    val (outF6: Array[Float], runtime6) = Execute(1,1,32,32,(false,false))(lambda(f6), input)

    assertArrayEquals(outGold, outGoldShort, 0.1f)
    assertArrayEquals(outGold, outF1, 0.1f)
    assertArrayEquals(outGold, outF2, 0.1f)
    assertArrayEquals(outGold, outF4, 0.1f)
    assertArrayEquals(outGold, outF5, 0.1f)
    //assertArrayEquals(outGold, outF6, 0.1f)
  }
}

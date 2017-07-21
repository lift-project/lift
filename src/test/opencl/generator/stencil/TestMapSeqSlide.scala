package opencl.generator.stencil

import ir.{ArrayTypeWSWC}
import ir.ast.{Get, Slide, Zip, fun}
import lift.arithmetic.{SizeVar}
import opencl.executor._
import org.junit.{AfterClass, BeforeClass}
import org.junit.Assert._
import org.junit._
import opencl.ir.pattern._
import ir.ast._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._

object TestMapSeqSlide
{
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

object MapSeqSlideHelpers
{

  val O = 2 + SizeVar("O")
  val N = 2 + SizeVar("N")
  val M = 2 + SizeVar("M")

  def original1DStencil(size: Int, step: Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      MapGlb(0)(
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
      MapGlb(1)(
        MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeqUnroll(id)) o
            ReduceSeq(add, 0.0f) o Join() $ neighbours
        }))
      ) o Slide2D(size,step) $ input
    })


  def stencil2D(size: Int, step :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (input) =>
      MapGlb(0)(fun(x => {
        toGlobal(MapSeqSlide(MapSeq(id) o ReduceSeq(add, 0.0f) o Join(), size, step)) o  Transpose() $ x
      })) o Slide(size,step)  $ input
  )

  def getMiddle2D(size: Int, step :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (input) =>
      MapGlb(0)(fun(x => {
        toGlobal(MapSeqSlide( (fun (x => {  toGlobal(id) $ x.at(1).at(1) })), size, step)) o  Transpose() $ x
      })) o Slide(size,step)   $ input
  )

  def original3DStencil(a: Int, b: Int) = fun(
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
        //toGlobal(id) $ `tile[0][0][0]`

      }))))
        o Slide3D(a,b) $ mat)
    })

  def stencil3D(a: Int ,b :Int) = fun(
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

          }), a,b)) o Transpose() o Map(Transpose()) $ x }))) o Slide2D(a,b)  $ input
  )
}

class TestMapSeqSlide
{

  /** 1D **/
  @Test
  def reduceSlide1DTestSize3Step1(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 100
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output : Array[Float], _) = Execute(2, 2)(MapSeqSlideHelpers.stencil1D(slidesize, slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize5Step3(): Unit = {

    val slidesize = 5
    val slidestep = 3
    val size = 20
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil1D(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize5Step5(): Unit = {

    val slidesize = 5
    val slidestep = 5
    val size = 20
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil1D(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize3Step2Length10(): Unit = {

    val slidesize = 3
    val slidestep = 2
    val size = 10
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    // drop right one on the comparison array because scala sliding does not work exactly the same as Lift sliding ...
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).dropRight(1)

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil1D(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestWithWeights(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val weights = Array( 0.5f, 1.0f, 0.5f )
    val gold = Array( 4.0f,6.0f,8.0f,10.0f,12.0f,14.0f )

    val orgStencil = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      ArrayTypeWSWC(Float, 3),
      (input,wgts) => {
        MapSeqSlide(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(add, 0.0f) o
            MapSeqUnroll(mult) $
            Zip(wgts, neighbourhood)
          }), slidesize, slidestep)
        }  $ input
    )

    val (output: Array[Float], _) = Execute(2,2)( orgStencil, values, weights )

    assertArrayEquals(gold, output, 0.1f)
  }

  /** 2D **/

  @Test
  def at2DOneLine3AcrossStencil(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val N = 2 + SizeVar("N")

    val compare2Dstencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (mat) => {
        MapGlb(1)(MapGlb(0)(fun(neighbours => {

          val `tile[1][1]` = neighbours.at(1).at(1)
          val `tile[1][0]` = neighbours.at(1).at(0)
          val `tile[1][2]` = neighbours.at(1).at(2)

          val stencil =  fun(x => add(x,`tile[1][2]`)) o
            fun(x => add(x,`tile[1][0]`)) $ `tile[1][1]`

          toGlobal(id) $ stencil

        }))) o Slide2D(slidesize, slidestep) $ mat
      })

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(toGlobal(MapSeqSlide(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) , a,b)  o Join())) o Slide(1,b) $ input
    )

    val (compare: Array[Float], _) = Execute(2,2)(compare2Dstencil, values)
    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)

    assertArrayEquals(compare, output.dropRight(size-2).drop(size-2), 0.1f)

  }

  @Test
  def at2DThreeLinesThreeAcrossStencil(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val N = 2 + SizeVar("N")

    val compare2Dstencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (mat) => {
        MapGlb(1)(MapGlb(0)(fun(neighbours => {

          val `tile[1][1]` = neighbours.at(1).at(1)
          val `tile[1][0]` = neighbours.at(1).at(0)
          val `tile[1][2]` = neighbours.at(1).at(2)

          val stencil =  fun(x => add(x,`tile[1][2]`)) o
            fun(x => add(x,`tile[1][0]`)) $ `tile[1][1]`

          toGlobal(id) $ stencil

        }))
        ) o Slide2D(slidesize, slidestep) $ mat
      })

    def stencil2D(a: Int, b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(toGlobal(MapSeqSlide(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) , a,b))) o Transpose() /* o Slide2D(a,b)*/ $ input
    )

    val (compare: Array[Float], _) = Execute(2,2)(compare2Dstencil, values)
    val (output: Array[Float], _) = Execute(2,2)(stencil2D(slidesize,slidestep), values)

    assertArrayEquals(compare, output.dropRight(size-2).drop(size-2), 0.1f)
  }

  @Test
  def reduceSlide2DTestVersionOne(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(slidesize,slidestep).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")


    def stencil2D(a: Int, b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(fun(x => {

          val side1 =  MapSeq( fun( t => { toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ t })) o Slide(a,b) $ x.at(0)
          val side2 = MapSeq( fun( t => { toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ t })) o Slide(a,b) $ x.at(2)
          val sum = toGlobal(MapSeq(id) o MapSeq(addTuple)) $ Zip(Join() $ side1,Join() $ side2)
          val tmpSum = 0.0f
          val ssp =  toGlobal(MapSeqSlide(MapSeq(id) o ReduceSeq(absAndSumUp,tmpSum) , a,b))  $ x.at(1)
          val actSum = toGlobal(MapSeq(addTuple)) $ Zip( Join() $ ssp,sum)
          actSum
        })) o Slide(a,b) o Transpose() $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil2D(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTestVersionTwo(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(slidesize,slidestep).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil2D(a: Int, b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(fun(x => {

          val sumSide33 =  MapSeq( fun( t => {

            val tP = Join() $ t

            val p0 = tP.at(0).at(0)
            val p1 = tP.at(0).at(1)
            val p2 = tP.at(0).at(2)
            val p6 = tP.at(2).at(0)
            val p7 = tP.at(2).at(1)
            val p8 = tP.at(2).at(2)

            val stencil =  fun(x => add(x, p0)) o
              fun(x => add(x,p1)) o
              fun(x => add(x,p2)) o
              fun(x => add(x,p6)) o
              fun(x => add(x,p7)) $ p8

              toGlobal(id) $ stencil

               })) o Slide2D(a,b) o Transpose() $ x
          val tmpSum = 0.0f
          val ssp =  toGlobal(MapSeqSlide(MapSeq(id) o ReduceSeq(absAndSumUp, tmpSum) , a,b))  $ x.at(1)
          val actSum = toGlobal(MapSeq(addTuple)) $ Zip( Join() $ ssp,sumSide33)
          actSum
        })) o Slide(a,b) o Transpose() $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil2D(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }


  @Test
  def reduceSlide2DTest9PointStencil(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil2D(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.original2DStencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest9WithAt(): Unit = {

    val size = 14

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def original2DAt(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (mat) => {
        (MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[1][1]` = m.at(1).at(1)
          val `tile[0][1]` = m.at(0).at(1)
          val `tile[1][0]` = m.at(1).at(0)
          val `tile[1][2]` = m.at(1).at(1)
          val `tile[2][1]` = m.at(2).at(1)

          val stencil =  fun(x => add(x,`tile[0][1]`)) o
            fun(x => add(x,`tile[1][0]`)) o
            fun(x => add(x,`tile[1][1]`)) o
            fun(x => add(x,`tile[1][1]`)) o
            fun(x => add(x,`tile[1][2]`)) $ `tile[2][1]`

          toGlobal(id) $ stencil

        })))
          o Slide2D(a,b) $ mat)
      })

    def stencil2Dat(a: Int, b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapGlb(0)(fun(x => {

          toGlobal(MapSeqSlide(fun(m => {

            val `tile[1][1]` = m.at(1).at(1)
            val `tile[0][1]` = m.at(0).at(1)
            val `tile[1][0]` = m.at(1).at(0)
            val `tile[1][2]` = m.at(1).at(1)
            val `tile[2][1]` = m.at(2).at(1)

            val stencil =  fun(x => add(x,`tile[0][1]`)) o
              fun(x => add(x,`tile[1][0]`)) o
              fun(x => add(x,`tile[1][1]`)) o
              fun(x => add(x,`tile[1][1]`)) o
              fun(x => add(x,`tile[1][2]`)) $ `tile[2][1]`

            toGlobal(id) $ stencil

          }), a,b)) o  Transpose() $ x
        })) o Slide(a,b)  $ input
    )

    val (output: Array[Float], runtimeNew: Double) = Execute(2,2)(stencil2Dat(slidesize,slidestep), values)
    val (gold: Array[Float], runtimeOrg: Double) = Execute(2,2)(original2DAt(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest9PointStencilAsymmetric(): Unit = {

    val sizeX = 16
    val sizeY = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(sizeY,sizeX) { (i,j) => (i*sizeX + j + 1).toFloat }

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil2D(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.original2DStencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest9PointSize5Step3(): Unit = {

    val size = 15
    val slidesize = 7
    val slidestep = 4
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil2D(slidesize,slidestep), values)
    val (goldExec: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.original2DStencil(slidesize,slidestep), values)

    assertArrayEquals(goldExec, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest9PointWithWeights(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def original2DWeightStencil(slidesize: Int, slidestep: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(id)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(slidesize, slidestep) $ mat
      })


    def stencil2D(a: Int, b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (input, weights) =>
        MapGlb(0)(fun(x => {
          val tmpSum = 0.0f
          toGlobal(MapSeqSlide(fun(neighbours => {
            toGlobal(MapSeq(id)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }), a,b)) o  Transpose() $ x
        })) o Slide(a,b)  $ input
    )

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))(stencil2D(slidesize,slidestep), values, StencilUtilities.weightsArr)
    val (gold: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))(original2DWeightStencil(slidesize,slidestep), values, StencilUtilities.weightsArr)

    assertArrayEquals(gold, output, 0.1f)

  }

  /** 3D **/

  @Test
  def reduceSlide3DTest7PointWithAt(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }

    val O = 2 + SizeVar("O")
    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")


    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil3D(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.original3DStencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide3DTest7PointWithAtSize5Step3(): Unit = {

    val size = 10

    val slidesize = 5
    val slidestep = 3
    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil3D(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.original3DStencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide3DTest7PointWithAtSize5Step5(): Unit = {

    val size = 15

    val slidesize = 5
    val slidestep = 5
    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }

    val (output: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.stencil3D(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2)(MapSeqSlideHelpers.original3DStencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide3DTest7PointWithAtAsymmetric(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size+3,size+6) { (i,j,k) => (i*(size+6)*(size+3) + j*(size+6) + k + 1).toFloat }

    val O = 2 + SizeVar("O")
    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))(MapSeqSlideHelpers.stencil3D(slidesize,slidestep), values)
    val (gold: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))(MapSeqSlideHelpers.original3DStencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }
  @Test
  def reduceSlide3DTest27PointWithWeights(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }

    val O = 2 + SizeVar("O")
    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil3DCompareWeights(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      ArrayTypeWSWC(Float, slidesize*slidesize*slidesize),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, weights)
        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    def stencil3DWeights(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      ArrayTypeWSWC(Float, slidesize*slidesize*slidesize),
      (input,weights) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(MapSeqSlide(
            fun(neighbours => {
              toGlobal(MapSeq(id)) o
                ReduceSeqUnroll(\((acc, next) =>
                  multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, weights)
            }) /*o PrintType()*/, a,b)) o  Transpose() o Map(Transpose()) $ x
        }))) o Slide2D(a,b)  $ input
    )

  println(Compile(stencil3DWeights(3,1)))

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))(stencil3DWeights(slidesize,slidestep), values,StencilUtilities.weights3D.flatten.flatten)
    val (gold: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(stencil3DCompareWeights(slidesize,slidestep), values, StencilUtilities.weights3D.flatten.flatten)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide3DTest27PointWithWeightsSwapTranspose(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }

    val O = 2 + SizeVar("O")
    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil3DCompareWeights(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      ArrayTypeWSWC(Float, slidesize*slidesize*slidesize),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, weights)
        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    def stencil3DWeights(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, O), N), M),
      ArrayTypeWSWC(Float, slidesize*slidesize*slidesize),
      (input,weights) =>
        MapGlb(1)(MapGlb(0)(
          fun(x => {
          toGlobal(MapSeqSlide(
            fun(neighbours => {
              toGlobal(MapSeq(id)) o
                ReduceSeqUnroll(\((acc, next) =>
                  multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, weights)
            }) /*o PrintType()*/, a,b)) /*o  Transpose() o Map(Transpose())*/ $ x
        }))) o PrintType () o  Map(Map(Transpose())) o Map(Map(Map(Transpose()))) o Slide2D(a,b)  $ input
    )

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2,(true,true))(stencil3DWeights(slidesize,slidestep), values,StencilUtilities.weights3D.flatten.flatten)
    val (gold: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(stencil3DCompareWeights(slidesize,slidestep), values, StencilUtilities.weights3D.flatten.flatten)

    assertArrayEquals(gold, output, 0.1f)

  }

}

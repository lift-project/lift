package opencl.generator.stencil

import ir.{ArrayType, ArrayTypeWSWC, TupleType}
import ir.ast.{Get, Pad, Slide, Zip, fun}
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import org.junit.{AfterClass, BeforeClass}
import org.junit.Assert._
import org.junit._
import opencl.ir.pattern._
import ir.ast._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import utils.OutputKernelJSON

object TestSlideSeqPlus
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

object SlideSeqPlusHelpers
{

  val compareSize = 1000000
  val iterations = 10
  val N = 2+ SizeVar("N")

  def stencil(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(Float, SizeVar("N")),
    (input) =>
       toGlobal(SlideSeqPlus(MapSeqUnroll(id) o ReduceSeqUnroll(absAndSumUp,0.0f), a,b)) $ input
  )

  def stencil2D(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
    (input) =>
      MapGlb(0)(toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) o Join() , a,b) /*o  Join() o Slide2D(a,b)*/)) /*o Slide(a,b)*/ o Slide(a,b) $ input
  )
}

class TestSlideSeqPlus
{

  @Test
  def reduceSlide1DTestSize3Step1(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(SlideSeqPlusHelpers.compareSize) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val lambda = Compile(SlideSeqPlusHelpers.stencil(slidesize,slidestep))
//    println(lambda)

    //OutputKernelJSON(SlideSeqPlusHelpers.stencil(slidesize,slidestep),"/home/reese/workspace/phd/sandbox/perftests/","stencil1Dssp.json","stencil1Dssp.cl")
    var outputX = Array[Float]()
    var runtime = 0.0
    var runTimeTotal = 0.0

    for(x <- 1 to SlideSeqPlusHelpers.iterations) {
      val (outputX, runtime) = Execute(2, 2)(SlideSeqPlusHelpers.stencil(slidesize, slidestep), values)
      runTimeTotal += runtime
    }
   // assertArrayEquals(gold, outputX, 0.1f)

    println("Runtime: "+runTimeTotal/SlideSeqPlusHelpers.iterations)

  }

  @Test
  def reduceSlide1DTestSize3Step1Original(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(SlideSeqPlusHelpers.compareSize) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val N = 2+SizeVar("N")

    val stencil2DOrg = fun(
      ArrayTypeWSWC(Float, N),
      (input) =>
        MapGlb(0)(
          fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbours
          } )) o Slide(slidesize,slidestep) $ input
    )

    var outputX = Array[Float]()
    var runtime = 0.0
    var runTimeTotal = 0.0

    for(x <- 1 to SlideSeqPlusHelpers.iterations) {
      //  OutputKernelJSON(stencil2DOrg,"/home/reese/workspace/phd/sandbox/perftests/","stencil1Dorg.json","stencil1Dorg.cl")
      val (outputX, runtime) = Execute(2, 2)(stencil2DOrg, values)
      runTimeTotal += runtime
    }

//    assertArrayEquals(gold, outputX, 0.1f)

    println("Runtime: "+runTimeTotal/SlideSeqPlusHelpers.iterations)

  }

  @Test
  def reduceSlide1DTestSize5Step3(): Unit = {

    val slidesize = 5
    val slidestep = 3
    val size = 20
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

/*
    StencilUtilities.print1DArray(values)
    StencilUtilities.print2DArray(values.sliding(slidesize,slidestep).toArray)

    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)
*/

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize5Step5(): Unit = {

    val slidesize = 5
    val slidestep = 5
    val size = 20
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

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

    val (output: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestWithWeights(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val weights = Array( 0.5f, 1.0f, 0.5f )
    val gold = Array( 4.0f,6.0f,8.0f,10.0f,12.0f,14.0f ) //values.sliding(3,1).toArray.map(x => x.reduceLeft(0.5f*_ + 0.5f*_))

    val orgStencil = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      ArrayTypeWSWC(Float, 3),
      (input,wgts) => {
        SlideSeqPlus(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(add, 0.0f) o
            MapSeqUnroll(mult) $
            Zip(wgts, neighbourhood)
          }), slidesize, slidestep)
        }  $ input
    )

    val source = Compile(orgStencil)
    val (output: Array[Float], _) = Execute(2,2)(source, orgStencil, values, weights)

    assertArrayEquals(gold, output, 0.1f)

  }

  // TODO - 2D, etc

  @Test
  def at2DOneLine3AcrossStencil(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val N = 2 + SizeVar("N")

    val atNeigh = fun(
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

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) , a,b)  o Join() /* o Slide2D(a,b)*/)) o Slide(1,b) $ input
    )

    def stencil2DRSwap(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        toGlobal(
          SlideSeqPlus(
            toGlobal(ReduceSeq(fun( (acc, array) => {
              fun( x =>
                toGlobal(add)(acc, x.at(0))
              ) o toGlobal(ReduceSeq(add, 0.0f)) $ array
            }), 0.0f)),
            a,b)
        )  $ input
    )
    //val (compare: Array[Float], _) = Execute(2,2)(atNeigh, values)
    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    println("******")
    //StencilUtilities.print1DArrayAs2DArray(compare, size-2)
    println("******")
    StencilUtilities.print1DArrayAs2DArray(output, size) //.dropRight(size-2).drop(size-2),size-2)

    //assertArrayEquals(compare, output.dropRight(size-2).drop(size-2), 0.1f)

  }

  @Test
  def at2DThreeLines3AcrossStencil(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val N = 2 + SizeVar("N")

    val atNeigh = fun(
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

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) , a,b) o Join() o Join() o Join() o Slide2D(a,b)/* o Slide2D(a,b)*/)) o Slide(a,b) $ input
    )


    def stencil2DCTest(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) , a,b))) o Transpose() /* o Slide2D(a,b)*/ $ input
    )

    println(Compile(stencil2DCTest(3,1)))

    val (compare: Array[Float], _) = Execute(2,2)(atNeigh, values)
    val (output: Array[Float], _) = Execute(2,2)(stencil2DCTest(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    println("******")
    StencilUtilities.print1DArrayAs2DArray(compare, size-2)
    println("******")
    StencilUtilities.print1DArrayAs2DArray(output.dropRight(size-2).drop(size-2),size-2)

    assertArrayEquals(compare, output.dropRight(size-2).drop(size-2), 0.1f)

  }

  @Test
  def reduceSlide2DTestUnoptimised(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    val orgStencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (mat) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(3,1) $ mat
      })

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(fun(x => {

          val side1 =  MapSeq( fun( t => { toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ t })) o Slide(3,1) $ x.at(0)
          val side2 = MapSeq( fun( t => { toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ t })) o Slide(3,1) $ x.at(2)
          val sum = toGlobal(MapSeq(id) o MapSeq(addTuple)) $ Zip(Join() $ side1,Join() $ side2)
          val tmpSum = 0.0f
          val ssp =  toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,tmpSum) , a,b))  $ x.at(1)
          val actSum = toGlobal(MapSeq(addTuple)) $ Zip( Join() $ ssp,sum)
          actSum
        })) o Slide(3,1) o Transpose() $ input
    )

    println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTestMoreOptimised(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    val orgStencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (mat) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(3,1) $ mat
      })

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(fun(x => {

          val sumSide33 =  MapSeq( fun( t => {

            val tP = Join() $ t


            val p0 = tP.at(0).at(1)
            val p1 = tP.at(1).at(0)
            val p2 = tP.at(1).at(1)
            val p6 = tP.at(1).at(1)
            val p7 = tP.at(1).at(2)
            val p8 = tP.at(2).at(1)

            val stencil =  fun(x => add(x, p0)) o
              fun(x => add(x,p1)) o
              fun(x => add(x,p2)) o
              fun(x => add(x,p6)) o
              fun(x => add(x,p7)) $ p8

              toGlobal(id) $ stencil

               })) o Slide2D(3,1) o Transpose() $ x
          val tmpSum = 0.0f
          val ssp =  toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp, tmpSum) , a,b))  $ x.at(1)
          val actSum = toGlobal(MapSeq(addTuple)) $ Zip( Join() $ ssp,sumSide33)
//          val actSum = toGlobal(MapSeq(addTuple)) $ Zip( Join() $ ssp,sumSide33)
          actSum
        })) o Slide(3,1) o Transpose() $ input
    )

    println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTestWithWeights(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }
    val weights = Array( 0.0f, 0.5f, 0.0f, 0.5f, 1.0f, 0.5f, 0.0f, 0.5f, 0.0f )
    val gold = Array( 9.0f,12.0f,15.0f,18.0f,21.0f,24.0f,12.0f,15.0f,18.0f,21.0f,24.0f,27.0f,15.0f,18.0f,21.0f,24.0f,27.0f,30.0f,18.0f,21.0f,24.0f,27.0f,30.0f,33.0f,21.0f,24.0f,27.0f,30.0f,33.0f,36.0f,24.0f,27.0f,30.0f,33.0f,36.0f,39.0f )

    val N = 2 + SizeVar("N")

    val orgStencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Float, weights.length),
      (input,wgts) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours,  wgts)
          }))
        ) o Slide2D(3,1) $ input
      })



    def stencil2DRW(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Float, 3),
      (input,wgts) =>
        MapGlb(0)(toGlobal(
          /*SlideSeqPlus(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(add, 0.0f) o
            MapSeqUnroll(mult) $
            Zip(wgts, neighbourhood)
          }), slidesize, slidestep)*/
          SlideSeqPlus(fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(add, 0.0f) o
            MapSeqUnroll(mult) $
            Zip(wgts, Join() o Slide2D(a,b) $ neighbourhood)}), a,b)  /* o Slide2D(a,b)*/)) o Slide(a,b)
          $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil2DRW(slidesize,slidestep), values, weights)
    val (compare: Array[Float], _) = Execute(2,2)(orgStencil, values, weights)

    assertArrayEquals(compare, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestMapAt(): Unit = {

    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _))


    val stencil = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) => {
        MapGlb(
          toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(fun((acc, y) => {
              absAndSumUp.apply(acc, y)
            }), 0.0f))
      } o Slide(3, 1)  $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil, values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }
}

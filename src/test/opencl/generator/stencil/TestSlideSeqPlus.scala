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
  val M = 2+ SizeVar("M")

  def stencil(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
       toGlobal(SlideSeqPlus(MapSeqUnroll(id) o ReduceSeqUnroll(absAndSumUp,0.0f), a,b)) $ input
  )

  def original2DStencil(size: Int, step: Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (mat) => {
      MapGlb(1)(
        MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeqUnroll(id)) o
            ReduceSeq(add, 0.0f) o Join() $ neighbours
        }))
      ) o Slide2D(size,step) $ mat
    })

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

  def stencil2D(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (input) =>
      MapGlb(0)(fun(x => {
        val tmpSum = 0.0f
        toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,tmpSum) o Join() o PrintType(), a,b)) o  Transpose() $ x
      })) o Slide(3,1)  $ input
  )

  def getMiddle(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
    (input) =>
      MapGlb(0)(fun(x => {
        toGlobal(SlideSeqPlus( (fun (x => {  toGlobal(id) $ x.at(1).at(1) })), a,b)) o  Transpose() $ x
      })) o Slide(3,1)   $ input
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
    println(lambda)

    //OutputKernelJSON(SlideSeqPlusHelpers.stencil(slidesize,slidestep),"/home/reese/workspace/phd/sandbox/perftests/","stencil1Dssp.json","stencil1Dssp.cl")
    var outputX = Array[Float]()
    var runtime = 0.0
    var runTimeTotal = 0.0

    println(Compile(SlideSeqPlusHelpers.stencil(slidesize,slidestep)))

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

    println(Compile(SlideSeqPlusHelpers.stencil(slidesize,slidestep)))


    StencilUtilities.print1DArray(values)
    StencilUtilities.print2DArray(values.sliding(slidesize,slidestep).toArray)

    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)


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

  /** 2D **/

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

    val (compare: Array[Float], _) = Execute(2,2)(atNeigh, values)
    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    println("******")
    //StencilUtilities.print1DArrayAs2DArray(compare, size-2)
    println("******")
    StencilUtilities.print1DArrayAs2DArray(output, size) //.dropRight(size-2).drop(size-2),size-2)

    assertArrayEquals(compare, output.dropRight(size-2).drop(size-2), 0.1f)
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

    def stencil2DCTest(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      (input) =>
        MapGlb(0)(toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,0.0f) , a,b))) o Transpose() /* o Slide2D(a,b)*/ $ input
    )

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

    def stencil2DR(a: Int ,b :Int) = fun(
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

               })) o Slide2D(3,1) o Transpose() $ x
          val tmpSum = 0.0f
          val ssp =  toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp, tmpSum) , a,b))  $ x.at(1)
          val actSum = toGlobal(MapSeq(addTuple)) $ Zip( Join() $ ssp,sumSide33)
          actSum
        })) o Slide(3,1) o Transpose() $ input
    )

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }


  @Test
  def reduceSlide2DTest9PointStencil(): Unit = {

    val size = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapGlb(0)(fun(x => {
          val tmpSum = 0.0f
          toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,tmpSum) o Join(), a,b)) o  Transpose() $ x
        })) o Slide(3,1)  $ input
    )

//    println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)
    val (goldExec: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.original2DStencil(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
  //  StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)

//    for(i <- 0 to goldExec.length-1)    goldExec(i) -= output(i)
    StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)
//    StencilUtilities.print1DArray(output)
//    StencilUtilities.print1DArray(gold)

    assertArrayEquals(goldExec, output, 0.1f)

  }


  @Test
  def reduceSlide2DTest9PointWithWeights(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (input, weights) =>
        MapGlb(0)(fun(x => {
          val tmpSum = 0.0f
          toGlobal(SlideSeqPlus(fun(neighbours => {
            toGlobal(MapSeq(id)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }), a,b)) o  Transpose() $ x
        })) o Slide(3,1)  $ input
    )

    //    println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values, StencilUtilities.weightsArr)
    val (goldExec: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.original2DWeightStencil(slidesize,slidestep), values, StencilUtilities.weightsArr)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    //  StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)

    //    for(i <- 0 to goldExec.length-1)    goldExec(i) -= output(i)
    StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)
    //    StencilUtilities.print1DArray(output)
    //    StencilUtilities.print1DArray(gold)

    assertArrayEquals(goldExec, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest9PointWithWeightsAndAt(): Unit = {

    val size = 14

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def lambdaNeighAt(a: Int, b: Int) = fun(
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

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapGlb(0)(fun(x => {

          toGlobal(SlideSeqPlus(fun(m => {

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

    //    println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], runtimeNew: Double) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)
    val (goldExec: Array[Float], runtimeOrg: Double) = Execute(2,2)(lambdaNeighAt(slidesize,slidestep), values)

    //  StencilUtilities.print2DArray(values)
    //  StencilUtilities.print1DArrayAs2DArray(output,size-2)
    //  StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)

    //    for(i <- 0 to goldExec.length-1)    goldExec(i) -= output(i)
    //    StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)
    //    StencilUtilities.print1DArray(output)
    //    StencilUtilities.print1DArray(gold)

    assertArrayEquals(goldExec, output, 0.1f)

    println("Kernel compare: "+runtimeOrg+ " " + runtimeNew)

  }



  @Test
  def reduceSlide2DTest9PointStencilAsymmetric(): Unit = {

    val sizeX = 16
    val sizeY = 8
    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(sizeY,sizeX) { (i,j) => (i*sizeX + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapGlb(0)(fun(x => {
          val tmpSum = 0.0f
          toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,tmpSum) o Join(), a,b)) o  Transpose() $ x
        })) o Slide(3,1)  $ input
    )

    //println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)
    val (goldExec: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.original2DStencil(slidesize,slidestep), values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,sizeX-2)
     StencilUtilities.print1DArrayAs2DArray(goldExec,sizeX-2)
   val sub = goldExec.clone()

   /*for(i <- 0 to goldExec.length-1)    sub(i) -= output(i)
     StencilUtilities.print1DArrayAs2DArray(sub,sizeX-2)
     StencilUtilities.print1DArray(output)
     StencilUtilities.print1DArray(gold) */

    assertArrayEquals(goldExec, output, 0.1f)

  }

  @Test
  def reduceSlide2DTest9PointSize5Step3(): Unit = {

    val size = 8
    val slidesize = 5
    val slidestep = 3
    val values = Array.tabulate(size,size) { (i,j) => (i*size + j + 1).toFloat }

    val firstSlide = values.sliding(slidesize,slidestep).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten

    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        MapGlb(0)(fun(x => {
          val tmpSum = 0.0f
          toGlobal(SlideSeqPlus(MapSeq(id) o ReduceSeq(absAndSumUp,tmpSum) o Join(), a,b)) o  Transpose() $ x
        })) o Slide(a,b)  $ input
    )

//    println(Compile(stencil2DR(slidesize,slidestep)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)
    val (goldExec: Array[Float], _) = Execute(2,2)(SlideSeqPlusHelpers.original2DStencil(slidesize,slidestep), values)


    StencilUtilities.print2DArray(values)

    // These print incorrectly because size depends ALSO on step
    println("=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*")
     StencilUtilities.print1DArrayAs2DArray(output,size-2)
    //  StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)
    println("=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*")

    //    for(i <- 0 to goldExec.length-1)    goldExec(i) -= output(i)
    StencilUtilities.print1DArrayAs2DArray(goldExec,size-2)
    //    StencilUtilities.print1DArray(output)
    //    StencilUtilities.print1DArray(gold)

    assertArrayEquals(goldExec, output, 0.1f)

  }


  /** 3D **/


  @Test
  def reduceSlide3DTest9PointWithWeightsAndAt(): Unit = {

    val size = 8

    val slidesize = 3
    val slidestep = 1
    val values = Array.tabulate(size,size,size) { (i,j,k) => (i*size*size + j*size + k + 1).toFloat }

    val O = 2 + SizeVar("O")
    val N = 2 + SizeVar("N")
    val M = 2 + SizeVar("M")

    val x = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), O)

    def lambdaNeighAt(a: Int, b: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), O),
      (mat) => {
        (MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

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

        }))))
          o Slide3D(a,b) $ mat)
      })

    def stencil2DR(a: Int ,b :Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), O),
      (input) =>
        MapGlb(1)(MapGlb(0)(fun(x => {
          toGlobal(SlideSeqPlus(fun(m => {

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

          }), a,b)) o  Transpose() $ x
        }))) o Slide2D(a,b)  $ input
    )

    //    println(Compile(stencil2DR(3,1)))

    val (output: Array[Float], _) = Execute(2,2)(stencil2DR(slidesize,slidestep), values)
    val (goldExec: Array[Float], _) = Execute(2,2)(lambdaNeighAt(slidesize,slidestep), values)

    StencilUtilities.print3DArray(values)
    //StencilUtilities.print1DArrayAs3DArray(output,size-2)
    //  StencilUtilities.print1DArrayAs3DArray(goldExec,size-2)

    //    for(i <- 0 to goldExec.length-1)    goldExec(i) -= output(i)
    StencilUtilities.print1DArrayAs3DArray(goldExec,size-2,size-2,size-2)
    //    StencilUtilities.print1DArray(output)
    //    StencilUtilities.print1DArray(gold)

//    assertArrayEquals(goldExec, output, 0.1f)

  }

}

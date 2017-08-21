package opencl.generator.stencil.acoustic
import java.io._

import ir.ArrayTypeWSWC
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.language.implicitConversions
import scala.reflect.ClassTag

object StencilUtilities
{

  /* globals */
  val iter = 5
  val printOutput = false
  val stencilDelta = 0.00002f
  val slidesize = 3;
  val slidestep = 1;
  val stencilDim = 8;
  val stencilSize = stencilDim - 2;

  /* shared data */
  val weights9 = Array.fill(9)(1).map(_.toFloat)

  val weightsArr = Array(
    0.0f, 1.0f, 0.0f,
    1.0f, 0.0f, 1.0f,
    0.0f, 1.0f, 0.0f)

  val weightsMiddleArr = Array(
    0.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f,
    0.0f, 0.0f, 0.0f)


  val weights = Array(
    Array(0.0f, 1.0f, 0.0f),
    Array(1.0f, 0.0f, 1.0f),
    Array(0.0f, 1.0f, 0.0f))

  val weightsMiddle = Array(
    Array(0.0f, 0.0f, 0.0f),
    Array(0.0f, 1.0f, 0.0f),
    Array(0.0f, 0.0f, 0.0f))

  val weights3D = Array(
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 1.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f)),
    Array(Array(0.0f, 1.0f, 0.0f),
      Array(1.0f, 0.0f, 1.0f),
      Array(0.0f, 1.0f, 0.0f)),
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 1.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f))
  )

  val weightsMiddle3D = Array(
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f)),
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 1.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f)),
    Array(Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.0f, 0.0f))
  )


  /* helper functions */
  def print1DArray[T](input: Array[T]) = {
    println(input.mkString(","))
  }

  def print2DArray[T](input: Array[Array[T]]) = {
    println(input.deep.mkString("\n"))
  }

  def print3DArray[T](input: Array[Array[Array[T]]]) = {
    for (i <- 0 to input.length-1){
      print2DArray(input(i))
      println()
    }
  }

  def print1DArrayAs2DArray[T](input: Array[T], dimX: Int) {
    var count = 1
    println()
    input.foreach(x => if (count % dimX > 0) {
      print(x + " ");
      count += 1
    } else {
      println(x + " ");
      count += 1
    })
    println()
  }

  def print1DArrayAs3DArray[T](input: Array[T], dimX: Int, dimY: Int, dimZ: Int): Unit = {
    val area = dimX*dimY
    val vol = input.length

    for(i <- 0 to dimZ-1)
    {
      print1DArrayAs2DArray(input.slice(i*area,i*area+area),dimX)
    }

  }

  def printOriginalAndOutput2D[T](original: Array[Array[T]], output: Array[T], dimX: Int): Unit = {
    println("ORIGINAL:")
    print2DArray(original)
    println("*********************")
    println("OUTPUT:")
    print1DArrayAs2DArray(output, dimX)
  }

  def printOriginalAndOutput3Das1D[T:ClassTag](original: Array[Array[Array[T]]], output: Array[T]): Unit = {

    val org = (original.flatten).flatten
    println("ORIGINAL:" + org.length)
    print1DArray(org)
    println("*********************")
    println("OUTPUT:" + output.length)
    print1DArray(output)
  }

  def printOriginalAndOutput3DSame[T:ClassTag](original: Array[Array[Array[T]]], output: Array[T]): Unit = {

    println("ORIGINAL:" + original.flatten.flatten.length)
    print3DArray(original)
    println("*********************")
    println("OUTPUT:" + output.length)
    print1DArrayAs3DArray(output,original.length,original(0).length,original(0)(0).length)
  }

  def printOriginalAndOutput3D[T:ClassTag](original: Array[Array[Array[T]]], output: Array[T]): Unit = {

    println("ORIGINAL:" + original.flatten.flatten.length)
    print3DArray(original)
    println("*********************")
    println("OUTPUT:" + output.length)
    print1DArrayAs3DArray(output,original(0)(0).length-2,original(0).length-2,original.length-2)
  }
    /* only one (value) layer of padding around 2D matrix */
    def createFakePaddingFloat2D(input: Array[Array[Float]], padValue: Float): Array[Array[Float]] = {
      val padSize = input(0).length
      val actualSize = padSize+2
      val padLR = Array.fill(1)(padValue)
      val toppad = Array.fill(1)(Array.fill(actualSize)(padValue))
      val output = input.map(i => padLR ++ i ++ padLR)
      toppad ++ output ++ toppad
    }

    def createFakePaddingFloat3D(input: Array[Array[Array[Float]]], padValue: Float, dimX: Int, dimY: Int): Array[Array[Array[Float]]] = {
      val z0 = Array.fill(dimY+2,dimX+2)(0.0f)
      for(i <- 0 to input.length-1) input(i) = createFakePaddingFloat2D(input(i),0.0f)
      Array(z0) ++ input ++ Array(z0)
    }

    def createFakePaddingInt2D(input: Array[Array[Int]], padValue: Int): Array[Array[Int]] = {

      val padSize = input(0).length
      val actualSize = padSize+2
      val padLR = Array.fill(1)(padValue)
      val toppad = Array.fill(1)(Array.fill(actualSize)(padValue))
      val output = input.map(i => padLR ++ i ++ padLR)
      toppad ++ output ++ toppad

    }

    def createDataFloat2D(sizeX: Int, sizeY: Int) = {

      val dim = sizeX+2
      val filling = Array.tabulate(sizeY,sizeX) { (i,j) => (j + 1).toFloat }
      createFakePaddingFloat2D(filling,0.0f)
    }


    def createDataFloat3D(sizeX: Int, sizeY: Int, sizeZ: Int) = {
      Array.tabulate(sizeZ,sizeY,sizeX) { (i,j,k) => (i + j + k + 1).toFloat }
    }

    def createDataFloat3DWithPadding(sizeX: Int, sizeY: Int, sizeZ: Int) = {
      val filling = createDataFloat3D(sizeX,sizeY, sizeZ)
      createFakePaddingFloat3D(filling,0.0f,sizeX,sizeY)
    }

    /* these helper functions do not work, but it would be nice if they did! */
  def map2D(f: Lambda1): FunDecl = {
    fun(x => x :>> MapSeq(fun(row => row :>> MapSeq(f))))
  }

  def reduce2D(f: Lambda2, init: Expr): FunDecl = {
    fun(x => x :>> MapSeq(fun(row => row :>> ReduceSeq(f, init))) :>> Transpose()
      :>> MapSeq(fun(n => n :>> ReduceSeq(f, init))) :>> Join())
  }

  val zip2D = fun((A, B) =>
    Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(A, B)
  )

  val zip3d2 = \((A,B) =>    Map(Map(\(tuple2 => Zip(tuple2._0, tuple2._1)))) o Map( \(tuple => Zip(tuple._0, tuple._1))) $ Zip(A,B)  )

  val zip3d3 = \((A,B,C) =>     Map(Map(\(tuple2 => Zip(tuple2._0, tuple2._1, tuple2._2)))) o Map( \(tuple => Zip(tuple._0, tuple._1, tuple._2))) $ Zip(A,B,C))


  def writeToBinaryFile(arr: Array[Float], filename: String)
  {
      var fileOut = new FileOutputStream(filename)
      var dOut = new DataOutputStream(fileOut)

      for(i <- arr) dOut.writeDouble(i)

    dOut.flush()
    dOut.close()
    fileOut.close()
  }

  // outer => inner
  def vonNeumann5pt(x: Param) = {
    val N = x.at(0).at(1)
    val S = x.at(2).at(1)
    val W = x.at(1).at(0)
    val E = x.at(1).at(2)
    val C = x.at(1).at(1)
    (N, S, W, E, C)
  }

  def vonNeumann7pt(x: Param) = {
    //         z      y     x
    val N = x.at(1).at(0).at(1)
    val S = x.at(1).at(2).at(1)
    val W = x.at(1).at(1).at(0)
    val E = x.at(1).at(1).at(2)
    val C = x.at(1).at(1).at(1)
    val F = x.at(0).at(1).at(1)
    val B = x.at(2).at(1).at(1)
    (N, S, W, E, F, B, C)
  }

  def moore9pt(x: Param) = {
    val NW = x.at(0).at(0)
    val N  = x.at(0).at(1)
    val NE = x.at(0).at(2)
    val W  = x.at(1).at(0)
    val C  = x.at(1).at(1)
    val E  = x.at(1).at(2)
    val SW = x.at(2).at(0)
    val S  = x.at(2).at(1)
    val SE = x.at(2).at(2)
    (NW, N, NE, W, C, E, SW, S, SE)
  }

  /*
  def moore25pt(x: Param) = {
    val NNWW = x.at(0).at(0)
    val NNW = x.at(0).at(1)
    val NN = x.at(0).at(2)
    val NNE = x.at(0).at(3)
    val NNEE = x.at(0).at(4)
    val NWW  = x.at(1).at(0)
    val NW  = x.at(1).at(1)
    val N  = x.at(1).at(2)
    val NE  = x.at(1).at(3)
    val NEE  = x.at(1).at(4)
    val WW  = x.at(2).at(0)
    val W  = x.at(2).at(1)
    val C  = x.at(2).at(2)
    val E  = x.at(2).at(3)
    val EE  = x.at(2).at(4)
    val SWW = x.at(3).at(0)
    val SW = x.at(3).at(1)
    val S = x.at(3).at(2)
    val SE = x.at(3).at(3)
    val SEE = x.at(3).at(4)
    val SSWW  = x.at(4).at(0)
    val SSW  = x.at(4).at(1)
    val SS  = x.at(4).at(2)
    val SSE = x.at(4).at(3)
    val SSEE = x.at(4).at(4)
    (NNWW, NNW, NN, NNE, NNEE,
      NWW, NW, N, NE, NEE,
      WW, W, C, E, EE,
      SWW, SW, S, SE, SEE,
      SSWW, SSW, SS, SSE, SSEE
    )
  }
  */
}

object TestAcousticStencils {
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

class TestAcousticStencils {


  val stencilarr = StencilUtilities.createDataFloat2D(StencilUtilities.stencilSize,StencilUtilities.stencilSize)
  val stencilarrsame = StencilUtilities.createDataFloat2D(StencilUtilities.stencilSize,StencilUtilities.stencilSize)
  val stencilarrCopy = stencilarr.map(x => x.map(y => y * 2.0f))

@Test
  def testStencil2DSimple(): Unit = {

    /* u[cp] = S */

    val compareData = Array(3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 11.0f,
      4.0f, 8.0f, 12.0f, 16.0f, 20.0f, 17.0f,
      4.0f, 8.0f, 12.0f, 16.0f, 20.0f, 17.0f,
      4.0f, 8.0f, 12.0f, 16.0f, 20.0f, 17.0f,
      4.0f, 8.0f, 12.0f, 16.0f, 20.0f, 17.0f,
      3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 11.0f
    )

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                // Where does "acc" come from ? !!!!
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, StencilUtilities.weightsArr)

    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

@Test
  def testStencil2DSimpleTimesConstant(): Unit = {

    val compareData = Array(
      6.0f, 12.0f, 18.0f, 24.0f, 30.0f, 22.0f,
      8.0f, 16.0f, 24.0f, 32.0f, 40.0f, 34.0f,
      8.0f, 16.0f, 24.0f, 32.0f, 40.0f, 34.0f,
      8.0f, 16.0f, 24.0f, 32.0f, 40.0f, 34.0f,
      8.0f, 16.0f, 24.0f, 32.0f, 40.0f, 34.0f,
      6.0f, 12.0f, 18.0f, 24.0f, 30.0f, 22.0f
    )

    /* cp => index
       S => stencil sum
      u[cp] = S*l2  */

    val constant = 2.0f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(mult, constant) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, StencilUtilities.weightsArr)

    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }


@Test
  def testStencil2DSimpleTimesConstantPlusSelf(): Unit = {

    val compareData = Array(
      18.0f, 36.0f, 54.0f, 72.0f, 90.0f, 66.0f,
      24.0f, 48.0f, 72.0f, 96.0f, 120.0f, 102.0f,
      24.0f, 48.0f, 72.0f, 96.0f, 120.0f, 102.0f,
      24.0f, 48.0f, 72.0f, 96.0f, 120.0f, 102.0f,
      24.0f, 48.0f, 72.0f, 96.0f, 120.0f, 102.0f,
      18.0f, 36.0f, 54.0f, 72.0f, 90.0f, 66.0f
    )

    /* u[cp] = S*l2 + u1[cp] */

    val add2 = UserFun("add2", Array("x", "y"), "{ return y+y; }", Seq(Float, Float), Float).
      setScalaFun(xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])

    val constant = 3.0f

    val timesConstantPlusSelf = UserFun("timesConstantPlusSelf", Array("x", "y"), "{ return x + x; }", Seq(Float, Float), Float)

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(id)) o
              MapSeq(fun(x => add(x, x))) o
              MapSeq(fun(x => mult(x, constant))) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, StencilUtilities.weightsArr)

    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

@Test
  def testStencil2DSimpleAccessTwoWeightsMultConstOne(): Unit = {

    val compareData = Array(10.0f, 20.0f, 30.0f, 40.0f, 50.0f, 39.0f,
      13.0f, 26.0f, 39.0f, 52.0f, 65.0f, 57.0f,
      13.0f, 26.0f, 39.0f, 52.0f, 65.0f, 57.0f,
      13.0f, 26.0f, 39.0f, 52.0f, 65.0f, 57.0f,
      13.0f, 26.0f, 39.0f, 52.0f, 65.0f, 57.0f,
      10.0f, 20.0f, 30.0f, 40.0f, 50.0f, 39.0f)

    val constant = 3.0f

    /* u[cp] = S*l2 + u[cp] */

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat, weights, weightsMiddle) => {
        MapGlb(1)(
          MapGlb(0)(fun(n => {
            toGlobal(MapSeq(addTuple)) $ Zip(
              ReduceSeq(mult, constant) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(n, weights),
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(n, weightsMiddle)
            )
          }))) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val source = Compile(lambdaNeigh)
    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(source,lambdaNeigh, stencilarr, StencilUtilities.weights, StencilUtilities.weightsMiddle)
    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Ignore // KEEP THIS
  @Test
  def testStencil2DSimpleTimesConstantPlusSelfPlusPrevious(): Unit = // Nothing here, just aborted ideas
  {

    val computeTwoStencils = Join() o (fun(tuple => {
      val left = Get(tuple, 0)
      val right = Get(tuple, 1)
      // only possible because reduce returns array of size 1!
      Zip(
        ReduceSeq(add, 0.0f) o Join() $ left,
        ReduceSeq(add, 0.0f) o Join() $ right)
    }))

    val dataBeforeCompute = fun(inputTile => Join() o computeTwoStencils o Split(StencilUtilities.stencilDim) $ Zip(
      Join() o Slide2D(3, 1) $ inputTile,
      Join() o Slide2D(3, 1) $ inputTile
    ))

    val lambdaTwoStencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      (inp) => {
        MapGlb(1)(
          MapGlb(0)(
            toGlobal(MapSeqUnroll(id)) o
              toGlobal(MapSeq(addTuple)) o dataBeforeCompute)) $ inp
      })

  }

  @Ignore // KEEP THIS
  @Test
  def testStencil2DSimpleAccessTwoWeightsBAD(): Unit = {
    /*
        Attempt to pull out using two stencils using zip2D / map2D / reduce2D
        ... which doesn't work
    */


    val constant = 3.0f

    val neighbourhoodFun = fun((nbh, w1, w2) => {
      val nbhw1 = StencilUtilities.zip2D(nbh, w1) :>> StencilUtilities.map2D(multTuple) :>> StencilUtilities.reduce2D(add, 0.0f)
      val nbhw2 = StencilUtilities.zip2D(nbh, w2) :>> StencilUtilities.map2D(multTuple) :>> StencilUtilities.reduce2D(add, 0.0f)

      Zip(nbhw1, nbhw2) :>> Map(addTuple) :>> toGlobal(MapSeqUnroll(id))
    })

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat, weights, weightsMiddle) => {
        MapGlb(1)(
          MapGlb(0)(fun(n =>
            neighbourhoodFun(n, weights, weightsMiddle)
          ))) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, StencilUtilities.weights, StencilUtilities.weightsMiddle)
    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)
  }

@Test
  def testStencil2DTwoGridsSwap(): Unit = {

    val compareData = Array(3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 18.0f,
      3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 18.0f,
      3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 18.0f,
      3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 18.0f,
      3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 18.0f,
      3.0f, 6.0f, 9.0f, 12.0f, 15.0f, 18.0f)

    /* u[cp] = u1[cp] + u[cp] */

    val constant = 3.0f

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (matrix1, matrix2, wghts1, wghts2) => MapGlb(fun((m) =>
        MapSeq(fun(n => MapSeq(id) $ n))
          $ Get(m, 0)
      )) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ matrix1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ matrix2)))
    )

  val n = SizeVar("N")
  val m = SizeVar("M")

  val lambdaNeigh = fun(
    ArrayTypeWSWC(ArrayTypeWSWC(Float, n), m),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, n), m),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(addTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weightsMiddle),
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 1), weightsMiddle)
          )
        }))) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))

      })

    //    Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrCopy, StencilUtilities.weights, StencilUtilities.weightsMiddle)
    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

@Test
  def twoGridSwapWith3DifferentWeightsAndConstants(): Unit = {

    val compareData = Array(22.0f, 44.0f, 66.0f, 88.0f, 110.0f, 90.0f,
      28.0f, 56.0f, 84.0f, 112.0f, 140.0f, 126.0f,
      28.0f, 56.0f, 84.0f, 112.0f, 140.0f, 126.0f,
      28.0f, 56.0f, 84.0f, 112.0f, 140.0f, 126.0f,
      28.0f, 56.0f, 84.0f, 112.0f, 140.0f, 126.0f,
      22.0f, 44.0f, 66.0f, 88.0f, 110.0f, 90.0f)

    /* u[cp] = S*l1 + u[cp]*l2 */

    val constant1 = 4.0f
    val constant2 = 3.0f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(addTuple)) $ Zip(
            ReduceSeq(mult, constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weightsMiddle),
            ReduceSeq(mult, constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 1), weights)
          )
        }))) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))

      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrCopy, StencilUtilities.weights, StencilUtilities.weightsMiddle)
    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)
    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

@Test
  def twoGridSwapWith3DifferentWeightsAndConstantsPlusSelf(): Unit = {

    val compareData = Array(
    31.51f,63.02f,94.53f,126.04f,157.55f,150.7f,
    36.99f,73.98f,110.97f,147.96f,184.95f,183.58f,
    36.99f,73.98f,110.97f,147.96f,184.95f,183.58f,
    36.99f,73.98f,110.97f,147.96f,184.95f,183.58f,
    36.99f,73.98f,110.97f,147.96f,184.95f,183.58f,
    31.51f,63.02f,94.53f,126.04f,157.55f,150.7f
    )

    /* u[cp] = X * ( S*l1 + u[cp]*l2 + u1[cp]*l3) */

    val constant0 = 2.0f
    val constant1 = 4.0f
    val constant2 = 3.0f
    val Xvalue = 1.37f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr(0).length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr(0).length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(fun(x => mult(x,Xvalue))) o MapSeq(addTuple)) $ Zip(
            ReduceSeq(mult, constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weightsMiddle),
            MapSeq(addTuple) $ Zip(ReduceSeq(mult, constant0) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0, tuple1._1))) $ Zip(Get(m, 1), weights),
              ReduceSeq(mult, constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0, tuple1._1))) $ Zip(Get(m, 1), weightsMiddle))

          )
        }))) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))
      })

    val source = Compile(lambdaNeigh)
    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(source,lambdaNeigh, stencilarr, stencilarrCopy, StencilUtilities.weights, StencilUtilities.weightsMiddle)
    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)
    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }


 /* let's iterate */

@Test
  def testSimpleStencilIterate5(): Unit = {

    /* u[cp] = S */

    val compareData = Array(
      462.0f,917.0f,1337.0f,1589.0f,1526.0f,938.0f,
      791.0f,1575.0f,2289.0f,2765.0f,2611.0f,1652.0f,
      945.0f,1883.0f,2751.0f,3311.0f,3171.0f,1981.0f,
      945.0f,1883.0f,2751.0f,3311.0f,3171.0f,1981.0f,
      791.0f,1575.0f,2289.0f,2765.0f,2611.0f,1652.0f,
      462.0f,917.0f,1337.0f,1589.0f,1526.0f,938.0f
    )

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })


    // there must be a better way ...
    var input = stencilarr
    var outputX = Array[Float]()
    var runtime = 0.0f

    for(x <- 1 to StencilUtilities.iter) {
      val (output: Array[Float], runtime) = Execute(input.length, input.length)(lambdaNeigh, input, StencilUtilities.weightsArr)
      if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(input, output, StencilUtilities.stencilSize)
      // need to re-pad, then slide and iterate
      input = StencilUtilities.createFakePaddingFloat2D(output.sliding(StencilUtilities.stencilSize,StencilUtilities.stencilSize).toArray,0.0f)
      outputX = output
    }

    assertArrayEquals(compareData, outputX, StencilUtilities.stencilDelta)

  }

  @Test
  def testStencil2DTwoGridsSwapIterate5(): Unit = {

    val compareData = Array(
    21.0f,42.0f,63.0f,84.0f,105.0f,126.0f,
    21.0f,42.0f,63.0f,84.0f,105.0f,126.0f,
    21.0f,42.0f,63.0f,84.0f,105.0f,126.0f,
    21.0f,42.0f,63.0f,84.0f,105.0f,126.0f,
    21.0f,42.0f,63.0f,84.0f,105.0f,126.0f,
    21.0f,42.0f,63.0f,84.0f,105.0f,126.0f
    )

    /* u[cp] = u1[cp] + u[cp] */

    val constant = 3.0f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(addTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weightsMiddle),
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 1), weightsMiddle)
          )
        }))) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))

      })

    var inputArr = stencilarr
    var inputArrCopy = stencilarrCopy
    var outputX = Array[Float]()
    var runtime = 0.0f

    for(x <- 1 to StencilUtilities.iter)
    {
      // why does this zip work but not the other one ? ? ? (in SimpleRoom..)
      val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, inputArr, inputArrCopy, StencilUtilities.weights, StencilUtilities.weightsMiddle)
      if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

      inputArr = inputArrCopy
      inputArrCopy = StencilUtilities.createFakePaddingFloat2D(output.sliding(StencilUtilities.stencilSize,StencilUtilities.stencilSize).toArray,0.0f)

      outputX = output
    }

    assertArrayEquals(compareData, outputX, StencilUtilities.stencilDelta)

  }


  @Test
  def testSimple2DStencilAsym1(): Unit = {

    /* u[cp] = S */

    val asymDimX =  10
    val asymDimY = 14

    val stencilarr = StencilUtilities.createDataFloat2D(asymDimX,asymDimY)

    val compareData = Array(
    462.0f,924.0f,1386.0f,1848.0f,2310.0f,2761.0f,3157.0f,3289.0f,2926.0f,1738.0f,
    792.0f,1584.0f,2376.0f,3168.0f,3960.0f,4741.0f,5412.0f,5709.0f,5016.0f,3058.0f,
    957.0f,1914.0f,2871.0f,3828.0f,4785.0f,5731.0f,6567.0f,6919.0f,6171.0f,3718.0f,
    1012.0f,2024.0f,3036.0f,4048.0f,5060.0f,6061.0f,6952.0f,7359.0f,6556.0f,3993.0f,
    1023.0f,2046.0f,3069.0f,4092.0f,5115.0f,6127.0f,7029.0f,7447.0f,6655.0f,4048.0f,
    1024.0f,2048.0f,3072.0f,4096.0f,5120.0f,6133.0f,7036.0f,7455.0f,6664.0f,4058.0f,
    1024.0f,2048.0f,3072.0f,4096.0f,5120.0f,6133.0f,7036.0f,7455.0f,6664.0f,4058.0f,
    1024.0f,2048.0f,3072.0f,4096.0f,5120.0f,6133.0f,7036.0f,7455.0f,6664.0f,4058.0f,
    1024.0f,2048.0f,3072.0f,4096.0f,5120.0f,6133.0f,7036.0f,7455.0f,6664.0f,4058.0f,
    1023.0f,2046.0f,3069.0f,4092.0f,5115.0f,6127.0f,7029.0f,7447.0f,6655.0f,4048.0f,
    1012.0f,2024.0f,3036.0f,4048.0f,5060.0f,6061.0f,6952.0f,7359.0f,6556.0f,3993.0f,
    957.0f,1914.0f,2871.0f,3828.0f,4785.0f,5731.0f,6567.0f,6919.0f,6171.0f,3718.0f,
    792.0f,1584.0f,2376.0f,3168.0f,3960.0f,4741.0f,5412.0f,5709.0f,5016.0f,3058.0f,
    462.0f,924.0f,1386.0f,1848.0f,2310.0f,2761.0f,3157.0f,3289.0f,2926.0f,1738.0f
    )

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(Float, StencilUtilities.weightsArr.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })


    // there must be a better way ...
    var input = stencilarr
    var outputX = Array[Float]()

    for(x <- 1 to StencilUtilities.iter) {
      val (output: Array[Float], runtime) = Execute(input.length, input.length)(lambdaNeigh, input, StencilUtilities.weightsArr)
      if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(input, output, asymDimX)
      // need to re-pad, then slide and iterate
      input = StencilUtilities.createFakePaddingFloat2D(output.sliding(asymDimX,asymDimX).toArray,0.0f)
      outputX = output
    }

    assertArrayEquals(compareData, outputX, StencilUtilities.stencilDelta)

  }

  @Test
  def twoGridSwapWith3DifferentWeightsAndConstantsPlusSelfIterate5Asym2D(): Unit = {

    val asymDimX = 14
    val asymDimY = 6

    val stencilarr = StencilUtilities.createDataFloat2D(asymDimX,asymDimY)
    val stencilarrCopy = stencilarr.map(i => i.map(j => j*2.0f))

    val compareData = Array(
    421.73438f,843.46875f,1265.2031f,1686.9375f,2108.6719f,2530.4062f,2952.1406f,3373.875f,3795.6094f,4216.4062f,4622.4375f,4911.2812f,4677.9375f,3123.1875f,
    648.96875f,1297.9375f,1946.9062f,2595.875f,3244.8438f,3893.8125f,4542.7812f,5191.75f,5840.7188f,6488.75f,7117.328f,7579.0312f,7252.7656f,4871.5f,
    718.53125f,1437.0625f,2155.5938f,2874.125f,3592.6562f,4311.1875f,5029.7188f,5748.25f,6466.7812f,7184.375f,7882.5156f,8404.406f,8068.4844f,5441.3125f,
    718.53125f,1437.0625f,2155.5938f,2874.125f,3592.6562f,4311.1875f,5029.7188f,5748.25f,6466.7812f,7184.375f,7882.5156f,8404.406f,8068.4844f,5441.3125f,
    648.96875f,1297.9375f,1946.9062f,2595.875f,3244.8438f,3893.8125f,4542.7812f,5191.75f,5840.7188f,6488.75f,7117.328f,7579.0312f,7252.7656f,4871.5f,
    421.73438f,843.46875f,1265.2031f,1686.9375f,2108.6719f,2530.4062f,2952.1406f,3373.875f,3795.6094f,4216.4062f,4622.4375f,4911.2812f,4677.9375f,3123.1875f
    )

    /* u[cp] = X * ( S*l1 + u[cp]*l2 + u1[cp]*l3) */

    val constant0 = 2.0f
    val constant1 = 4.0f
    val constant2 = 3.0f
    val Xvalue = 0.25f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr(0).length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr(0).length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(fun(x => mult(x,Xvalue))) o MapSeq(addTuple)) $ Zip(
            ReduceSeq(mult, constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weightsMiddle),
            MapSeq(addTuple) $ Zip(ReduceSeq(mult, constant0) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0, tuple1._1))) $ Zip(Get(m, 1), weights),
                                   ReduceSeq(mult, constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0, tuple1._1))) $ Zip(Get(m, 1), weightsMiddle))

          )
        }))) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))
      })

    // until there's a better way ...
    var input = stencilarr
    var inputArrCopy = stencilarrCopy
    var outputX = Array[Float]()

    for(x <- 1 to StencilUtilities.iter) {
      val (output: Array[Float], runtime) = Execute(2,2)(lambdaNeigh, input, inputArrCopy, StencilUtilities.weights, StencilUtilities.weightsMiddle)
      input = inputArrCopy
      inputArrCopy = StencilUtilities.createFakePaddingFloat2D(output.sliding(asymDimX,asymDimX).toArray,0.0f)

      outputX = output
    }

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, outputX, asymDimX)
    assertArrayEquals(compareData, outputX, StencilUtilities.stencilDelta)

  }

  @Test
  def testStencil3DSimple(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    /* u[cp] = S */

    val localDim = 3
    val dim = localDim + 2
    val input = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (j+1).toFloat }
    val input3D = StencilUtilities.createFakePaddingFloat3D(input, 0.0f, localDim, localDim)

    val compareData = Array(
    4.0f,5.0f,4.0f,
    8.0f,10.0f,8.0f,
    8.0f,11.0f,8.0f,
    5.0f,6.0f,5.0f,
    10.0f,12.0f,10.0f,
    11.0f,14.0f,11.0f,
    4.0f,5.0f,4.0f,
    8.0f,10.0f,8.0f,
    8.0f,11.0f,8.0f
    )

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim), dim),
      ArrayTypeWSWC(Float, StencilUtilities.slidesize*StencilUtilities.slidesize*StencilUtilities.slidesize),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeq(id)) o
              ReduceSeqUnroll(\((acc, next) =>
                multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, weights)
          })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val source = Compile(lambdaNeigh)
    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,lambdaNeigh, input3D, StencilUtilities.weights3D.flatten.flatten)

    if(StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput3Das1D(input3D, output)
      StencilUtilities.print3DArray(input3D)
      StencilUtilities.print1DArrayAs3DArray(output, localDim, localDim, localDim)
    }
    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

 @Test
  def testStencil3DSwap(): Unit = {
   assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    /* u[cp] = u[cp] + u1[cp] */

    val localDim = 3
    val dim = localDim + 2

    val input = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (i+j+k+1).toFloat }
    val input2 = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (2*(i+j+k+1)).toFloat }

    val input3D = StencilUtilities.createFakePaddingFloat3D(input, 0.0f, localDim, localDim)
    val input3D2 = StencilUtilities.createFakePaddingFloat3D(input2, 0.0f, localDim, localDim)

    val compareData = Array(
      3.0f,6.0f,9.0f,
      6.0f,9.0f,12.0f,
      9.0f,12.0f,15.0f,
      6.0f,9.0f,12.0f,
      9.0f,12.0f,15.0f,
      12.0f,15.0f,18.0f,
      9.0f,12.0f,15.0f,
      12.0f,15.0f,18.0f,
      15.0f,18.0f,21.0f
   )

    val lambdaNeigh2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim), dim),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim), dim),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, weightsMiddle) => {
        MapGlb((fun(m => {
            toGlobal(MapSeq(addTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip( Join() $ Get(m, 0), Join() $ weightsMiddle),
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip( Join() $ Get(m, 1), Join() $ weightsMiddle))
        }))
        ) $ Zip((Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))
      })

    val source = Compile(lambdaNeigh2)
    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,lambdaNeigh2, input3D, input3D2, StencilUtilities.weightsMiddle3D)

    if(StencilUtilities.printOutput)
    {
      StencilUtilities.printOriginalAndOutput3Das1D(input3D, output)
      StencilUtilities.print3DArray(input3D)
      StencilUtilities.print1DArrayAs3DArray(output, localDim, localDim, localDim)
    }
    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def twoGridSwapWith3weightsCalculations3D(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val compareData = Array(
    8.75f,15.5f,22.25f,24.0f,
    15.5f,24.25f,32.0f,33.75f,
    22.25f,32.0f,39.75f,40.5f,
    24.0f,33.75f,40.5f,39.25f,
    15.5f,24.25f,32.0f,33.75f,
    24.25f,35.0f,43.75f,45.5f,
    32.0f,43.75f,52.5f,53.25f,
    33.75f,45.5f,53.25f,52.0f,
    22.25f,32.0f,39.75f,40.5f,
    32.0f,43.75f,52.5f,53.25f,
    39.75f,52.5f,61.25f,61.0f,
    40.5f,53.25f,61.0f,58.75f,
    24.0f,33.75f,40.5f,39.25f,
    33.75f,45.5f,53.25f,52.0f,
    40.5f,53.25f,61.0f,58.75f,
    39.25f,52.0f,58.75f,54.5f
    )

    /* u[cp] = X * ( S*l1 + u[cp]*l2 + u1[cp]*l3) */

    val localDim = 4
    val dim = localDim + 2

    val input = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (i+j+k+1).toFloat }
    val input2 = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (2*(i+j+k+1)).toFloat }

    val input3D = StencilUtilities.createFakePaddingFloat3D(input, 0.0f, localDim, localDim)
    val input3D2 = StencilUtilities.createFakePaddingFloat3D(input2, 0.0f, localDim, localDim)

    val constant0 = 2.0f
    val constant1 = 4.0f
    val constant2 = 3.0f
    val Xvalue = 0.25f


    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim), dim),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim), dim),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun(m => {
            toGlobal(MapSeq(fun(x => mult(x,Xvalue))) o MapSeq(addTuple)) $ Zip(
            ReduceSeq(mult, constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(m, 0), Join() $ weightsMiddle),
            MapSeq(addTuple) $ Zip(ReduceSeq(mult, constant0) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0, tuple1._1))) $ Zip(Join() $ Get(m, 1), Join() $ weights),
            ReduceSeq(mult, constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0, tuple1._1))) $ Zip(Join() $ Get(m, 1), Join() $ weightsMiddle))
          )
        }))
        ) $ Zip((Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)))
      })

    val source = Compile(lambdaNeigh)
    val (output: Array[Float], runtime) = Execute(dim,dim,dim,dim,dim,dim, (true,true))(source,lambdaNeigh, input3D, input3D2, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)

    if(StencilUtilities.printOutput)
    {
      StencilUtilities.printOriginalAndOutput3D(input3D, output)
    }
    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
  }

}

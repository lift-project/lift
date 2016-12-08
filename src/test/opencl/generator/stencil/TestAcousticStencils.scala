package opencl.generator

import java.io.FileInputStream
import java.util.Scanner

import scala.util.Random
import ir.printer.DotPrinter
import apart.arithmetic.{ArithExpr, Cst, Lookup, SizeVar}
import benchmarks.Stencil1D
import ir.{ArrayType, TupleType}
import ir.ast._
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}
import opencl.executor.Utils
import scala.language.implicitConversions


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

  /* globals */
  val slidesize = 3;
  val slidestep = 1;

  val dim = 8;
  val size = dim - 2;
  val delta = 0.2f

  /* helper functions */
  def print2DArray[T](input: Array[Array[T]]) = {
    println(input.deep.mkString("\n"))
  }

  def print1DArray[T](input: Array[T]) = {
    println(input.mkString(","))
  }

  def print1DArrayAs2DArray[T](input: Array[T], dim: Int) {
    var count = 1
    println()
    input.foreach(x => if (count % dim > 0) {
      print(x + " ");
      count += 1
    } else {
      println(x + " ");
      count += 1
    })
    println()
  }

  def printOriginalAndOutput[T](original: Array[Array[T]], output: Array[T], N: Int): Unit = {
    println("ORIGINAL:")
    print2DArray(original)
    println("*********************")
    println("OUTPUT:")
    print1DArrayAs2DArray(output, N)
  }

  implicit def bool2int(b:Boolean) = if (b) 1 else 0
  def intBang(i:Int) = if (i==1) 0 else 1

  val invertInt = UserFun("invertInt", Array("x"), "{ return x ? 0 : 1; }", Seq(Int), Int)

  val idIF = UserFun("idIF", "x", "{ return (float)x; }", Int, Float)

  /* only one (value) layer of padding around 2D matrix */

  /**** Why doesn't this work?? !!!! *****/
/*
  def createFakePadding[T](input: Array[Array[T]], padSize: Int, padValue: T): Array[Array[T]] = {

    val padLR = Array.fill(1)(padValue)
    val toppad = Array.fill(1)(Array.fill(padSize)(padValue))
    val output = input.map(i => padLR ++ i ++ padLR)
    toppad ++ output ++ toppad

  }
*/

  def createFakePaddingFloat(input: Array[Array[Float]], padSize: Int, padValue: Float): Array[Array[Float]] = {

    val padLR = Array.fill(1)(padValue)
    val toppad = Array.fill(1)(Array.fill(padSize)(padValue))
    val output = input.map(i => padLR ++ i ++ padLR)
    toppad ++ output ++ toppad

  }

  def createFakePaddingInt(input: Array[Array[Int]], padSize: Int, padValue: Int): Array[Array[Int]] = {

    val padLR = Array.fill(1)(padValue)
    val toppad = Array.fill(1)(Array.fill(padSize)(padValue))
    val output = input.map(i => padLR ++ i ++ padLR)
    toppad ++ output ++ toppad

  }

  /* create mask of 0s and 1s at the boundary for a 2D Matrix */
  def createMask(input: Array[Array[Float]], msize: Int, maskValue: Int): Array[Array[Int]] = {

    val mask =input.flatten.zipWithIndex.map(i => !( (i._2%msize != 0) && i._2%msize!=(msize-1)  && i._2>(msize-1) && i._2<(msize*msize)-msize) )
    mask.map(i => i*1).sliding(msize,msize).toArray

  }

  def createData() ={

    val filling = Array.tabulate(size) { i => i + 1 }
    // val filling = Array.fill(size)(1)
    val padLR = Array.fill(1)(0)
    val toppad = Array.fill(1)(Array.fill(dim)(0.0f))
    val line = padLR ++ filling ++ padLR

    val mat = Array.fill(size)(line.map(_.toFloat))
    toppad ++ mat ++ toppad
  }

  /* these helper functions do not work, but it would be nice if they did! */

  def map2D(f: Lambda1): FunDecl = {
    fun(x => x :>> MapSeq(fun(row => row :>> MapSeq(f))))
  }

  def reduce2D(f: Lambda2, init: Expr): FunDecl = {
    fun(x => x :>> MapSeq(fun(row => row :>> ReduceSeq(f,init))) :>> Transpose()
      :>> MapSeq(fun(n => n :>> ReduceSeq(f,init))) :>> Join())
  }
  val zip2D = fun((A, B) =>
    Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(A, B)
  )

  /* shared data */
  val weights9 = Array.fill(9)(1).map(_.toFloat)

  val weights = Array(
  Array(  0.0f, 1.0f, 0.0f),
   Array( 1.0f, 0.0f, 1.0f),
   Array( 0.0f, 1.0f, 0.0f))

  val weightsMiddle = Array(
   Array( 0.0f, 0.0f, 0.0f),
   Array( 0.0f, 1.0f, 0.0f),
    Array(0.0f, 0.0f, 0.0f))

  val stencilarr = createData()
  val stencilarrC = createData()
  val stencilarrCopy = stencilarrC.map(x => x.map(y => y*2.0f))

  /* testing comparison data */

  @Ignore
  @Test
  def testStencil2DSimple(): Unit =
  {

    /* u[cp] = S */

    // JUST CREATES THE GROUPS !!
/*     val lambda = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (domain) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours =>
            MapSeqOrMapSeqUnroll(MapSeqOrMapSeqUnroll(id)) $ neighbours
          ))
        ) o Slide2D(slidesize, slidestep) $ domain
      }
    )*/

    val lambda = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      (mat) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(slidesize, slidestep) $ mat
      })

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      ArrayType(Float, weights.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => { // Where does "acc" come from ? !!!!
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(slidesize, slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, weights)

    printOriginalAndOutput(stencilarr,output,size)

    //println(Compile(lambda))

  }

  @Test
  def testStencil2DSimpleTimesConstant(): Unit =
  {
    val constant = 2.0f

    /* cp => index
       S => stencil sum
      u[cp] = S*l2  */

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      ArrayType(Float, weights.length),
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
        ) o Slide2D(slidesize, slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, weights)

    printOriginalAndOutput(stencilarr,output,size)

  }


  @Test
  def testStencil2DSimpleTimesConstantPlusSelf(): Unit =
  {

    /* u[cp] = S*l2 + u1[cp] */

    val add2 = UserFun("add2", Array("x", "y"), "{ return y+y; }", Seq(Float, Float), Float).
      setScalaFun( xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float] )

    val constant = 3.0f

    val timesConstantPlusSelf = UserFun("timesConstantPlusSelf", Array("x", "y"), "{ return x + x; }", Seq(Float, Float), Float)

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(Float, weights.length),
      (mat, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              MapSeqUnroll(fun(x => add(x,x))) o
              MapSeqUnroll(fun(x => mult(x,constant))) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }),0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(slidesize, slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, weights)

    printOriginalAndOutput(stencilarr,output,size)

  }

  @Ignore // KEEP THIS
  @Test
  def testStencil2DSimpleTimesConstantPlusSelfPlusPrevious(): Unit =  // Nothing here, just aborted ideas
  {

    val computeTwoStencils = Join() o (fun(tuple => {
      val left = Get(tuple, 0)
      val right = Get(tuple, 1)
      // only possible because reduce returns array of size 1!
      Zip(
        ReduceSeq(add, 0.0f) o Join() $ left,
        ReduceSeq(add, 0.0f) o Join() $ right)
    }))

    val dataBeforeCompute = fun(inputTile => Join() o computeTwoStencils o Split(dim) $ Zip(
      Join() o Slide2D(3,1) $ inputTile,
      Join() o Slide2D(3,1) $ inputTile
    ))

    val lambdaTwoStencil = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
    (inp) => {
      MapGlb(1)(
        MapGlb(0)(
          toGlobal(MapSeqUnroll(id)) o
          toGlobal(MapSeq(addTuple)) o dataBeforeCompute)) $ inp
    })

    /*
      val f2 = fun(
        ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
        ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
        (matrix1, matrix2) =>
          MapGlb(1)(
            MapGlb(0)(fun((r) => {
              MapSeq(id) $ Get(r, 0)
            }      ))) $ Zip(matrix1, matrix2)
      )
    */

    /* Idea:
        - Pass in neighborhood (non-zipped)
        - Save the neighborhood in a function
        - use "scala" primitives to "fake" the data how you want it
        - reduce once and save
        - reduce twice and save
        - combine two reductions
     */

  }

  @Ignore // KEEP THIS
  @Test
  def testStencil2DSimpleAccessTwoWeightsBAD(): Unit =
  {
    /*
        Attempt to pull out using two stencils using zip2D / map2D / reduce2D
        ... which doesn't work
    */

    val constant = 3.0f

    val neighbourhoodFun =  fun( (nbh, w1, w2) => {
      val nbhw1 = zip2D(nbh, w1) :>> map2D (multTuple) :>> reduce2D(add,0.0f)
      val nbhw2 = zip2D(nbh, w2) :>> map2D (multTuple) :>> reduce2D(add,0.0f)

      Zip(nbhw1, nbhw2) :>> Map(addTuple) :>> toGlobal(MapSeqUnroll(id))
    })

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat, weights, weightsMiddle) => {
        MapGlb(1)(
          MapGlb(0)(fun(n =>
            neighbourhoodFun(n, weights, weightsMiddle)
          ))) o Slide2D(slidesize, slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, weights, weightsMiddle)
    printOriginalAndOutput(stencilarr,output,size)
  }

  @Ignore
  @Test
  def testStencil2DSimpleAccessTwoWeightsMultConstOne(): Unit =
  {

    val compareData = Array(10.0f,20.0f,30.0f,40.0f,50.0f,39.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,57.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,57.0f,
   13.0f,26.0f,39.0f,52.0f,65.0f,57.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,57.0f,
    10.0f,20.0f,30.0f,40.0f,50.0f,39.0f )

    val constant = 3.0f

    /* u[cp] = S*l2 + u[cp] */

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, SizeVar("M")), SizeVar("N")),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat, weights, weightsMiddle) => {
        MapGlb(1)(
          MapGlb(0)(fun(n => {
            toGlobal(MapSeq(addTuple)) $ Zip(
               ReduceSeq(mult, constant) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(n, weights),
               ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(n, weightsMiddle)
            )
          }  ))) o Slide2D(slidesize, slidestep) $ mat
      })

    Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, weights, weightsMiddle)
    printOriginalAndOutput(stencilarr,output,size)

    assertArrayEquals(compareData, output, delta)

  }

  @Ignore
  @Test
  def testStencil2DTwoGridsSwap(): Unit =
  {

    val compareData = Array( 3.0f,6.0f,9.0f,12.0f,15.0f,18.0f,
    3.0f,6.0f,9.0f,12.0f,15.0f,18.0f,
    3.0f,6.0f,9.0f,12.0f,15.0f,18.0f,
    3.0f,6.0f,9.0f,12.0f,15.0f,18.0f,
    3.0f,6.0f,9.0f,12.0f,15.0f,18.0f,
    3.0f,6.0f,9.0f,12.0f,15.0f,18.0f )

    /* u[cp] = u1[cp] + u[cp] */

    val constant = 3.0f

    val f = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (matrix1, matrix2, wghts1, wghts2) => MapGlb(fun((m) =>
      MapSeq(fun(n =>  MapSeq(id) $ n ))
      $  Get(m,0)
      )) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ matrix1)),  (Join() $ (Slide2D(slidesize,slidestep) $ matrix2)))
    )

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(addTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weightsMiddle),
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,1), weightsMiddle)
          )
       }  ))) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)),  (Join() $ (Slide2D(slidesize,slidestep) $ mat2)))

      })

//    Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrCopy, weights, weightsMiddle)
//    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(f, stencilarr, stencilarrCopy)
    printOriginalAndOutput(stencilarr,output,size)
    println("COPY:::")
    print2DArray(stencilarrCopy)

    assertArrayEquals(compareData, output, delta)

  }

  @Ignore
  @Test
  def twoGridSwapWith3DifferentWeightsAndConstants(): Unit =
  {

    val compareData = Array( 22.0f,44.0f,66.0f,88.0f,110.0f,90.0f,
    28.0f,56.0f,84.0f,112.0f,140.0f,126.0f,
    28.0f,56.0f,84.0f,112.0f,140.0f,126.0f,
    28.0f,56.0f,84.0f,112.0f,140.0f,126.0f,
    28.0f,56.0f,84.0f,112.0f,140.0f,126.0f,
    22.0f,44.0f,66.0f,88.0f,110.0f,90.0f )

    /* u[cp] = S*l1 + u[cp]*l2 */

    val constant1 = 4.0f
    val constant2 = 3.0f

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(addTuple)) $ Zip(
            ReduceSeq(mult,constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weightsMiddle),
            ReduceSeq(mult,constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,1), weights)
          )
        }  ))) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)),  (Join() $ (Slide2D(slidesize,slidestep) $ mat2)))

      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrCopy, weights, weightsMiddle)
    printOriginalAndOutput(stencilarr,output,size)
    assertArrayEquals(compareData, output, delta)

  }

  @Ignore
  @Test
  def twoGridSwapWith3DifferentWeightsAndConstantsPlusSelf(): Unit =
  {

    val compareData = Array( 11.0f,22.0f,33.0f,44.0f,55.0f,52.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,64.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,64.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,64.0f,
    13.0f,26.0f,39.0f,52.0f,65.0f,64.0f,
    11.0f,22.0f,33.0f,44.0f,55.0f,52.0f  )

    /* u[cp] = X * ( S*l1 + u[cp]*l2 + u1[cp]*l3) */

    val constant0 = 2.0f
    val constant1 = 4.0f
    val constant2 = 3.0f
    val X =  0.5f

   /**** Why doesn't this work? !!!! ****/
    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal( ReduceSeq(mult, X) o (addTuple)) $ Zip(
            ReduceSeq(mult,constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weightsMiddle),
            ReduceSeq(mult,constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(Get(Get(tuple1,0),0), Get(Get(tuple1,0),0)))) $ Zip(Zip(Get(m,1), weights),Zip(Get(m,1),weightsMiddle))
          )
        }  ))) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)),  (Join() $ (Slide2D(slidesize,slidestep) $ mat2)))
      })

    val lambdaNeigh2= fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o ReduceSeq(mult, X) o MapSeq(addTuple)) $ Zip(
            ReduceSeq(mult,constant1) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weightsMiddle),
            MapSeq(addTuple) $ Zip(ReduceSeq(mult,constant0) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0,Get(Get(tuple1,1),0)))) $ Zip(Get(m,1),Zip(weights,weightsMiddle)),
                                   ReduceSeq(mult,constant2) o ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\((tuple1) => Zip(tuple1._0,Get(Get(tuple1,1),1)))) $ Zip(Get(m,1),Zip(weights,weightsMiddle)))
          )
        }  ))) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)),  (Join() $ (Slide2D(slidesize,slidestep) $ mat2)))
      })


    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh2, stencilarr, stencilarrCopy, weights, weightsMiddle)
    printOriginalAndOutput(stencilarr,output,size)
    assertArrayEquals(compareData, output, delta)

  }

  @Ignore
  @Test
  def testSimpleOneGridWithBoundaryCheckMask(): Unit =
  {

    val addZero = UserFun("addTuple", "x", "{return x._0;}", TupleType(Float, Float), Float)

    val compareData = Array( 15.0f,30.0f,45.0f,60.0f,75.0f,55.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      15.0f,30.0f,45.0f,60.0f,75.0f,55.0f)

    val constantOriginal = 2.0f
    val constantBorder = 5.0f


    val initMat = Array.tabulate(size,size){ (i,j) => (i+j+1).toFloat }
    val matMat = createFakePaddingFloat(initMat,size+2,0)
    val maskArray = createMask(initMat,size,0).map(i => i.map(j => j.toString.toArray))
    val mask = createMask(initMat,size,0).map(i => i.map(j => j.toString.toArray))
    val mask2 = mask.map(i => i.map(j => j.map(k => k.toInt-48)))

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1),size), size),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x,constantBorder))) o MapSeq(idIF) $ Get(m,1),MapSeq(fun(x => mult(x,constantOriginal))) o MapSeq(idIF) o MapSeq(invertInt) $ Get(m,1))
            //MapSeq(id) o MapSeq(add) $ Zip(Map( fun(x => mult(x,constantBorder))) o MapSeq(idIF) $ Get(m,1), Map(fun(x => mult(x,constantOriginal))) o MapSeq(idIF) o MapSeq(invertInt) $ Get(m,1))
          )
          }))
          ) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)),  Join() $ mask1)
      })

    val lambdaNeighCompare = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1),size), size),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(addZero)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weights),
            MapSeq(idIF) $ Get(m,1)
          )
        }))
        ) $ Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)),  Join() $ mask1)
      })



    //    Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, mask2, weights)
    val (output2: Array[Float], runtime2) = Execute(stencilarr.length, stencilarr.length)(lambdaNeighCompare, stencilarr, mask2, weights)
    printOriginalAndOutput(stencilarr,output,size)
    print1DArrayAs2DArray(output2,size)

    assertArrayEquals(compareData, output, delta)

  }

  @Ignore
  @Test
  def testSimplTwoGridsWithBoundaryCheckMask(): Unit = {
  }

/////////////////// JUNKYARD ///////////////////
@Ignore
  @Test
  def testScalaData(): Unit =
  {
    stencilarr.transpose.map( x => x.sliding(3,1).toArray).sliding(3,1).toArray

  }


  @Ignore
  @Test
  def testZip2DEffects(): Unit =
  {

    val function = fun(
      ArrayType(ArrayType(Float, dim), dim),
      ArrayType(ArrayType(Float, dim), dim),
      (A, B) => {
        MapGlb(fun(t => {
          MapSeq(fun(x => add.apply(x, Get(t, 1)))) $ Get(t, 0)
        }))
        } $ Zip(A, B)
    )

    val f = fun(
      ArrayType(TupleType(Float, Float), dim),
      ArrayType(TupleType(Float, Float), dim),
      (left, right) =>
        MapGlb(1)(
          MapGlb(0)(fun(zippedMat => {
            val currentStencil = zippedMat._0
            val futureStencil = zippedMat._1
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(add,futureStencil) $ zippedMat
          }))) $ Zip(left, right)
    )

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, dim), dim),
      ArrayType(ArrayType(Float, dim), dim),
      (mat1, mat2) => {
        MapGlb(1)(
          MapGlb(0)(fun(zippedMat => {
            val currentStencil = zippedMat._0
            val futureStencil = zippedMat._1
            toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(add,futureStencil) $ zippedMat
          }))) $ Zip(mat1,mat2)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarrCopy.length)(function, stencilarr, stencilarrCopy)

    print2DArray(stencilarr)
    println("*********************")
    print1DArrayAs2DArray(output,size)
  }

  @Ignore
  @Test
  def testZip1DEffects(): Unit =
  {

    val filling1 = Array.tabulate(size){ i => i+1}
    val filling2 = Array.tabulate(size){ i => i*2+1}

    val gold = (filling1,filling2).zipped.map(_+_)

//    val test = (filling1,filling2).zipped(0)
    val test = gold(0)
    println("test: " +test)
    gold.foreach(println)

//    gold.foreach(x => println("0: " + x._0 +  " 1: " + x._1))

/*    val function = fun(
      ArrayType(Float, dim),
      ArrayType(Float, dim),
      (A, B) =>
        MapGlb(fun(t => {
          MapSeq(add)
        })) $ Zip(A, B)
    )
*/
    print1DArray(filling1)
    print1DArray(filling2)
   // val (output: Array[Float], runtime) = Execute(filling1.length, filling2.length)(function, filling1, filling2)

    println("*********************")
    //print1DArray(output)
  }

  def scalaSlide2D(input: Array[Array[Float]],
                   size1: Int, step1: Int,
                   size2: Int, step2: Int) = {
    val firstSlide = input.sliding(size1, step1).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(size2, step2).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    neighbours
  }

  def leggySlide2D(input: Array[Array[Float]],
                   size1: Int, step1: Int,
                   size2: Int, step2: Int) =
  {

    val first = input.drop(1).dropRight(1).sliding(3, 1).toArray
    val second = input.transpose.drop(1).dropRight(1).sliding(3, 1).toArray

    val firsec = first(0) ++ second(0)
    Array.fill(1)(Array.fill(1)(firsec))
  }

  @Ignore
  @Test
  def testLeggyGroup(): Unit = {

    val data2D = Array.tabulate(3, 3) { (i, j) => 3 * i + j }.map(x => x.map(_.toFloat))

    println(leggySlide2D(data2D, 3,1,3,1).deep.mkString(","))
    println("*******")
    println(scalaSlide2D(data2D, 3, 1, 3, 1).deep.mkString(","))
  }

}


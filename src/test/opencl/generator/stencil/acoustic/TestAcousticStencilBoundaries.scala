package opencl.generator.stencil.acoustic

import ir.ast._
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.language.implicitConversions

object BoundaryUtilities
{

  /* helper functions */

  /* to get around casting ints to strings to chars to ints (in order to wrap ints in Arrays quickly) ... */
  def parseIntAsCharAsInt(inp: Int): Int = { // let's do some voodoo magic
     val f = inp.toString.toArray.map(i => i.toInt)
     f(0)
  }

  implicit def bool2int(b:Boolean) = if (b) 1 else 0
  def intBang(i:Int) = if (i==1) 0 else 1

  val invertInt = UserFun("invertInt", Array("x"), "{ return x ? 0 : 1; }", Seq(Int), Int)
  val getFirstTuple = UserFun("getFirstTuple", "x", "{return x._0;}", TupleType(Float, Float), Float) // dud helper
  val getSecondTuple = UserFun("getSecondTuple", "x", "{return x._1;}", TupleType(Float, Float), Float) // dud helper
  val idIF = UserFun("idIF", "x", "{ return (float)x; }", Int, Float)

  /* create mask of 0s and 1s at the boundary for a 2D Matrix */
  def createMask(input: Array[Array[Float]], msize: Int, maskValue: Int): Array[Array[Int]] = {

    val mask =input.flatten.zipWithIndex.map(i => !( (i._2%msize != 0) && i._2%msize!=(msize-1)  && i._2>(msize-1) && i._2<(msize*msize)-msize) )
    mask.map(i => i*1).sliding(msize,msize).toArray

  }

  /* should create asym version! */
  def createMaskData2D(size: Int) = {

    val initMat = Array.tabulate(size,size){ (i,j) => (i+j+1).toFloat }
    val matMat = StencilUtilities.createFakePaddingFloat2D(initMat,0.0f)
    val maskArray = createMask(initMat,size,0).map(i => i.map(j => j.toString.toArray))
    val mask = createMask(initMat,size,0).map(i => i.map(j => j.toString.toArray))
    mask.map(i => i.map(j => j.map(k => k.toInt-parseIntAsCharAsInt(0))))
  }



  def maskValue(m: Expr, c1: Float, c2: Float): Expr = {
    MapSeq(add) $ Zip(MapSeq(fun(x => mult(x,c1))) o MapSeq(idIF) $ Get(m,1),MapSeq(fun(x => mult(x,c2))) o MapSeq(idIF) o MapSeq(invertInt) $ Get(m,1))
  }

}

object TestAcousticStencilBoundaries {
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

class TestAcousticStencilBoundaries {


  val stencilarr = StencilUtilities.createDataFloat(StencilUtilities.stencilSize,StencilUtilities.stencilSize)
  val stencilarrsame = StencilUtilities.createDataFloat(StencilUtilities.stencilSize,StencilUtilities.stencilSize)
  val stencilarrCopy = stencilarr.map(x => x.map(y => y * 2.0f))


  /* globals */
  val mask = BoundaryUtilities.createMaskData2D(StencilUtilities.stencilSize)

  @Test
  def testSimpleOneGridWithBoundaryCheckMask(): Unit =
  {

    /* u[cp] = S*( boundary ? constantBorder : constantOriginal) */

    val compareData = Array( 15.0f,30.0f,45.0f,60.0f,75.0f,55.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      20.0f,16.0f,24.0f,32.0f,40.0f,85.0f,
      15.0f,30.0f,45.0f,60.0f,75.0f,55.0f)

    val constantOriginal = 2.0f
    val constantBorder = 5.0f

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1),StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x,constantBorder))) o MapSeq(BoundaryUtilities.idIF) $ Get(m,1),MapSeq(fun(x => mult(x,constantOriginal))) o MapSeq(BoundaryUtilities.idIF) o MapSeq(BoundaryUtilities.invertInt) $ Get(m,1))
            //MapSeq(id) o MapSeq(add) $ Zip(Map( fun(x => mult(x,constantBorder))) o MapSeq(idIF) $ Get(m,1), Map(fun(x => mult(x,constantOriginal))) o MapSeq(idIF) o MapSeq(invertInt) $ Get(m,1))
          )
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)),  Join() $ mask1)
      })

    val lambdaNeighCompare = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1),StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(BoundaryUtilities.getFirstTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weights),
            MapSeq(BoundaryUtilities.idIF) $ Get(m,1)
          )
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)),  Join() $ mask1)
      })



    //    Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, mask, StencilUtilities.weights)
    val (output2: Array[Float], runtime2) = Execute(stencilarr.length, stencilarr.length)(lambdaNeighCompare, stencilarr, mask, StencilUtilities.weights)
    if(StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput(stencilarr, output, StencilUtilities.stencilSize)
      StencilUtilities.print1DArrayAs2DArray(output2, StencilUtilities.stencilSize)
    }

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testSimpleGridWithTwoBoundaryCheckMask(): Unit =
  {
    /* u[cp] = ( boundary ? constantBorder2 : constantOriginal2) + S*( boundary ? constantBorder : constantOriginal) */

    val compareData = Array(
    10.0f,16.0f,22.0f,28.0f,34.0f,26.0f,
    12.0f,10.0f,14.0f,18.0f,22.0f,38.0f,
    12.0f,10.0f,14.0f,18.0f,22.0f,38.0f,
    12.0f,10.0f,14.0f,18.0f,22.0f,38.0f,
    12.0f,10.0f,14.0f,18.0f,22.0f,38.0f,
    10.0f,16.0f,22.0f,28.0f,34.0f,26.0f
    )

    val constantOriginal = Array(1.0f,2.0f)
    val constantBorder = Array(2.0f,4.0f)


    /*
      1) Use borders with Arrays with what works already
      2) get same value for both
      3) update to show case above
      4) try to pull out into separate function
     */

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1),StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          val maskedValConst = BoundaryUtilities.maskValue(m,constantBorder(1), constantOriginal(1))
          val maskedValGrid = BoundaryUtilities.maskValue(m,constantBorder(0), constantOriginal(0))
          toGlobal(MapSeq(id) o MapSeq(addTuple)) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m,0), weights),
            MapSeq(id) $ maskedValGrid
          ),
            MapSeq(id) $ maskedValConst)
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)),  Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, mask, StencilUtilities.weights)

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testTwoGridsThreeCalculationsWithMask(): Unit =
  {
    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u1[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val compareData = Array(
    128.0f,256.0f,384.0f,512.0f,640.0f,656.0f,
    144.0f,72.0f,108.0f,144.0f,180.0f,752.0f,
    144.0f,72.0f,108.0f,144.0f,180.0f,752.0f,
    144.0f,72.0f,108.0f,144.0f,180.0f,752.0f,
    144.0f,72.0f,108.0f,144.0f,180.0f,752.0f,
    128.0f,256.0f,384.0f,512.0f,640.0f,656.0f
    )

    val constantOriginal = Array(1.0f,2.0f,3.0f, 4.0f)
    val constantBorder = Array(2.0f,4.0f,6.0f, 8.0f)

    // why doesn't this work @ end?? MapSeq(fun(x => mult(x,maskedValMult))) o

    val lambdaNeigh = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1),StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(m,constantBorder(3), constantOriginal(3))
          val maskedValConstSec = BoundaryUtilities.maskValue(m,constantBorder(2), constantOriginal(2))
          val maskedValConstOrg = BoundaryUtilities.maskValue(m,constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(m,constantBorder(0), constantOriginal(0))
          val orgMat = Get(Get(m,0),0)
          val secMat = Get(Get(m,0),1)

          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip( MapSeq(addTuple) $ Zip( MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m,0),0), weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m,0),0), weights),
              MapSeq(id) $ maskedValStencil
            ))
            ,
              (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq( ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m,0),1), weightsMiddle),
              MapSeq(id) $ maskedValConstSec)
          ),
            maskedValMult)
        }))
        ) $ Zip(Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)),  (Join() $ (Slide2D(StencilUtilities.slidesize,StencilUtilities.slidestep) $ mat2))), Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrsame, mask, StencilUtilities.weights, StencilUtilities.weightsMiddle)

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

}


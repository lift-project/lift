package opencl.generator.stencil.acoustic

import ir.ast._
import ir.{ArrayTypeWSWC, TupleType}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._
import rewriting.SimplifyAndFuse

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

  val invertIntToFloat = UserFun("invertInt", Array("x"), "{ return x ? 0.0 : 1.0; }", Seq(Int), Float)
  val invertFloat = UserFun("invertFloat", Array("x"), "{ return ((x-1.0) == 0.0) ? 0.0 : 1.0; }", Seq(Float), Float)
  val convertFloat = UserFun("convertFloat", Array("x"), "{ return ((x-1.0) == 0.0) ? 1.0 : 0.0; }", Seq(Float), Float)
  val getFirstTuple = UserFun("getFirstTuple", "x", "{return x._0;}", TupleType(Float, Float), Float) // dud helper
  val getSecondTuple = UserFun("getSecondTuple", "x", "{return x._1;}", TupleType(Float, Float), Float) // dud helper
  val idIF = UserFun("idIF", "x", "{ return (float)(x*1.0); }", Int, Float)

  val invertBoundaryCountToFloat = UserFun("invertInt", Array("x"), "{ return (x<6) ? 0.0 : 1.0; }", Seq(Int), Float)
  val convertBoundaryCountToFloat = UserFun("convertInt", Array("x"), "{ return (x<6) ? 1.0 : 0.0; }", Seq(Int), Float)

  /* create mask of 0s and 1s at the boundary for a 2D Matrix */
  def createMask(input: Array[Array[Float]], msizeX: Int, msizeY: Int, maskValue: Int): Array[Array[Int]] = {

    val mask =input.flatten.zipWithIndex.map(i => !( (i._2%msizeX != 0) && i._2%msizeX!=(msizeX-1)  && i._2>msizeX && i._2<(msizeX*msizeY)-msizeX) )
    mask.map(i => i*1).sliding(msizeX,msizeX).toArray

  }

  def createMaskData2D(size: Int)  =
  {
    createMaskDataAsym2D(size,size)
  }

  def createMaskDataAsym2D(sizeX: Int, sizeY: Int) =
  {
    val initMat = Array.tabulate(sizeX,sizeY){ (i,j) => (i+j+1).toFloat }
    val mask = createMask(initMat,sizeX,sizeY,0).map(i => i.map(j => j.toString.toArray))
    mask.map(i => i.map(j => j.map(k => k.toInt-parseIntAsCharAsInt(0))))
  }

  def createMaskDataAsym3D(sizeX: Int, sizeY: Int, sizeZ: Int): Array[Array[Array[Array[Int]]]] = {

    val pad2D = createMaskDataAsym2D(sizeX, sizeY)
    val one2D = Array.fill(sizeY, sizeX)(Array(1))
    val addArr = Array.fill(sizeZ-2)(pad2D)

    one2D +: addArr :+ one2D
  }

  def createMaskDataAsym3DNoArray(sizeX: Int, sizeY: Int, sizeZ: Int) = {

    val initMat = Array.tabulate(sizeX,sizeY){ (i,j) => (i+j+1).toFloat }
    val pad2D = createMask(initMat, sizeX, sizeY,0)
    val one2D = Array(Array.fill(sizeY,sizeX)(1))
    var addArr = Array(pad2D)

    for(i <- 1 to sizeZ-3) addArr = addArr ++ Array(pad2D)
    one2D ++ addArr ++ one2D
  }

  def createMaskDataWithNumBoundaryPts(sizeX: Int, sizeY: Int, sizeZ: Int) = {

    val indices = Array(4,10,12,14,16,22)
    val initMat = Array.tabulate(sizeX,sizeY){ (i,j) => (i+j+1).toFloat }
    val mask =initMat.flatten.zipWithIndex.map(i => !( (i._2%sizeX != 0) && i._2%sizeX!=(sizeX-1)  && i._2>sizeX && i._2<(sizeX*sizeY)-sizeX) )
    val pad2D = mask.map(i => i*1).sliding(sizeX,sizeX).toArray
    val one2D = Array(Array.fill(sizeY,sizeX)(1))
    var addArr = Array(pad2D)

    for(i <- 1 to sizeZ-3) addArr = addArr ++ Array(pad2D)
    val mask3D = one2D ++ addArr ++ one2D
    val mask3Dinvert = mask3D.map(x => x.map(y => y.map(z => Math.abs(z-1))))
    val data3tiles =mask3Dinvert.map(x => x.map(y => y.sliding(3,1).toArray).sliding(3,1).toArray.map(x => x.transpose)).sliding(3,1).toArray.map(x => x.transpose.map(y => y.transpose))
    data3tiles.map(x => x.map(y => y.map(z => indices map z.flatten.flatten.lift))).map(x => x.map(y => y.map(z=> z.map(k => k.getOrElse(0)).reduceLeft(_+_))))

  }

  def createMaskData3D(size: Int) =
  {
    createMaskDataAsym3D(size,size,size)
  }

  def maskValue(m: Expr, c1: Float, c2: Float): Expr = {
    toPrivate(MapSeq(add)) $ Zip(toPrivate(MapSeq(fun(x => mult(x,c1)))) o toPrivate(MapSeq(idIF))  $ m, toPrivate(MapSeq(fun(x => mult(x,c2)))) o toPrivate(MapSeq(invertIntToFloat)) $ m)
  }

  def maskValueBoundaryPoints(m: Expr, c1: Float, c2: Float): Expr = {
    toPrivate(MapSeq(add)) $ Zip(toPrivate(MapSeq(fun(x => mult(x,c1)))) o toPrivate(MapSeq(convertBoundaryCountToFloat)) $ m, toPrivate(MapSeq(fun(x => mult(x,c2)))) o toPrivate(MapSeq(invertBoundaryCountToFloat)) $ m)
  }

  def maskValueNoArray(m: Expr, c1: Float, c2: Float): Expr = {
    toPrivate(addTuple) $ Tuple(toPrivate(fun(x => mult(x,c1))) o toPrivate(idIF)  $ m, toPrivate(fun(x => mult(x,c2))) o toPrivate(invertIntToFloat) $ m)
  }

  def maskValueNoArrayBoundaryPoints(m: Expr, c1: Float, c2: Float): Expr = {
    toPrivate(addTuple) $ Tuple(toPrivate(fun(x => mult(x,c1))) o toPrivate(convertBoundaryCountToFloat)  $ m, toPrivate(fun(x => mult(x,c2))) o toPrivate(invertBoundaryCountToFloat) $ m)
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

  val localDim = 8

  val stencilarr = StencilUtilities.createDataFloat2D(StencilUtilities.stencilSize, StencilUtilities.stencilSize)
  val stencilarrsame = StencilUtilities.createDataFloat2D(StencilUtilities.stencilSize, StencilUtilities.stencilSize)
  val stencilarrCopy = stencilarr.map(x => x.map(y => y * 2.0f))

  val stencilarr3D = StencilUtilities.createDataFloat3DWithPadding(localDim, localDim, localDim)
  val stencilarrsame3D = StencilUtilities.createDataFloat3DWithPadding(localDim, localDim, localDim)
  val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))

  /* globals */
  val mask = BoundaryUtilities.createMaskData2D(StencilUtilities.stencilSize)
  val mask3D = BoundaryUtilities.createMaskData3D(localDim)

  @Test
  def testSimpleOneGridWithBoundaryCheckMask2D(): Unit = {

    /* u[cp] = S*( boundary ? constantBorder : constantOriginal) */

    val compareData = Array(15.0f, 30.0f, 45.0f, 60.0f, 75.0f, 55.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 85.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 85.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 85.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 85.0f,
      15.0f, 30.0f, 45.0f, 60.0f, 75.0f, 55.0f)

    val constantOriginal = 2.0f
    val constantBorder = 5.0f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x, constantBorder))) o MapSeq(BoundaryUtilities.idIF) $ Get(m, 1), MapSeq(fun(x => mult(x, constantOriginal))) o MapSeq(BoundaryUtilities.invertIntToFloat) $ Get(m, 1))
          )
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), Join() $ mask1)
      })

    val source = Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(source, lambdaNeigh, stencilarr, mask, StencilUtilities.weights)
    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)
    }

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testSimpleOneGridWithBoundaryCheckMaskAsym2D(): Unit = {

    /* u[cp] = S*( boundary ? constantBorder : constantOriginal) */

    val localDimX = 16
    val localDimY = 8

    val stencilarr = StencilUtilities.createDataFloat2D(localDimX, localDimY)
    val mask2D = BoundaryUtilities.createMaskDataAsym2D(localDimX, localDimY)


    val compareData = Array(
      15.0f, 30.0f, 45.0f, 60.0f, 75.0f, 90.0f, 105.0f, 120.0f, 135.0f, 150.0f, 165.0f, 180.0f, 195.0f, 210.0f, 225.0f, 155.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 48.0f, 56.0f, 64.0f, 72.0f, 80.0f, 88.0f, 96.0f, 104.0f, 112.0f, 120.0f, 235.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 48.0f, 56.0f, 64.0f, 72.0f, 80.0f, 88.0f, 96.0f, 104.0f, 112.0f, 120.0f, 235.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 48.0f, 56.0f, 64.0f, 72.0f, 80.0f, 88.0f, 96.0f, 104.0f, 112.0f, 120.0f, 235.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 48.0f, 56.0f, 64.0f, 72.0f, 80.0f, 88.0f, 96.0f, 104.0f, 112.0f, 120.0f, 235.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 48.0f, 56.0f, 64.0f, 72.0f, 80.0f, 88.0f, 96.0f, 104.0f, 112.0f, 120.0f, 235.0f,
      20.0f, 16.0f, 24.0f, 32.0f, 40.0f, 48.0f, 56.0f, 64.0f, 72.0f, 80.0f, 88.0f, 96.0f, 104.0f, 112.0f, 120.0f, 235.0f,
      15.0f, 30.0f, 45.0f, 60.0f, 75.0f, 90.0f, 105.0f, 120.0f, 135.0f, 150.0f, 165.0f, 180.0f, 195.0f, 210.0f, 225.0f, 155.0f
    )

    val constantOriginal = 2.0f
    val constantBorder = 5.0f
    
    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr(0).length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), localDimX), localDimY),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x, constantBorder))) o MapSeq(BoundaryUtilities.idIF) $ Get(m, 1),
              MapSeq(fun(x => mult(x, constantOriginal)))  o MapSeq(BoundaryUtilities.invertIntToFloat) $ Get(m, 1))
          )
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, mask2D, StencilUtilities.weights)
    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(stencilarr, output, localDimX)
    }

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testSimpleGridWithTwoBoundaryCheckMask2D(): Unit = {
    /* u[cp] = ( boundary ? constantBorder2 : constantOriginal2) + S*( boundary ? constantBorder : constantOriginal) */

    val compareData = Array(
      10.0f, 16.0f, 22.0f, 28.0f, 34.0f, 26.0f,
      12.0f, 10.0f, 14.0f, 18.0f, 22.0f, 38.0f,
      12.0f, 10.0f, 14.0f, 18.0f, 22.0f, 38.0f,
      12.0f, 10.0f, 14.0f, 18.0f, 22.0f, 38.0f,
      12.0f, 10.0f, 14.0f, 18.0f, 22.0f, 38.0f,
      10.0f, 16.0f, 22.0f, 28.0f, 34.0f, 26.0f
    )

    val constantOriginal = Array(1.0f, 2.0f)
    val constantBorder = Array(2.0f, 4.0f)


    /*
      1) Use borders with Arrays with what works already
      2) get same value for both
      3) update to show case above
      4) try to pull out into separate function
     */

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          val maskedValConst = BoundaryUtilities.maskValue(Get(m,1), constantBorder(1), constantOriginal(1))
          val maskedValGrid = BoundaryUtilities.maskValue(Get(m,1), constantBorder(0), constantOriginal(0))
          toGlobal(MapSeq(id) o MapSeq(addTuple)) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weights),
            MapSeq(id) $ maskedValGrid
          ),
            MapSeq(id) $ maskedValConst)
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, mask, StencilUtilities.weights)

    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testTwoGridsThreeCalculationsWithMask2D(): Unit = {
    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u1[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val compareData = Array(
      128.0f, 256.0f, 384.0f, 512.0f, 640.0f, 656.0f,
      144.0f, 72.0f, 108.0f, 144.0f, 180.0f, 752.0f,
      144.0f, 72.0f, 108.0f, 144.0f, 180.0f, 752.0f,
      144.0f, 72.0f, 108.0f, 144.0f, 180.0f, 752.0f,
      144.0f, 72.0f, 108.0f, 144.0f, 180.0f, 752.0f,
      128.0f, 256.0f, 384.0f, 512.0f, 640.0f, 656.0f
    )

    val constantOriginal = Array(1.0f, 2.0f, 3.0f, 4.0f)
    val constantBorder = Array(2.0f, 4.0f, 6.0f, 8.0f)

    // why doesn't this work @ end?? MapSeq(fun(x => mult(x,maskedValMult))) o

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr.length), stencilarr.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(Get(m,1), constantBorder(3), constantOriginal(3))
          val maskedValConstSec = BoundaryUtilities.maskValue(Get(m,1), constantBorder(2), constantOriginal(2))
          val maskedValConstOrg = BoundaryUtilities.maskValue(Get(m,1), constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(Get(m,1), constantBorder(0), constantOriginal(0))
          val orgMat = Get(Get(m, 0), 0)
          val secMat = Get(Get(m, 0), 1)

          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 0), weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 1), weights),
              MapSeq(id) $ maskedValStencil
            ))
            ,
            (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 1), weightsMiddle),
              MapSeq(id) $ maskedValConstSec)
          ),
            maskedValMult)
        }))
        ) $ Zip(Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))), Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrsame, mask, StencilUtilities.weights, StencilUtilities.weightsMiddle)

    if (StencilUtilities.printOutput)
      StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }


  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym2D(): Unit = {
    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u1[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val localDimX = 6
    val localDimY = 10

    val stencilarr2D = StencilUtilities.createDataFloat2D(localDimX, localDimY)
    val stencilarrsame2D = StencilUtilities.createDataFloat2D(localDimX, localDimY)
    val stencilarr2DCopy = stencilarr2D.map(x => x.map(y => y * 2.0f))
    val mask2D = BoundaryUtilities.createMaskDataAsym2D(localDimX, localDimY)

    val compareData = Array(
      9.5f, 19.0f, 28.5f, 38.0f, 47.5f, 43.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      11.5f, 8.0f, 12.0f, 16.0f, 20.0f, 55.0f,
      9.5f, 19.0f, 28.5f, 38.0f, 47.5f, 43.0f
    )

    val constantOriginal = Array(1.0f, 2.0f, 3.0f, 0.25f)
    val constantBorder = Array(2.0f, 4.0f, 1.5f, 0.5f)

    // why doesn't this work @ end?? MapSeq(fun(x => mult(x,maskedValMult))) o

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr2D(0).length), stencilarr2D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr2D(0).length), stencilarr2D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), localDimX), localDimY),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          val maskedValMult = BoundaryUtilities.maskValue(Get(m,1), constantBorder(3), constantOriginal(3))
          val maskedValConstSec = BoundaryUtilities.maskValue(Get(m,1), constantBorder(2), constantOriginal(2))
          val maskedValConstOrg = BoundaryUtilities.maskValue(Get(m,1), constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(Get(m,1), constantBorder(0), constantOriginal(0))
          val orgMat = Get(Get(m, 0), 0)
          val secMat = Get(Get(m, 0), 1)

          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 0), weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 1), weights),
              MapSeq(id) $ maskedValStencil
            ))
            ,
            (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 1), weightsMiddle),
              MapSeq(id) $ maskedValConstSec)
          ),
            maskedValMult)
        }))
        ) $ Zip(Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))), Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(stencilarr2D.length, stencilarr2D.length)(lambdaNeigh, stencilarr2D, stencilarr2DCopy, mask2D, StencilUtilities.weights, StencilUtilities.weightsMiddle)

    if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput2D(stencilarr2D, output, localDimX)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testSimpleOneGridWithBoundaryCheckMask3D(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val localDim = 4
    val stencilarr3D = StencilUtilities.createDataFloat3DWithPadding(localDim, localDim, localDim)
    val mask3D = BoundaryUtilities.createMaskData3D(localDim)

    /* u[cp] = S*( boundary ? constantBorder : constantOriginal) */

    val compareData = Array(
      30.0f, 50.0f, 70.0f, 65.0f,
      50.0f, 80.0f, 105.0f, 100.0f,
      70.0f, 105.0f, 130.0f, 120.0f,
      65.0f, 100.0f, 120.0f, 100.0f,
      50.0f, 80.0f, 105.0f, 100.0f,
      80.0f, 48.0f, 60.0f, 145.0f,
      105.0f, 60.0f, 72.0f, 170.0f,
      100.0f, 145.0f, 170.0f, 150.0f,
      70.0f, 105.0f, 130.0f, 120.0f,
      105.0f, 60.0f, 72.0f, 170.0f,
      130.0f, 72.0f, 84.0f, 195.0f,
      120.0f, 170.0f, 195.0f, 170.0f,
      65.0f, 100.0f, 120.0f, 100.0f,
      100.0f, 145.0f, 170.0f, 150.0f,
      120.0f, 170.0f, 195.0f, 170.0f,
      100.0f, 150.0f, 170.0f, 135.0f
    )

    val constantOriginal = 2.0f
    val constantBorder = 5.0f

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr3D(0)(0).length), stencilarr3D(0).length), stencilarr3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), localDim), localDim), localDim),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(m, 0), Join() $ weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x, constantBorder))) o MapSeq(BoundaryUtilities.idIF) $ Get(m, 1),
              MapSeq(fun(x => mult(x, constantOriginal)))  o MapSeq(BoundaryUtilities.invertIntToFloat) $ Get(m, 1)))
        }))
        ) $ Zip((Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), Join() o Join() $ mask1)
      })

    val (output: Array[Float], runtime) = Execute(2, 2, 2, 2, 2, 2, (true, true))(lambdaNeigh, stencilarr3D, mask3D, StencilUtilities.weights3D)

    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
    }

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }


  @Test
  def testTwoGridsThreeCalculationsWithMask3D(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val localDim = 4
    val stencilarr3D = StencilUtilities.createDataFloat3DWithPadding(localDim, localDim, localDim)
    val stencilarrsame3D = StencilUtilities.createDataFloat3DWithPadding(localDim, localDim, localDim)
    val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskData3D(localDim)

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val compareData = Array(
      16.25f, 28.5f, 40.75f, 43.0f,
      28.5f, 44.75f, 59.0f, 61.25f,
      40.75f, 59.0f, 73.25f, 73.5f,
      43.0f, 61.25f, 73.5f, 69.75f,
      28.5f, 44.75f, 59.0f, 61.25f,
      44.75f, 17.5f, 21.875f, 83.5f,
      59.0f, 21.875f, 26.25f, 97.75f,
      61.25f, 83.5f, 97.75f, 94.0f,
      40.75f, 59.0f, 73.25f, 73.5f,
      59.0f, 21.875f, 26.25f, 97.75f,
      73.25f, 26.25f, 30.625f, 112.0f,
      73.5f, 97.75f, 112.0f, 106.25f,
      43.0f, 61.25f, 73.5f, 69.75f,
      61.25f, 83.5f, 97.75f, 94.0f,
      73.5f, 97.75f, 112.0f, 106.25f,
      69.75f, 94.0f, 106.25f, 96.5f
    )

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr3D(0)(0).length), stencilarr3D(0).length), stencilarr3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr3DCopy(0)(0).length), stencilarr3DCopy(0).length), stencilarr3DCopy.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), localDim), localDim), localDim),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(Get(m,1), constantBorder(3), constantOriginal(3))
          val maskedValConstOrg = BoundaryUtilities.maskValue(Get(m,1), constantBorder(2), constantOriginal(2))
          val maskedValConstSec = BoundaryUtilities.maskValue(Get(m,1), constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(Get(m,1), constantBorder(0), constantOriginal(0))

          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(Get(m, 0), 0), Join() $ weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(Get(m, 0), 1), Join() $ weights),
              MapSeq(id) $ maskedValStencil
            ))
            ,
            (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(Get(m, 0), 1), Join() $ weightsMiddle),
              MapSeq(id) $ maskedValConstSec)
          ),
            maskedValMult)
        }))
        ) $ Zip(Zip((Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))), Join() o Join() $ mask1)
      })
    try {
      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(lambdaNeigh, stencilarr3D, stencilarr3DCopy, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }

  }

  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3D(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val localDimX = 6
    val localDimY = 8
    val localDimZ = 12

    val stencilarr3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrsame3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3D(localDimX, localDimY, localDimZ)

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    // s/\.\([0-9]\+\)/\.\1f,/gc   -- helpful vim regex
    val compareData = AcousticComparisonArrays.testTwoGridsThreeCalculationsWithMaskAsym3DComparisonData6x8x12

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr3D(0)(0).length), stencilarr3D(0).length), stencilarr3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, stencilarr3DCopy(0)(0).length), stencilarr3DCopy(0).length), stencilarr3DCopy.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), localDimX), localDimY), localDimZ),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)(fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(Get(m,2), constantBorder(3), constantOriginal(3))
          val maskedValConstOrg = BoundaryUtilities.maskValue(Get(m,2), constantBorder(2), constantOriginal(2))
          val maskedValConstSec = BoundaryUtilities.maskValue(Get(m,2), constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(Get(m,2), constantBorder(0), constantOriginal(0))

          toGlobal(MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(m, 0),
              Join() $ weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(m, 1),
                Join() $ weights),
              MapSeq(id) $ maskedValStencil
            ))
            ,
            (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(m, 1),
                Join() $ weightsMiddle),
              MapSeq(id) $ maskedValConstSec)
          ),
            maskedValMult)
        })))
        ) $ Zip3D((Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1), (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2), mask1)
      })
    try {
      val newLambda = SimplifyAndFuse(lambdaNeigh)
      val source = Compile(newLambda)
      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, newLambda, stencilarr3D, stencilarr3DCopy, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneral(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val localDimX = 4
    val localDimY = 6
    val localDimZ = 10
    val stencilarr3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrsame3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3D(localDimX, localDimY, localDimZ)

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)

    val compareData = AcousticComparisonArrays.testTwoGridsThreeCalculationsWithMaskAsym3DGeneralComparisonData4x6x10

    val n = SizeVar("N")
    val m = SizeVar("M")
    val o = SizeVar("O")
    val a = SizeVar("A")
    val x = SizeVar("X")
    val y = SizeVar("Y")
    val z = SizeVar("Z")


    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), m - 2), n - 2), o - 2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mat3, mask1, weights, weightsMiddle) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)((fun((m) =>

          toGlobal(MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join()
              $ Get(m, 0), Join() $ weightsMiddle),
            MapSeq(id) $ BoundaryUtilities.maskValue(Get(m,2), constantBorder(2), constantOriginal(2))
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $
                Get(m, 1), Join() $ weights),
              MapSeq(id) $ BoundaryUtilities.maskValue(Get(m,2), constantBorder(0), constantOriginal(0))
            ))
            ,
            (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $
                Get(m, 1), Join() $ weightsMiddle),
              MapSeq(id) $ BoundaryUtilities.maskValue(Get(m,2), constantBorder(1), constantOriginal(1)))
          ),
            BoundaryUtilities.maskValue(Get(m,2), constantBorder(3), constantOriginal(3)))
        ))))
        ) $ Zip3D((Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1), (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2), mask1)
      })

    try
    {
      val newLambda = SimplifyAndFuse(lambdaNeigh)
      val source = Compile(newLambda)

      // OutputKernelJSON(newLambda,"/home/reese/workspace/sandbox/")
      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, newLambda, stencilarr3D, stencilarr3DCopy, stencilarr3D, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput)
      {
        StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
      }
      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
    }
    catch
      {
        case e: DeviceCapabilityException =>
          Assume.assumeNoException("Device not supported.", e)
      }

  }

  @Test
  def testTwoGridsThreeCalculationsAsym3DGeneralNoMask(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val compareData  = AcousticComparisonArrays.testTwoGridsThreeCalculationsAsym3DGeneralNoMaskComparisonData8x4x12

    val localDimX = 8
    val localDimY = 4
    val localDimZ = 12
    val stencilarr3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrsame3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)

    /* u[cp] = X * ( S*l0 + u1[cp]*l1 + u[cp]*l2) */

    val n = SizeVar("N")
    val m = SizeVar("M")
    val o = SizeVar("O")
    val a = SizeVar("A")
    val x = SizeVar("X")
    val y = SizeVar("Y")
    val z = SizeVar("Z")

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)((fun((m) =>
          MapSeq(toGlobal(fun(x => mult(x,constantOriginal(3))))) o
            MapSeq(addTuple) $
            Zip(MapSeq(addTuple) $
              Zip(((MapSeq(fun(x => mult(x,constantOriginal(2))))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                Zip(Join() $ Get(m, 0), Join() $ weightsMiddle)),
                (MapSeq(fun(x => mult(x, constantOriginal(0)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                  Zip(Join() $ Get(m, 1), Join() $ weights))),
              (MapSeq(fun(x => mult(x,constantOriginal(1)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                Zip(Join() $ Get(m, 1), Join() $ weightsMiddle)))
        ))))) $ Zip3D((Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1), (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))
      })

    try
    {
      val newLambda = SimplifyAndFuse(lambdaNeigh)
      val source = Compile(newLambda)

      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, newLambda, stencilarr3D, stencilarr3DCopy, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput)
      {
        StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
      }
      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

    }
    catch
      {
        case e: DeviceCapabilityException =>
          Assume.assumeNoException("Device not supported.", e)
      }

  }

}

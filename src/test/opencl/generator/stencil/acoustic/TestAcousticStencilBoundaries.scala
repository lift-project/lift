package opencl.generator.stencil.acoustic

import com.sun.xml.internal.ws.developer.Serialization
import ir.ast._
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, DeviceCapabilityException, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import java.io._

import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.util.parsing.json.JSON

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
  val convertInt = UserFun("convertInt", Array("x"), "{ return x ? 1 : 0; }", Seq(Int), Int)
  val invertFloat = UserFun("invertFloat", Array("x"), "{ return ((x-1.0) == 0.0) ? 0.0 : 1.0; }", Seq(Float), Float)
  val convertFloat = UserFun("convertFloat", Array("x"), "{ return ((x-1.0) == 0.0) ? 1.0 : 0.0; }", Seq(Float), Float)
  val getFirstTuple = UserFun("getFirstTuple", "x", "{return x._0;}", TupleType(Float, Float), Float) // dud helper
  val getSecondTuple = UserFun("getSecondTuple", "x", "{return x._1;}", TupleType(Float, Float), Float) // dud helper
  val idIF = UserFun("idIF", "x", "{ return (float)(x*1.0); }", Int, Float)

  /* create mask of 0s and 1s at the boundary for a 2D Matrix */
  def createMask(input: Array[Array[Float]], msizeX: Int, msizeY: Int, maskValue: Int): Array[Array[Int]] = {

    val mask =input.flatten.zipWithIndex.map(i => !( (i._2%msizeX != 0) && i._2%msizeX!=(msizeX-1)  && i._2>msizeX && i._2<(msizeX*msizeY)-msizeX) )
    mask.map(i => i*1).sliding(msizeX,msizeX).toArray

  }

  /* should create asym version! */
  def createMaskData2D(size: Int)  =
  {
    createMaskDataAsym2D(size,size)
  }

    def createMaskDataAsym2D(sizeX: Int, sizeY: Int) =
  {
    val initMat = Array.tabulate(sizeX,sizeY){ (i,j) => (i+j+1).toFloat }
    val maskArray = createMask(initMat,sizeX,sizeY,0).map(i => i.map(j => j.toString.toArray))
    val mask = createMask(initMat,sizeX,sizeY,0).map(i => i.map(j => j.toString.toArray))
    mask.map(i => i.map(j => j.map(k => k.toInt-parseIntAsCharAsInt(0))))
  }

  def createMaskDataAsym3D(sizeX: Int, sizeY: Int, sizeZ: Int) = {

    val pad2D = createMaskDataAsym2D(sizeX, sizeY)
    val one2D = Array(Array.fill(sizeX,sizeY)(Array(1)))
    var addArr = Array(pad2D)

    for(i <- 1 to sizeZ-3) addArr = addArr ++ Array(pad2D)
    one2D ++ addArr ++ one2D
  }

  def createMaskData3D(size: Int) =
  {
      createMaskDataAsym3D(size,size,size)
  }



  def maskValue(m: Expr, c1: Float, c2: Float): Expr = {
    MapSeq(add) $ Zip(MapSeq(fun(x => mult(x,c1))) o MapSeq(idIF) o MapSeq(convertInt) $ Get(m,1),MapSeq(fun(x => mult(x,c2))) o MapSeq(idIF) o MapSeq(invertInt) $ Get(m,1))
  }



  def collectParams(lambda: Lambda, source: String, outputDir: String) =
  {

    // collect parameters

    val params = TypedOpenCLMemory.get(lambda.body, lambda.params, includePrivate = false)

    var lm = scala.collection.immutable.ListMap[String,String]()

    var newParams = params.toString().stripPrefix("ArrayBuffer(").split(",").filter(_.contains("global"))
    val ignoreable = ", \t({}"
    val stripParams = newParams.map(x => x.split(":")(0).dropWhile(c => ignoreable.indexOf(c) >= 0).stripSuffix("}").split(";").filter(x => !x.contains("global")))
    val map = stripParams.map{ case Array(x,y) => x -> y}

    stripParams.foreach(x => lm += (x(0) -> x(1)))

    // add in general values (ints)

    val kernelStr = source.split("\n").filter(x => x.toString().contains("kernel"))

    val generalVals = kernelStr(0).split(",").filter(x => !x.contains("*"))

    val genVals = generalVals.map(x => x.trim.stripSuffix("){"))

    genVals.foreach(x => lm += (x -> ""))

    // convert to json object
    var json = scala.util.parsing.json.JSONObject(lm).toString()

    // write to file
    writeStringToFile(json,outputDir+"/kernel.json")

    // print kernel to same place (pass in param!)
    writeStringToFile(source,outputDir+"/lift_kernel.cl")

  }

  def writeStringToFile(str: String, outputFile: String): Unit =
  {
    val pw = new PrintWriter(new File(outputFile))
    pw.write(str)
    pw.close

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

  val stencilarr3D = StencilUtilities.createDataFloat3D(localDim, localDim, localDim)
  val stencilarrsame3D = StencilUtilities.createDataFloat3D(localDim, localDim, localDim)
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
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x, constantBorder))) o MapSeq(BoundaryUtilities.idIF) $ Get(m, 1), MapSeq(fun(x => mult(x, constantOriginal))) o MapSeq(BoundaryUtilities.idIF) o MapSeq(BoundaryUtilities.invertInt) $ Get(m, 1))
            //MapSeq(id) o MapSeq(add) $ Zip(Map( fun(x => mult(x,constantBorder))) o MapSeq(idIF) $ Get(m,1), Map(fun(x => mult(x,constantOriginal))) o MapSeq(idIF) o MapSeq(invertInt) $ Get(m,1))
          )
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), Join() $ mask1)
      })

    val lambdaNeighCompare = fun(
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(BoundaryUtilities.getFirstTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weights),
            MapSeq(BoundaryUtilities.idIF) $ Get(m, 1)
          )
        }))
        ) $ Zip((Join() $ (Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), Join() $ mask1)
      })



    val source = Compile(lambdaNeigh)

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(source, lambdaNeigh, stencilarr, mask, StencilUtilities.weights)
    val (output2: Array[Float], runtime2) = Execute(stencilarr.length, stencilarr.length)(lambdaNeighCompare, stencilarr, mask, StencilUtilities.weights)
    if (StencilUtilities.printOutput) {
      StencilUtilities.printOriginalAndOutput2D(stencilarr, output, StencilUtilities.stencilSize)
      StencilUtilities.print1DArrayAs2DArray(output2, StencilUtilities.stencilSize)
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
      ArrayType(ArrayType(Float, stencilarr(0).length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1), localDimY), localDimX),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(m, 0), weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x, constantBorder))) o MapSeq(BoundaryUtilities.idIF) o MapSeq(BoundaryUtilities.convertInt) $ Get(m, 1),
              MapSeq(fun(x => mult(x, constantOriginal))) o MapSeq(BoundaryUtilities.idIF) o MapSeq(BoundaryUtilities.invertInt) $ Get(m, 1))
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
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          val maskedValConst = BoundaryUtilities.maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValGrid = BoundaryUtilities.maskValue(m, constantBorder(0), constantOriginal(0))
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
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(Float, stencilarr.length), stencilarr.length),
      ArrayType(ArrayType(ArrayType(Int, 1), StencilUtilities.stencilSize), StencilUtilities.stencilSize),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(m, constantBorder(3), constantOriginal(3))
          val maskedValConstSec = BoundaryUtilities.maskValue(m, constantBorder(2), constantOriginal(2))
          val maskedValConstOrg = BoundaryUtilities.maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(m, constantBorder(0), constantOriginal(0))
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
      ArrayType(ArrayType(Float, stencilarr2D(0).length), stencilarr2D.length),
      ArrayType(ArrayType(Float, stencilarr2D(0).length), stencilarr2D.length),
      ArrayType(ArrayType(ArrayType(Int, 1), localDimY), localDimX),
      ArrayType(ArrayType(Float, StencilUtilities.weights(0).length), StencilUtilities.weights.length),
      ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle(0).length), StencilUtilities.weightsMiddle.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {
          val maskedValMult = BoundaryUtilities.maskValue(m, constantBorder(3), constantOriginal(3))
          val maskedValConstSec = BoundaryUtilities.maskValue(m, constantBorder(2), constantOriginal(2))
          val maskedValConstOrg = BoundaryUtilities.maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(m, constantBorder(0), constantOriginal(0))
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

    val localDim = 4
    val stencilarr3D = StencilUtilities.createDataFloat3D(localDim, localDim, localDim)
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
      ArrayType(ArrayType(ArrayType(Float, stencilarr3D(0)(0).length), stencilarr3D(0).length), stencilarr3D.length),
      ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), localDim), localDim), localDim),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      (mat1, mask1, weights) => {
        MapGlb((fun((m) => {
          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $ Get(m, 0), Join() $ weights),
            MapSeq(id) o MapSeq(add) $ Zip(MapSeq(fun(x => mult(x, constantBorder))) o MapSeq(BoundaryUtilities.idIF) o MapSeq(BoundaryUtilities.convertInt) $ Get(m, 1),
              MapSeq(fun(x => mult(x, constantOriginal))) o MapSeq(BoundaryUtilities.idIF) o MapSeq(BoundaryUtilities.invertInt) $ Get(m, 1)))
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
    val localDim = 4
    val stencilarr3D = StencilUtilities.createDataFloat3D(localDim, localDim, localDim)
    val stencilarrsame3D = StencilUtilities.createDataFloat3D(localDim, localDim, localDim)
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
      ArrayType(ArrayType(ArrayType(Float, stencilarr3D(0)(0).length), stencilarr3D(0).length), stencilarr3D.length),
      ArrayType(ArrayType(ArrayType(Float, stencilarr3DCopy(0)(0).length), stencilarr3DCopy(0).length), stencilarr3DCopy.length),
      ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), localDim), localDim), localDim),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(m, constantBorder(3), constantOriginal(3))
          val maskedValConstOrg = BoundaryUtilities.maskValue(m, constantBorder(2), constantOriginal(2))
          val maskedValConstSec = BoundaryUtilities.maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(m, constantBorder(0), constantOriginal(0))

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
    val localDimX = 6
    val localDimY = 8
    val localDimZ = 12

    val stencilarr3D = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarrsame3D = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3D(localDimX, localDimY, localDimZ)

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    // s/\.\([0-9]\+\)/\.\1f,/gc   -- helpful vim regex
    val compareData = Array(
      16.25f, 28.5f, 40.75f, 53.0f, 65.25f, 63.5f,
      28.5f, 44.75f, 59.0f, 73.25f, 87.5f, 85.75f,
      40.75f, 59.0f, 73.25f, 87.5f, 101.75f, 98.0f,
      53.0f, 73.25f, 87.5f, 101.75f, 116.0f, 110.25f,
      65.25f, 87.5f, 101.75f, 116.0f, 130.25f, 122.5f,
      77.5f, 101.75f, 116.0f, 130.25f, 144.5f, 134.75f,
      89.75f, 116.0f, 130.25f, 144.5f, 158.75f, 147.0f,
      84.0f, 110.25f, 122.5f, 134.75f, 147.0f, 131.25f,
      28.5f, 44.75f, 59.0f, 73.25f, 87.5f, 85.75f,
      44.75f, 17.5f, 21.875f, 26.25f, 30.625f, 112.0f,
      59.0f, 21.875f, 26.25f, 30.625f, 35.0f, 126.25f,
      73.25f, 26.25f, 30.625f, 35.0f, 39.375f, 140.5f,
      87.5f, 30.625f, 35.0f, 39.375f, 43.75f, 154.75f,
      101.75f, 35.0f, 39.375f, 43.75f, 48.125f, 169.0f,
      116.0f, 39.375f, 43.75f, 48.125f, 52.5f, 183.25f,
      110.25f, 140.5f, 154.75f, 169.0f, 183.25f, 167.5f,
      40.75f, 59.0f, 73.25f, 87.5f, 101.75f, 98.0f,
      59.0f, 21.875f, 26.25f, 30.625f, 35.0f, 126.25f,
      73.25f, 26.25f, 30.625f, 35.0f, 39.375f, 140.5f,
      87.5f, 30.625f, 35.0f, 39.375f, 43.75f, 154.75f,
      101.75f, 35.0f, 39.375f, 43.75f, 48.125f, 169.0f,
      116.0f, 39.375f, 43.75f, 48.125f, 52.5f, 183.25f,
      130.25f, 43.75f, 48.125f, 52.5f, 56.875f, 197.5f,
      122.5f, 154.75f, 169.0f, 183.25f, 197.5f, 179.75f,
      53.0f, 73.25f, 87.5f, 101.75f, 116.0f, 110.25f,
      73.25f, 26.25f, 30.625f, 35.0f, 39.375f, 140.5f,
      87.5f, 30.625f, 35.0f, 39.375f, 43.75f, 154.75f,
      101.75f, 35.0f, 39.375f, 43.75f, 48.125f, 169.0f,
      116.0f, 39.375f, 43.75f, 48.125f, 52.5f, 183.25f,
      130.25f, 43.75f, 48.125f, 52.5f, 56.875f, 197.5f,
      144.5f, 48.125f, 52.5f, 56.875f, 61.25f, 211.75f,
      134.75f, 169.0f, 183.25f, 197.5f, 211.75f, 192.0f,
      65.25f, 87.5f, 101.75f, 116.0f, 130.25f, 122.5f,
      87.5f, 30.625f, 35.0f, 39.375f, 43.75f, 154.75f,
      101.75f, 35.0f, 39.375f, 43.75f, 48.125f, 169.0f,
      116.0f, 39.375f, 43.75f, 48.125f, 52.5f, 183.25f,
      130.25f, 43.75f, 48.125f, 52.5f, 56.875f, 197.5f,
      144.5f, 48.125f, 52.5f, 56.875f, 61.25f, 211.75f,
      158.75f, 52.5f, 56.875f, 61.25f, 65.625f, 226.0f,
      147.0f, 183.25f, 197.5f, 211.75f, 226.0f, 204.25f,
      77.5f, 101.75f, 116.0f, 130.25f, 144.5f, 134.75f,
      101.75f, 35.0f, 39.375f, 43.75f, 48.125f, 169.0f,
      116.0f, 39.375f, 43.75f, 48.125f, 52.5f, 183.25f,
      130.25f, 43.75f, 48.125f, 52.5f, 56.875f, 197.5f,
      144.5f, 48.125f, 52.5f, 56.875f, 61.25f, 211.75f,
      158.75f, 52.5f, 56.875f, 61.25f, 65.625f, 226.0f,
      173.0f, 56.875f, 61.25f, 65.625f, 70.0f, 240.25f,
      159.25f, 197.5f, 211.75f, 226.0f, 240.25f, 216.5f,
      89.75f, 116.0f, 130.25f, 144.5f, 158.75f, 147.0f,
      116.0f, 39.375f, 43.75f, 48.125f, 52.5f, 183.25f,
      130.25f, 43.75f, 48.125f, 52.5f, 56.875f, 197.5f,
      144.5f, 48.125f, 52.5f, 56.875f, 61.25f, 211.75f,
      158.75f, 52.5f, 56.875f, 61.25f, 65.625f, 226.0f,
      173.0f, 56.875f, 61.25f, 65.625f, 70.0f, 240.25f,
      187.25f, 61.25f, 65.625f, 70.0f, 74.375f, 254.5f,
      171.5f, 211.75f, 226.0f, 240.25f, 254.5f, 228.75f,
      102.0f, 130.25f, 144.5f, 158.75f, 173.0f, 159.25f,
      130.25f, 43.75f, 48.125f, 52.5f, 56.875f, 197.5f,
      144.5f, 48.125f, 52.5f, 56.875f, 61.25f, 211.75f,
      158.75f, 52.5f, 56.875f, 61.25f, 65.625f, 226.0f,
      173.0f, 56.875f, 61.25f, 65.625f, 70.0f, 240.25f,
      187.25f, 61.25f, 65.625f, 70.0f, 74.375f, 254.5f,
      201.5f, 65.625f, 70.0f, 74.375f, 78.75f, 268.75f,
      183.75f, 226.0f, 240.25f, 254.5f, 268.75f, 241.0f,
      114.25f, 144.5f, 158.75f, 173.0f, 187.25f, 171.5f,
      144.5f, 48.125f, 52.5f, 56.875f, 61.25f, 211.75f,
      158.75f, 52.5f, 56.875f, 61.25f, 65.625f, 226.0f,
      173.0f, 56.875f, 61.25f, 65.625f, 70.0f, 240.25f,
      187.25f, 61.25f, 65.625f, 70.0f, 74.375f, 254.5f,
      201.5f, 65.625f, 70.0f, 74.375f, 78.75f, 268.75f,
      215.75f, 70.0f, 74.375f, 78.75f, 83.125f, 283.0f,
      196.0f, 240.25f, 254.5f, 268.75f, 283.0f, 253.25f,
      126.5f, 158.75f, 173.0f, 187.25f, 201.5f, 183.75f,
      158.75f, 52.5f, 56.875f, 61.25f, 65.625f, 226.0f,
      173.0f, 56.875f, 61.25f, 65.625f, 70.0f, 240.25f,
      187.25f, 61.25f, 65.625f, 70.0f, 74.375f, 254.5f,
      201.5f, 65.625f, 70.0f, 74.375f, 78.75f, 268.75f,
      215.75f, 70.0f, 74.375f, 78.75f, 83.125f, 283.0f,
      230.0f, 74.375f, 78.75f, 83.125f, 87.5f, 297.25f,
      208.25f, 254.5f, 268.75f, 283.0f, 297.25f, 265.5f,
      138.75f, 173.0f, 187.25f, 201.5f, 215.75f, 196.0f,
      173.0f, 56.875f, 61.25f, 65.625f, 70.0f, 240.25f,
      187.25f, 61.25f, 65.625f, 70.0f, 74.375f, 254.5f,
      201.5f, 65.625f, 70.0f, 74.375f, 78.75f, 268.75f,
      215.75f, 70.0f, 74.375f, 78.75f, 83.125f, 283.0f,
      230.0f, 74.375f, 78.75f, 83.125f, 87.5f, 297.25f,
      244.25f, 78.75f, 83.125f, 87.5f, 91.875f, 311.5f,
      220.5f, 268.75f, 283.0f, 297.25f, 311.5f, 277.75f,
      125.0f, 159.25f, 171.5f, 183.75f, 196.0f, 172.25f,
      159.25f, 197.5f, 211.75f, 226.0f, 240.25f, 216.5f,
      171.5f, 211.75f, 226.0f, 240.25f, 254.5f, 228.75f,
      183.75f, 226.0f, 240.25f, 254.5f, 268.75f, 241.0f,
      196.0f, 240.25f, 254.5f, 268.75f, 283.0f, 253.25f,
      208.25f, 254.5f, 268.75f, 283.0f, 297.25f, 265.5f,
      220.5f, 268.75f, 283.0f, 297.25f, 311.5f, 277.75f,
      192.75f, 241.0f, 253.25f, 265.5f, 277.75f, 240.0f
    )

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)

    val lambdaNeigh = fun(
      ArrayType(ArrayType(ArrayType(Float, stencilarr3D(0)(0).length), stencilarr3D(0).length), stencilarr3D.length),
      ArrayType(ArrayType(ArrayType(Float, stencilarr3DCopy(0)(0).length), stencilarr3DCopy(0).length), stencilarr3DCopy.length),
      ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), localDimZ), localDimY), localDimX),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(m, constantBorder(3), constantOriginal(3))
          val maskedValConstOrg = BoundaryUtilities.maskValue(m, constantBorder(2), constantOriginal(2))
          val maskedValConstSec = BoundaryUtilities.maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(m, constantBorder(0), constantOriginal(0))

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
      val source = Compile(lambdaNeigh)
      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, lambdaNeigh, stencilarr3D, stencilarr3DCopy, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)
    }
  }

  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneral(): Unit = {
    val localDimX = 4
    val localDimY = 4
    val localDimZ = 4
    val stencilarr3D = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarrsame3D = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3DCopy = stencilarr3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3D(localDimX, localDimY, localDimZ)

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)


    val n = SizeVar("N")
    val m = SizeVar("M")
    val o = SizeVar("O")
    val a = SizeVar("A")
    val x = SizeVar("X")
    val y = SizeVar("Y")
    val z = SizeVar("Z")

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

    val lambdaNeigh = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), m - 2), n - 2), o - 2),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = BoundaryUtilities.maskValue(m, constantBorder(3), constantOriginal(3))
          val maskedValConstOrg = BoundaryUtilities.maskValue(m, constantBorder(2), constantOriginal(2))
          val maskedValConstSec = BoundaryUtilities.maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValue(m, constantBorder(0), constantOriginal(0))

          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join()
              $ Get(Get(m, 0), 0), Join() $ weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $
                Get(Get(m, 0), 1), Join() $ weights),
              MapSeq(id) $ maskedValStencil
            ))
            ,
            (MapSeq(multTuple)) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $
                Get(Get(m, 0), 1), Join() $ weightsMiddle),
              MapSeq(id) $ maskedValConstSec)
          ),
            maskedValMult)
        }))
        ) $ Zip(Zip((Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat1)), (Join() o Join() $ (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))), Join() o Join() $ mask1)
      })

    val source = Compile(lambdaNeigh)
    try {
      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, lambdaNeigh, stencilarr3D, stencilarr3DCopy, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput) {
        StencilUtilities.printOriginalAndOutput3D(stencilarr3D, output)
        StencilUtilities.print3DArray(mask3D)
      }
      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
    } catch {
      case e: DeviceCapabilityException =>
        Assume.assumeNoException("Device not supported.", e)


    }

    BoundaryUtilities.collectParams(lambdaNeigh, source, "/home/reese/workspace/phd")
  }
}

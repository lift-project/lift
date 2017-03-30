package opencl.generator.stencil.acoustic

import ir.ArrayType
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, DeviceCapabilityException, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import rewriting.SimplifyAndFuse
import utils.OutputKernelJSON

import scala.collection.immutable
import scala.collection.parallel.immutable
import scala.language.implicitConversions

object TestAcousticActualRoom {
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

object RoomConstants {

  val SR = 441.0f
  val alpha = 0.005f
  val c = 344.0f
  val NF = 4410
  val k = 1 / SR
  val h = Math.sqrt(3.0f) * c * k
  val lambda = c * k / h

  val loss1 = 1.0f / (1.0f + lambda * alpha)
  val loss2 = 1.0f - lambda * alpha

  val l2 = ((c * c * k * k) / (h * h)).toFloat
  val cf = Array(loss1.toFloat, 1.0f)
  val cf2 = Array(loss2.toFloat, 1.0f)


}


class TestAcousticActualRoom {


  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneralOneWeightsOrg(): Unit = {

   val compareData = Array(
     16.25f, 28.5f, 40.75f, 43.0f,
     28.5f, 44.75f, 59.0f, 61.25f,
     40.75f, 59.0f, 73.25f, 73.5f,
     53.0f, 73.25f, 87.5f, 85.75f,
     65.25f, 87.5f, 101.75f, 98.0f,
     63.5f, 85.75f, 98.0f, 90.25f,
     28.5f, 44.75f, 59.0f, 61.25f,
     44.75f, 17.5f, 21.875f, 83.5f,
     59.0f, 21.875f, 26.25f, 97.75f,
     73.25f, 26.25f, 30.625f, 112.0f,
     87.5f, 30.625f, 35.0f, 126.25f,
     85.75f, 112.0f, 126.25f, 118.5f,
     40.75f, 59.0f, 73.25f, 73.5f,
     59.0f, 21.875f, 26.25f, 97.75f,
     73.25f, 26.25f, 30.625f, 112.0f,
     87.5f, 30.625f, 35.0f, 126.25f,
     101.75f, 35.0f, 39.375f, 140.5f,
     98.0f, 126.25f, 140.5f, 130.75f,
     53.0f, 73.25f, 87.5f, 85.75f,
     73.25f, 26.25f, 30.625f, 112.0f,
     87.5f, 30.625f, 35.0f, 126.25f,
     101.75f, 35.0f, 39.375f, 140.5f,
     116.0f, 39.375f, 43.75f, 154.75f,
     110.25f, 140.5f, 154.75f, 143.0f,
     65.25f, 87.5f, 101.75f, 98.0f,
     87.5f, 30.625f, 35.0f, 126.25f,
     101.75f, 35.0f, 39.375f, 140.5f,
     116.0f, 39.375f, 43.75f, 154.75f,
     130.25f, 43.75f, 48.125f, 169.0f,
     122.5f, 154.75f, 169.0f, 155.25f,
     77.5f, 101.75f, 116.0f, 110.25f,
     101.75f, 35.0f, 39.375f, 140.5f,
     116.0f, 39.375f, 43.75f, 154.75f,
     130.25f, 43.75f, 48.125f, 169.0f,
     144.5f, 48.125f, 52.5f, 183.25f,
     134.75f, 169.0f, 183.25f, 167.5f,
     89.75f, 116.0f, 130.25f, 122.5f,
     116.0f, 39.375f, 43.75f, 154.75f,
     130.25f, 43.75f, 48.125f, 169.0f,
     144.5f, 48.125f, 52.5f, 183.25f,
     158.75f, 52.5f, 56.875f, 197.5f,
     147.0f, 183.25f, 197.5f, 179.75f,
     102.0f, 130.25f, 144.5f, 134.75f,
     130.25f, 43.75f, 48.125f, 169.0f,
     144.5f, 48.125f, 52.5f, 183.25f,
     158.75f, 52.5f, 56.875f, 197.5f,
     173.0f, 56.875f, 61.25f, 211.75f,
     159.25f, 197.5f, 211.75f, 192.0f,
     114.25f, 144.5f, 158.75f, 147.0f,
     144.5f, 48.125f, 52.5f, 183.25f,
     158.75f, 52.5f, 56.875f, 197.5f,
     173.0f, 56.875f, 61.25f, 211.75f,
     187.25f, 61.25f, 65.625f, 226.0f,
     171.5f, 211.75f, 226.0f, 204.25f,
     104.5f, 134.75f, 147.0f, 131.25f,
     134.75f, 169.0f, 183.25f, 167.5f,
     147.0f, 183.25f, 197.5f, 179.75f,
     159.25f, 197.5f, 211.75f, 192.0f,
     171.5f, 211.75f, 226.0f, 204.25f,
     151.75f, 192.0f, 204.25f, 178.5f
   )
    val localDimX = 4
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3D(localDimX, localDimY, localDimZ)

    /* u[cp] = ( boundary ? constantBorder0 : constantOriginal0 )  * ( S*( boundary ? constantBorder1 : constantOriginal1 ) + u1[cp]*( boundary ? constantBorder2 : constantOriginal2 ) + u[cp]*( boundary ? constantBorder3 : constantOriginal3 )  */

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)


    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

   val lambdaNeigh = fun(
     ArrayType(ArrayType(ArrayType(ArrayType(Float,1),m-2), n-2), o-2),
     ArrayType(ArrayType(ArrayType(Float, m ), n ), o ),
     ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), m-2), n-2), o-2),
     ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
     ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
     (mat1, mat2, mask1, weights, weightsMiddle) => {
       MapGlb(0)(MapGlb(1)(MapGlb(2)((fun((m) =>
         toGlobal(MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
           ReduceSeq(add, 0.0f) $ Get(m, 0),
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
       ))
       ))) $ Zip3D(mat1, (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2), mask1)
     })

    try
    {

      val newLambda = SimplifyAndFuse(lambdaNeigh)
      val source = Compile(newLambda)

      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, newLambda,  stencilarr3D, stencilarrOther3D, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput)
      {
        StencilUtilities.printOriginalAndOutput3D(data, output)
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
  def test3DAsymMaskStencilWithAt(): Unit = {

    val compareData = Array(
    4.9884863f, 7.3179812f, 10.312224f, 13.306468f, 16.30071f, 19.294954f, 22.289196f, 24.618692f,
    7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
    10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
    13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
    16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
    18.630207f, 20.959702f, 23.953945f, 26.948189f, 29.94243f, 32.936672f, 35.93092f, 38.260414f,
    7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
    9.647477f, 12.0f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.277685f,
    12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
    15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
    18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
    20.959702f, 23.289198f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 40.589905f,
    10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
    12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
    15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
    18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
    21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
    23.953945f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 43.584152f,
    13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
    15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
    18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
    21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
    24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
    26.948189f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 46.578396f,
    16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
    18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
    21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
    24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
    27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
    29.94243f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 49.57264f,
    19.294954f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 38.925163f,
    21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
    24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
    27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
    30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
    32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
    22.289196f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 41.919403f,
    24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
    27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
    30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
    33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
    35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
    25.283442f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 44.913643f,
    27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
    30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
    33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
    36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
    38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
    28.277685f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 45.578396f, 47.90789f,
    30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
    33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
    36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
    39.589912f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.999996f, 59.220116f,
    41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
    30.60718f, 32.936672f, 35.93092f, 38.925163f, 41.919403f, 44.913643f, 47.90789f, 50.237385f,
    32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
    35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
    38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
    41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
    44.248898f, 46.578396f, 49.57264f, 52.566883f, 55.561127f, 58.55537f, 61.549614f, 63.879105f
    )
    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3DBP = BoundaryUtilities.createMaskDataWithNumBoundaryPts(localDimX+2, localDimY+2, localDimZ+2)

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val lambdaNeighAt = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m+2), n+2), o+2),
      ArrayType(ArrayType(ArrayType(Int, m), n), o),
      (mat1, mat2,mask) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val maskedValMult = BoundaryUtilities.maskValueNoArrayBoundaryPoints(Get(m,2), RoomConstants.cf(0), RoomConstants.cf(1))
          val maskedValConstOrg = BoundaryUtilities.maskValueNoArrayBoundaryPoints(Get(m,2), RoomConstants.cf2(0), RoomConstants.cf2(1))
          val maskedValStencil = RoomConstants.l2

          val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

          val `tile[0][1][1]` = Get(m,1).at(0).at(1).at(1)
          val `tile[1][0][1]` = Get(m,1).at(1).at(0).at(1)
          val `tile[1][1][0]` = Get(m,1).at(1).at(1).at(0)
          val `tile[1][1][2]` = Get(m,1).at(1).at(1).at(2)
          val `tile[1][2][1]` = Get(m,1).at(1).at(2).at(1)
          val `tile[2][1][1]` = Get(m,1).at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          val valueMat1 = Get(m,0)
          val valueMask = toPrivate(BoundaryUtilities.idIF) $ Get(m,2)

          toGlobal(id) o toPrivate(fun( x => mult(x,maskedValMult))) o toPrivate(addTuple) $
             Tuple(toPrivate(multTuple) $ Tuple(toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,RoomConstants.l2))) $ valueMask, `tile[1][1][1]`),
              toPrivate(subtractTuple) $ Tuple(
                toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
                toPrivate(fun(x => mult(x,maskedValConstOrg))) $ valueMat1))

        })))
        ) $ Zip3D(mat1, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, mask)
      })


    /*
        val newLambda = SimplifyAndFuse(lambdaNeighAt)
        OutputKernelJSON(newLambda,"/home/reese/workspace/phd/sandbox/")
        println(OutputKernelJSON.getJsonString(newLambda))
    */

        val newLambda = SimplifyAndFuse(lambdaNeighAt)
        val source = Compile(newLambda)

        val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,newLambda, data, stencilarrOther3D, mask3DBP)

        if(StencilUtilities.printOutput)
        {
            StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
            StencilUtilities.print3DArray(mask3DBP)
        }

        assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
  }


  @Ignore
  @Test
  def test3DAsymMaskStencilWithAtAndTiling(): Unit = {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3DNoArray(localDimX, localDimY, localDimZ)
    val mask3DBP = BoundaryUtilities.createMaskDataWithNumBoundaryPts(localDimX+2, localDimY+2, localDimZ+2)

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val stencilOrg = fun(
      ArrayType(ArrayType(ArrayType(Float, localDimX), localDimY), localDimZ),
      ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length*StencilUtilities.weightsMiddle3D(0).length*StencilUtilities.weightsMiddle3D.length),
      (matrix, weights) => {
        Untile3D() o MapWrg(2)(MapWrg(1)(MapWrg(0)(fun(tile =>
          MapLcl(2)(MapLcl(1)(MapLcl(0)(
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeq(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() o Join() $ elem,  weights)
            })
          ))) o Slide3D(3,1) o
            toLocal(MapLcl(2)(MapLcl(1)(MapLcl(0)(id)))) $ tile
        )))) o
          Slide3D(4,2)  $ matrix})

    val lambdaNeighAt = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m+2), n+2), o+2),
      ArrayType(ArrayType(ArrayType(Int, m), n), o),
      (mat1, mat2,mask) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val maskedValMult = BoundaryUtilities.maskValueNoArrayBoundaryPoints(Get(m,2), RoomConstants.cf(0), RoomConstants.cf(1))
          val maskedValConstOrg = BoundaryUtilities.maskValueNoArrayBoundaryPoints(Get(m,2), RoomConstants.cf2(0), RoomConstants.cf2(1))
          val maskedValStencil = RoomConstants.l2

          val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

          val `tile[0][1][1]` = Get(m,1).at(0).at(1).at(1)
          val `tile[1][0][1]` = Get(m,1).at(1).at(0).at(1)
          val `tile[1][1][0]` = Get(m,1).at(1).at(1).at(0)
          val `tile[1][1][2]` = Get(m,1).at(1).at(1).at(2)
          val `tile[1][2][1]` = Get(m,1).at(1).at(2).at(1)
          val `tile[2][1][1]` = Get(m,1).at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          val valueMat1 = Get(m,0)
          val valueMask = toPrivate(BoundaryUtilities.idIF) $ Get(m,2)

          toGlobal(id) o toPrivate(fun( x => mult(x,maskedValMult))) o toPrivate(addTuple) $
            Tuple(toPrivate(multTuple) $ Tuple(toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,RoomConstants.l2))) $ valueMask, `tile[1][1][1]`),
              toPrivate(subtractTuple) $ Tuple(
                toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
                toPrivate(fun(x => mult(x,maskedValConstOrg))) $ valueMat1))

        })))
        ) $ Zip3D(mat1, MapWrg(2)(MapWrg(1)(MapWrg(0)(toLocal(MapLcl(2)(MapLcl(1)(MapLcl(0)(id))))))) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, mask)
      })

    /*
    val newLambda = SimplifyAndFuse(lambdaNeighAt)
        OutputKernelJSON(newLambda,"/home/reese/workspace/sandbox/")
        println(OutputKernelJSON.getJsonString(newLambda))*/

    val newLambda = SimplifyAndFuse(lambdaNeighAt)
    val source = Compile(newLambda)

    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,newLambda, data, stencilarrOther3D, mask3DBP)


    //    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneralOneWeightsRoom(): Unit = {

    val compareData = Array(
      4.9884863f, 7.3179812f, 10.312224f, 13.306468f, 16.30071f, 19.294954f, 22.289196f, 24.618692f,
      7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
      10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
      13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
      16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
      18.630207f, 20.959702f, 23.953945f, 26.948189f, 29.94243f, 32.936672f, 35.93092f, 38.260414f,
      7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
      9.647477f, 12.0f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.277685f,
      12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      20.959702f, 23.289198f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 40.589905f,
      10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
      12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      23.953945f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 43.584152f,
      13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      26.948189f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 46.578396f,
      16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      29.94243f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 49.57264f,
      19.294954f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 38.925163f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
      22.289196f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 41.919403f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
      25.283442f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 44.913643f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
      38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
      28.277685f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 45.578396f, 47.90789f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
      39.589912f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.999996f, 59.220116f,
      41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
      30.60718f, 32.936672f, 35.93092f, 38.925163f, 41.919403f, 44.913643f, 47.90789f, 50.237385f,
      32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
      35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
      38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
      41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
      44.248898f, 46.578396f, 49.57264f, 52.566883f, 55.561127f, 58.55537f, 61.549614f, 63.879105f
    )
    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3DBP = BoundaryUtilities.createMaskDataWithNumBoundaryPts(localDimX+2, localDimY+2, localDimZ+2).map(x => x.map(y => y.map(z => Array(z))))

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val lambdaNeigh = fun(
      ArrayType(ArrayType(ArrayType(ArrayType(Float,1),m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m+2 ), n+2 ), o+2 ),
      ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), m), n), o),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)((fun((m) => {

          val maskedValMulti = BoundaryUtilities.maskValueBoundaryPoints(Get(m, 2), RoomConstants.cf(0), RoomConstants.cf(1))
          val maskedValConstOrg = BoundaryUtilities.maskValueBoundaryPoints(Get(m,2), RoomConstants.cf2(0), RoomConstants.cf2(1))
          val valueMask = toPrivate(MapSeq(BoundaryUtilities.idIF)) $ Get(m,2)

          toGlobal(MapSeq(multTuple)) $
                  Zip(MapSeq(addTuple) $
                          Zip(MapSeq(subtractTuple) $ Zip(
                                                          MapSeq(fun(x => mult(x,RoomConstants.l2))) o
                                                          ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $
                                                          Get(m, 1), Join() $ weights),
                                                      (MapSeq(multTuple)) $ Zip(
                                                        ReduceSeq(add, 0.0f) $ Get(m, 0),
                                                        maskedValConstOrg)
                                                        ),
                            (MapSeq(multTuple)) $
                                   Zip(MapSeq(fun(x => (subtract(2.0f,x) ))) o MapSeq(fun(y => mult(y,RoomConstants.l2))) $ valueMask, ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() $
                                             Get(m, 1), Join() $ weightsMiddle)))
          ,
            maskedValMulti)
        }
        ))
        ))) $ Zip3D(mat1, (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2), mask1)
      })

    try
    {

      val newLambda = SimplifyAndFuse(lambdaNeigh)
      val source = Compile(newLambda)
      println(source)
      /*
      OutputKernelJSON(newLambda,"/home/reese/workspace/sandbox/")
      println(OutputKernelJSON.getJsonString(newLambda))
    */

      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, newLambda,  stencilarr3D, stencilarrOther3D, mask3DBP, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput)
      {
        StencilUtilities.printOriginalAndOutput3D(data, output)
      }

      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

    }
    catch
      {
        case e: DeviceCapabilityException =>
          Assume.assumeNoException("Device not supported.", e)
      }

  }

  @Ignore
  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneralOneWeightsRoomTiling(): Unit = {

    val compareData = Array(
      4.9884863f, 7.3179812f, 10.312224f, 13.306468f, 16.30071f, 19.294954f, 22.289196f, 24.618692f,
      7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
      10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
      13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
      16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
      18.630207f, 20.959702f, 23.953945f, 26.948189f, 29.94243f, 32.936672f, 35.93092f, 38.260414f,
      7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
      9.647477f, 12.0f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.277685f,
      12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      20.959702f, 23.289198f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 40.589905f,
      10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
      12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      23.953945f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 43.584152f,
      13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      26.948189f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 46.578396f,
      16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      29.94243f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 49.57264f,
      19.294954f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 38.925163f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
      22.289196f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 41.919403f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
      25.283442f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 44.913643f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
      38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
      28.277685f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 45.578396f, 47.90789f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
      39.589912f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.999996f, 59.220116f,
      41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
      30.60718f, 32.936672f, 35.93092f, 38.925163f, 41.919403f, 44.913643f, 47.90789f, 50.237385f,
      32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
      35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
      38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
      41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
      44.248898f, 46.578396f, 49.57264f, 52.566883f, 55.561127f, 58.55537f, 61.549614f, 63.879105f
    )
    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3DBP = BoundaryUtilities.createMaskDataWithNumBoundaryPts(localDimX+2, localDimY+2, localDimZ+2).map(x => x.map(y => y.map(z => Array(z))))

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    /*
    val prepareData = \((tiles) =>
      Zip(
        Map(Map(Map(tiles._0))),
        // first component
        Map(Map(\(heatNbh =>
          Zip(coeff, Join() $ heatNbh)
        ))) o Slide2D(3, 1) o
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._0,
        // second component
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._1)
      Map()
    )
    */

    val lambdaNeigh = fun(
      ArrayType(ArrayType(ArrayType(ArrayType(Float,1),m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m+2 ), n+2 ), o+2 ),
      ArrayType(ArrayType(ArrayType(ArrayType(Int, 1), m), n), o),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapWrg(2)(MapWrg(1)(MapWrg(0)(MapLcl(2)(MapLcl(1)(MapLcl(0)(\(m => {

          val maskedValMulti = BoundaryUtilities.maskValueBoundaryPoints(Get(m, 2), RoomConstants.cf(0), RoomConstants.cf(1))
          val maskedValConstOrg = BoundaryUtilities.maskValueBoundaryPoints(Get(m,2), RoomConstants.cf2(0), RoomConstants.cf2(1))
          val valueMask = toPrivate(MapSeq(BoundaryUtilities.idIF)) $ Get(m,2)

          toGlobal(MapSeq(multTuple)) $
            Zip(MapSeq(addTuple) $
              Zip(MapSeq(subtractTuple) $ Zip(
                MapSeq(fun(x => mult(x,RoomConstants.l2))) o
                  ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join() o
                  Map(Map(Map(Slide(3,1)))) o toLocal(MapLcl(2)(MapLcl(1)(MapLcl(0)(id)))) $ Get(m,1), Join() $ weights),
                (MapSeq(multTuple)) $ Zip(
                  ReduceSeq(add, 0.0f) $ Get(m, 0),
                  maskedValConstOrg)
              ),
                (MapSeq(multTuple)) $
                  Zip(MapSeq(fun(x => (subtract(2.0f,x) ))) o MapSeq(fun(y => mult(y,RoomConstants.l2))) $ valueMask, ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Join()
                    o Map(Map(Map(Slide3D(3,1)))) o toLocal(MapLcl(2)(MapLcl(1)(MapLcl(0)(id)))) $ Get(m,1), Join() $ weightsMiddle)))
              ,
              maskedValMulti)
        }
        ) //o prepareData
        )))
        ))) $ Zip3D((Slide(2,2) $ mat1), (Slide3D(4,2) $ mat2), (Slide(2,2) $ mask1))
      })

    try
    {

      val newLambda = SimplifyAndFuse(lambdaNeigh)
      val source = Compile(newLambda)
      /*
      OutputKernelJSON(newLambda,"/home/reese/workspace/sandbox/")
      println(OutputKernelJSON.getJsonString(newLambda))
    */

      val (output: Array[Float], runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))(source, newLambda,  stencilarr3D, stencilarrOther3D, mask3DBP, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
      if (StencilUtilities.printOutput)
      {
        StencilUtilities.printOriginalAndOutput3D(data, output)
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
  def test3DAsymMaskStencilWithAtAndArrayGen(): Unit = {

    val compareData = Array(
      4.9884863f, 7.3179812f, 10.312224f, 13.306468f, 16.30071f, 19.294954f, 22.289196f, 24.618692f,
      7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
      10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
      13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
      16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
      18.630207f, 20.959702f, 23.953945f, 26.948189f, 29.94243f, 32.936672f, 35.93092f, 38.260414f,
      7.3179812f, 9.647477f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 26.948189f,
      9.647477f, 12.0f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.277685f,
      12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      20.959702f, 23.289198f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 40.589905f,
      10.312224f, 12.641721f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 29.94243f,
      12.641721f, 14.999999f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 32.271927f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      23.953945f, 26.283443f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 43.584152f,
      13.306468f, 15.635964f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 32.936672f,
      15.635964f, 18.0f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 35.266174f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      26.948189f, 29.277685f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 46.578396f,
      16.30071f, 18.630209f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 35.93092f,
      18.630209f, 21.0f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 38.260418f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      29.94243f, 32.271927f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 49.57264f,
      19.294954f, 21.62445f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 38.925163f,
      21.62445f, 24.0f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 41.254658f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
      22.289196f, 24.618694f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 41.919403f,
      24.618694f, 26.999998f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 44.2489f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
      25.283442f, 27.612938f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 44.913643f,
      27.612938f, 29.999998f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 47.243145f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
      38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
      28.277685f, 30.60718f, 33.60142f, 36.59567f, 39.589912f, 42.584156f, 45.578396f, 47.90789f,
      30.60718f, 33.0f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.23739f,
      33.60142f, 36.0f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.231632f,
      36.59567f, 39.0f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.225876f,
      39.589912f, 42.0f, 45.0f, 48.0f, 50.999996f, 53.999996f, 56.999996f, 59.220116f,
      41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
      30.60718f, 32.936672f, 35.93092f, 38.925163f, 41.919403f, 44.913643f, 47.90789f, 50.237385f,
      32.936672f, 35.266174f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 52.566883f,
      35.93092f, 38.260418f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 55.561127f,
      38.925163f, 41.254658f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 58.55537f,
      41.919403f, 44.2489f, 47.243145f, 50.23739f, 53.231632f, 56.225876f, 59.220116f, 61.549614f,
      44.248898f, 46.578396f, 49.57264f, 52.566883f, 55.561127f, 58.55537f, 61.549614f, 63.879105f
    )
    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
      "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }", Seq(Int,Int,Int,Int,Int,Int), Int)

    val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int,Float,Float), Float)


    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val arraySig = ArrayType(ArrayType(ArrayType(Int, m), n), o)

    val lambdaNeighAt = fun(
      ArrayType(ArrayType(ArrayType(Float, m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m+2), n+2), o+2),
      (mat1, mat2) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val cf = toPrivate( fun(x => getCF(x,RoomConstants.cf(0), RoomConstants.cf(1))) ) $ Get(m,2)
          val cf2 = toPrivate( fun(x => getCF(x,RoomConstants.cf2(0), RoomConstants.cf2(1))) ) $ Get(m,2)
          val maskedValStencil = RoomConstants.l2

          val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

          val `tile[0][1][1]` = Get(m,1).at(0).at(1).at(1)
          val `tile[1][0][1]` = Get(m,1).at(1).at(0).at(1)
          val `tile[1][1][0]` = Get(m,1).at(1).at(1).at(0)
          val `tile[1][1][2]` = Get(m,1).at(1).at(1).at(2)
          val `tile[1][2][1]` = Get(m,1).at(1).at(2).at(1)
          val `tile[2][1][1]` = Get(m,1).at(2).at(1).at(1)

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          val valueMat1 = Get(m,0)
          val valueMask = toPrivate(BoundaryUtilities.idIF) $ Get(m,2)

          toGlobal(id) o toPrivate(fun( x => mult(x,cf))) o toPrivate(addTuple) $
            Tuple(toPrivate(multTuple) $ Tuple(toPrivate(fun(x => subtract(2.0f,x))) o toPrivate(fun(x => mult(x,RoomConstants.l2))) $ valueMask, `tile[1][1][1]`),
              toPrivate(subtractTuple) $ Tuple(
                toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil,
                toPrivate(fun(x => mult(x,cf2))) $ valueMat1))

        })))
        ) $ Zip3D(mat1, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
      })


/*    OutputKernelJSON(newLambda,"/home/reese/workspace/phd/sandbox/")
    println(OutputKernelJSON.getJsonString(newLambda))
*/
    val newLambda = SimplifyAndFuse(lambdaNeighAt)
    val source = Compile(newLambda)
//    val source = Compile(newLambda, 64,4,2,512,512,404, immutable.Map())
//    println(source)

        val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,newLambda, data, stencilarrOther3D)
        if(StencilUtilities.printOutput)
        {
            StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
        }

        assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
}

}




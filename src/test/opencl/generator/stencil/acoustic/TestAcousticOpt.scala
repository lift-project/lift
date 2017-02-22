package opencl.generator.stencil.acoustic

import ir.ast._
import ir.{ArrayType, TupleType}
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor.{Compile, DeviceCapabilityException, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import rewriting.SimplifyAndFuse

import scala.language.implicitConversions

object TestAcousticOpt {
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

class TestAcousticOpt {

  @Test
  def testTwoGridsThreeCalculationsAsym3DGeneralNoMaskWithOnlyOneWeights(): Unit = {

    val compareData = Array(

    4.375f, 7.75f, 11.125f, 14.5f, 17.875f, 21.25f, 24.625f, 23.5f,
    7.75f, 12.125f, 16.0f, 19.875f, 23.75f, 27.625f, 31.5f, 30.375f,
    11.125f, 16.0f, 19.875f, 23.75f, 27.625f, 31.5f, 35.375f, 33.75f,
    12.0f, 16.875f, 20.25f, 23.625f, 27.0f, 30.375f, 33.75f, 31.125f,
    7.75f, 12.125f, 16.0f, 19.875f, 23.75f, 27.625f, 31.5f, 30.375f,
    12.125f, 17.5f, 21.875f, 26.25f, 30.625f, 35.0f, 39.375f, 38.25f,
    16.0f, 21.875f, 26.25f, 30.625f, 35.0f, 39.375f, 43.75f, 42.125f,
    16.875f, 22.75f, 26.625f, 30.5f, 34.375f, 38.25f, 42.125f, 39.5f,
    11.125f, 16.0f, 19.875f, 23.75f, 27.625f, 31.5f, 35.375f, 33.75f,
    16.0f, 21.875f, 26.25f, 30.625f, 35.0f, 39.375f, 43.75f, 42.125f,
    19.875f, 26.25f, 30.625f, 35.0f, 39.375f, 43.75f, 48.125f, 46.0f,
    20.25f, 26.625f, 30.5f, 34.375f, 38.25f, 42.125f, 46.0f, 42.875f,
    14.5f, 19.875f, 23.75f, 27.625f, 31.5f, 35.375f, 39.25f, 37.125f,
    19.875f, 26.25f, 30.625f, 35.0f, 39.375f, 43.75f, 48.125f, 46.0f,
    23.75f, 30.625f, 35.0f, 39.375f, 43.75f, 48.125f, 52.5f, 49.875f,
    23.625f, 30.5f, 34.375f, 38.25f, 42.125f, 46.0f, 49.875f, 46.25f,
    17.875f, 23.75f, 27.625f, 31.5f, 35.375f, 39.25f, 43.125f, 40.5f,
    23.75f, 30.625f, 35.0f, 39.375f, 43.75f, 48.125f, 52.5f, 49.875f,
    27.625f, 35.0f, 39.375f, 43.75f, 48.125f, 52.5f, 56.875f, 53.75f,
    27.0f, 34.375f, 38.25f, 42.125f, 46.0f, 49.875f, 53.75f, 49.625f,
    21.25f, 27.625f, 31.5f, 35.375f, 39.25f, 43.125f, 47.0f, 43.875f,
    27.625f, 35.0f, 39.375f, 43.75f, 48.125f, 52.5f, 56.875f, 53.75f,
    31.5f, 39.375f, 43.75f, 48.125f, 52.5f, 56.875f, 61.25f, 57.625f,
    30.375f, 38.25f, 42.125f, 46.0f, 49.875f, 53.75f, 57.625f, 53.0f,
    24.625f, 31.5f, 35.375f, 39.25f, 43.125f, 47.0f, 50.875f, 47.25f,
    31.5f, 39.375f, 43.75f, 48.125f, 52.5f, 56.875f, 61.25f, 57.625f,
    35.375f, 43.75f, 48.125f, 52.5f, 56.875f, 61.25f, 65.625f, 61.5f,
    33.75f, 42.125f, 46.0f, 49.875f, 53.75f, 57.625f, 61.5f, 56.375f,
    28.0f, 35.375f, 39.25f, 43.125f, 47.0f, 50.875f, 54.75f, 50.625f,
    35.375f, 43.75f, 48.125f, 52.5f, 56.875f, 61.25f, 65.625f, 61.5f,
    39.25f, 48.125f, 52.5f, 56.875f, 61.25f, 65.625f, 70.0f, 65.375f,
    37.125f, 46.0f, 49.875f, 53.75f, 57.625f, 61.5f, 65.375f, 59.75f,
    31.375f, 39.25f, 43.125f, 47.0f, 50.875f, 54.75f, 58.625f, 54.0f,
    39.25f, 48.125f, 52.5f, 56.875f, 61.25f, 65.625f, 70.0f, 65.375f,
    43.125f, 52.5f, 56.875f, 61.25f, 65.625f, 70.0f, 74.375f, 69.25f,
    40.5f, 49.875f, 53.75f, 57.625f, 61.5f, 65.375f, 69.25f, 63.125f,
    34.75f, 43.125f, 47.0f, 50.875f, 54.75f, 58.625f, 62.5f, 57.375f,
    43.125f, 52.5f, 56.875f, 61.25f, 65.625f, 70.0f, 74.375f, 69.25f,
    47.0f, 56.875f, 61.25f, 65.625f, 70.0f, 74.375f, 78.75f, 73.125f,
    43.875f, 53.75f, 57.625f, 61.5f, 65.375f, 69.25f, 73.125f, 66.5f,
    38.125f, 47.0f, 50.875f, 54.75f, 58.625f, 62.5f, 66.375f, 60.75f,
    47.0f, 56.875f, 61.25f, 65.625f, 70.0f, 74.375f, 78.75f, 73.125f,
    50.875f, 61.25f, 65.625f, 70.0f, 74.375f, 78.75f, 83.125f, 77.0f,
    47.25f, 57.625f, 61.5f, 65.375f, 69.25f, 73.125f, 77.0f, 69.875f,
    35.0f, 43.875f, 47.25f, 50.625f, 54.0f, 57.375f, 60.75f, 54.125f,
    43.875f, 53.75f, 57.625f, 61.5f, 65.375f, 69.25f, 73.125f, 66.5f,
    47.25f, 57.625f, 61.5f, 65.375f, 69.25f, 73.125f, 77.0f, 69.875f,
    42.625f, 53.0f, 56.375f, 59.75f, 63.125f, 66.5f, 69.875f, 61.75f

    )

    val localDimX = 8
    val localDimY = 4
    val localDimZ = 12

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))

    val n = SizeVar("N")
    val m = SizeVar("M")
    val o = SizeVar("O")

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val const1 = constantOriginal(2)

    val lambdaZip3D = fun(
      ArrayType(ArrayType(ArrayType(ArrayType(Float,1), m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m + 2), n + 2), o + 2),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      Float,
      (mat1, mat2, weights, weightsMiddle,c1) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)((fun((m) =>
          MapSeq(toGlobal(fun(x => mult(x,constantOriginal(3))))) o
            MapSeq(addTuple) $
            Zip(MapSeq(addTuple) $
                Zip(toPrivate(MapSeq(fun(x => mult(x,c1)))) $ Get(m,0),
                  (toPrivate(MapSeq(fun(x => mult(x, constantOriginal(0))))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                  Zip(( Join() $ Get(m,1)), Join() $ weights))),
              (toPrivate(MapSeq(fun(x => mult(x,constantOriginal(1))))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                Zip(Join() $ Get(m, 1), Join() $ weightsMiddle)))
            ))))) $ Zip3D(mat1, (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))
      })

    try
    {
      val newLambda = SimplifyAndFuse(lambdaZip3D)

//      OutputKernelJSON(newLambda,"/home/reese/workspace/sandbox/")
//      OutputKernelJSON.printJSON(newLambda)

      val source = Compile(newLambda)

      val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true, true))(source, newLambda,stencilarr3D,stencilarrOther3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D,const1) // stencilarr3D, stencilarr3DCopy, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)

      if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)

      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
    }
    catch
      {
        case e: DeviceCapabilityException =>
          Assume.assumeNoException("Device not supported.", e)
      }
  }


  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneralOneWeights(): Unit = {

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
    val a = SizeVar("A")
    val x = SizeVar("X")
    val y = SizeVar("Y")
    val z = SizeVar("Z")


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
      StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)

      assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

    }
    catch
      {
        case e: DeviceCapabilityException =>
          Assume.assumeNoException("Device not supported.", e)
      }

  }


  @Test
  def test3DConvolutionTile(): Unit = {

    val localDim = 16
    val dim = localDim + 2
    val input3D = StencilUtilities.createDataFloat3DWithPadding(localDim, localDim, localDim)

    val M = SizeVar("M")

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, dim), dim), dim),
      ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length*StencilUtilities.weightsMiddle3D(0).length*StencilUtilities.weightsMiddle3D.length),
      (matrix, weights) => {
        Untile3D() o MapWrg(2)(MapWrg(1)(MapWrg(0)(fun(tile =>

          MapLcl(2)(MapLcl(1)(MapLcl(0)(
            // stencil computation
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
          Slide3D(10,8)  $ matrix
      }
    )

    val lambdaNeigh = fun(
      ArrayType(ArrayType(ArrayType(Float, dim), dim), dim),
      ArrayType(Float, StencilUtilities.slidesize*StencilUtilities.slidesize*StencilUtilities.slidesize),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {
          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() o Join() $ neighbours, weights)
        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val (output: Array[Float], runtime) = Execute(8,8,8,64,64,64, (true, true))(stencil, input3D, StencilUtilities.weightsMiddle3D.flatten.flatten)

    StencilUtilities.printOriginalAndOutput3D(input3D, output)
//    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
  }

  @Test
  def testSimple3DStencilWithAt(): Unit = {

    val localDim = 4
    val dim = localDim + 2

    val input = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (i+j+k+1).toFloat }
    val input3D = StencilUtilities.createFakePaddingFloat3D(input, 0.0f, localDim, localDim)

    val lambdaNeigh = fun(
      ArrayType(ArrayType(ArrayType(Float, dim), dim), dim),
      ArrayType(Float, StencilUtilities.slidesize*StencilUtilities.slidesize*StencilUtilities.slidesize),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {

          val `tile[1][1][1]` = neighbours.at(1).at(1).at(1)

          toGlobal(id) $ `tile[1][1][1]`

        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val source = Compile(lambdaNeigh)
    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,lambdaNeigh, input3D, StencilUtilities.weightsMiddle3D.flatten.flatten)

    StencilUtilities.printOriginalAndOutput3D(input3D, output)

  }


}

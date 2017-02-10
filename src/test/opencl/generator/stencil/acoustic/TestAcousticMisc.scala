package opencl.generator.stencil.acoustic

import ir.ast._
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, DeviceCapabilityException, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import rewriting.SimplifyAndFuse

import scala.language.implicitConversions

object TestAcousticMisc {
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

class TestAcousticMisc {

  /* currently the same, but in case we want to change .. */
  val stencilarr = StencilUtilities.createDataFloat2D(StencilUtilities.stencilSize,StencilUtilities.stencilSize)
  val stencilarrsame = StencilUtilities.createDataFloat2D(StencilUtilities.stencilSize,StencilUtilities.stencilSize)
  val stencilarrCopy = stencilarr.map(x => x.map(y => y * 2.0f))

  /* globals */
  val mask = BoundaryUtilities.createMaskData2D(StencilUtilities.stencilSize)

  /* test iterating and swapping arrays? */
  @Test
  def testTwoGridsThreeCalculationsWithMaskIterate5(): Unit = {

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

          toGlobal(MapSeq(id) o MapSeq(multTuple)) $ Zip(MapSeq(addTuple) $ Zip(MapSeq(addTuple) $ Zip((MapSeq(multTuple)) $ Zip(
            ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 0), weightsMiddle),
            MapSeq(id) $ maskedValConstOrg
          ),
            MapSeq(multTuple) $ Zip(
              ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(Get(Get(m, 0), 0), weights),
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


      val source = Compile(lambdaNeigh)
      for(x <- 1 to 2)
      {
        val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(source, lambdaNeigh, stencilarr, stencilarrsame, mask, StencilUtilities.weights, StencilUtilities.weightsMiddle)
      }

  }

  @Test
  def testTwoGridsThreeCalculationsAsym3DGeneralWithOnlyOneWeights(): Unit = {

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
    val a = SizeVar("A")
    val x = SizeVar("X")
    val y = SizeVar("Y")
    val z = SizeVar("Z")
    val testDim = 5

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)

    val lambdaZip3D = fun(
      ArrayType(ArrayType(ArrayType(ArrayType(Float,1), m), n), o),
      ArrayType(ArrayType(ArrayType(Float, m + 2), n + 2), o + 2),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayType(ArrayType(ArrayType(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
      (mat1, mat2, weights, weightsMiddle) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)((fun((m) =>
          MapSeq(fun(x => mult(x,constantOriginal(3)))) o
            MapSeq(addTuple) $
            Zip(MapSeq(addTuple) $
                Zip(MapSeq(fun(x => mult(x,constantOriginal(2)))) $ Get(m,0),
                  (MapSeq(fun(x => mult(x, constantOriginal(0)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                  Zip(( Join() $ Get(m,1)), Join() $ weights))),
              (MapSeq(fun(x => mult(x,constantOriginal(1)))) o ReduceSeq(add, 0.0f) o Join() o MapSeq(ReduceSeq(add, id $ 0.0f) o MapSeq(multTuple)) o Map(\(tuple => Zip(tuple._0, tuple._1))) $
                Zip(Join() $ Get(m, 1), Join() $ weightsMiddle)))
            ))))) $ StencilUtilities.zip3d(mat1, (Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2))
      })

    try
    {
      val newLambda = SimplifyAndFuse(lambdaZip3D)

//      BoundaryUtilities.writeKernelJSONToFile(newLambda,"/home/reese/workspace/phd/sandbox/")
      val source = Compile(newLambda)

      val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true, true))(source, newLambda,stencilarr3D,stencilarrOther3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D) // stencilarr3D, stencilarr3DCopy, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)

      StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)

    }
    catch
      {
        case e: DeviceCapabilityException =>
          Assume.assumeNoException("Device not supported.", e)
      }
  }

}

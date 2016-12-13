package opencl.generator.stencil.acoustic

import ir.ast._
import ir.{ArrayType, TupleType}
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.language.implicitConversions

object TestSimpleRoomCode {
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

class TestSimpleRoomCode extends TestAcousticStencilBoundaries {


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
      ArrayType(ArrayType(ArrayType(Int, 1), size), size),
      ArrayType(ArrayType(Float, weights(0).length), weights.length),
      ArrayType(ArrayType(Float, weightsMiddle(0).length), weightsMiddle.length),
      (mat1, mat2, mask1, weights, weightsMiddle) => {
        MapGlb((fun((m) => {

          val maskedValMult = maskValue(m, constantBorder(3), constantOriginal(3))
          val maskedValConstSec = maskValue(m, constantBorder(2), constantOriginal(2))
          val maskedValConstOrg = maskValue(m, constantBorder(1), constantOriginal(1))
          val maskedValStencil = maskValue(m, constantBorder(0), constantOriginal(0))
          val orgMat = Get(Get(m, 0), 0)
          val secMat = Get(Get(m, 0), 1)

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
        ) $ Zip(Zip((Join() $ (Slide2D(slidesize, slidestep) $ mat1)), (Join() $ (Slide2D(slidesize, slidestep) $ mat2))), Join() $ mask1)
      })


      for(x <- 1 to 2)
      {
   //     val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrsame, mask, weights, weightsMiddle)
      }

    val (output: Array[Float], runtime) = Execute(stencilarr.length, stencilarr.length)(lambdaNeigh, stencilarr, stencilarrsame, mask, weights, weightsMiddle)

//    if (printOutput) printOriginalAndOutput(stencilarr, output, size)

//    assertArrayEquals(compareData, output, delta)

  }

  /* Test with non-symmetrical array size */

  /* Test with 3D */

  /* Test room like stencil */


}

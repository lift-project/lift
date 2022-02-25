package opencl.generator.stencil.acoustic

import ir.ArrayTypeWSWC
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, DeviceCapabilityException, Execute, _}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._
import rewriting.SimplifyAndFuse

import scala.language.implicitConversions

object TestAcousticOpt extends TestWithExecutor

class TestAcousticOpt {

  @Test
  def testTwoGridsThreeCalculationsAsym3DGeneralNoMaskWithOnlyOneWeights(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val compareData = AcousticComparisonArrays.testTwoGridsThreeCalculationsAsym3DGeneralNoMaskComparisonData8x4x12

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float,1), m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m + 2), n + 2), o + 2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
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

      val (output, runtime) = Execute(2,2,2,2,2,2, (true, true))[Array[Float]](source, newLambda,stencilarr3D,stencilarrOther3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D,const1) // stencilarr3D, stencilarr3DCopy, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)

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
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

   val compareData = AcousticComparisonArrays.testTwoGridsThreeCalculationsWithMaskAsym3DGeneralComparisonData4x6x10

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
     ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float,1),m-2), n-2), o-2),
     ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m ), n ), o ),
     ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), m-2), n-2), o-2),
     ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
     ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
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

      val (output, runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))[Array[Float]](source, newLambda,  stencilarr3D, stencilarrOther3D, mask3D, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
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
  def test3DConvolutionTile(): Unit = {


    val localDimx = 12
    val localDimy = 12
    val localDimz = 16
    val compareData = StencilUtilities.createDataFloat3D(localDimx,localDimy,localDimz)
    val input3D = StencilUtilities.createDataFloat3DWithPadding(localDimx, localDimy, localDimz)

    val M = SizeVar("M")
    val N = SizeVar("N")
    val O = SizeVar("O")

    val stencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),O),
      ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length*StencilUtilities.weightsMiddle3D(0).length*StencilUtilities.weightsMiddle3D.length),
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
          Slide3D(8,6,8,6,10,8)  $ matrix
      }
    )

    val (output, runtime) = Execute(2,2,2,2,2,2, (true, true))[Array[Float]](stencil, input3D, StencilUtilities.weightsMiddle3D.flatten.flatten)

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(input3D, output)
    assertArrayEquals(compareData.flatten.flatten, output, StencilUtilities.stencilDelta)

  }


  @Test
  def testSimple3DStencilWithAt(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val compareData = Array(
      1.0f, 2.0f, 3.0f, 4.0f,
      2.0f, 3.0f, 4.0f, 5.0f,
      3.0f, 4.0f, 5.0f, 6.0f,
      4.0f, 5.0f, 6.0f, 7.0f,
      2.0f, 3.0f, 4.0f, 5.0f,
      3.0f, 4.0f, 5.0f, 6.0f,
      4.0f, 5.0f, 6.0f, 7.0f,
      5.0f, 6.0f, 7.0f, 8.0f,
      3.0f, 4.0f, 5.0f, 6.0f,
      4.0f, 5.0f, 6.0f, 7.0f,
      5.0f, 6.0f, 7.0f, 8.0f,
      6.0f, 7.0f, 8.0f, 9.0f,
      4.0f, 5.0f, 6.0f, 7.0f,
      5.0f, 6.0f, 7.0f, 8.0f,
      6.0f, 7.0f, 8.0f, 9.0f,
      7.0f, 8.0f, 9.0f, 10.0f
    )

    val localDim = 4
    val dim = localDim + 2

    val input = Array.tabulate(localDim,localDim,localDim){ (i,j,k) => (i+j+k+1).toFloat }
    val input3D = StencilUtilities.createFakePaddingFloat3D(input, 0.0f, localDim, localDim)

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim), dim),
      ArrayTypeWSWC(Float, StencilUtilities.slidesize*StencilUtilities.slidesize*StencilUtilities.slidesize),
      (mat, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {

          val `tile[1][1][1]` = neighbours.at(1).at(1).at(1)

          toGlobal(id) $ `tile[1][1][1]`

        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val source = Compile(lambdaNeigh)
    val (output, runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,lambdaNeigh, input3D, StencilUtilities.weightsMiddle3D.flatten.flatten)

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(input3D, output)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def test3DAsymNoMaskStencilWithAt(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val compareData = AcousticComparisonArrays.testTwoGridsThreeCalculationsAsym3DGeneralNoMaskComparisonData8x4x12

    val localDimX = 8
    val localDimY = 4
    val localDimZ = 12

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3D(localDimX, localDimY, localDimZ)

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val lambdaNeighAt = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m-2), n-2), o-2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(Float, StencilUtilities.slidesize*StencilUtilities.slidesize*StencilUtilities.slidesize),
      (mat1, mat2, weights) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

          val `tile[0][1][1]` = Get(m,1).at(0).at(1).at(1)
          val `tile[1][0][1]` = Get(m,1).at(1).at(0).at(1)
          val `tile[1][1][0]` = Get(m,1).at(1).at(1).at(0)
          val `tile[1][1][2]` = Get(m,1).at(1).at(1).at(2)
          val `tile[1][2][1]` = Get(m,1).at(1).at(2).at(1)
          val `tile[2][1][1]` = Get(m,1).at(2).at(1).at(1)

          val stencil =  fun(x => add(x,`tile[0][1][1]`)) o
                         fun(x => add(x,`tile[1][0][1]`)) o
                         fun(x => add(x,`tile[1][1][0]`)) o
                         fun(x => add(x,`tile[1][1][2]`)) o
                         fun(x => add(x,`tile[1][2][1]`)) $ `tile[2][1][1]`

          val valueMat1 = Get(m,0)

          toGlobal(id) o toPrivate(fun( x => mult(x,constantOriginal(3)))) o addTuple $
                    Tuple(addTuple $ Tuple(fun(x => mult(x,constantOriginal(1))) $ `tile[1][1][1]`, fun(x => mult(x,constantOriginal(2))) $ valueMat1),
                      fun(x => mult(x, constantOriginal(0))) $ stencil )

        })))
        ) o PrintType() $ Zip3D(mat1, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2)
      })

    val source = Compile(lambdaNeighAt)

    val (output, runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,lambdaNeighAt, data, stencilarrOther3D, StencilUtilities.weightsMiddle3D.flatten.flatten)

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Test
  def test3DAsymMaskStencilWithAt(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val compareData = AcousticComparisonArrays.testTwoGridsThreeCalculationsWithMaskAsym3DGeneralComparisonData4x6x10
    val localDimX = 4
    val localDimY = 6
    val localDimZ = 10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3D = BoundaryUtilities.createMaskDataAsym3DNoArray(localDimX, localDimY, localDimZ)

    val constantOriginal = Array(1.0f, 2.0f, 1.5f, 0.25f)
    val constantBorder = Array(2.0f, 3.0f, 2.5f, 0.5f)

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float,1),m-2), n-2), o-2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m ), n ), o ),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), m-2), n-2), o-2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
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

    def addP = toPrivate(add)

    val lambdaNeighAt = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m-2), n-2), o-2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, m-2), n-2), o-2),
      (mat1, mat2,mask) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(m => {

          val maskedValMult = BoundaryUtilities.maskValueNoArray(Get(m,2), constantBorder(3), constantOriginal(3))
          val maskedValConstOrg = BoundaryUtilities.maskValueNoArray(Get(m,2), constantBorder(2), constantOriginal(2))
          val maskedValConstSec = BoundaryUtilities.maskValueNoArray(Get(m,2), constantBorder(1), constantOriginal(1))
          val maskedValStencil = BoundaryUtilities.maskValueNoArray(Get(m,2), constantBorder(0), constantOriginal(0))

          val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

          val `tile[0][1][1]` = Get(m,1).at(0).at(1).at(1)
          val `tile[1][0][1]` = Get(m,1).at(1).at(0).at(1)
          val `tile[1][1][0]` = Get(m,1).at(1).at(1).at(0)
          val `tile[1][1][2]` = Get(m,1).at(1).at(1).at(2)
          val `tile[1][2][1]` = Get(m,1).at(1).at(2).at(1)
          val `tile[2][1][1]` = Get(m,1).at(2).at(1).at(1)

          val x = addP(`tile[0][1][1]`,toPrivate(add)(`tile[0][1][1]`,`tile[1][0][1]`))

          val stencil =  toPrivate(fun(x => add(x,`tile[0][1][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][0][1]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][0]`))) o
            toPrivate(fun(x => add(x,`tile[1][1][2]`))) o
            toPrivate(fun(x => add(x,`tile[1][2][1]`))) $ `tile[2][1][1]`

          val valueMat1 = Get(m,0)

          toGlobal(id) o toPrivate(fun( x => mult(x,maskedValMult))) o toPrivate(addTuple) $
            Tuple(toPrivate(addTuple) $ Tuple(toPrivate(fun(x => mult(x,maskedValConstSec))) $ `tile[1][1][1]`, toPrivate(fun(x => mult(x,maskedValConstOrg))) $ valueMat1),
              toPrivate(fun(x => mult(x, maskedValStencil))) $ stencil )

        })))
        ) $ Zip3D(mat1, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, mask)
      })

    val newLambda = SimplifyAndFuse(lambdaNeighAt)
    val source = Compile(newLambda)

    val (output, runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,newLambda, data, stencilarrOther3D, mask3D)

    if(StencilUtilities.printOutput) StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Ignore
  @Test
  def simpleMapTransposeTest(): Unit =
  {
    val dim = 4
    val dimX = dim
    val dimY = dim
    val data = Array.tabulate(dimX,dimY){(i,j) => (i+j+1).toFloat}

    val stencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, dim), dim),
      (matrix) => {
        MapWrg(2)(MapWrg(1)(MapWrg(0)(
          toGlobal(MapSeq(id))
        ))) o Map(Transpose()) o Slide(3,1) o Map(Slide(3,1)) $ matrix
      }
    )

    val (output, runtime) = Execute(4,4,4,4,4,4, (true, true))[Array[Float]](stencil, data)
    StencilUtilities.print2DArray(data)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArrayAs3DArray(output,3,3,4)

  }


  @Test
  def testNumNeighboursUserFun(): Unit = {

    val localDimX = 6
    val localDimY = 8
    val localDimZ = 4

    val input3D =  Array.fill(localDimZ,localDimY,localDimX)(1)

    val mask3DBP = BoundaryUtilities.createMaskDataWithNumBoundaryPts(localDimX+2, localDimY+2, localDimZ+2)

    val idxF = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
       "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }", Seq(Int,Int,Int,Int,Int,Int), Int)

    val inp3d = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, SizeVar("O")), SizeVar("N")), SizeVar("M"))

    val numberOfNeighbours = fun(inp3d,
      input => toGlobal(MapGlb(MapSeq(MapSeq(idI)))) $ Array3DFromUserFunGenerator(idxF, inp3d)
    )

    val (output, _) = Execute(2,2,2,2,2,2,(true,true))[Array[Int]](numberOfNeighbours, input3D)

    if(StencilUtilities.printOutput)
    {
      StencilUtilities.print3DArray(mask3DBP)
      StencilUtilities.print3DArray(input3D)
      StencilUtilities.print1DArrayAs3DArray(output, localDimX, localDimY, localDimZ)
    }
    assertArrayEquals(mask3DBP.flatten.flatten, output)

  }

}




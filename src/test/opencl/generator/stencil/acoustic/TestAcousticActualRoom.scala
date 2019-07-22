package opencl.generator.stencil.acoustic

import ir.ArrayTypeWSWC
import ir.ast._
import ir.ast.debug.PrintType
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._
import rewriting.SimplifyAndFuse

import scala.collection.immutable
import scala.language.implicitConversions

object TestAcousticActualRoom extends TestWithExecutor

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
  def test3DAsymMaskStencilWithAt(): Unit = {

    val localDimX = 8
    val localDimY = 6
    val localDimZ = 10

    val compareData = AcousticComparisonArrays.test3DAsymMaskStencilComparisonData8x6x10

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarr3D = data.map(x => x.map(y => y.map(z => Array(z))))
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)
    val stencilarrOther3D = stencilarrpadded3D.map(x => x.map(y => y.map(z => z * 2.0f)))
    val mask3DBP = BoundaryUtilities.createMaskDataWithNumBoundaryPts(localDimX+2, localDimY+2, localDimZ+2)

    val m = SizeVar("M")
    val n = SizeVar("N")
    val o = SizeVar("O")

    val lambdaNeighAt = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m+2), n+2), o+2),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, m), n), o),
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

        val (output, runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,newLambda, data, stencilarrOther3D, mask3DBP)

        if(StencilUtilities.printOutput)
        {
            StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
            StencilUtilities.print3DArray(mask3DBP)
        }

        assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
  }

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, localDimX+2), localDimY+2), localDimZ+2),
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
          Slide3D(4,2)  $ matrix})

    val newLambda = SimplifyAndFuse(stencilOrg)
    val source = Compile(newLambda)

    val (output, runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,newLambda, stencilarrpadded3D, StencilUtilities.weightsMiddle3D.flatten.flatten)

    assertArrayEquals(data.flatten.flatten, output, StencilUtilities.stencilDelta)

  }

  @Test
  def testTwoGridsThreeCalculationsWithMaskAsym3DGeneralOneWeightsRoom(): Unit = {

    val compareData = AcousticComparisonArrays.test3DAsymMaskStencilComparisonData8x6x10

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
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float,1),m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m+2 ), n+2 ), o+2 ),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, 1), m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weights3D(0)(0).length), StencilUtilities.weights3D(0).length), StencilUtilities.weights3D.length),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, StencilUtilities.weightsMiddle3D(0)(0).length), StencilUtilities.weightsMiddle3D(0).length), StencilUtilities.weightsMiddle3D.length),
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
      /*
      println(source)
      OutputKernelJSON(newLambda,"/home/reese/workspace/sandbox/")
      println(OutputKernelJSON.getJsonString(newLambda))
    */

      val (output, runtime) = Execute(8, 8, 8, 8, 8, 8, (true, true))[Array[Float]](source, newLambda,  stencilarr3D, stencilarrOther3D, mask3DBP, StencilUtilities.weights3D, StencilUtilities.weightsMiddle3D)
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

    val compareData = AcousticComparisonArrays.test3DAsymMaskStencilComparisonData8x6x10

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

    val dx = 256
    val dy = 256
    val dz = 202

    val arraySig = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, dx), dy), dz)

    val lambdaNeighAt = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dx), dy), dz),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, dx+2), dy+2), dz+2),
      (mat1, mat2) => {
        MapGlb(0)(MapGlb(1)(MapGlb(2)(fun(m => {

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

        })))) o PrintType() $ Zip3D(mat1, Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
      })

    val newLambda = SimplifyAndFuse(lambdaNeighAt)
    //val source = Compile(newLambda, 64,4,2,dx,dy,dz, immutable.Map())
    val source = Compile(newLambda)
    println(source)

    /*    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))(source,newLambda, data, stencilarrOther3D)
        if(StencilUtilities.printOutput)
        {
            StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
        }
    StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
    println("*************************** OUT PUT ***********************")

        assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)
        */
}


  @Ignore
  @Test
  def roomCodeUsingMapSeqSlide(): Unit = {

    val compareData = AcousticComparisonArrays.test3DAsymMaskStencilComparisonData8x6x10
    val size = 3
    val step =1
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

    val arraySig = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, m), n), o)

    val lambdaNeighAt = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m+2), n+2), o+2),
      (mat1, mat2) => {
        MapGlb(1)(MapGlb(0)(

          fun(inp => {
            toGlobal(MapSeqSlide(fun( m => {


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


            }),size,step)) /*o Transpose() o Map(Transpose()) */ $ inp
          }))) o PrintType() $  Zip2D( mat1, Map(Map(Transpose())) o Map(Map(Map(Transpose()))) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
      })


    val newLambda = SimplifyAndFuse(lambdaNeighAt)
    val source = Compile(newLambda)
    println(source)

    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,newLambda, data, stencilarrOther3D)

    if(StencilUtilities.printOutput)
    {
      StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
    }

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

  @Ignore
  @Test
  def roomCodeUsingMapSeqSlideSingleOutMiddleFromZip(): Unit =
  {

    val size = 3
    val step =1

    val localDimX = 8
    val localDimY = 8
    val localDimZ = 8

    val data = StencilUtilities.createDataFloat3D(localDimX, localDimY, localDimZ)
    val stencilarrpadded3D = StencilUtilities.createDataFloat3DWithPadding(localDimX, localDimY, localDimZ)

    val getNumNeighbours = UserFun("idxF", Array("i", "j", "k", "m", "n", "o"), "{ " +
      "int count = 6; if(i == (m-1) || i == 0){ count--; } if(j == (n-1) || j == 0){ count--; } if(k == (o-1) || k == 0){ count--; }return count; }", Seq(Int,Int,Int,Int,Int,Int), Int)

    val getCF = UserFun("getCF", Array("neigh", "cfB", "cfI"), "{ if(neigh < 6) { return cfB; } else{ return cfI;} }", Seq(Int,Float,Float), Float)

    val m = SizeVar("M")+2
    val n = SizeVar("N")+2
    val o = SizeVar("O")+2

    val arraySig = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Int, m), n), o)

    val lambdaNeigh = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      (mat) => {
        MapGlb(2)(MapGlb(1)(MapGlb(0)(fun(neighbours => {

          val `tile[1][1][1]` = neighbours.at(1).at(1).at(1)

          toGlobal(id) $ `tile[1][1][1]`

        })))
        ) o Slide3D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat
      })

    val lambdaNeighMapSeqSlide = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m), n), o),
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, m+2), n+2), o+2),
      (mat1, mat2) => {
        MapGlb(1)(MapGlb(0)(
          fun(inp => {
            val firstArr = inp._0
            val secondArr = inp._1
            val thirdArr = inp._2

            toGlobal(MapSeqSlide(fun( m => {

              val leftVal = Get(m,0)
              val `tile[1][1][1]` = Get(m,1).at(1).at(1).at(1)

              toGlobal(id) $ `tile[1][1][1]`

          }),size,step)) o PrintType() $ inp

          })
        )) /*o debug.PrintType()*/ $  Zip2D( mat1, Map(Map(Transpose())) o Map(Map(Map(Transpose()))) o Slide2D(StencilUtilities.slidesize, StencilUtilities.slidestep) $ mat2, Array3DFromUserFunGenerator(getNumNeighbours, arraySig))
      })

    val newLambda = SimplifyAndFuse(lambdaNeighMapSeqSlide)
    val source = Compile(newLambda)
    println(source)

    val (output: Array[Float], runtime) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](source,newLambda, data, stencilarrpadded3D)
    val (compareData: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](lambdaNeigh, stencilarrpadded3D)

      StencilUtilities.printOriginalAndOutput3D(stencilarrpadded3D, output)
      StencilUtilities.print1DArrayAs3DArray(compareData,localDimX,localDimY,localDimZ)

    assertArrayEquals(compareData, output, StencilUtilities.stencilDelta)

  }

}





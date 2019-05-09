package opencl.generator.stencil

import ir.ArrayTypeWSWC
import ir.ast.debug.PrintType
import ir.ast.{ArrayAccess, ArrayFromExpr, Concat, Get, Join, PadConstant, Slide, Transpose, UserFun, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestConcat extends TestWithExecutor

object TestConcatHelpers
{

  val delta = 0.01f

  val slidesize = 3
  val slidestep = 1
  val localSizeX = 6
  val localSizeY = 8
  val N = SizeVar("N")

  val constL = 2.0f
  val constR = 3.0f

  val size = 10

  val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
  val padValue = 0
  val padLR = Array.fill(1)(padValue.toFloat)
  val padValueL = values(0)
  val padValueR = values(values.length-1)
  val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR

}

//noinspection ScalaUnnecessaryParentheses
class TestConcat
{

  /** 1D **/

  @Test
  def zipTwoArraysMapSeq1D(): Unit = {

    val input = Array.fill[Float](2)(1.0f)
    val gold = Array(2.0f,4.0f,2.0f,4.0f)

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    val ziplike = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) =>
         toGlobal(MapSeq(tf_id)) $ Zip(MapSeq(mult2) $ input, MapSeq(add3) $ input)
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](ziplike,input)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  @Test
  def combineTwoArraysMapSeq1D(): Unit = {

    val input = Array.fill[Float](2)(1.0f)
    val gold = Array(2.0f,2.0f,4.0f,4.0f)

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // concat the results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    val concatlike = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) =>
         toGlobal(Concat(2))(MapSeq(mult2) $ input, MapSeq(add3) $ input)
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concatlike, input)

    StencilUtilities.print1DArray(input)
    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  @Test
  def combineThreeArraysMapSeq1D(): Unit = {

    val input = Array.fill[Float](2)(1.0f)
    val gold = Array(2.0f,2.0f,4.0f,4.0f,0.0f,0.0f)

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // mapseq over array of 1s and subtract 1
    // concat results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)
    val subtract1 = UserFun("subtract1", Array("x"), "{ return x-1; }", Seq(Float), Float)

    val concatlike = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) =>
        toGlobal(Concat(3))(MapSeq(mult2) $ input, MapSeq(add3) $ input, MapSeq(subtract1) $ input)
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concatlike,input)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  @Test
  def boundaryTest1D(): Unit = {

    val gold = Array(1.0f,10.0f)

    def boundary = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      (input) => {
            toGlobal(MapSeq(tf_id)) $ Zip(MapSeq(id) $ ArrayFromExpr(input.at(0)), MapSeq(id) $ ArrayFromExpr(input.at(TestConcatHelpers.N-1)))
      })

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](boundary, TestConcatHelpers.values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def boundaryTest2DRow(): Unit = {

    val size = 12

    val values = Array.tabulate(TestConcatHelpers.localSizeX,TestConcatHelpers.localSizeY) { (i,j) => (i*size + j + 1).toFloat }
    val gold = Array(1.0f,61.0f,2.0f,62.0f,3.0f,63.0f,4.0f,64.0f,5.0f,65.0f,6.0f,66.0f,7.0f,67.0f,8.0f,68.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")

    def original2DStencil(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),
      (input) => {
            toGlobal(MapSeq(tf_id)) $ Zip(MapSeq(id) $ input.at(0), MapSeq(id) $ input.at(M-1))
      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original2DStencil(TestConcatHelpers.slidesize,TestConcatHelpers.slidestep),values)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def boundaryTest2DColumnWorkaround(): Unit = {

    val values = Array.tabulate(TestConcatHelpers.localSizeX,TestConcatHelpers.localSizeY) { (i,j) => (i*TestConcatHelpers.localSizeY + j + 1).toFloat }
    val gold = Array(8.0f,16.0f,24.0f,32.0f,40.0f,48.0f)

    val M = SizeVar("M")

    def original2DStencil() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, TestConcatHelpers.N),M),
      (input) => {
        toGlobal(MapSeq(id)) o ArrayAccess(TestConcatHelpers.N-1) o toGlobal(MapSeq(MapSeq(id))) o Transpose() $ input
      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original2DStencil,values)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def boundaryTest2DColumnWeird(): Unit = {

    val values = Array.tabulate(TestConcatHelpers.localSizeX,TestConcatHelpers.localSizeY) { (i,j) => (i*TestConcatHelpers.localSizeY+ j + 1).toFloat }
    val gold = Array(8.0f,16.0f,24.0f,32.0f,40.0f,48.0f)

    val N = SizeVar("N")
    val M = SizeVar("M")

    def original2DStencil() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N),M),
      (input) => {
        MapSeq(id) o ArrayAccess(N-1) o Transpose() $ input//.at(N-1)
      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original2DStencil,values)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def boundaryTest2DRowAgainEvenWeirder(): Unit = {

    val size = 12

    val values = Array.tabulate(TestConcatHelpers.localSizeX,TestConcatHelpers.localSizeY) { (i,j) => (i*size + j + 1).toFloat }
    val gold = Array(1.0f,61.0f,2.0f,62.0f,3.0f,63.0f,4.0f,64.0f,5.0f,65.0f,6.0f,66.0f,7.0f,67.0f,8.0f,68.0f)

    val M = SizeVar("M")

    def original2DStencil() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float,TestConcatHelpers.N),M),
      (input) => {
        val inputT = Transpose() o Transpose() $ input
        toGlobal(MapSeq(tf_id)) $ Zip(MapSeq(id) $ inputT.at(0), MapSeq(id) $ inputT.at(M-1))
      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original2DStencil,values)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def boundaryTest1DTwoPointsWithZip(): Unit = {

    val gold = Array(1.0f,2.0f,9.0f,10.0f)

    def boundary = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      (input) => {
        toGlobal(MapSeq(tf4_id)) $ Zip(MapSeq(id) $ ArrayFromExpr(input.at(0)),MapSeq(id) $ ArrayFromExpr(input.at(1)), MapSeq(id) $ ArrayFromExpr(input.at(TestConcatHelpers.N-2)),MapSeq(id) $ ArrayFromExpr(input.at(TestConcatHelpers.N-1)))
      })

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](boundary, TestConcatHelpers.values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def boundaryTest2DRowTwoRows(): Unit = {

    val size = 12

    val values = Array.tabulate(TestConcatHelpers.localSizeX,TestConcatHelpers.localSizeY) { (i,j) => (i*size + j + 1).toFloat }
    val gold = Array(1.0f,13.0f,49.0f,61.0f,2.0f,14.0f,50.0f,62.0f,3.0f,15.0f,51.0f,63.0f,4.0f,16.0f,52.0f,64.0f,5.0f,17.0f,53.0f,65.0f,6.0f,18.0f,54.0f,66.0f,7.0f,19.0f,55.0f,67.0f,8.0f,20.0f,56.0f,68.0f)

    val M = SizeVar("M")

    def original2DStencil(size: Int, step: Int) = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, TestConcatHelpers.N),M),
      (input) => {
        toGlobal(MapSeq(tf4_id)) $ Zip(MapSeq(id) $ input.at(0), MapSeq(id) $ input.at(1), MapSeq(id) $ input.at(M-2),MapSeq(id) $ input.at(M-1))
      })

    val (output: Array[Float], _) = Execute(2,2,2,2,2,2, (true,true))[Array[Float]](original2DStencil(TestConcatHelpers.slidesize,TestConcatHelpers.slidestep),values)

    assertArrayEquals(output, gold, StencilUtilities.stencilDelta)

  }

  @Test
  def joinValueAndValue(): Unit = {

    val gold = Array[Float](2.0f,30.0f)

    def concatValues() = fun(
      ArrayTypeWSWC(Float, TestConcatHelpers.N),
      (input) => {
        Concat(2)(
          // 1
          toGlobal(MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(0),TestConcatHelpers.constL)),
          // 3
          toGlobal(MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(TestConcatHelpers.N - 1),TestConcatHelpers.constR))
        )
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concatValues(),TestConcatHelpers.values)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  @Test
  def joinSimpleArrayAndValue(): Unit = {

    val gold = TestConcatHelpers.values ++ Array(TestConcatHelpers.constR*TestConcatHelpers.values(TestConcatHelpers.values.length-1))

    def concatArrayAndValue() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      (input) => {
        Concat(2)(
          toGlobal(MapSeq(id)) $ input,
          toGlobal(MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(TestConcatHelpers.N-1),TestConcatHelpers.constR))
        )
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concatArrayAndValue(), TestConcatHelpers.values)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  @Test
  def joinStencilComputeAndValue(): Unit = {

    val padR = Array(TestConcatHelpers.constR*TestConcatHelpers.values(TestConcatHelpers.values.length-1))
    val gold =  TestConcatHelpers.paddedValues.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _)) ++ padR

    def stencil1DConcat() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      (input) => {
        Concat(2)(
          fun(nbr => {
            toGlobal(PrintType() o Join() o MapSeq(MapSeqUnroll(id) o ReduceSeq(absAndSumUp,0.0f))) $ nbr
          }) o Slide(3,1) o PadConstant(1,1,0.0f) $ input,
           toGlobal(MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(TestConcatHelpers.N-1),TestConcatHelpers.constR))
        )
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1DConcat(),TestConcatHelpers.values)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

 @Test
 def joinStencilComputeTupleAndValue(): Unit = {

   val padR = Array(TestConcatHelpers.constR*TestConcatHelpers.values(TestConcatHelpers.values.length-1))
   val gold =  TestConcatHelpers.paddedValues.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _)) ++ padR

   def stencil1DConcatTuple() = fun(
     ArrayTypeWSWC(Float,TestConcatHelpers.N),
     (input) => {
      Concat(2)( Join() o
         MapSeq(fun(tup => {
           val neighbourhood = Get(tup,1)
           toGlobal( MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
         })) $ Zip(input, Slide(3,1) o PadConstant(1,1,0.0f) $ input),
        toGlobal( PrintType() o MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(TestConcatHelpers.N-1),TestConcatHelpers.constR))
      )
     }
   )

   val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1DConcatTuple(), TestConcatHelpers.values)

   assertArrayEquals(gold, output, TestConcatHelpers.delta)

 }

  // concat main stencil with boundary points from original array
  @Test
  def joinMainStencilAndConstantTimesBoundaryBoth1D(): Unit = {

    val padL = Array(TestConcatHelpers.constL*TestConcatHelpers.values(0))
    val padR = Array(TestConcatHelpers.constR*TestConcatHelpers.values(TestConcatHelpers.values.length-1))
    val gold =  padL ++ TestConcatHelpers.paddedValues.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _)) ++ padR

    def stencil1DConcatTuple() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      (input) => {
        Concat(3)(
          toGlobal( PrintType() o MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(0),TestConcatHelpers.constL)),
          Join() o MapSeq(fun(tup => {
            val neighbourhood = Get(tup,1)
            toGlobal( MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          })) $ Zip(input, Slide(3,1) o PadConstant(1,1,0.0f) $ input),
          toGlobal( PrintType() o MapSeq(id)) $ ArrayFromExpr(toPrivate(mult)(input.at(TestConcatHelpers.N-1),TestConcatHelpers.constR))
        )
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1DConcatTuple(), TestConcatHelpers.values)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  // calculate main stencil from one array
  // concat together with original boundary points multiplied by corresponding values in another array
  @Ignore // for now
  @Test
  def joinMainStencilAndOtherArrayTimesOriginalBoundary1D(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val bPts = 3 // boundary points
    val N = SizeVar("N") // number of values in original array (+ pad constant)
    val M = SizeVar("M") // size of boundary ( 3 ) --> need to be able to loop over 3 separately

    val constL = 2.0f
    val constR = 3.0f

    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val boundaryValues = Array.tabulate(bPts) { (i) => (i * 3).toFloat }
    StencilUtilities.print1DArray(boundaryValues)

    val bL = values(0)
    val bR = values(size-1)
    val bAdded = bL + bR
    val padValue = 0
    val padLR = Array.fill(1)(padValue.toFloat)
    val padValueL = values(0)
    val padValueR = values(values.length-1)
    val padL = Array.fill(1)(padValueL.toFloat * constL)
    val padR = Array.fill(1)(padValueR.toFloat * constR)
    val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR
    val gold =  padL ++ paddedValues.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).map(z => z*bAdded) ++ padR


    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      ArrayTypeWSWC(Float,M),
      (input,boundaryA) => {
//        ConcatFunction(
//          toGlobal(MapSeqUnroll(id)) o toPrivate(fun(x => mult(x,input.at(0)))) $ boundaryA,
          MapGlb(0)(fun(tup => {

            val neighbourhood = Get(tup,1)
            val t = Get(tup,0)

            //val main = toPrivate(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood

            toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood

          })) $ boundaryA //Zip(input, Slide(a,b) o PadConstant(1,1,0.0f) $ input)
 //         ,toPrivate(fun(x => mult(x,constR))) $ input.at(N-1))
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values, boundaryValues)

    /*
    // sanity check
    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(gold)
    */
    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }

  /**
   *  TODO:
    *  Concat(point1, point2, point3...etc, main stencil, point n-3,point n-2,point n-1) is not really
    *  a sustainable model
    *  How do we want to compose these?
    ***/

}



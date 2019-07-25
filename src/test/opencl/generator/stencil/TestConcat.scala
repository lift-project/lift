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

  val boundaryA = Array(2.0f, 3.0f, 4.0f)
  val boundaryB = Array(2.5f, 3.5f, 4.5f)

  val slidesize = 3
  val slidestep = 1
  val localSizeX = 6
  val localSizeY = 8
  val M = SizeVar("M")
  val N = SizeVar("N")

  val constL = 2.0f
  val constR = 3.0f

  val size = 10

  val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
  val largeValues = Array.tabulate(size) { (i) => ((i + 1) * 10).toFloat }
  val padValue = 0
  val padLR = Array.fill(1)(padValue.toFloat)
  val padValueL = values(0)
  val padValueR = values(values.length-1)
  val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR

  val padLBoundary = boundaryA.map(x => (TestConcatHelpers.constL*TestConcatHelpers.largeValues(0)) - x)
  val padRBoundary = boundaryB.map(x => (TestConcatHelpers.constR*TestConcatHelpers.largeValues(TestConcatHelpers.largeValues.length-1)) - x)
  val paddedLargeValues = Array(0.0f) ++ TestConcatHelpers.largeValues ++ Array(0.0f)

}

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

  @Test
  def joinMainStencilAndBoundaryUpdateBoundaryAfter(): Unit  = {

    val M = SizeVar("M")

    val boundary = Array(2.0f, 3.0f, 4.0f)

    val padL = Array(boundary(0) * TestConcatHelpers.constL*TestConcatHelpers.values(0))
    val padR = Array(boundary(boundary.length-1)* TestConcatHelpers.constR*TestConcatHelpers.values(TestConcatHelpers.values.length-1))
    val gold =  padL ++ TestConcatHelpers.paddedValues.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _)) ++ padR


    def stencil1DConcatBoundaryExtra() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      ArrayTypeWSWC(Float,M),
      (input,boundaryA) => {
        Concat(3)(
          toGlobal( PrintType() o MapSeq(id)) $ ArrayFromExpr(toPrivate(fun(x => mult(x, boundaryA.at(0)))) o toPrivate(fun(x => mult(x, TestConcatHelpers.constL))) $ input.at(0)),
          Join() o MapSeq(fun(tup => {
            val neighbourhood = Get(tup,1)
            toGlobal( MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          })) $ Zip(input, Slide(3,1) o PadConstant(1,1,0.0f) $ input),
          toGlobal( PrintType() o MapSeq(id)) $ ArrayFromExpr(toPrivate(fun(x => mult(x,boundaryA.at(M-1)))) o toPrivate(fun(x => mult(x,TestConcatHelpers.constR))) $ input.at(TestConcatHelpers.N-1)))
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1DConcatBoundaryExtra(), TestConcatHelpers.values,boundary)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }


  def stencil1DConcatBoundaryExtra() = fun(
    ArrayTypeWSWC(Float,TestConcatHelpers.N),
    ArrayTypeWSWC(Float,TestConcatHelpers.M),
    ArrayTypeWSWC(Float,TestConcatHelpers.M),
    (input,boundaryA,boundaryB) => {
      Concat(3)(
        {
          val inp0 = toPrivate(fun(x => (mult(TestConcatHelpers.constL,x)))) $ input.at(0)
          toGlobal(MapSeq(id)) o PrintType() o MapSeqUnroll(id) o ReduceSeq(subtractUp,inp0) $ boundaryA
        },
        Join() o MapSeq(fun(tup => {
          val neighbourhood = Get(tup,1)
          toGlobal( MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
        })) $ Zip(input, Slide(3,1) o PadConstant(1,1,0.0f) $ input),
        {
          val inpN = toPrivate(fun(x => (mult(TestConcatHelpers.constR,x)))) $ input.at(TestConcatHelpers.N-1)
          toGlobal(MapSeq(id)) o MapSeqUnroll(id) o ReduceSeq(subtractUp,inpN) $ boundaryB
        }
      )
    }
  )

  // first mult boundary value by constant
  // then loop over boundary A/B and subtract from boundary value
  @Ignore
  @Test
  def joinMainStencilAndIterateOverBoundaryAfter(): Unit  = {

    val gold =  TestConcatHelpers.padLBoundary ++ TestConcatHelpers.paddedLargeValues.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _)) ++ TestConcatHelpers.padRBoundary

    def stencil1DConcatBoundaryExtra() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      ArrayTypeWSWC(Float,TestConcatHelpers.M),
      ArrayTypeWSWC(Float,TestConcatHelpers.M),
      (input,boundaryA,boundaryB) => {
        Concat(3)(
          {
            val inp0 = toPrivate(fun(x => (mult(TestConcatHelpers.constL,x)))) $ input.at(0)
            toGlobal(MapSeq(id)) o PrintType() o MapSeqUnroll(id) o ReduceSeq(subtractUp,inp0) $ boundaryA
          },
          Join() o MapSeq(fun(tup => {
            val neighbourhood = Get(tup,1)
            toGlobal( MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          })) $ Zip(input, Slide(3,1) o PadConstant(1,1,0.0f) $ input),
          {
            val inpN = toPrivate(fun(x => (mult(TestConcatHelpers.constR,x)))) $ input.at(TestConcatHelpers.N-1)
            toGlobal(MapSeq(id)) o MapSeqUnroll(id) o ReduceSeq(subtractUp,inpN) $ boundaryB
          }
        )
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1DConcatBoundaryExtra(), TestConcatHelpers.largeValues,TestConcatHelpers.boundaryA,TestConcatHelpers.boundaryB)

     assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  def getIteratedGold(num_iterations : Int, padLeft : Array[Float], padRight : Array[Float], values : Array[Float]): Array[Float] =
  {
    val padLBoundary = Array(2.0f, 3.0f, 4.0f)
    val padRBoundary = Array(2.5f, 3.5f, 4.5f)
    val paddedLargeValues = Array(0.0f) ++ values ++ Array(0.0f)

    var outputValues = paddedLargeValues

    println("gold def start: ")
    StencilUtilities.print1DArray(outputValues)

    for(i <- 1 to num_iterations)
    {
      val leftVal = TestConcatHelpers.constL*outputValues(0)
      val rightVal = TestConcatHelpers.constR*outputValues(outputValues.length-1)

      val updatedPadL = padLBoundary.foldLeft(leftVal)( (acc,y) => acc - y )
      val updatedPadR = padRBoundary.foldLeft(rightVal)( (acc,y) => acc - y )

      outputValues =  outputValues.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _))

      outputValues =  Array(updatedPadL) ++ outputValues ++ Array(updatedPadR)

    }

    outputValues

  }

  // add outer iterative example
  @Test
  def iterateOverMainStencilJoinedWithIterateOverBoundary(): Unit  = {

    val num_iterations = 5

    var inputPadded = TestConcatHelpers.paddedLargeValues
    var input = TestConcatHelpers.largeValues

    val gold = getIteratedGold(num_iterations,TestConcatHelpers.boundaryA,TestConcatHelpers.boundaryB,TestConcatHelpers.largeValues)

    def stencil1DConcatBoundaryExtra() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      ArrayTypeWSWC(Float,TestConcatHelpers.N - 2),
      ArrayTypeWSWC(Float,TestConcatHelpers.M),
      ArrayTypeWSWC(Float,TestConcatHelpers.M),
      (inputPadded,input,boundaryA,boundaryB) => {
        Concat(3)(
          {
            val inp0 = toPrivate(fun(x => (mult(TestConcatHelpers.constL,x)))) $ inputPadded.at(0)
            toGlobal(MapSeq(id)) o PrintType() o MapSeqUnroll(id) o ReduceSeq(subtractUp,inp0) $ boundaryA
          },
          Join() o MapSeq(fun(tup => {
            val neighbourhood = Get(tup,1)
            toGlobal( MapSeqUnroll(id)) o ReduceSeq(sumUp,0.0f) $ neighbourhood
          })) $ Zip(input, Slide(3,1) /* o PadConstant(1,1,0.0f) */ $ inputPadded),
          {
            val inpN = toPrivate(fun(x => (mult(TestConcatHelpers.constR,x)))) $ inputPadded.at(TestConcatHelpers.N-1)
            toGlobal(MapSeq(id)) o MapSeqUnroll(id) o ReduceSeq(subtractUp,inpN) $ boundaryB
          }
        )
      }
    )

    for (i <- 1 to num_iterations) {

      val (output, _) = Execute(2,2)[Array[Float]](stencil1DConcatBoundaryExtra(), inputPadded,input,TestConcatHelpers.boundaryA,TestConcatHelpers.boundaryB)

      inputPadded = output

    }

    assertArrayEquals(gold, inputPadded, TestConcatHelpers.delta)

  }
 /*
    def stencil1DConcatBoundaryExtra() = fun(
      ArrayTypeWSWC(Float,TestConcatHelpers.N),
      ArrayTypeWSWC(Float,M),
      (input,boundaryA) => {

        // this does not work:
        val reduction = PrintType() o ArrayAccess(0) o  ReduceSeq(absAndSumUp,0.0f)  $ boundaryA // produces float
        val reduction_plus = PrintType() o toGlobal(fun(x => add(x, input.at(0)))) $ reduction // produces float
        val output = ArrayFromExpr(reduction_plus)
        toGlobal(MapSeq(id)) o PrintType() $ output

        // this works:
        toGlobal(MapSeq(id)) o MapSeqUnroll(fun(x => add(x,input.at(0)))) o ReduceSeq(absAndSumUp,0.0f) $ boundaryA

        //          toGlobal( PrintType() o MapSeq(id)) $ ArrayFromExpr(toPrivate(fun(x => mult(x,boundaryA.at(M-1)))) o toPrivate(fun(x => mult(x,TestConcatHelpers.constR))) $ input.at(TestConcatHelpers.N-1)))

      }
    )
   */

  @Ignore
  @Test
  def testingGround() : Unit =
  {

    val input = Array(Array(1.0f,2.0f,3.0f,4.0f),Array(5.0f,6.0f,7.0f,8.0f),Array(9.0f,10.0f,11.0f,12.0f))
    val gold = Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f),Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)).flatten

    val sideA = Array(Array(1.3f,2.3f,3.3f))
    val sideB = Array(Array(1.7f,2.7f,3.7f,4.7f))
    val sideC = Array(Array(21.2f,34.1f,67.1f,48.5f,89.9f))

    def concat2DTransposedX() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.N), 1),
      (input,sideA) =>
        Transpose() o
          MapGlb(0)(
            fun(zInp => {
              val inp = Get(zInp,0)
              val side = Get(zInp,1)
              toGlobal(Concat(2))(MapSeq(id) /* o Transpose() */ $ ArrayFromExpr(inp), MapSeq(id) $ side )
            } )) o PrintType() $ Zip( input,Transpose() $ sideA)
    )

    def concat2DTransposedY() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M), 1),
      (input,sideB) =>
          toGlobal(Concat(2))(MapSeq(MapSeq(id)) o PrintType() $ input, MapSeq(MapSeq(id)) o PrintType() $ sideB))


    val (outputX : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2DTransposedX(), input,sideA)
    val (outputY : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2DTransposedY(), input,sideB)
//    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](original2DStencilzip(3,1), input)
//    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](original1DStencil(3,1), input.flatten)
//    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](original2DStencil(3,1), input)
//    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2D, input)

    StencilUtilities.print2DArray(input)
    println("**********************")
    StencilUtilities.print1DArrayAs2DArray(outputX,input.length+2)
    StencilUtilities.print1DArrayAs2DArray(outputY,input.length+1)
    println("**********************")

    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArrayAs2DArray(gold,input(0).length*2)

    assertArrayEquals(gold, outputX, TestConcatHelpers.delta)

  }

  // add 2D example
  @Test
  def concatTwoMatricesMapGlb2D(): Unit = {

    val input = Array(Array(1.0f,2.0f,3.0f),Array(4.0f,5.0f,6.0f),Array(7.0f,8.0f,9.0f))
    val gold = Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f),Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)).flatten

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // concat the results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    def concat2D() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, SizeVar("M")), SizeVar("N")),
      (input) =>
        MapGlb(0)(
          fun(inp => {
            toGlobal(Concat(2))(MapSeq(mult2) $ inp, MapSeq(add3) $ inp )
          } ))  $ input
    )

    //println(Compile(concat2D()))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2D, input)

    StencilUtilities.print2DArray(input)
    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArrayAs2DArray(gold,input(0).length*2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArrayAs2DArray(output,input(0).length*2)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  // add 2D asym example
  // TODO: this
  @Ignore
  @Test
  def concatTwoMatricesMapGlb2DAsym(): Unit = {

    val input = Array(Array(1.0f,2.0f,3.0f),Array(4.0f,5.0f,6.0f),Array(7.0f,8.0f,9.0f))
    val gold = Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f),Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)).flatten

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // concat the results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    def concat2D() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, SizeVar("M")), SizeVar("N")),
      (input) =>
        MapGlb(0)(
          fun(inp => {
            toGlobal(Concat(3))(MapSeq(mult2) $ inp, MapSeq(add3) $ inp, MapSeq(mult2) $ inp )
          } ))  $ input
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2D, input)

    StencilUtilities.print2DArray(input)
    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArrayAs2DArray(gold,input(0).length*2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArrayAs2DArray(output,input(0).length*2)

    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }


  // add 2D example
  @Test
  def concatThreeFrom2DMatrix(): Unit = {

    val size = 6
    val input = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }
//    val gold = Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f),Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)).flatten

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // concat the results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    def concat2D() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.N), TestConcatHelpers.N),
      (input) =>
        MapGlb(0)(
          fun(inp => {
            toGlobal(Concat(3))(MapSeq(mult2) $ ArrayFromExpr(inp.at(0)), MapSeq(add3) $ inp, MapSeq(mult2) $ ArrayFromExpr(inp.at(TestConcatHelpers.N-1)))
          } ))  $ input
    )

    //println(Compile(concat2D()))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2D, input)

    StencilUtilities.print2DArray(input)
    println("******************************")
    StencilUtilities.print1DArray(output)
    println("******************************")
    StencilUtilities.print1DArrayAs2DArray(output,input(0).length*2)

//    assertArrayEquals(gold, output, TestConcatHelpers.delta)

  }

  // add 3D example
  @Test
  def concatTwoCubesMapGlb3D(): Unit = {

    val input3D = Array(Array(Array(1.0f,2.0f,3.0f), Array(4.0f,5.0f,6.0f),Array(7.0f,8.0f,9.0f)),
      Array(Array(10.0f,11.0f,12.0f),Array(13.0f,14.0f,15.0f),Array(16.0f,17.0f,18.0f)),
      Array(Array(19.0f,20.0f,21.0f),Array(22.0f,23.0f,24.0f),Array(25.0f,26.0f,27.0f)))

    val gold3D = Array(Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f), Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)),
      Array(Array(20.0f,22.0f,24.0f,13.0f,14.0f,15.0f),Array(26.0f,28.0f,30.0f,16.0f,17.0f,18.0f),Array(32.0f,34.0f,36.0f,19.0f,20.0f,21.0f)),
      Array(Array(38.0f,40.0f,42.0f,22.0f,23.0f,24.0f),Array(44.0f,46.0f,48.0f,25.0f,26.0f,27.0f),Array(50.0f,52.0f,54.0f,28.0f,29.0f,30.0f))).flatten.flatten

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // concat the results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    def concat3D() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC( Float, SizeVar("M")), SizeVar("N")), SizeVar("O")),
      (input) =>
        MapGlb(1)(MapGlb(0)(
          fun(inp => {
            toGlobal(Concat(2))(MapSeq(mult2) $ inp, MapSeq(add3) $ inp )
          } ))) $ input
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concat3D, input3D)


    println("*******Input**********")
    StencilUtilities.print3DArray(input3D)
    println("*******Gold**********")
    StencilUtilities.print1DArray(gold3D)
    StencilUtilities.print1DArrayAs3DArray(gold3D,input3D(0)(0).length*2,input3D(0).length,input3D.length)
    println("*******Output**********")
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArrayAs3DArray(output,input3D(0)(0).length*2,input3D(0).length,input3D.length)

    assertArrayEquals(gold3D, output, TestConcatHelpers.delta)

  }


  @Ignore
  @Test
  def concatTwoLinesOnFaceMatrix() : Unit = {

    val input = Array(Array(1.0f,2.0f,3.0f,4.0f),Array(5.0f,6.0f,7.0f,8.0f),Array(9.0f,10.0f,11.0f,12.0f))
    val input2 = Array(Array(1.0f,1.0f,1.0f,2.0f,3.0f,4.0f),Array(1.0f,1.0f,5.0f,6.0f,7.0f,8.0f),Array(1.0f,1.0f,9.0f,10.0f,11.0f,12.0f))
    val gold = Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f),Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)).flatten

    val sideA = Array(Array(1.3f,2.3f,3.3f))
    val sideB = Array(Array(1.7f,2.7f,3.7f,4.7f))
    val sideC = Array(Array(14.5f,21.2f,34.1f,67.1f,48.5f,89.9f))

    def concat2DTransposedX() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.N), 1),
      (input,sideA) =>
        Transpose() o
          MapGlb(0)(
            fun(zInp => {
              val inp = Get(zInp,0)
              val side = Get(zInp,1)
              toGlobal(Concat(2))(MapSeq(id) /* o Transpose() */ $ ArrayFromExpr(inp), MapSeq(id) $ side )
            } )) o PrintType() $ Zip( input,Transpose() $ sideA)
    )

    def concat2DTransposedY() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M), 1),
      (input,sideB) =>
        toGlobal(Concat(2))(MapSeq(MapSeq(id)) o PrintType() $ input, MapSeq(MapSeq(id)) o PrintType() $ sideB))

    def concat2DTransposeXYtmp() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.N), 1),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M+1), 1),
      (input,sideA,sideC) =>
        toGlobal(Concat(2))(
          MapSeq(MapSeq(id)) o PrintType() $ sideC,
          Concat(2)(MapSeq(MapSeq(id)) $ sideA,MapSeq(MapSeq(id)) o Transpose() $ input, MapSeq(MapSeq(id)) $ sideA ),
/*         Transpose() o MapSeq(
            fun(zInp => {
              val inp = Get(zInp,0)
              val side = Get(zInp,1)
              toGlobal(Concat(2))( MapSeq(id) $ side, MapSeq(id) /* o Transpose() */ $ ArrayFromExpr(inp), MapSeq(id) $ side )
            } )) o PrintType() $ Zip( input,Transpose() $ sideA),*/
            MapSeq(MapSeq(id)) o PrintType() $ input,
            /*MapSeq(MapSeq(id)) o PrintType() $ input,*/
          MapSeq(MapSeq(id)) o PrintType() $ sideC))


    // this works:
    def concat2DTransposeXYNoTop() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.N), 1),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M+2), 1),
      (input,sideA,sideC) =>
        toGlobal(Concat(2))(
         // PrintType() o MapSeq(MapSeq(id)) $ sideC,
                    MapGlb(0)(
                      fun(zInp => {
                        val row = Get(zInp,0)
                        val side = Get(zInp,1)
                        toGlobal(Concat(3))( MapSeq(id) $ side, MapSeq(id) /* o Transpose() */ $ ArrayFromExpr(row), MapSeq(id) $ side )
                      } )) o PrintType() $ Zip( input,Transpose() $ sideA),
//          MapSeq(MapSeq(id)) o PrintType() $ input,
          MapSeq(MapSeq(id)) o PrintType() $ sideC))

    // ***** THIS IS BROKEN *****
    def concat2DTransposeXYNoBottom() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M),TestConcatHelpers.N),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.N), 1),
      ArrayTypeWSWC(ArrayTypeWSWC( Float, TestConcatHelpers.M+2), 1),
      (input,sideA,sideC) =>
        toGlobal(Concat(2))(
          PrintType() o MapSeq(MapSeq(id)) $ sideC,
          MapGlb(0)(
            fun(zInp => {
              val inp = Get(zInp,0)
              val side = Get(zInp,1)
              toGlobal(Concat(3))( MapSeq(id) $ side, MapSeq(id) /* o Transpose() */ $ ArrayFromExpr(inp), MapSeq(id) $ side )
            } )) o PrintType() $ Zip( input,Transpose() $ sideA)))//,

    //println(Compile(concat2DTransposeXYNoTop()))
    //println(Compile(concat2DTransposeXYNoBottom()))

    val (outputX : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2DTransposedX(), input,sideA)
    val (outputY : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2DTransposedY(), input,sideB)
    val (outputXYnoTop : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2DTransposeXYNoTop(), input,sideA,sideC)
    val (outputXYnoBottom : Array[Float], _) = Execute(2, 2)[Array[Float]](concat2DTransposeXYNoBottom(), input,sideA,sideC)

    println(input.length)
    println("**********************")
    StencilUtilities.print2DArray(input)
    println("**********************")
    StencilUtilities.print1DArrayAs2DArray(outputX,input(0).length+1)
    StencilUtilities.print1DArrayAs2DArray(outputY,input(0).length)
    StencilUtilities.print1DArrayAs2DArray(outputXYnoTop,input(0).length+2)
    StencilUtilities.print1DArrayAs2DArray(outputXYnoBottom,input(0).length+2)
    println("**********************")

    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArrayAs2DArray(gold,input(0).length*2)

    assertArrayEquals(gold, outputX, TestConcatHelpers.delta)


 }

  // add 2D halo around 3D cube
  // ( does not work yet !!)
  // then add ones for YFace and ZFace
  @Ignore
  @Test
  def concatTwoMatricesOnXFace3DCube(): Unit = {

    // main cube
    val input3D = Array(Array(Array(1.0f,2.0f,3.0f), Array(4.0f,5.0f,6.0f),Array(7.0f,8.0f,9.0f)),
      Array(Array(10.0f,11.0f,12.0f),Array(13.0f,14.0f,15.0f),Array(16.0f,17.0f,18.0f)),
      Array(Array(19.0f,20.0f,21.0f),Array(22.0f,23.0f,24.0f),Array(25.0f,26.0f,27.0f)))


    // matrices to concat on
    val sideA = Array(Array(1.3f,2.3f,3.3f),Array(4.3f,5.3f,6.3f),Array(7.3f,8.3f,9.3f))
    val sideB = Array(Array(1.7f,2.7f,3.7f),Array(4.7f,5.7f,6.7f),Array(7.7f,8.7f,9.7f))



    // result cube
    val gold3D = Array(Array(Array(2.0f,4.0f,6.0f,4.0f,5.0f,6.0f),Array(8.0f,10.0f,12.0f,7.0f,8.0f,9.0f), Array(14.0f,16.0f,18.0f,10.0f,11.0f,12.0f)),
      Array(Array(20.0f,22.0f,24.0f,13.0f,14.0f,15.0f),Array(26.0f,28.0f,30.0f,16.0f,17.0f,18.0f),Array(32.0f,34.0f,36.0f,19.0f,20.0f,21.0f)),
      Array(Array(38.0f,40.0f,42.0f,22.0f,23.0f,24.0f),Array(44.0f,46.0f,48.0f,25.0f,26.0f,27.0f),Array(50.0f,52.0f,54.0f,28.0f,29.0f,30.0f))).flatten.flatten

    // mapseq over array of 1s and multiply by 2
    // mapseq over array of 1s and add 3
    // concat the results

    val mult2 = UserFun("mult2", "x", "{ return x*2; }", Float, Float)
    val add3 = UserFun("add3", Array("x"), "{ return x+3; }", Seq(Float), Float)

    def concat3D() = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC( Float, SizeVar("M")), SizeVar("N")), SizeVar("O")),
      (input) =>
        MapGlb(1)(MapGlb(0)(
          fun(inp => {
            toGlobal(Concat(2))(MapSeq(mult2) $ inp, MapSeq(add3) $ inp )
          } ))) $ input
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](concat3D, input3D)


    println("*******Input**********")
    StencilUtilities.print3DArray(input3D)
    println("*******Gold**********")
    StencilUtilities.print1DArray(gold3D)
    StencilUtilities.print1DArrayAs3DArray(gold3D,input3D(0)(0).length*2,input3D(0).length,input3D.length)
    println("*******Output**********")
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArrayAs3DArray(output,input3D(0)(0).length*2,input3D(0).length,input3D.length)

    assertArrayEquals(gold3D, output, TestConcatHelpers.delta)

  }


  // add separate kernel example

  /**
   *  TODO:
    *  - Concat(point1, point2, point3...etc, main stencil, point n-3,point n-2,point n-1) is not really
    *  a sustainable model
    *  How do we want to compose these?
    *
    *  - 2D, 3D ...
    *
    *  - Composability dimension wise, but also "data wise"
    *
    ***/

}



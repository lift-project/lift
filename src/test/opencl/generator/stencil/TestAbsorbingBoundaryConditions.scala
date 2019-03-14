package opencl.generator.stencil

import ir.ast._
import ir.ast.debug.PrintType
import ir.{ArrayType, ArrayTypeWSWC}
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

import scala.language.implicitConversions

object TestAbsorbingBoundaryConditions extends TestWithExecutor

object ABCConstants {

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

object ABCStencilHelpers{

  val O = 2 + SizeVar("O")
  val N = 2 + SizeVar("N")
  val M = 2 + SizeVar("M")

  def original1DStencil(size: Int, step: Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      MapGlb(0)(
        fun(neighbours => {
          toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp, 0.0f) $ neighbours
        })) o Slide(size, step) $ input
  )

  def stencil1D(a: Int, b: Int) = fun(
    ArrayTypeWSWC(Float, N),
    (input) =>
      toGlobal(MapSeqSlide(MapSeqUnroll(id) o ReduceSeqUnroll(absAndSumUp, 0.0f), a, b)) $ input
  )
}

class TestAbsorbingBoundaryConditions
{

  /** 1D **/
  // do stencil, access just boundary points
  @Test
  def accessMainStencilAndBoundary1D(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val bL = values(0)
    val bR = values(size-1)
    val bAdded = bL + bR
    val padValue = 0
    val padLR = Array.fill(1)(padValue.toFloat)
    val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR
    val gold = paddedValues.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).map(z => z*bAdded)

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        MapGlb(0)(fun(neighbourhood => {

//          val main = toPrivate(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          val main = PrintType()  o toPrivate(MapSeqUnroll(id)) o /* toPrivate(id) o*/ ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          val stencil = main.at(0)
          val boundaryL = PrintType() o toPrivate(id) $ input.at(0)
          val boundaryR = toPrivate(id) $ input.at(N-1)

          val returnValue =  toGlobal(id) o toPrivate(fun(x => mult(x,stencil))) o
            toPrivate(fun(x => add(x,boundaryL))) $ boundaryR

           returnValue

          })) o Slide(a,b) o PadConstant(1,1,0.0f) $ input // Zip( , 0.0f) // ArrayFromUserFunGenerator(0,ArrayTypeWSWC(Float,size+2)), ArrayFromValue(input.at(N-1),ArrayTypeWSWC(Float,size+2)))
      }
    )
//    println(Compile(stencil1D(3,1)))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

//    StencilUtilities.print1DArray(values)
//    StencilUtilities.print1DArray(gold)
//    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }

  // do stencil, update boundary points
  // zip together inputs (time-stepping innit)
  // calculate main stencil from one array
  // update boundary values with constants
  // rejoin array
  @Test
  def joinMainStencilAndBoundary1D(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val bL = values(0)
    val bR = values(size-1)
    val bAdded = bL + bR
    val padValue = 0
    val padLR = Array.fill(1)(padValue.toFloat)
    val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR
    val gold = paddedValues.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).map(z => z*bAdded)

    val constL = 2.0f
    val constR = 3.0f

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    val stencilLambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        toGlobal(MapGlb(id)) o Join() o MapGlb(ReduceSeq(add, 0.0f)) o
          Slide(3, 1) o PadConstant(1, 1, 0.0f) $ input
      }
    )

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        PrintType() o
         MapGlb(0)(fun(tup => {

          val neighbourhood = Get(tup,1)
          val t = Get(tup,0)

          //val main = toPrivate(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          val main = /* Join() o  PadConstant(1,1,0.0f) o */ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          val stencil = main.at(0)
          val boundaryL = toPrivate(fun(x => mult(x,constL))) $ input.at(0)
          val boundaryR = toPrivate(fun(x => mult(x,constR))) $ input.at(N-1)

          // try mapping over an ArrayTypeWSWC "stub" with value to be in array
          // Join() requires a 2D array
          // somehow try to form Array(Array(boundaryL),Array(stencil),Array(boundaryR)) -- BUT this needs to be done outside of the Map itself!
          // // otherwise will end up with something like Array(Array(boundaryL, value calculated from stencil 1, boundaryR), Array(boundaryL, value calculated from stencil 2, boundaryR)....)

          // toGlobal(MapSeqUnroll(id)) o Join() $ Value(0.0f,ArrayTypeWSWC(ArrayTypeWSWC(Float, 1),1)) // this works but doesn't do anything useful
          // PadConstant(1,0,0.0f) o toGlobal(MapSeqUnroll(id))  $ main  // this does something unexpected!
           toGlobal(MapSeqUnroll(id))  $ main

        })) $ Zip(input, Slide(a,b) o PadConstant(1,1,0.0f) $ input) // Zip( , 0.0f) // ArrayFromUserFunGenerator(0,ArrayTypeWSWC(Float,size+2)), ArrayFromValue(input.at(N-1),ArrayTypeWSWC(Float,size+2)))

      }
    )
    println(Compile(stencil1D(3,1)))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)

//    assertArrayEquals(gold, output, 0.1f)

  }


  @Test
  def padOutside(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        toGlobal(MapGlb(0)(id)) o PadConstant(2,2,4.2f) o Join() o
        PadConstant(2,2,1.0f) o
          MapGlb(0)(fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          })) o Slide(a,b) o PadConstant(1,1,0.0f) $ input
      }
    )
    println(Compile(stencil1D(3,1)))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

  }

  @Test
  def padFunction(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val idxF = UserFun("times2", Array("i", "n"), "{ return ; }", Seq(Int, Int), Int)

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        toGlobal(MapGlb(0)(id)) o PadFunction(1, 1, (i, n) => id $ input.at(i)) o Join() o
          PadConstant(2,2,1.0f) o
          MapGlb(0)(fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          })) o Slide(a,b) o PadConstant(1,1,0.0f) $ input
      }
    )
    println(Compile(stencil1D(3,1)))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

  }

  // raised as separate issue
  @Test
  def Issue159(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = Array(0.0f,3.0f,0.0f,0.0f,6.0f,0.0f,0.0f,9.0f,0.0f,0.0f,12.0f,0.0f,0.0f,15.0f,0.0f,0.0f,18.0f,0.0f,0.0f,21.0f,0.0f,0.0f,24.0f,0.0f,0.0f,27.0f,0.0f)

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
          MapGlb(0)(fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o PadConstant(1,1,0.0f) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
          })) o Slide(a,b) o PadConstant(1,1,0.0f) $ input
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

//    assertArrayEquals(gold, output, 0.1f)

  }

  // works, but does not do anything useful
  @Test
  def returnShell(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        MapGlb(0)(fun(neighbourhood => {
          toGlobal(MapSeqUnroll(id)) o Join() $ Value(1.0f,ArrayTypeWSWC(ArrayTypeWSWC(Float, 1),1))
        }))  $ input
      }
    )
    println(Compile(stencil1D(3,1)))

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

  }


  @Test
  def stencilPlayground(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2 // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val bL = values(0)
    val bR = values(size-1)
    val bAdded = bL + bR
    val padValue = 0
    val padLR = Array.fill(1)(padValue.toFloat)
    val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR
    val gold = paddedValues.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).map(z => z*bAdded)

    val constL = 2.0f
    val constR = 3.0f

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    val stencilLambda = fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        PadConstant(1,1,0.0f) o toGlobal(MapGlb(id)) o Join() o MapGlb(ReduceSeq(absAndSumUp, 0.0f)) o
          Slide(3, 1) o PadConstant(1, 1, 0.0f) $ input
      }
    )

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float,N),
      (input) => {
        PrintType() o
          MapGlb(0)(fun(neighbourhood => {

            //val main = toPrivate(MapSeqUnroll(id)) o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
            val main = /* Join() o  PadConstant(1,1,0.0f) o */  PrintType() o ReduceSeq(absAndSumUp,0.0f) $ neighbourhood
            val stencil = main.at(0)

            // toGlobal(MapSeqUnroll(id)) o Join() $ Value(0.0f,ArrayTypeWSWC(ArrayTypeWSWC(Float, 1),1)) // this works but doesn't do anything useful
            // PadConstant(1,0,0.0f) o toGlobal(MapSeqUnroll(id))  $ main  // this does something unexpected!
            toGlobal(MapSeqUnroll(id))  $ main

          })) o  Slide(a,b) o PadConstant(1,1,0.0f) $ input // Zip( , 0.0f) // ArrayFromUserFunGenerator(0,ArrayTypeWSWC(Float,size+2)), ArrayFromValue(input.at(N-1),ArrayTypeWSWC(Float,size+2)))

      }
    )
    // println(Compile(stencil1D(3,1)))
  println(Compile(stencilLambda))

   // val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(3,1), values)
    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencilLambda, values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)

    //    assertArrayEquals(gold, output, 0.1f)

  }

  // Try to combine arrays output from two different global(?) maps:
  @Test
  def combineOutputsTwoMaps(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 10
    val N = SizeVar("N")

    val nBpts = 2
    // number of boundary points
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val bL = values(0)
    val bR = values(size - 1)
    val bAdded = bL + bR
    val padValue = 0
    val padLR = Array.fill(1)(padValue.toFloat)
    val paddedValues = padLR ++ Array.tabulate(size) { (i) => (i + 1).toFloat } ++ padLR
    val gold = paddedValues.sliding(slidesize, slidestep).toArray.map(x => x.reduceLeft(_ + _)).map(z => z * bAdded)

    val idxF = UserFun("idxF", Array("i", "n"), "{ return i; }", Seq(Int, Int), Int)

    def stencil1D(a: Int, b: Int) = fun(
      ArrayTypeWSWC(Float, N),
      (input) => {
        Join() o PrintType() o
        MapGlb(0)(fun(neighbourhood => {
          toGlobal(MapSeqUnroll(id)) o /* toPrivate(id) o*/ ReduceSeq(absAndSumUp, 0.0f) $ neighbourhood

        })) o Slide(a, b) o PadConstant(1, 1, 0.0f) $ input // Zip( , 0.0f) // ArrayFromUserFunGenerator(0,ArrayTypeWSWC(Float,size+2)), ArrayFromValue(input.at(N-1),ArrayTypeWSWC(Float,size+2)))
      }
    )

    val (output : Array[Float], _) = Execute(2, 2)[Array[Float]](stencil1D(slidesize, slidestep), values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)



  }



}





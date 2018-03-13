package utils;

import benchmarks.DotProduct
import c.executor.Compile
import ir._
import ir.ast.{PrintType, Split, _}
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor.{Execute, Executor, TestWithExecutor, Utils}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._

import scala.util.Random

object TestVisualization extends TestWithExecutor

class TestVisualization{

  val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}

    @Test
    def psiPaperTestExpression(): Unit = {
        def M = Var("M")
        def N = Var("N")
      val expressionText = " PrintType(visual = true,render = true) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)"
        def expression =  PrintType(visual = true,render = true,expressionText) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)

                val lambda = \(ArrayType(Float, N), input => expression $ input)
        TypeChecker(lambda)
    }

  @Test
  def using3DArrays(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")
    val O = SizeVar("O")
    val expressionText = "MapGlb(id) o PrintType(visual = true,render = true) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true)"
    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, N), M ), O),
      input => MapGlb(id) o PrintType(visual = true,render = true,expressionText) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true) $ input
    )
    TypeChecker(lambda)
    //println(Compile(lambda))
  }


  @Test
  def tupleType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText =" MapGlb(\\(tuple => id(tuple._0))) o PrintType(visual = true,render = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        MapGlb(\(tuple => id(tuple._0))) o PrintType(visual = true,render = true,expressionText) $ Zip(input, input)
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test
  def vectorType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText ="PrintType(visual = true,render = true) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        PrintType(visual = true,render = true,expressionText) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)  $ input
    )

    TypeChecker(lambda)
   // val kernel = Compile(lambda)
    //println(kernel)
  }


  @Test
  def tupleOfTouplesType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expression ="    def lambda = fun(\n      TupleType(ArrayType(ArrayType(TupleType(VectorType(Float,4),ArrayType(TupleType(Float,Float),2)),2),2),Float),\n      input => PrintType(visual = true,render = true) $ input\n    )"
    def lambda = fun(
      TupleType(ArrayType(ArrayType(TupleType(VectorType(Float,4),ArrayType(TupleType(Float,Float),2)),2),2),Float),
      input => PrintType(visual = true,render = true,expression) $ input
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test
  def ArrayOfMatrixes(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expression ="    def lambda = fun(\n      ArrayType(ArrayType(ArrayType(Float,4),4),4),\n      input => PrintType(visual = true,render = true,expression) $ input\n    )"
    def lambda = fun(
      ArrayType(TupleType(ArrayType(ArrayType(Float,4),4),ArrayType(ArrayType(Float,4),4)),2),
      input => PrintType(visual = true,render = true,expression) $ input
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }


  private def dotProd(left: Array[Float], right: Array[Float]): Float = {
    (left,right).zipped.map(_*_).sum
  }

  @Test def addArrayOfVectors_EVALUATION(): Unit = {
    val inputSize = 4
    val numVectors = 6
    val inputArray = Array.fill(numVectors, inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.reduce((x, y) => (x, y).zipped.map(_+_))

    val test = inputArray.transpose.map(_.sum)
    //assertArrayEquals(gold, test, 0.001f)

    var expression = "    val addArrayOfVectorsFun = fun(\n      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar(\"M\")), SizeVar(\"N\")),\n      input => PrintType(true,true,expression) o Join() o PrintType(true) o \n        MapGlb(toGlobal(MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f)) o PrintType(true) o Transpose() o PrintType(true) $ input\n    )"
    val addArrayOfVectorsFun = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      input => PrintType(true,true,expression) o Join() o PrintType(true) o
        MapGlb(toGlobal(MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f)) o PrintType(true) o Transpose() o PrintType(true) $ input
    )

    val kernel = Compile(addArrayOfVectorsFun)
    println(kernel)
  }

  @Test def VECTOR_ADD_SIMPLE_EVALUATION(): Unit = {

    val inputSize = 4

    val N = SizeVar("N")
    var expression = "    val vectorAddFun = fun(\n      ArrayTypeWSWC(Float, N),\n      ArrayTypeWSWC(Float, N),\n      (vector1, vector2) =>\n        PrintType(true,true,expression) o MapSeq(add)  o PrintType(true) $ Zip(vector1, vector2)\n    )"
    val vectorAddFun = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (vector1, vector2) =>
        PrintType(true,true,expression) o MapSeq(add)  o PrintType(true) $ Zip(vector1, vector2)
    )

    val kernel = Compile(vectorAddFun)
    println(kernel)
  }


  @Test def DOT_PRODUCT_SIMPLE_EVALUATION(): Unit = {

    var expression = "    val inputSize = 4\n    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)\n    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)\n\n\n    val N = SizeVar(\"N\")\n\n    val dotProductSimple = fun(ArrayTypeWSWC(Float, N),\n      ArrayTypeWSWC(Float, N), (left, right) => {\n        PrintType(true,true,expression) o Join() o\n          PrintType(true) o\n          MapWrg(\n           PrintType(true) o Join() o PrintType(true)  o\n             MapLcl(\n               toGlobal(MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f) o PrintType(true) o MapSeq(mult) o PrintType(true) \n             ) o\n             PrintType(true) o Split(4) o PrintType(true)\n          ) o\n          PrintType(true) o Split(inputSize) o PrintType(true) $ Zip(left, right)\n      })"

    val inputSize = 4
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)


    val N = SizeVar("N")

    val dotProductSimple = fun(ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N), (left, right) => {
        PrintType(true,true,expression) o Join() o
          PrintType(true) o
          MapWrg(
           PrintType(true) o Join() o PrintType(true)  o
             MapLcl(
               toGlobal(MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f) o PrintType(true) o MapSeq(mult) o PrintType(true)
             ) o
             PrintType(true) o Split(4) o PrintType(true)
          ) o
          PrintType(true) o Split(inputSize) o PrintType(true) $ Zip(left, right)
      })

    TypeChecker(dotProductSimple)
    //val (output, runtime) = Execute(inputSize)[Array[Float]](dotProductSimple, leftInputData, rightInputData)

    //println("output.length = " + output.length)
   // println("output(0) = " + output(0))
   // println("runtime = " + runtime)

  //  assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)

  }

  @Test def MATRIX_MATRIX_SIMPLE_EVALUATION(): Unit = {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val Msize = 64
    val Ksize = 128
    val Nsize = 256
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val expression = "    val f = fun(\n      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),\n      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),\n      (A, B) => {\n        PrintType(true,true,expression) o MapGlb(fun( Arow =>\n          PrintType(true) o Join() o PrintType(true)  o MapSeq(fun( Bcol =>\n             toGlobal( MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f) o PrintType(true) o MapSeq(mult) o PrintType(true) $ Zip(Arow, Bcol)\n          )) o PrintType(true) o Transpose() $ B\n        )) o PrintType(true) $ A\n      })"
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        PrintType(true,true,expression) o MapGlb(fun( Arow =>
          PrintType(true) o Join() o PrintType(true)  o MapSeq(fun( Bcol =>
             toGlobal( MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f) o PrintType(true) o MapSeq(mult) o PrintType(true) $ Zip(Arow, Bcol)
          )) o PrintType(true) o Transpose() $ B
        )) o PrintType(true) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }



  /* **********************************************************
    UTILS
 ***********************************************************/
  // boundary condition implemented in scala to create gold versions
  val SCALABOUNDARY = Utils.scalaClamp
  val BOUNDARY = Pad.Boundary.Clamp
  val data = Array.tabulate(5)(_ * 1.0f)
  val randomData = Seq.fill(1024)(Random.nextFloat()).toArray

  @Test def simple3Point1DStencil_EVALUATION(): Unit = {
    val expression = "    val weights = Array(1, 2, 1).map(_.toFloat)\n    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)\n\n    val stencil = fun(\n      //ArrayType(Float, inputLength), // more precise information\n      ArrayType(Float, Var(\"N\", StartFromRange(2))),\n      ArrayType(Float, weights.length),\n      (input, weights) => {\n        PrintType(true,true,expression) o Join() o PrintType(true) o  MapGlb(\n          PrintType(true) o\n          fun(neighbourhood => {\n            toGlobal(MapSeqUnroll(id)) o\n              ReduceSeqUnroll(\n                  fun((acc, y) =>\n                    { multAndSumUp.apply(acc, Get(y, 0), Get(y, 1)) }), 0.0f) o PrintType(true) $ Zip(weights, neighbourhood)\n          })\n        ) o PrintType(true) o Slide(3, 1) o PrintType(true) o Pad(1, 1, BOUNDARY) o PrintType(true) $ input\n      }\n    )"
    val weights = Array(1, 2, 1).map(_.toFloat)
    val gold = Utils.scalaCompute1DStencil(randomData, 3, 1, 1, 1, weights, SCALABOUNDARY)

    val stencil = fun(
      //ArrayType(Float, inputLength), // more precise information
      ArrayType(Float, Var("N", StartFromRange(2))),
      ArrayType(Float, weights.length),
      (input, weights) => {
        PrintType(true,true,expression) o Join() o PrintType(true) o  MapGlb(
          PrintType(true) o
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(
                  fun((acc, y) =>
                    { multAndSumUp.apply(acc, Get(y, 0), Get(y, 1)) }), 0.0f) o PrintType(true) $ Zip(weights, neighbourhood)
          })
        ) o PrintType(true) o Slide(3, 1) o PrintType(true) o Pad(1, 1, BOUNDARY) o PrintType(true) $ input
      }
    )

    val (output, _) = Execute(randomData.length)[Array[Float]](stencil, randomData, weights)
    assertArrayEquals(gold, output, 0.1f)
  }




  @Test def tiledMatrixMultiply_MOTIVATION_TYPE_EXSAMPLE(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

     val N = SizeVar("N")
     val M = SizeVar("M")
     val K = SizeVar("K")

    val mSize = 16
    val kSize = 16
    val nSize = 16
    val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)

    val tileSize = 4

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten
    val expression = "      (A, B) => {\n        // Undo the tiling\n        PrintType(true,true,expression) o Untile2D() o\n          PrintType(true) o MapWrg(0)(fun( aRows =>\n            PrintType(true) o MapWrg(1)(fun( bCols =>\n\n              // Reduce the partial results (matrices), so that the reduce is innermost\n              PrintType(true) o MapLcl(0)(PrintType(true) o Join() o PrintType(true) o  MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()) o PrintType(true) o  Transpose())  o Transpose()  o PrintType(true) o\n\n                // Multiply all necessary combinations of tiles\n                 PrintType(true) o MapSeq(fun( tiles =>\n                  MapLcl(0)( fun(aTile =>\n                    MapLcl(1)( fun( bTile =>\n                         toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(aTile, bTile)\n                    )) $ Get(tiles, 1)\n                  )) $ Get(tiles, 0)\n                )) o PrintType(true) $ Zip(aRows, bCols)\n\n              // Tile the matrices\n            )) o PrintType(true) o Tile(tileSize)o PrintType(true) $ B\n          )) o Tile(tileSize) $ A"
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N),
      (A, B) => {
        // Undo the tiling
        PrintType() o Untile2D() o
          PrintType() o MapWrg(0)(fun( aRows =>
            PrintType() o MapWrg(1)(fun( bCols =>

              // Reduce the partial results (matrices), so that the reduce is innermost
              PrintType() o MapLcl(0)(PrintType() o Join() o PrintType() o  MapLcl(1)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join()) o PrintType() o  Transpose())  o Transpose()  o PrintType() o

                // Multiply all necessary combinations of tiles
                 PrintType() o MapSeq(fun( tiles =>
                  MapLcl(0)( fun(aTile =>
                    MapLcl(1)( fun( bTile =>
                         toGlobal(MapSeq(id)) o ReduceSeq(fun((acc, y) => multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))), 0.0f)  $ Zip(aTile, bTile)
                    )) $ Get(tiles, 1)
                  )) $ Get(tiles, 0)
                )) o PrintType() $ Zip(aRows, bCols)

              // Tile the matrices
            )) o PrintType(true,true) o Tile(tileSize)o PrintType() $ B
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(4, 4, mSize, nSize, (false, false))[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }

}



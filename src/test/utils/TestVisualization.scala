package utils;

import ir._
import ir.ast.debug.{PrintType, VisualOutput}
import ir.ast.{Split, _}
import lift.arithmetic.{SizeVar, Var}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit._


object TestVisualization extends TestWithExecutor

class TestVisualization{

  val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}

    @Test
    @Ignore
    def psiPaperTestExpression(): Unit = {
        def M = Var("M")
        def N = Var("N")
      val expressionText = " PrintType(visual = true,render = true) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)"
        def expression =  PrintType(VisualOutput(render = true,expressionText)) o Join() o  PrintType(VisualOutput()) o Map(Reduce(add, 0.0f))o PrintType(VisualOutput()) o Split(M) o PrintType(VisualOutput())

                val lambda = \(ArrayType(Float, N), input => expression $ input)
        TypeChecker(lambda)
    }

  @Test
  @Ignore
  def using3DArrays(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")
    val O = SizeVar("O")
    val expressionText = "MapGlb(id) o PrintType(visual = true,render = true) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true)"
    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, N), M ), O),
      input => MapGlb(id) o PrintType(VisualOutput(render = true,expressionText)) o Join() o PrintType(VisualOutput()) o Join() o PrintType(VisualOutput()) $ input
    )
    TypeChecker(lambda)
    //println(Compile(lambda))
  }


  @Test
  @Ignore
  def tupleType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText =" MapGlb(\\(tuple => id(tuple._0))) o PrintType(visual = true,render = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        MapGlb(\(tuple => id(tuple._0))) o PrintType(VisualOutput(render = true,expressionText)) $ Zip(input, input)
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test
  @Ignore
  def vectorType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText ="PrintType(visual = true,render = true) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        PrintType(VisualOutput(render = true,expressionText)) o MapGlb(toGlobal(idF4)) o PrintType(VisualOutput())  o asVector(4) o PrintType(VisualOutput())  $ input
    )

    TypeChecker(lambda)
   // val kernel = Compile(lambda)
    //println(kernel)
  }


  @Test
  @Ignore
  def ArrayOfMatrixes(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expression ="    def lambda = fun(\n      ArrayType(ArrayType(ArrayType(Float,4),4),4),\n      input => PrintType(visual = true,render = true,expression) $ input\n    )"
    def lambda = fun(
      ArrayType(TupleType(ArrayType(ArrayType(Float,4),4),ArrayType(ArrayType(Float,4),4)),2),
      input => PrintType(VisualOutput(render = true, expression)) $ input
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }


  @Ignore
  @Test def addArrayOfVectors_EVALUATION(): Unit = {
    val inputSize = 4
    val numVectors = 6
    val inputArray = Array.fill(numVectors, inputSize)(util.Random.nextInt(5).toFloat)

    val gold = inputArray.reduce((x, y) => (x, y).zipped.map(_+_))

    val test = inputArray.transpose.map(_.sum)
    //assertArrayEquals(gold, test, 0.001f)

    var expression = "ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar(\"M\")), SizeVar(\"N\")),\ninput => PrintType(true,true,expression) o Join() o PrintType(true) o\n         MapGlb(toGlobal(MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f)) o PrintType(true) o Transpose() o PrintType(true) $ input"
    val addArrayOfVectorsFun = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      input => PrintType(VisualOutput(render = true,expression)) o Join() o PrintType(VisualOutput()) o
        MapGlb(toGlobal(MapSeq(id)) o PrintType(VisualOutput()) o ReduceSeq(add, 0.0f)) o PrintType(VisualOutput()) o Transpose() o PrintType(VisualOutput()) $ input
    )

    val kernel = Compile(addArrayOfVectorsFun)
    println(kernel)
  }


  @Ignore
  @Test def MATRIX_MATRIX_EVALUATION(): Unit = {

    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val Msize = 64
    val Ksize = 128
    val Nsize = 256
    val matrixA = Array.tabulate(Msize, Ksize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
    val matrixB = Array.tabulate(Ksize, Nsize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
    val expression = "ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),\nArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),\n(A, B) => {\n PrintType(true,true,expression) o MapGlb(fun( Arow =>\n  PrintType(true) o Join() o PrintType(true)  o MapSeq(fun( Bcol =>\n   toGlobal( MapSeq(id)) o PrintType(true) o ReduceSeq(add, 0.0f) o PrintType(true) o MapSeq(mult) o PrintType(true) $ Zip(Arow, Bcol)\n  )) o PrintType(true) o Transpose() $ B\n )) o PrintType(true) $ A\n}"
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        PrintType(VisualOutput(render = true,expression)) o MapGlb(fun( Arow =>
          PrintType(VisualOutput()) o Join() o PrintType(VisualOutput())  o MapSeq(fun( Bcol =>
             toGlobal( MapSeq(id)) o PrintType(VisualOutput()) o ReduceSeq(add, 0.0f) o PrintType(VisualOutput()) o MapSeq(mult) o PrintType(VisualOutput()) $ Zip(Arow, Bcol)
          )) o PrintType(VisualOutput()) o Transpose() $ B
        )) o PrintType(VisualOutput()) $ A
      })

    val (output, _) = Execute(Msize * Nsize)[Array[Float]](f, matrixA, matrixB)

    val gold = Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

    assertArrayEquals(gold, output, 0.0f)
  }


  @Ignore
  @Test
  def strongNesting_EVALUATION(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    def lambda = fun(
      TupleType(ArrayType(ArrayType(TupleType(VectorType(Float,4),ArrayType(TupleType(Float,Float),2)),2),2),Float),
      input => PrintType(VisualOutput(render = true)) $ input
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }


  @Ignore
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
            )) o PrintType(VisualOutput(render = true)) o Tile(tileSize)o PrintType() $ B
          )) o Tile(tileSize) $ A
      })

    val (output, _) = Execute(4, 4, mSize, nSize, (false, false))[Array[Float]](f, matrixA, matrixB.transpose)
    assertArrayEquals(gold, output, 0.0001f)
  }


}




package opencl.executor

import ir.ast.Lambda

object Utils {

  /*
   * Matrix printing methods
   */

  def myPrint(m: Array[Array[Array[Float]]]): Unit = {
    m.foreach( r => {
      println(r.map( e => {
        "(" + e.map("%2.0f".format(_)).reduce(_ + ", " + _) + ")"
      }).reduce(_ + " " + _))
    } )
  }

  def myPrint(m: Array[Array[Float]]): Unit = {
    m.foreach( r => {
      println(r.map("%2.0f".format(_)).reduce(_ + " " + _))
    } )
  }

  def myPrint(m: Array[Float], cols: Int): Unit = {
    val (row, rest) = m.splitAt(cols)
    if (row.nonEmpty) println(row.map("%2.0f".format(_)).reduce(_ + " " + _))
    if (rest.nonEmpty) myPrint(rest, cols)
  }

  def printRow(r: Array[Float], elems: Int): Unit = {
    val (elem, rest) = r.splitAt(elems)
    if (elem.nonEmpty) print("(" + elem.map("%2.0f".format(_)).reduce(_ + ", " + _) + ") ")
    if (rest.nonEmpty) printRow(rest, elems)
  }

  def myPrint(m: Array[Float], cols: Int, elems: Int): Unit = {
    val (row, rest) = m.splitAt(cols*elems)
    if (row.nonEmpty) printRow(row, elems); println("")
    if (rest.nonEmpty) myPrint(rest, cols, elems)
  }


  def add(A: Array[Array[Float]], B: Array[Array[Float]]) = {
    if (A.length != B.length || A.head.length != B.length)
      throw new IllegalArgumentException

    (A, B).zipped.map((x, y) => (x, y).zipped.map(_+_))
  }

  /*
   * Matrix multiplication methods
   */

  def matrixMatrixPatternMultiply(A: Array[Array[Float]], B: Array[Array[Float]]): Array[Array[Float]] = {
    val Bt = B.transpose
    A.map( Arow =>
      Bt.map( Bcol => (Arow, Bcol).zipped.map(_ * _).sum )
    )
  }

  def matrixMatrixMultiply(A: Array[Array[Float]], B: Array[Array[Float]]) :  Array[Array[Float]] = {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val res =  Array.ofDim[Float](aRows, bCols)

    if (A.head.length != B.length)
      throw new IllegalArgumentException

    @inline def computeRow(row: Int) {
      // while statements are much faster than for statements
      var col = 0
      while(col < bCols) { var i = 0; var sum = 0.0f
        while(i < aCols) {
          sum += A(row)(i) * B(i)(col)
          i += 1
        }

        res(row)(col) = sum
        col += 1
      }
    }

    (0 until aRows).par.foreach( computeRow )

    res
  }

  def matrixMatrixMultiply(A: Array[Array[Float]],
                           B: Array[Array[Float]],
                           C: Array[Array[Float]],
                           alpha: Float,
                           beta: Float) :  Array[Array[Float]] = {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val res =  Array.ofDim[Float](aRows, bCols)

    if (A.head.length != B.length)
      throw new IllegalArgumentException

    @inline def computeRow(row: Int) {
      // while statements are much faster than for statements
      var col = 0
      while(col < bCols) { var i = 0; var sum = 0.0f
        while(i < aCols) {
          sum += A(row)(i) * B(i)(col)
          i += 1
        }

        res(row)(col) =  alpha * sum + C(row)(col) * beta
        col += 1
      }
    }

    (0 until aRows).par.foreach( computeRow )

    res
  }

  def matrixVector(matrix: Array[Array[Float]], vector: Array[Float]): Array[Float] = {
    matrix.map(
      (row) => (row, vector).zipped.map(_ * _).sum
    )
  }

  def matrixVector(matrix: Array[Array[Float]], vectorX: Array[Float], vectorY: Array[Float]): Array[Float] = {
    val tmp = matrix.map(
      (row) => (row, vectorX).zipped.map(_ * _).sum
    )
    (tmp, vectorY).zipped.map(_ + _)
  }

  def matrixVector(matrix: Array[Array[Float]], vector: Array[Float], alpha: Float): Array[Float] = {
    matrix.map(
      (row) => (row, vector).zipped.map(_ * _).sum * alpha
    )
  }

  def matrixVector(matrix: Array[Array[Float]], vectorX: Array[Float], vectorY: Array[Float], alpha: Float, beta: Float): Array[Float] = {
    val tmp = matrix.map(
      (row) => (row, vectorX).zipped.map(_ * _).sum * alpha
    )

    val scaledY = vectorY.map(_ * beta)

    (tmp, scaledY).zipped.map(_ + _)
  }

  /*
   * Some helper methods for execution
   */

  def execute(f: Lambda, values: Seq[Any], localSize: Int, globalSize: Int,
              injectSizes: (Boolean, Boolean) = (false, false)): (Array[Float], Double, String) = {
    execute(f, values, localSize, 1, 1, globalSize, 1, 1, injectSizes)
  }

  def execute(f: Lambda, values: Seq[Any],
              localSize1: Int, localSize2: Int, globalSize1: Int,  globalSize2: Int,
              injectSizes: (Boolean, Boolean)): (Array[Float], Double, String) = {
    execute(f, values, localSize1, localSize2, 1, globalSize1, globalSize2, 1, injectSizes)
  }

  def execute(f: Lambda, values: Seq[Any],
              localSize1: Int, localSize2: Int, localSize3: Int,
              globalSize1: Int,  globalSize2: Int, globalSize3: Int,
              injectSizes: (Boolean, Boolean)): (Array[Float], Double, String) = {

    val code = compile(f, values, localSize1, localSize2, localSize3,
      globalSize1, globalSize2, globalSize3,
      injectSizes)

    val (output: Array[Float], runtime) = Execute(localSize1, localSize2, localSize3,
                                                  globalSize1, globalSize2, globalSize3,
                                                  injectSizes)(code, f, values:_*)

    (output, runtime, code)
  }

  def compile(f: Lambda, values: Seq[Any],
              localSize1: Int, localSize2: Int, localSize3: Int,
              globalSize1: Int,  globalSize2: Int, globalSize3: Int,
              injectSizes: (Boolean, Boolean)) : String = {
    val valueMap = Execute.createValueMap(f, values:_*)

    if (injectSizes._1)
      if (injectSizes._2)
        Compile(f, localSize1, localSize2, localSize3,
          globalSize1, globalSize2, globalSize3, valueMap)
      else
        Compile(f, localSize1, localSize2, localSize3)
    else
      Compile(f)
  }
}

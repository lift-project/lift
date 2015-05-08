package opencl.generator

import ir.Lambda
import opencl.executor.{Compile, Execute}

object TestUtils {
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
    val valueMap = Execute.createValueMap(f, values:_*)

    var code = ""

    if (injectSizes._1)
      if (injectSizes._2)
        code = Compile(f, localSize1, localSize2, localSize3,
          globalSize1, globalSize2, globalSize3, valueMap)
      else
        code = Compile(f, localSize1, localSize2, localSize3)
    else
      code = Compile(f)

    val (output: Array[Float], runtime) = Execute(localSize1, localSize2, localSize3,
                                                  globalSize1, globalSize2, globalSize3,
                                                  injectSizes)(code, f, values:_*)

    (output, runtime, code)
  }
}

package benchmarks

import apart.arithmetic.Var

import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

class Stencil2D(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("Stencil2D", Seq(1024, 1024), f, 0.01f) {


  override def runScala(inputs: Any*): Array[Float] = {
    Stencil2D.runScala(inputs(0).asInstanceOf[Array[Array[Float]]])
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)
    //val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 0.1f)
    val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => util.Random.nextFloat())

    Seq(inputData, Stencil2D.weights)
  }

  override def globalSize: Array[Int] = {
    Array(inputSizes()(0),inputSizes()(1),1)
  }
}

object Stencil2D{

  val scalaClamp = (idx: Int, length: Int) => {
    if(idx<0) 0 else if(idx>length-1) length-1 else idx
  }

  val scalaWrap = (idx: Int, length: Int) => {
    (idx % length + length) % length
  }

  val scalaMirror = (idx: Int, length: Int) => {
    val id = (if(idx < 0) -1-idx else idx) % (2*length)
    if(id >= length) length+length-id-1 else id
  }

  val leftHalo = 1
  val center = 1
  val rightHalo = 1
  val scalaBoundary = scalaWrap
  val makePositive = UserFun("makePositive", "i", "{ return (i < 0) ? 0 : i;  }", Float, Float)
  val weights = Array(0f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)

  def runScala(input: Array[Array[Float]]): Array[Float] = {
   // scala2DStencil(input, leftHalo, center, rightHalo, leftHalo, center, rightHalo, weights)
    input.flatten
  }

  /*
  def scala2DStencil(data: Array[Array[Float]],
                     l1: Int, c1: Int, r1: Int,
                     l2: Int, c2: Int, r2: Int,
                     weights: Array[Float]): Array[Float] = {
    val nrRows = data.length
    val nrColumns = data(0).length

    val neighbours: IndexedSeq[Array[Float]] = (0 until nrRows).flatMap(row =>
      (0 until nrColumns).map(column =>
        scala2DNeighbours(data, l1, c1, r1, l2, c2, r2, row, column))
    )

    val clamp = (x: Float) => if(x < 0.0f) 0.0f else x

    neighbours.map(_.zip(weights).foldLeft(0.0f)((acc, p) => acc + p._1 * p._2)).toArray.map(clamp(_))
  }

  def scala2DNeighbours(data: Array[Array[Float]],
                        l1: Int, c1: Int, r1: Int,
                        l2: Int, c2: Int, r2: Int,
                        r: Int,
                        c: Int,
                        boundary: (Int, Int) => Int = scalaBoundary) = {
    val nrRows = data.length
    val nrColumns = data(0).length

    relRows.flatMap(x => {
      var newR = boundary(r + x, nrRows)

      relColumns.map(y => {
        var newC = boundary(c + y, nrRows)
        data(newR)(newC)
      })
    })
  }
  */

  def ninePoint2DStencil(boundary: Pad.BoundaryFun): Lambda = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, 9),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(makePositive)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Group2D(leftHalo, center, rightHalo) o Pad2D(Math.max(leftHalo, rightHalo), boundary)$ matrix
      })

  def apply() = new Stencil2D(
    Seq(
      ("9_POINT_2D_STENCIL_CLAMP", Array[Lambda](ninePoint2DStencil(Pad.Boundary.Clamp))),
      ("9_POINT_2D_STENCIL_MIRROR_UNSAFE", Array[Lambda](ninePoint2DStencil(Pad.Boundary.MirrorUnsafe))),
      ("9_POINT_2D_STENCIL_WRAP", Array[Lambda](ninePoint2DStencil(Pad.Boundary.Wrap))),
      ("9_POINT_2D_STENCIL_MIRROR", Array[Lambda](ninePoint2DStencil(Pad.Boundary.Mirror)))))

  def main(args: Array[String]) = {
    Stencil2D().run(args)
  }
}

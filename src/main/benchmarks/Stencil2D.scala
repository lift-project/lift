package benchmarks

import apart.arithmetic.{SizeVar, StartFromRange, Var}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast.{Pad2D, _}
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

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

  val size = 3
  val step = 1
  val left = 1
  val right = 1
  val scalaBoundary = scalaWrap
  val makePositive = UserFun("makePositive", "i", "{ return (i < 0) ? 0 : i;  }", Float, Float)
  val weights = Array(0f, 0.12f, 0.08f,
      0.12f, 0.20f, 0.12f,
      0.08f, 0.12f, 0.08f)

  def runScala(input: Array[Array[Float]]): Array[Float] = {
    Utils.scalaCompute2DStencil(input, size, step, size, step, left, right, weights, scalaBoundary)
  }

  def ninePoint2DStencil(boundary: BoundaryFun): Lambda2 = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, weights.length),
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
        ) o Slide2D(size, step) o Pad2D(left, right, boundary)$ matrix
      })
  }

  def tiledNinePoint2DStencil(boundary: Pad.BoundaryFun, tileSize: Int, tileStep: Int) = fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, weights.length),
    (matrix, weights) => {
      Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

        MapLcl(1)(MapLcl(0)(
          fun(elem => {
            toGlobal(MapSeqUnroll(makePositive)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })

        )) o Slide2D(size, step) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o Slide2D(tileSize, tileStep) o Pad2D(left,right, boundary)$ matrix
    }
  )

  def TiledCopy(boundary: Pad.BoundaryFun): Lambda = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      ArrayType(Float, 9),
      (matrix, weights) => {
        MapWrg(1)(MapWrg(0)(fun( tile =>

         toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile

        ))) o Slide2D(4, 2) o Pad2D(1, 1, boundary)$ matrix
      })

  def apply() = new Stencil2D(
    Seq(
      ("9_POINT_2D_STENCIL_CLAMP", Array[Lambda](ninePoint2DStencil(Pad.Boundary.Clamp))),
      ("9_POINT_2D_STENCIL_MIRROR_UNSAFE", Array[Lambda](ninePoint2DStencil(Pad.Boundary.MirrorUnsafe))),
      ("9_POINT_2D_STENCIL_WRAP", Array[Lambda](ninePoint2DStencil(Pad.Boundary.Wrap))),
      ("9_POINT_2D_STENCIL_MIRROR", Array[Lambda](ninePoint2DStencil(Pad.Boundary.Mirror))),
      ("TILED_9P2D_WRAP_10/8", Array[Lambda](tiledNinePoint2DStencil(Pad.Boundary.Wrap, 10,8))),
      ("TILED_9P2D_WRAP_18/16", Array[Lambda](tiledNinePoint2DStencil(Pad.Boundary.Wrap, 18,16))),
      ("TILED_9P2D_WRAP_34/32", Array[Lambda](tiledNinePoint2DStencil(Pad.Boundary.Wrap, 34,32))),
      ("TILED_9P2D_WRAP_66/64", Array[Lambda](tiledNinePoint2DStencil(Pad.Boundary.Wrap, 66,64)))
    )
  )

  def main(args: Array[String]): Unit = {
    Stencil2D().run(args)
  }
}

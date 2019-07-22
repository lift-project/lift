package benchmarks

import lift.arithmetic.{SizeVar, StartFromRange, Var}
import ir._
import ir.ast.Pad.BoundaryFun
import ir.ast.{Pad2D, _}
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

@deprecated("Uses an old benchmark infrastructure", "")
class Stencil2D(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("Stencil2D", Seq(1024, 1024), f, 0.01f) {


  override def runScala(inputs: Any*): Array[Float] = {
    val input = inputs(0).asInstanceOf[Array[Array[Float]]]
    val weights = inputs(1).asInstanceOf[Array[Float]]
    variant match {
      case 0 => Stencil2D.runScala(input, weights, 1,1,3,1, 0,0,1,1, Stencil2D.scalaClamp)
      case 1 => Stencil2D.runScala(input, weights, 3,1,1,1, 1,1,0,0, Stencil2D.scalaClamp)
      case 2 => throw new IllegalArgumentException("no scala check defined for this benchmark")
      case 3 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaClamp)
      case 4 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaMirror)
      case 5 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaWrap)
      case 6 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaMirror)
      case 7 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaWrap)
      case 8 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaWrap)
      case 9 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaWrap)
      case 10 => Stencil2D.runScala(input, weights, 3,1,3,1, 1,1,1,1, Stencil2D.scalaWrap)
      case 11 => Stencil2D.runScala(input, weights, 1,1,17,1, 0,0,8,8, Stencil2D.scalaClamp)
      case 12 => Stencil2D.runScala(input, weights, 1,1,17,1, 0,0,8,8, Stencil2D.scalaClamp)
      case 13 => Stencil2D.runScala(input, weights, 17,1,1,1, 8,8,0,0, Stencil2D.scalaClamp)
      case _ => throw new IllegalArgumentException("no scala check defined for this benchmark")
    }
  }

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)
    val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c)
    //val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => util.Random.nextFloat())

    //Seq(inputData, if(variant > 2) {Array.fill[Float](17)(1.0f)}
      //                        else {Array.fill[Float](9)(1.0f)})

    Seq(inputData, variant match {
      case 0 => Array.fill[Float](3)(1.0f)
      case 1 => Array.fill[Float](3)(1.0f)
      case 2 => Array.fill[Float](3)(1.0f)
      case 11 => Array.fill[Float](17)(1.0f)
      case 12 => Array.fill[Float](17)(1.0f)
      case 13 => Array.fill[Float](17)(1.0f)
      case _ => Array.fill[Float](9)(1.0f)
    })
    //Seq(inputData, weights)
  }

  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val (output, runtime) = super.runOpenCL(inputs:_*)
    (Array(output.sum), runtime)
  }

//  override def globalSize: Array[Int] = {
//    Array(inputSizes()(0),inputSizes()(1),1)
//  }
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

  val makePositive = UserFun("makePositive", "i", "{ return (i < 0) ? 0 : i;  }", Float, Float)

  def runScala(input: Array[Array[Float]], weights: Array[Float],
               size1: Int, step1: Int,
               size2: Int, step2: Int,
               top: Int, bottom: Int,
               left: Int, right: Int,
               boundary: (Int, Int) => Int): Array[Float] = {
    //val weights = Array(0f, 0.12f, 0.08f, 0.12f, 0.20f, 0.12f, 0.08f, 0.12f, 0.08f)
    Utils.scalaCompute2DStencil(input, size1,step1, size2,step2, top,bottom,left,right, weights, scalaClamp)
  }

  def blurSeparated2DStencil(size1: Int, step1: Int,
                         size2: Int, step2: Int,
                         top: Int, bottom: Int,
                         left: Int, right: Int,
                         boundary: BoundaryFun,
                             weightSize: Int): Lambda2 = {
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(Float, weightSize),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(size1,step1, size2,step2) o Pad2D(top,bottom, left,right, boundary)$ matrix
      })
  }

  def ninePoint2DStencil(size1: Int, step1: Int,
                         size2: Int, step2: Int,
                         top: Int, bottom: Int,
                         left: Int, right: Int,
                         boundary: BoundaryFun): Lambda2 = {
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(Float, 9),
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
        ) o Slide2D(size1,step1, size2,step2) o Pad2D(top,bottom, left,right, boundary)$ matrix
      })
  }

  def tiledNinePoint2DStencil(size1: Int, step1: Int,
                              size2: Int, step2: Int,
                              top: Int, bottom: Int,
                              left: Int, right: Int,
                              boundary: Pad.BoundaryFun,
                              tileSize1: Int, tileStep1: Int,
                              tileSize2: Int, tileStep2: Int) = {
    fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(Float, size1 * size2),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

          MapLcl(1)(MapLcl(0)(
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeqUnroll(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })

          )) o Slide2D(size1, step1, size2, step2) o toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o Slide2D(tileSize1, tileStep1, tileSize2, tileStep2) o Pad2D(top, bottom, left, right, boundary) $ matrix
      }
    )
  }

  def TiledCopy(boundary: Pad.BoundaryFun): Lambda = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("M")), Var("N")),
      ArrayTypeWSWC(Float, 9),
      (matrix, weights) => {
        MapWrg(1)(MapWrg(0)(fun( tile =>

         toGlobal(MapLcl(1)(MapLcl(0)(id))) $ tile

        ))) o Slide2D(4, 2) o Pad2D(1, 1, boundary)$ matrix
      })

  def apply() = new Stencil2D(
    Seq(
      // from here weights.size == 3
      ("BLUR_X_CLAMP", Array[Lambda](blurSeparated2DStencil(1,1,3,1, 0,0,1,1,Pad.Boundary.Clamp, 3))),
      ("BLUR_Y_CLAMP", Array[Lambda](blurSeparated2DStencil(3,1,1,1, 1,1,0,0,Pad.Boundary.Clamp, 3))),
      ("BLUR_X_AND_Y_CLAMP", Array[Lambda](blurSeparated2DStencil(1,1,3,1, 0,0,1,1,Pad.Boundary.Clamp, 3),
                                           blurSeparated2DStencil(3,1,1,1, 1,1,0,0,Pad.Boundary.Clamp, 3))),
      // from here weights.size == 9
      ("9_POINT_2D_STENCIL_CLAMP", Array[Lambda](ninePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Clamp))),
      ("9_POINT_2D_STENCIL_MIRROR_UNSAFE", Array[Lambda](ninePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.MirrorUnsafe))),
      ("9_POINT_2D_STENCIL_WRAP", Array[Lambda](ninePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Wrap))),
      ("9_POINT_2D_STENCIL_MIRROR", Array[Lambda](ninePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Mirror))),
      ("TILED_9P2D_WRAP_10/8", Array[Lambda](tiledNinePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Wrap, 10,8,10,8))),
      ("TILED_9P2D_WRAP_18/16", Array[Lambda](tiledNinePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Wrap, 18,16,18,16))),
      ("TILED_9P2D_WRAP_34/32", Array[Lambda](tiledNinePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Wrap, 34,32,34,32))),
      ("TILED_9P2D_WRAP_66/64", Array[Lambda](tiledNinePoint2DStencil(3,1,3,1, 1,1,1,1, Pad.Boundary.Wrap, 66,64,66,64))),
      // from here weights.size == 9
      ("BLUR_X_CLAMP_17POINT", Array[Lambda](blurSeparated2DStencil(1,1,17,1, 0,0,8,8,Pad.Boundary.Clamp, 17))),
      ("BLUR_X_CLAMP_TILED_17POINT", Array[Lambda](tiledNinePoint2DStencil(1,1,17,1, 0,0,8,8,Pad.Boundary.Clamp, 1,1,48,32))),
      ("BLUR_Y_CLAMP_TILED_17POINT", Array[Lambda](tiledNinePoint2DStencil(17,1,1,1, 8,8,0,0,Pad.Boundary.Clamp, 528,512,1,1)))
    )
  )

  def main(args: Array[String]): Unit = {
    Stencil2D().run(args)
  }
}

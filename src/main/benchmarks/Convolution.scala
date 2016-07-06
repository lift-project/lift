package benchmarks

import apart.arithmetic.{StartFromRange, Var}
import ir._
import ir.ast.{Pad2D, _}
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

class Convolution(override val f: Seq[(String, Array[Lambda])]) extends Benchmark("Convolution", Seq(4096, 4096), f, 0.01f) {

  override def generateInputs(): Seq[Any] = {
    val inputSizeN = inputSizes()(0)
    val inputSizeM = inputSizes()(1)
    val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c)
    //val inputData = Array.tabulate(inputSizeM, inputSizeN)((r, c) => util.Random.nextFloat())

    Seq(variant match {
      case 0 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // convolution simple
      case 1 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // convolution tiled idle
      case 2 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // convolution tiled
      case 3 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // blur y
      case 4 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // blur y tiled
      case 5 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // blur y tiled 2d
      case 6 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 4096.0f + c) // blur y tiled 2d transposed
      case 7 => Array.tabulate(inputSizeM, inputSizeN)((r, c) => r * 3072.0f + c) // 3k blur y tiled
    }, variant match {
      case 0 => Array.fill[Float](17*17)(1.0f) // convolution simple
      case 1 => Array.fill[Float](17*17)(1.0f) // convolution tiled idle
      case 2 => Array.fill[Float](17*17)(1.0f) // convolution tiled
      case 3 => Array.fill[Float](17)(1.0f)    // blur y
      case 4 => Array.fill[Float](17)(1.0f)    // blur y tiled
      case 5 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d
      case 6 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d transposed
      case 7 => Array.fill[Float](17)(1.0f)    // 3k blur y tiled
    })
  }

  // no scala checks because 4k x 4k is too big
  override def runScala(inputs: Any*): Array[Float] = {
    throw new IllegalArgumentException("no scala check defined for this benchmark")
  }

  override def runOpenCL(inputs: Any*): (Array[Float], Double) = {
    val (output, runtime) = super.runOpenCL(inputs:_*)
    (Array(output.sum), runtime)
  }

  override def globalSize: Array[Int] = {
    variant match {
      case 0 => Array(4096, 4096, 1) // convolution simple
      case 1 => Array(4096, 4096, 1) // convolution tiled idle
      case 2 => Array(4096, 4096, 1) // convolution tiled
      case 3 => Array(4096, 4096, 1) // blur y
      case 4 => Array(4096, 512, 1)  // blur y tiled
      case 5 => Array(4096, 512, 1)  // blur y tiled 2d
      case 6 => Array(4096, 512, 1)  // blur y tiled 2d transposed
      case 7 => Array(3072, 384, 1)  // 3k blur y tiled 2d
    }
  }

  override def localSize: Array[Int] = {
    variant match {
      case 0 => Array(16, 16, 1) // convolution simple
      case 1 => Array(32, 32, 1) // convolution tiled idle
      case 2 => Array(16, 16, 1) // convolution tiled
      case 3 => Array(16, 16, 1) // blur y
      case 4 => Array(1, 8, 1)   // blur y tiled
      case 5 => Array(16, 8, 1)  // blur y tiled 2d
      case 6 => Array(16, 8, 1)  // blur y tiled 2d transposed
      case 7 => Array(16, 8, 1)  // 3k blur y tiled 2d
    }
  }
}

object Convolution{

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

  def runScala(input: Array[Array[Float]], weights: Array[Float],
               size1: Int, step1: Int,
               size2: Int, step2: Int,
               top: Int, bottom: Int,
               left: Int, right: Int,
               boundary: (Int, Int) => Int): Array[Float] = {
    Utils.scalaCompute2DStencil(input, size1,step1, size2,step2, top,bottom,left,right, weights, boundary)
  }

  /////////////////// LAMBDAS
  def convolutionSimple(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17*17),
      (matrix, weights) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, weights)
          }))
        ) o Slide2D(17,1, 17,1) o Pad2D(8,8, 8,8, Pad.Boundary.Wrap)$ matrix
      })
  }

  def convolutionTiled(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17*17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

          MapLcl(1)(MapLcl(0)(
            // stencil computation
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeq(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })
            // create neighbourhoods in tiles
          )) o Slide2D(17,1, 17,1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(32,16, 32,16) o
          Pad2D(8,8, 8,8, Pad.Boundary.Clamp) $ matrix
      }
    )
  }

  def blurY(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
        ) o Slide2D(17,1, 1,1) o Pad2D(8,8, 0,0, Pad.Boundary.Clamp)$ matrix
      })
  }

  def blurYTiled(): Lambda = {
    fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, 17),
    (matrix, weights) => {
      Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

        MapLcl(1)(MapLcl(0)(
          // stencil computation
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })
          // create neighbourhoods in tiles
        )) o Slide2D(17,1, 1,1) o
          // load to local memory
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o
        // tiling
        Slide2D(80,64, 1,1) o
        Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
    }
    )
  }

  def blurYTiled2D(): Lambda = {
    fun(
    ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayType(Float, 17),
    (matrix, weights) => {
      Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

        MapLcl(1)(MapLcl(0)(
          // stencil computation
          fun(elem => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ elem, weights)
          })
          // create neighbourhoods in tiles
        )) o Slide2D(17,1, 1,1) o
          // load to local memory
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o
        // tiling
        Slide2D(80,64, 16,16) o
        Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
    }
  )
  }

  def blurYTiled2DTransposed(): Lambda = {
    fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
      (matrix, weights) => {
        Untile() o MapWrg(1)(MapWrg(0)(fun( tile =>

          MapLcl(1)(MapLcl(0)(
            // stencil computation
            fun(elem => {
              toGlobal(MapSeqUnroll(id)) o
                ReduceSeqUnroll(fun((acc, pair) => {
                  val pixel = Get(pair, 0)
                  val weight = Get(pair, 1)
                  multAndSumUp.apply(acc, pixel, weight)
                }), 0.0f) $ Zip(Join() $ elem, weights)
            })
            // create neighbourhoods in tiles
          )) o Slide2D(17,1, 1,1) o
            // transposed load
            Transpose() o
            toLocal(MapLcl(0)(MapLcl(1)(id))) o
            Transpose() $ tile
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
  }

  def apply() = new Convolution(
    Seq(
      ("CONVOLUTION_SIMPLE", Array[Lambda](convolutionSimple())),
      ("CONVOLUTION_TILED_IDLE", Array[Lambda](convolutionTiled())),
      ("CONVOLUTION_TILED", Array[Lambda](convolutionTiled())),
      ("BLUR_Y", Array[Lambda](blurY)),
      ("BLUR_Y_TILED", Array[Lambda](blurYTiled)),
      ("BLUR_Y_TILED_2D", Array[Lambda](blurYTiled2D)),
      ("BLUR_Y_TILED_2D_TRANSPOSED", Array[Lambda](blurYTiled2DTransposed)),
      ("3K_ BLUR_Y_TILED_2D", Array[Lambda](blurYTiled2D))
    )
  )

  def main(args: Array[String]): Unit = {
    Convolution().run(args)
  }
}

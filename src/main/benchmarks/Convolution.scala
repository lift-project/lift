package benchmarks

import lift.arithmetic.{StartFromRange, Var, Cst}
import ir._
import ir.ast.{Pad2D, _}
import opencl.ir._
import opencl.ir.pattern._
import opencl.executor.Utils

@deprecated("Uses an old benchmark infrastructure", "")
class Convolution(override val f: Seq[(String, Array[Lambda])]) extends DeprecatedBenchmark("Convolution", Seq(4096, 4096), f, 0.01f) {

  // change in singleton object as well!
  val inputSize = 4096
  val smallerGlobalSize = 512
  //val inputSize = 3072
  //val smallerGlobalSize = 384

  override def generateInputs(): Seq[Any] = {
    Seq(Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c)
      /*
      variant match {
      case 0 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // convolution simple
      case 1 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // convolution tiled idle
      case 2 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // convolution tiled
      case 3 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur y
      case 4 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur y tiled
      case 5 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur y tiled 2d
      case 6 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur y tiled 2d tiled loading
      case 7 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur y tiled 2d transposed
      case 8 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur y tiled 2d tiled loading transposed
      case 9 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur x
      case 10 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur x tiled
      case 11 => Array.tabulate(inputSize, inputSize)((r, c) => r * inputSize * 1.0f + c) // blur x tiled 2d
    }*/
      , variant match {
      case 0 => Array.fill[Float](17*17)(1.0f) // convolution simple
      case 1 => Array.fill[Float](17*17)(1.0f) // convolution tiled idle
      case 2 => Array.fill[Float](17*17)(1.0f) // convolution tiled
      case 3 => Array.fill[Float](17)(1.0f)    // blur y
      case 4 => Array.fill[Float](17)(1.0f)    // blur y tiled
      case 5 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d
      case 6 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d tiled loading
      case 7 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d transposed
      case 8 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d tiled loading transposed
      case 9 => Array.fill[Float](17)(1.0f)    // blur x
      case 10 => Array.fill[Float](17)(1.0f)    // blur x tiled
      case 11 => Array.fill[Float](17)(1.0f)    // blur x tiled 2d
      case 12 => Array.fill[Float](17)(1.0f)    // blur y tiled 2d 32
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
      case 0 => Array(inputSize, inputSize, 1) // convolution simple
      case 1 => Array(inputSize, inputSize, 1) // convolution tiled idle
      case 2 => Array(inputSize, inputSize, 1) // convolution tiled
      case 3 => Array(inputSize, inputSize, 1) // blur y
      case 4 => Array(inputSize, smallerGlobalSize, 1)  // blur y tiled
      case 5 => Array(inputSize, smallerGlobalSize, 1)  // blur y tiled 2d
      case 6 => Array(inputSize, smallerGlobalSize, 1)  // blur y tiled 2d tiled loading
      case 7 => Array(inputSize, smallerGlobalSize, 1)  // blur y tiled 2d transposed
      case 8 => Array(inputSize, smallerGlobalSize, 1)  // blur y tiled 2d tiled loading transposed
      case 9 => Array(inputSize, inputSize, 1)  // blur x
      case 10 => Array(smallerGlobalSize, inputSize, 1)  // blur x tiled
      case 11 => Array(smallerGlobalSize, inputSize, 1)  // blur x tiled 2d
      case 12 => Array(inputSize, smallerGlobalSize, 1)  // blur y tiled 2d
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
      case 6 => Array(16, 8, 1)  // blur y tiled 2d tiled loading
      case 7 => Array(16, 8, 1)  // blur y tiled 2d transposed
      case 8 => Array(16, 8, 1)  // blur y tiled 2d tiled loading transposed
      case 9 => Array(16, 16, 1)  // blur x
      case 10 => Array(16, 1, 1)  // blur x tiled
      case 11 => Array(16, 4, 1)  // blur x tiled 2d
      case 12 => Array(32, 4, 1)  // blur y tiled 2d
    }
  }
}

object Convolution{

  val inputSize = 4096
  //val inputSize = 3072

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
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17*17),
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
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17*17),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
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
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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

  def blurYTiled2D32(): Lambda = {
    fun(
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
        Slide2D(80,64, 32,32) o
        Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
    }
  )
  }

  def blurYTiled2DTiledLoading(): Lambda = {
    fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
          )) o Slide2D(17,1, 1,1) o Join() o
            // load to local memory
            toLocal(MapSeqUnroll(MapLcl(1)(MapLcl(0)(id)))) o Split(8) $ tile
          // split tiles into chunks
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
  }

  def blurYTiled2DTransposed(): Lambda = {
    fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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

  def blurYTiled2DTiledLoadingTransposed(): Lambda = {
    fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
      (matrix, weights) => {
        Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
              Map(Join()) o
                // tiled loading
                toLocal(MapLcl(0)(MapSeqUnroll(MapLcl(1)(id)))) o
              // split tile into chunks
              Map(Split(8)) o
            Transpose() $ tile
        ))) o
          // tiling
          Slide2D(80,64, 16,16) o
          Pad2D(8,8, 0,0, Pad.Boundary.Clamp) $ matrix
      }
    )
  }

  def blurX(): Lambda = {
    fun(
      //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
      ArrayTypeWSWC(Float, 17),
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
        ) o Slide2D(1,1, 17,1) o Pad2D(0,0, 8,8, Pad.Boundary.Clamp)$ matrix
      })
  }

  def blurXTiled(): Lambda = {
    fun(
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
    ArrayTypeWSWC(Float, 17),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
        )) o Slide2D(1,1, 17,1) o
          // load to local memory
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o
        // tiling
        Slide2D(1,1, 144,128) o
        Pad2D(0,0, 8,8, Pad.Boundary.Clamp) $ matrix
    }
    )
  }

  def blurXTiled2D(): Lambda = {
    fun(
    //ArrayTypeWSWC(ArrayTypeWSWC(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
    ArrayTypeWSWC(ArrayTypeWSWC(Float, inputSize), inputSize),
    ArrayTypeWSWC(Float, 17),
    (matrix, weights) => {
      Untile2D() o MapWrg(1)(MapWrg(0)(fun(tile =>

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
        )) o Slide2D(1,1, 17,1) o
          // load to local memory
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
      ))) o
        // tiling
        Slide2D(4,4, 144,128) o
        Pad2D(0,0, 8,8, Pad.Boundary.Clamp) $ matrix
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
      ("BLUR_Y_TILED_2D_TILED_LOADING", Array[Lambda](blurYTiled2DTiledLoading)),
      ("BLUR_Y_TILED_2D_TRANSPOSED", Array[Lambda](blurYTiled2DTransposed)),
      ("BLUR_Y_TILED_2D_TILED_LOADING_TRANSPOSED", Array[Lambda](blurYTiled2DTiledLoadingTransposed)),
      ("BLUR_X", Array[Lambda](blurX)),
      ("BLUR_X_TILED", Array[Lambda](blurXTiled)),
      ("BLUR_X_TILED_2D", Array[Lambda](blurXTiled2D)),
      ("BLUR_Y_TILED_2D_32", Array[Lambda](blurYTiled2D32))
    )
  )

  def main(args: Array[String]): Unit = {
    Convolution().run(args)
  }
}

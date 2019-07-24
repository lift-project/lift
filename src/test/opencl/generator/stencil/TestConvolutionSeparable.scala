package opencl.generator.stencil

import ir._
import ir.ast._
import lift.arithmetic.{StartFromRange, Var}
import opencl.executor.{Execute, Utils, _}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{Ignore, Test}

object TestConvolutionSeparable extends TestWithExecutor

class TestConvolutionSeparable {

  @Test def convolutionSimple(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17 * 17),
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
        ) o Slide2D(17, 1, 17, 1) o Pad2D(8, 8, 8, 8, Pad.Boundary.Clamp) $ matrix
      })

    val weights = Array.fill[Float](17 * 17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 16, 128, 128, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 17, 1, 8, 8, 8, 8, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Test def convolutionTiled(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17 * 17),
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
          )) o Slide2D(17, 1, 17, 1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(32, 16, 32, 16) o
          Pad2D(8, 8, 8, 8, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17 * 17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(32, 8, 128, 128, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 17, 1, 8, 8, 8, 8, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    // idle threads
    //val (output: Array[Float], _) = Execute(32, 32, 4096, 4096, (true, true))(stencil, input, weights)
    // blocked loading to local mem
    //val (output: Array[Float], _) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Ignore //todo segfaults?
  @Test def blurY(): Unit = {
    val stencil = fun(
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
        ) o Slide2D(17, 1, 1, 1) o Pad2D(8, 8, 0, 0, Pad.Boundary.Wrap) $ matrix
      })

    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 16, 128, 128, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaWrap)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(17, 1, 1, 1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(80, 64, 1, 1) o
          Pad2D(8, 8, 0, 0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(1, 4, 128, 64, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(1, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled2D(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(17, 1, 1, 1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(80, 64, 16, 16) o
          Pad2D(8, 8, 0, 0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 4, 128, 64, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Ignore //fix
  @Test def blurYTiled2DTiledLoading(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(17, 1, 1, 1) o Join() o
            // load to local memory
            toLocal(MapSeqUnroll(MapLcl(1)(MapLcl(0)(id)))) o Split(8) $ tile
          // split tiles into chunks
        ))) o
          // tiling
          Slide2D(80, 64, 16, 16) o
          Pad2D(8, 8, 0, 0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 8, 128, 64, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled2DTransposed(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(17, 1, 1, 1) o
            // transposed load
            Transpose() o
            toLocal(MapLcl(0)(MapLcl(1)(id))) o
            Transpose() $ tile
        ))) o
          // tiling
          Slide2D(80, 64, 16, 16) o
          Pad2D(8, 8, 0, 0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 4, 128, 64, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Test def blurYTiled2DTiledLoadingTransposed(): Unit = {
    assumeFalse("Disabled on Apple OpenCL CPU.", Utils.isAppleCPU)

    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(17, 1, 1, 1) o
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
          Slide2D(80, 64, 16, 16) o
          Pad2D(8, 8, 0, 0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 4, 128, 64, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Ignore // pad is not the right primitive here, just to try things out
  @Test def blurYTiled2DTransposedPadded(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(17, 1, 1, 1) o
            // transposed load
            Transpose() o
            toLocal(MapLcl(0)(MapLcl(1)(id))) o
            Transpose() o
            // pad to avoid bank conflicts
            Pad(0, 1, Pad.Boundary.Clamp) $ tile
        ))) o
          // tiling
          Slide2D(80, 64, 16, 16) o
          Pad2D(8, 8, 0, 0, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing

    val input = Array.tabulate(128, 128) { (i, j) => i * 128.0f + j }
    val (output, _) = Execute(16, 4, 128, 64, (true, true))[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 17, 1, 1, 1, 8, 8, 0, 0, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 8, 4096, 512, (true, true))(stencil, input, weights)
  }

  @Test def blurX(): Unit = {
    val stencil = fun(
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
        ) o Slide2D(1, 1, 17, 1) o Pad2D(0, 0, 8, 8, Pad.Boundary.Clamp) $ matrix
      })

    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(256, 256) { (i, j) => i * 256.0f + j }
    val (output, _) = Execute(16, 16)[Array[Float]](stencil, input, weights)

    val gold = Utils.scalaCompute2DStencil(input, 1, 1, 17, 1, 0, 0, 8, 8, weights, Utils.scalaClamp)
    assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 16, 4096, 4096, (true, true))(stencil, input, weights)
  }

  @Ignore //fix
  @Test def blurXTiled(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(1, 1, 17, 1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(1, 1, 144, 128) o
          Pad2D(0, 0, 8, 8, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    //val input = Array.tabulate(3072, 3072) { (i, j) => i * 3072.0f + j }
    //val (output: Array[Float], _) = Execute(16, 1, 128, 3072, (true, true))(stencil, input, weights)

    //val gold = Utils.scalaCompute2DStencil(input, 1,1, 17,1, 0,0,8,8, weights, scalaClamp)
    //assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    val input = Array.tabulate(1024, 1024) { (i, j) => i * 1024.0f + j }
    val (output, _) = Execute(16, 1, 512, 1024, (true, true))[Array[Float]](stencil, input, weights)
  }

  @Ignore //fix
  @Test def blurXTiled2D(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(Float, Var("N", StartFromRange(100))), Var("M", StartFromRange(100))),
      ArrayType(Float, 17),
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
          )) o Slide2D(1, 1, 17, 1) o
            // load to local memory
            toLocal(MapLcl(1)(MapLcl(0)(id))) $ tile
        ))) o
          // tiling
          Slide2D(4, 4, 144, 128) o
          Pad2D(0, 0, 8, 8, Pad.Boundary.Clamp) $ matrix
      }
    )
    val weights = Array.fill[Float](17)(1.0f)

    // testing
    val input = Array.tabulate(256, 256) { (i, j) => i * 256.0f + j }
    val (output, _) = Execute(16, 4, 64, 256, (true, true))[Array[Float]](stencil, input, weights)

    //val gold = Utils.scalaCompute2DStencil(input, 1,1, 17,1, 0,0,8,8, weights, scalaClamp)
    //assertArrayEquals(gold, output, 0.2f)

    // for generating 4k kernel
    //val input = Array.tabulate(4096, 4096) { (i, j) => i * 4096.0f + j }
    //val (output: Array[Float], _) = Execute(16, 4, 512, 4096, (true, true))(stencil, input, weights)
  }
}

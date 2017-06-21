package opencl.generator.stencil

import ir._
import ir.ast._
import lift.arithmetic.{SizeVar, StartFromRange, Var}
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestTemporalBlocking {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestTemporalBlocking {
  /* **********************************************************
     TEMPORAL BLOCKING
 ***********************************************************/
  def createTempAndSpatialBlockingLambda(n: Int, s: Int,
                                         tileSize: Int, tileStep: Int,
                                         l: Int, r: Int, b: Pad.BoundaryFun) = {
    fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {
        Join() o MapWrg(
          //temporal blocking
          Join() o MapLcl(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
            Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f)))) o
          //spatial blocking
          Split(2) o
          // create data differently
          Map(Slide(n, s)) o Slide(n + 2, 1) o Pad(2, 2, b) $ input

        //        Join() o MapWrg(
        //          //temporal blocking
        //          Join() o MapLcl(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
        //            //Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
        //              Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f)))) o
        //        //spatial blocking
        //        Split(2) o //Slide(2,2) o
        //        //Slide(n,s) o Pad(l,r,b) o
        //          Slide(n,s) o Pad(l,r,b) o
        //            Slide(n,s) o Pad(l,r,b) $ input

        // temp before spatial
        //        MapGlb( fun(tile =>
        //          //temporal blocking
        //          Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o
        //            Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f))) o
        //            Slide(n,s) o /*Pad(l,r,b) o*/
        //            Slide(n,s) /*o Pad(l,r,b)*/ $ tile
        //          // spatial blocking
        //        )) o Slide(tileSize, tileStep) o Pad(l,r,b) $ input
      }
    )
  }

  def create1DStencilLambda(weights: Array[Float], size: Int, step: Int, left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, Var("N", StartFromRange(3))),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapGlb(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(add, 0.0f) o
              MapSeqUnroll(mult) $
              Zip(weights, neighbourhood)
          })
        ) o Slide(size, step) o Pad(left, right, Pad.Boundary.Clamp) $ input
      }
    )
  }
  @Test def tempAndSpatialBlocking1D(): Unit = {
    val weights = Array(1, 1, 1).map(_.toFloat)
    val randomData = Array(0, 1, 2, 3, 4, 5).map(_.toFloat)
    val length = randomData.length * 2

    val newLambda = create1DStencilLambda(weights, 3, 1, 1, 1)
    //gold computation
    val (firstIteration: Array[Float], _) = Execute(length, length)(newLambda, randomData, weights)
    //val (gold: Array[Float], runtime3) = Execute(length, length)(newLambda, firstIteration, weights)

    val (secondIteration: Array[Float], _) = Execute(length, length)(newLambda, firstIteration, weights)
    val (_, _) = Execute(length, length)(newLambda, secondIteration, weights)
    //println(gold.mkString(","))

    val stencil = createTempAndSpatialBlockingLambda(3, 1, 4, 2, 1, 1, Pad.Boundary.Clamp)
    val (_, _) = Execute(length, length)(stencil, randomData)
    //println(output.mkString(","))

    //compareGoldWithOutput(gold, output, runtime)
  }

  def createTemporalBlockingUsingTiles1DStencilLambda(weights: Array[Float],
                                                      boundary: Pad.BoundaryFun,
                                                      size: Int, step: Int,
                                                      tileSize: Int, tileStep: Int,
                                                      left: Int, right: Int): Lambda2 = {
    fun(
      ArrayType(Float, SizeVar("N")),
      ArrayType(Float, weights.length),
      (input, weights) => {
        MapWrg(fun(tile =>
          toGlobal(MapSeqUnroll(id)) o Iterate(2)(fun(localTile =>
            Join() o
              MapLcl(
                fun(neighbourhood => {
                  toLocal(MapSeqUnroll(id)) o
                    ReduceSeqUnroll(fun((acc, y) => {
                      multAndSumUp.apply(acc, Get(y, 0), Get(y, 1))
                    }), 0.0f) $
                    Zip(weights, neighbourhood)
                })
              ) o Slide(size, step) $ localTile)) o MapLcl(toLocal(id)) $ tile

        )) o Slide(tileSize, tileStep) o Pad(left, right, boundary) $ input
      }
    )
  }

  def createTemporalBlockingUsingRewriteLambda(b: Pad.BoundaryFun,
                                               n: Int, s: Int,
                                               l: Int, r: Int) = {
    fun(
      ArrayType(Float, SizeVar("N")),
      (input) => {

        //f:      toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)
        //map f:  MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)

        Join() o MapGlb(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o //first iteration
          Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f) o //second iteration
          Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f)))) o //third iteration
          //Join() o MapSeq(toGlobal(MapSeqUnroll(id)) o ReduceSeqUnroll(add, 0.0f))))) o  //fourth iteration
          Slide(n, s) o Pad(l, r, b) o
          Slide(n, s) o Pad(l, r, b) o
          Slide(n, s) o Pad(l, r, b) $ input //o
        //Slide(n,s) o Pad(l,r,b) $ input

        // fused maps - two iterations
        //Join() o
        //MapSeq(
        //          toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o  //f
        //            Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o //map f
        //        Slide(n,s) o Pad(l,r,b) o Slide(n,s) o Pad(l,r,b) $ input

        // before fused map
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
        //MapSeq(Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))) o
        //Slide(n,s) o Pad(l,r,b) o Slide(n,s) o Pad(l,r,b) $ input

        //christophes map f: Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f))
        // Pad and Map f vertauscht
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(n,s) o
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
        //Pad(l,r,b) o Slide(n,s) o Pad(l,r,b) $ input

        // simple two iterations after each other
        //MapSeq(id) o
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(n,s) o Pad(l,r,b) o
        //Join() o MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o Slide(n,s) o Pad(l,r,b) $ input
      }
    )
  }

  @Test def temporalBlockingUsingRewrite1D(): Unit = {
    val weights = Array(1, 1, 1).map(_.toFloat)
    val randomData = Array(0, 1, 2, 3, 4, 5).map(_.toFloat)
    val length = randomData.length * 2

    val newLambda = create1DStencilLambda(weights, 3, 1, 1, 1)
    //gold computation
    val (firstIteration: Array[Float], _) = Execute(length, length)(newLambda, randomData, weights)
    //val (gold: Array[Float], runtime3) = Execute(length, length)(newLambda, firstIteration, weights)

    val (secondIteration: Array[Float], _) = Execute(length, length)(newLambda, firstIteration, weights)
    val (gold: Array[Float], _) = Execute(length, length)(newLambda, secondIteration, weights)

    val stencil = createTemporalBlockingUsingRewriteLambda(Pad.Boundary.Clamp, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(length, length)(stencil, randomData)

    assertArrayEquals(gold, output, 0.1f)
  }

  @Ignore // output shrinks because it cant handle padding for multiple iterations
  @Test def temporalBlockingUsingTiles1D(): Unit = {
    val weights = Array(1, 1, 1).map(_.toFloat)
    val randomData = Array(0, 1, 2, 3, 4, 5).map(_.toFloat)
    val length = randomData.length

    val stencil = createTemporalBlockingUsingTiles1DStencilLambda(weights, Pad.Boundary.Clamp, 3, 1, 5, 1, 1, 1)
    val newLambda = create1DStencilLambda(weights, 3, 1, 1, 1)
    val (output: Array[Float], runtime) = Execute(length, length)(stencil, randomData, weights)
    val (firstIteration: Array[Float], _) = Execute(length, length)(newLambda, randomData, weights)
    val (gold: Array[Float], _) = Execute(length, length)(newLambda, firstIteration, weights)

    //val (secondIteration: Array[Float], runtime5) = Execute(length, length)(newLambda, firstIteration, weights)
    //val (gold: Array[Float], runtime3) = Execute(length, length)(newLambda, secondIteration, weights)
    assertArrayEquals(gold, output, 0.1f)
  }

}

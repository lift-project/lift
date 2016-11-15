package opencl.generator.stencil

import apart.arithmetic.{SizeVar, StartFromRange, Var}
import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import scala.util.Random

object TestStencilRodinia {
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

class TestStencilRodinia extends TestStencil2D {
  /* **********************************************************
     RODINIA HOTSPOT
 ***********************************************************/
  @Test def rodiniaHotspot(): Unit = {
    // p = powerValue, t = heatNbh; userfun has to compute:
    // out = t[0,0] + c(p + y(t[-1,0] + t[1,0] - 2t[0,0]) +
    //                      x(t[0,-1] + t[0,1] - 2t[0,0]) +
    //                      z(a - t[0,0]));
    // a, c, x, y, z are constants
    val addAmbientTemp = UserFun("addAmbientTemp", Array("x", "y"), "{ return x + y + (0.1f * 1.068e-7f * 80.0f); }", Seq(Float, Float), Float)

    // the two outermost dimensions of A and B have to be the same
    // zip matrices elementwise
    val zip2d = \((A, B) =>
      Map(\(tuple => Zip(tuple._0, tuple._1))) $ Zip(A, B)
    )

    // create 16x16 heat tile and 14x14 power tile
    val createTiles = \((heat, power) =>
      zip2d(Slide2D(16, 14) o Pad2D(1, 1, Pad.Boundary.Clamp) $ heat,
        Slide2D(14, 14) $ power)
    )

    // load into local memory and prepare data for single work-item
    // < < coeff, heat> :: [9] , power >
    val prepareData = \((coeff, tiles) =>
      zip2d(
        // first component
        Map(Map(\(heatNbh =>
          Zip(coeff, Join() $ heatNbh)
        ))) o Slide2D(3, 1) o
          toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._0,
        // second component
        toLocal(MapLcl(1)(MapLcl(0)(id))) $ tiles._1)
    )

    // how to compute output using required data:
    val stencil = fun(requiredData => {
      val coeffHeatTuple = requiredData._0
      val powerValue = requiredData._1

      toGlobal(MapSeq(id)) o
        MapSeq(\(x => addAmbientTemp(powerValue, x))) o
        ReduceSeqUnroll(\((acc, next) =>
          multAndSumUp(acc, next._0, next._1)), 0.0f) $ coeffHeatTuple
    })

    val rodinia = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      //ArrayType(ArrayType(Float, 8204), 8204),
      //ArrayType(ArrayType(Float, 8204), 8204),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(\(tiles =>
          MapLcl(1)(MapLcl(0)(stencil)) o prepareData(coeff) $ tiles)
        )) $ createTiles(heat, power)

      }
    )
    /*
    val stencil = fun(
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(ArrayType(Float, 1036), 1036),
      ArrayType(Float, 9),
      (heat, power, coeff) => {
        MapWrg(1)(MapWrg(0)(
          fun(tiles => {
            val powerTile = Get(tiles, 0)
            val heatTile = Get(tiles, 1)
            MapLcl(1)(MapLcl(0)(
              fun(nbhs => {
                val powerValue = Get(nbhs, 0) // Float
                val heatNbh = Get(nbhs, 1)    // [[Float]_3]_3
                toGlobal(MapSeq(id)) o
                  MapSeq( \(x => addAmbientTemp(powerValue, x))) o
                  ReduceSeqUnroll(\((acc, next) =>
                    multAndSumUp(acc, next._0, next._1)), 0.0f) $ Zip(Join() $ heatNbh, coeff)
              })
            )) o
              Split(14) $
              Zip(Join() o toLocal(MapLcl(1)(MapLcl(0)(id))) $ powerTile,
                  Join() o Slide2D(3,1,3,1) o
                           toLocal(MapLcl(1)(MapLcl(0)(id))) $ heatTile)
          })
        )) o
          Split(74) $
          Zip(Join() o Slide2D(14,14) $ power,
              Join() o Slide2D(16,14) o
                Pad2D(1,1,Pad.Boundary.Wrap) $ heat) //actually its mirror
      }
    )
    */

    val heat = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    val power = Array.tabulate(1036, 1036) { (i, j) => i * 1036.0f + j }
    //val heat = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    //val power = Array.tabulate(8204, 8204) { (i, j) => i * 8204.0f + j }
    val x = 0.1f;
    val y = 0.1f;
    val z = 1024000;
    val c = 1.068e-7f
    val coeff = Array(0, c * y, 0, c * x, c * (-2 * y - 2 * x - z + 1), c * x, 0, c * y, 0)

    val (output: Array[Float], runtime) = Execute(16, 16, 1184, 1184, (true, true))(rodinia, heat, power, coeff)
    //val (output: Array[Float], runtime) = Execute(16,16, 9376, 9376, (true, true))(rodinia, heat, power, coeff)
    println("Runtime: " + runtime)
  }

  /* **********************************************************
      HOTSPOT 3D RODINIA
  ***********************************************************/
  @Test def rodiniaHotspot3D(): Unit = {
    @Ignore //fix
    //val hotspot = UserFun("hotspot", "tuple", "{ return tuple_0; }", TupleType(Float, ArrayType(ArrayType(Float, 3),3)), Float)
    // segfaults
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      ArrayType(Float, 27),
      (input, weights) => {
        MapSeq(MapGlb(1)(MapGlb(0)(\(nbh =>

          toGlobal(MapSeq(id)) o
            ReduceSeqUnroll(\((acc, next) =>
              multAndSumUp(acc, next._0, next._1)), 0.0f) $
            Zip(Join() o Join() $ nbh,
              weights))
        ))) o Slide3D(3, 1) o Pad3D(1, 1, 1, Pad.Boundary.MirrorUnsafe) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }
    val weights = Array.tabulate(27) { (i) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 1, (true, true))(stencil, input, weights)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  // rodinia 3d opt1
  @Test def rodiniaHotspot3DOpt1(): Unit = {
    val zip3d = \((A, B, C) =>
      Map(Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2)))) o Map(\(tuple => Zip(tuple._0, tuple._1, tuple._2))) $ Zip(A, B, C)
    )

    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapGlb(1)(MapGlb(0)(\(nbh =>
          toGlobal(MapSeq(\(acc => add(acc, nbh._2)))) o
            MapSeq(\(acc => add(acc, nbh._1))) o
            ReduceSeqUnroll(add, 0.0f) o Join() o Join() $ nbh._0)
        ))) $ zip3d(
          Slide3D(3, 1, 3, 1, 1, 1) o Pad3D(1, 1, 0, Pad.Boundary.Clamp) $ input,
          input, // leftshift z - upper element
          input) // rightshift z - lower element
      }
    )

    val input = Array.fill(512)(Array.fill(512)(Array.fill(8)(1.0f)))
    val weights = Array.fill(3)(Array.fill(3)(1.0f))
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 8, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }

  @Test def rodiniaHotspot3DLocalMemory(): Unit = {
    val stencil = fun(
      ArrayType(ArrayType(ArrayType(Float, 512), 512), 8),
      (input) => {
        MapSeq(MapWrg(1)(MapWrg(0)(\(tiles =>
          MapSeq(MapLcl(1)(MapLcl(0)(\(nbh =>
            toGlobal(MapSeq(id)) o
              ReduceSeq(add, 0.0f) o Join() o Join() $ nbh)))) o
            Slide3D(3, 1) o
            toLocal(MapSeq(MapLcl(1)(MapLcl(0)(id)))) $ tiles)
        ))) o Slide3D(66, 64, 6, 4, 10, 10) o Pad3D(1, 1, 1, Pad.Boundary.Clamp) $ input
      }
    )

    // testing
    val input = Array.tabulate(512, 512, 8) { (i, j, k) => Random.nextFloat() }
    val (output: Array[Float], runtime) = Execute(64, 4, 1, 512, 512, 1, (true, true))(stencil, input)
    println("Runtime: " + runtime)
    //println(output.mkString(","))
  }
}

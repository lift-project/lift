package opencl.generator

import benchmarks.NBody
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor, Utils}
import opencl.generator.TestNBody._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Assume.assumeFalse
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestNBody {

  @BeforeClass
  def before(): Unit = {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass
  def after(): Unit = {
    Executor.shutdown()
  }

  def nBodyScala(deltaT: Float, espSqr: Float, input: Array[(Float, Float, Float, Float, Float, Float, Float)]): Array[Float] = {
    val gold = input.map(x => {
      val acceleration = input.map(y => {
        val r = Array(0.0f, 0.0f, 0.0f)

        r(0) = y._1 - x._1
        r(1) = y._2 - x._2
        r(2) = y._3 - x._3

        val distSqr = r(0) * r(0) + r(1) * r(1) + r(2) * r(2)

        val invDist = 1.0f / math.sqrt(distSqr + espSqr).toFloat

        val s = invDist * invDist * invDist * y._7

        (s * r(0), s * r(1), s * r(2))
      }).reduce((y, z) => (y._1 + z._1, y._2 + z._2, y._3 + z._3))

      val px = x._4 * deltaT + 0.5f * acceleration._1 * deltaT * deltaT
      val py = x._5 * deltaT + 0.5f * acceleration._2 * deltaT * deltaT
      val pz = x._6 * deltaT + 0.5f * acceleration._3 * deltaT * deltaT

      (x._1 + px.toFloat,
        x._2 + py.toFloat,
        x._3 + pz.toFloat,
        x._7,
        x._4 + acceleration._1.toFloat * deltaT,
        x._5 + acceleration._2.toFloat * deltaT,
        x._6 + acceleration._3.toFloat * deltaT,
        x._7)
    }).map(_.productIterator)
      .reduce(_ ++ _).asInstanceOf[Iterator[Float]].toArray
    gold
  }

  val inputSize = 512

  val deltaT = 0.005f
  val espSqr = 500.0f

  // x, y, z, velocity x, y, z, mass
  val input = Array.fill(inputSize)((
    util.Random.nextFloat() * 10,
    util.Random.nextFloat() * 10,
    util.Random.nextFloat() * 10,
    0.0f, 0.0f, 0.0f,
    util.Random.nextFloat() * 10
    ))

  val x = input.map(_._1)
  val y = input.map(_._2)
  val z = input.map(_._3)

  val velX = input.map(_._4)
  val velY = input.map(_._5)
  val velZ = input.map(_._6)

  val mass = input.map(_._7)

  val gold: Array[Float] = nBodyScala(deltaT, espSqr, input)

  val pos = Array.ofDim[Float](inputSize, 4)
  val vel = Array.ofDim[Float](inputSize, 4)

  for (i <- 0 until inputSize) {
    pos(i) = Array(x(i), y(i), z(i), mass(i))
    vel(i) = Array(velX(i), velY(i), velZ(i), mass(i))
  }

  val N = SizeVar("N")
}

class TestNBody {
  @Test
  def nBodyAMD(): Unit = {

    val function = NBody.amd

    val (output: Array[Float], _) =
      Execute(inputSize)(function, pos, vel, espSqr, deltaT)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test
  def nBodyPrivateMem(): Unit = {

    val function = NBody.lessLoadsToGlobal

    val (output: Array[Float], _) =
      Execute(inputSize)(function, pos, vel, espSqr, deltaT)

    assertArrayEquals(gold, output, 0.0001f)

  }

  @Test
  def nBodyLocalMem(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileX = 128

    val function = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(0)(fun(p1Chunk => // ArrayTypeWSWC(Flat4, 128)

            toGlobal(MapLcl( fun( p1 =>
              NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
            ))) $ Zip(p1Chunk,
              Join() o
                ReduceSeq(fun((acc, p2) =>
                Let(p2Local =>
                  Join() o
                    MapLcl(fun(p1 => // ( (float4, float4), float4 )

                      ReduceSeq(fun((acc, p2) =>
                        NBody.calcAccAndAccumulate(Get(Get(p1,0), 0), p2, deltaT, espSqr, acc)),
                        Get(p1,1)) $ p2Local
                    )) $ Zip(p1Chunk, acc)
                ) o toLocal(MapLcl(idF4)) $ p2
              ), MapLcl(idF4) $ Value("0.0f", ArrayTypeWSWC(Float4, tileX))) o Split(tileX) $ pos)
          )) o Split(tileX) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(tileX, inputSize, (true, false))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test
  def nBodyLocalMem2(): Unit = {
    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileX = 32

    val function = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(fun(p1Chunk => // ArrayTypeWSWC(Flat4, 128)
            \(newP1Chunk =>
              toGlobal(MapLcl( fun( p1 =>
                NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
              ))) $ Zip(newP1Chunk,
                Join() o
                  ReduceSeq(fun((acc, p2) =>
                    Let(p2Local =>
                      Join() o
                        MapLcl(fun(p1 => // ( (float4, float4), float4 )

                          ReduceSeq(fun((acc, p2) =>
                            NBody.calcAccAndAccumulate(Get(Get(p1,0), 0), p2, deltaT, espSqr, acc)),
                            Get(p1,1)) $ p2Local
                        )) $ Zip(newP1Chunk, acc)
                    ) o toLocal(MapLcl(idF4)) $ p2
                  ), MapLcl(idF4) $ Value("0.0f", ArrayTypeWSWC(Float4, tileX))) o Split(tileX) $ pos)
            ) $ Zip(toPrivate(MapLcl(idF4)) $ Get(Unzip() $ p1Chunk, 0), Get(Unzip() $ p1Chunk, 1))
          )) o Split(tileX) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(tileX, inputSize, (true, false))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }


  @Test
  def nBodyLocalMem_nvidia_no_MT(): Unit = {

    assumeFalse("Disabled on Apple OpenCL Platform.", Utils.isApplePlatform)

    val tileX = 256
    val tileY = 1
    val threadsX = inputSize
    val threadsY = tileY

    val function = NBody.nvidia

    val (output: Array[Float], _) =
      Execute(tileX, tileY, threadsX, threadsY, (true, true))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Ignore
  @Test
  def nBodyLocalMem_nvidia_MT(): Unit = {

    val tileX = 32
    val tileY = 8
    val threadsX = inputSize
    val threadsY = tileY

    val numGroups1 = threadsY / tileY

    // TODO: Unnecessary barrier in the final reduction which makes the kernel illegal.
    // TODO: And missing a barrier after toLocal(MapSeq(MapLcl(1)(MapLcl(0)(idF4))))
    // TODO: Non correctness issue: can't do the WRAP optimisation

    // Problems in the NVIDIA OpenCL version:
    //  * Final Reduction is performed by threads with get_local_id(0) == 0, but adressing with threadIdxx (get_local_id(0)).
    //    So every thread with l_id_0 == 0 will have the same result in `acc` because they all reduce over the same row.
    //  * All threads return `acc` which can contain partial accumulations
    //  * All threads use the `acc` (which can be partial/wrong) to update the position and velocity.
    //  * All threads write the updated locations to memory.

    val function = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(1)(Join() o MapWrg(0)(fun(p1Chunk => // ArrayTypeWSWC(Flat4, tileX)
            \(newP1Chunk =>
              MapLcl(1)(\(bla =>
                toGlobal(MapLcl(0)( fun( p1 =>
                  NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
                ))) $ Zip(newP1Chunk, bla)) o
                Join() o
                ReduceSeq(\( (acc, x) => MapLcl(0)(VectorizeUserFun(4, add)) $ Zip(acc, x)), MapLcl(0)(idF4) $ Value(0.0f, ArrayTypeWSWC(Float4, tileX))))
                o toLocal(MapSeq(MapLcl(1)(MapLcl(0)(idF4)))) o
                ReduceSeq(fun((acc, p2) =>
                  Let(p2Local =>
                    MapLcl(1)(\(accDim2 =>
                      Join() o
                        MapLcl(0)(fun(p1 => // ( (float4, float4), float4 )

                          ReduceSeq(fun((acc, p2) =>
                            VectorizeUserFun(4, add)(NBody.calcAccNoAdd(Get(Get(p1,0), 0), p2, deltaT, espSqr), acc)),
                            Get(p1,1)) $ accDim2._0
                        )) $ Zip(newP1Chunk, accDim2._1)
                    )) $ Zip(p2Local, acc)
                  ) o toLocal(MapLcl(1)(MapLcl(0)(idF4))) $ p2
                ), MapLcl(1)(MapLcl(0)(idF4)) $ Value("0.0f", ArrayTypeWSWC(ArrayTypeWSWC(Float4, tileX), tileY))) o Split(tileY) o Split(tileX) $ pos
            ) $ Zip(toPrivate(MapLcl(idF4)) $ Get(Unzip() $ p1Chunk, 0), Get(Unzip() $ p1Chunk, 1))
          )) o Split(tileX)) o Split(N/numGroups1) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(tileX, tileY, threadsX, threadsY, (true, true))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }

}

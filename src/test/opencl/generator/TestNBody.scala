package opencl.generator

import apart.arithmetic.SizeVar
import benchmarks.NBody
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.generator.TestNBody._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

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

        r(0) = x._1 - y._1
        r(1) = x._2 - y._2
        r(2) = x._3 - y._3

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
  val input = Array.fill(inputSize)((util.Random.nextFloat(),
    util.Random.nextFloat(),
    util.Random.nextFloat(),
    0.0f, 0.0f, 0.0f,
    util.Random.nextFloat()))

  val x = input.map(_._1)
  val y = input.map(_._2)
  val z = input.map(_._3)

  val velX = input.map(_._4)
  val velY = input.map(_._5)
  val velZ = input.map(_._6)

  val mass = input.map(_._7)

  val gold: Array[Float] = nBodyScala(deltaT, espSqr, input)

  val pos = Array.ofDim[Float](inputSize*4)
  val vel = Array.ofDim[Float](inputSize*4)

  for (i <- 0 until inputSize) {
    pos(4*i) = x(i)
    pos(4*i+1) = y(i)
    pos(4*i+2) = z(i)
    pos(4*i+3) = mass(i)

    vel(4*i) = velX(i)
    vel(4*i+1) = velY(i)
    vel(4*i+2) = velZ(i)
    vel(4*i+3) = mass(i)
  }

  val N = SizeVar("N")
}

class TestNBody {

  @Test
  def nBody(): Unit = {

    val gold = input.map(x => {
      val acceleration = input.map(y => {
        val r = Array(0.0f, 0.0f, 0.0f)

        r(0) = x._1 - y._1
        r(1) = x._2 - y._2
        r(2) = x._3 - y._3

        val distSqr = r.sum

        val invDist = 1.0f / math.sqrt(distSqr + espSqr)

        val s = invDist * invDist * invDist * y._7

        (s * r(0), s * r(1), s * r(2))
      }).reduce((y, z) => (y._1 + z._1, y._2 + z._2, y._3 + z._3))

      val px = x._4 * deltaT + 0.5f * acceleration._1 * deltaT * deltaT
      val py = x._5 * deltaT + 0.5f * acceleration._2 * deltaT * deltaT
      val pz = x._6 * deltaT + 0.5f * acceleration._3 * deltaT * deltaT

      (x._1 + px.toFloat,
        x._2 + py.toFloat,
        x._3 + pz.toFloat,
        x._4 + acceleration._1.toFloat * deltaT,
        x._5 + acceleration._2.toFloat * deltaT,
        x._6 + acceleration._3.toFloat * deltaT,
        x._7)
    }).map(_.productIterator)
      .reduce(_ ++ _).asInstanceOf[Iterator[Float]].toArray

    val calcAcc =
      UserFun("calcAcc",
        Array("x1", "y1", "z1", "x2", "y2", "z2", "mass2", "espSqr"),
        """|{
          |  float4 r = (x1 - x2, y1 - y2, z1 - z2, 0.0f);
          |  float distSqr = r.x + r.y + r.z;
          |  float invDist = 1.0f / sqrt(distSqr + espSqr);
          |  float invDistCube = invDist * invDist * invDist;
          |  float s = invDistCube * mass2;
          |  Tuple acc = {s * r.x, s * r.y, s * r.z};
          |  return acc;
          |}
          | """.stripMargin,
        Seq(Float, Float, Float, Float, Float, Float, Float, Float),
        TupleType(Float, Float, Float))

    val reduce =
      UserFun("reduce",
        Array("x", "y"),
        "{ Tuple t = {x._0 + y._0, x._1 + y._1, x._2 + y._2}; return t;}",
        Seq(TupleType(Float, Float, Float),
          TupleType(Float, Float, Float)),
        TupleType(Float, Float, Float))

    val update =
      UserFun("update",
        Array("x", "y", "z", "velX", "velY", "velZ", "mass",
          "deltaT", "acceleration"),
        """|{
          |  float px = velX * deltaT + 0.5f * acceleration._0 * deltaT * deltaT;
          |  float py = velY * deltaT + 0.5f * acceleration._1 * deltaT * deltaT;
          |  float pz = velZ * deltaT + 0.5f * acceleration._2 * deltaT * deltaT;
          |  Tuple1 t = {x + px, y + py, z + pz,
          |              velX + acceleration._0 * deltaT,
          |              velY + acceleration._1 * deltaT,
          |              velZ + acceleration._2 * deltaT, mass};
          |  return t;
          |}
        """.stripMargin,
        Seq(Float, Float, Float, Float, Float, Float, Float,
          Float, TupleType(Float, Float, Float)),
        TupleType(Float, Float, Float, Float, Float, Float, Float))

    val function = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      Float,
      Float,
      (x, y, z, velX, velY, velZ, mass, espSqr, deltaT) =>
        MapGlb(fun(x1y1z1 =>

          toGlobal(MapSeq(fun(acceleration =>
            update(Get(x1y1z1, 0), Get(x1y1z1, 1), Get(x1y1z1, 2),
              Get(x1y1z1, 3), Get(x1y1z1, 4), Get(x1y1z1, 5),
              Get(x1y1z1, 6), deltaT, acceleration))))

            o ReduceSeq(reduce, (0.0f, 0.0f, 0.0f))

            o MapSeq(fun(x2y2z2 =>
            calcAcc(Get(x1y1z1, 0), Get(x1y1z1, 1), Get(x1y1z1, 2),
              Get(x2y2z2, 0), Get(x2y2z2, 1), Get(x2y2z2, 2),
              Get(x2y2z2, 6), espSqr)))
            $ Zip(x, y, z, velX, velY, velZ, mass)

        )) $ Zip(x, y, z, velX, velY, velZ, mass)
    )

    val (output: Array[Float], _) =
      Execute(inputSize)(function,
        x, y, z, velX, velY, velZ, mass, espSqr, deltaT)

    assertArrayEquals(gold, output, 0.0001f)
  }

  val inputSize = 512

  val deltaT = 0.005f
  val espSqr = 500.0f

  // x, y, z, velocity x, y, z, mass
  val input = Array.fill(inputSize)((util.Random.nextFloat(),
    util.Random.nextFloat(),
    util.Random.nextFloat(),
    0.0f, 0.0f, 0.0f,
    util.Random.nextFloat()))

  val x = input.map(_._1)
  val y = input.map(_._2)
  val z = input.map(_._3)

  val velX = input.map(_._4)
  val velY = input.map(_._5)
  val velZ = input.map(_._6)

  val mass = input.map(_._7)

  val gold: Array[Float] = nBodyScala(deltaT, espSqr, input)

  val pos = Array.ofDim[Float](inputSize*4)
  val vel = Array.ofDim[Float](inputSize*4)

  for (i <- 0 until inputSize) {
    pos(4*i) = x(i)
    pos(4*i+1) = y(i)
    pos(4*i+2) = z(i)
    pos(4*i+3) = mass(i)

    vel(4*i) = velX(i)
    vel(4*i+1) = velY(i)
    vel(4*i+2) = velZ(i)
    vel(4*i+3) = mass(i)
  }

  val N = SizeVar("N")

  @Test
  def nBodyAMD(): Unit = {

    val function = NBody.function

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

    val function = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(0)(fun(p1Chunk => // ArrayType(Flat4, 128)

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
              ), MapLcl(idF4) $ Value("0.0f", ArrayType(Float4, 128))) o Split(128) $ pos)
          )) o Split(128) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(128, inputSize, (true, false))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test
  def nBodyLocalMem2(): Unit = {

    val tileX = 32

    val function = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(fun(p1Chunk => // ArrayType(Flat4, 128)
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
                  ), MapLcl(idF4) $ Value("0.0f", ArrayType(Float4, tileX))) o Split(tileX) $ pos)
            ) $ Zip(toPrivate(MapLcl(idF4)) $ Get(Unzip() $ p1Chunk, 0), Get(Unzip() $ p1Chunk, 1))
          )) o Split(tileX) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(tileX, inputSize, (true, false))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }


  @Test
  def nBodyLocalMem_nvidia_no_MT(): Unit = {

    val tileX = 32
    val tileY = 1
    val threadsX = inputSize
    val threadsY = tileY

    val numGroups1 = threadsY / tileY

    val function = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(1)(Join() o MapWrg(0)(fun(p1Chunk => // ArrayType(Flat4, tileX)
            \(newP1Chunk =>
              MapLcl(1)(\(bla =>
                toGlobal(MapLcl(0)( fun( p1 =>
                  NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
                ))) $ Zip(newP1Chunk, bla))) o
                Join() o
                ReduceSeq(fun((acc, p2) =>
                  Let(p2Local =>
                    MapLcl(1)(\(accDim2 =>
                      Join() o
                        MapLcl(0)(fun(p1 => // ( (float4, float4), float4 )

                          ReduceSeq(fun((acc, p2) =>
                            NBody.calcAccAndAccumulate(Get(Get(p1,0), 0), p2, deltaT, espSqr, acc)),
                            Get(p1,1)) $ accDim2._0
                        )) $ Zip(newP1Chunk, accDim2._1)
                    )) $ Zip(p2Local, acc)
                  ) o toLocal(MapLcl(1)(MapLcl(0)(idF4))) $ p2
                ), MapLcl(1)(MapLcl(0)(idF4)) $ Value("0.0f", ArrayType(ArrayType(Float4, tileX), tileY))) o Split(tileY) o Split(tileX) $ pos
            ) $ Zip(toPrivate(MapLcl(idF4)) $ Get(Unzip() $ p1Chunk, 0), Get(Unzip() $ p1Chunk, 1))
          )) o Split(tileX)) o Split(N/numGroups1) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(tileX, tileY, threadsX, threadsY, (true, false))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test
  def nBodyLocalMem_nvidia_MT(): Unit = {

    val tileX = 32
    val tileY = 2
    val threadsX = inputSize
    val threadsY = tileY

    val numGroups1 = threadsY / tileY

    // TODO: Without extra Reduction, adding toLocal(MapSeq(MapLcl(...))) won't be unrolled though it needs to be
    // TODO: Unnecessary barrier in the final reduction which makes the kernel illegal.
    // TODO: Non correctness issue: can't do the WRAP optimisation

    // Problems in the NVIDIA OpenCL version:
    //  * Final Reduction is performed by threads with get_local_id(0) == 0, but adressing with threadIdxx (get_local_id(0)).
    //    So every thread with l_id_0 == 0 will have the same result in `acc` because they all reduce over the same row.
    //  * All threads return `acc` which can contain partial accumulations
    //  * All threads use the `acc` (which can be partial/wrong) to update the position and velocity.
    //  * All threads write the updated locations to memory.

    val function = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(1)(Join() o MapWrg(0)(fun(p1Chunk => // ArrayType(Flat4, tileX)
            \(newP1Chunk =>
              MapLcl(1)(\(bla =>
                toGlobal(MapLcl(0)( fun( p1 =>
                  NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
                ))) $ Zip(newP1Chunk, bla)) o
                Join() o
                ReduceSeq(\( (acc, x) => MapLcl(0)(VectorizeUserFun(4, add)) $ Zip(acc, x)), MapLcl(0)(idF4) $ Value(0.0f, ArrayType(Float4, tileX)))) o toLocal(MapSeq(MapLcl(1)(MapLcl(0)(idF4)))) o
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
                ), MapLcl(1)(MapLcl(0)(idF4)) $ Value("0.0f", ArrayType(ArrayType(Float4, tileX), tileY))) o Split(tileY) o Split(tileX) $ pos
            ) $ Zip(toPrivate(MapLcl(idF4)) $ Get(Unzip() $ p1Chunk, 0), Get(Unzip() $ p1Chunk, 1))
          )) o Split(tileX)) o Split(N/numGroups1) $ Zip(pos, vel)
    )

    val (output: Array[Float], _) =
      Execute(tileX, tileY, threadsX, threadsY, (true, true))(function, pos, vel, espSqr, deltaT)
    assertArrayEquals(gold, output, 0.0001f)
  }

}

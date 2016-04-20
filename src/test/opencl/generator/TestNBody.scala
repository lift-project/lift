package opencl.generator

import java.io.{File, PrintWriter}

import apart.arithmetic.Var
import benchmarks.NBody
import ir._
import ir.ast._
import ir.printer.DotPrinter
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

import sys.process._

object TestNBody {

  @BeforeClass
  def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass
  def after() {
    Executor.shutdown()
  }
}

class TestNBody {

  @Test
  def nBody(): Unit = {

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

    val N = Var("N")

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

    val (output: Array[Float], runtime) =
      Execute(inputSize)(function,
        x, y, z, velX, velY, velZ, mass, espSqr, deltaT)

    assertArrayEquals(gold, output, 0.0001f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test
  def nBodyAMD(): Unit = {

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

    val function = NBody.function

    val (output: Array[Float], _) =
      Execute(inputSize)(function, pos, vel, espSqr, deltaT)

    assertArrayEquals(gold, output, 0.0001f)
  }

  @Test
  def nBodyPrivateMem(): Unit = {

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

    val function = NBody.lessLoadsToGlobal

    val (output: Array[Float], _) =
      Execute(inputSize)(function, pos, vel, espSqr, deltaT)

    assertArrayEquals(gold, output, 0.0001f)

  }

  @Test
  def nBodyLocalMem(): Unit = {

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

    val N = Var("N")

    val function = fun(
      ArrayType(Float4, N),
      ArrayType(Float4, N),
      Float,
      Float,
      (pos, vel, espSqr, deltaT) =>
        Join() o
          MapWrg(fun(p1Chunk => // ArrayType(Flat4, 128)

            toGlobal(MapLcl( fun( p1 =>
              NBody.update(Get(Get(p1,0), 0), Get(Get(p1, 0), 1), deltaT, Get(p1,1))
            ))) $ Zip(p1Chunk,
              Join() o
                ReduceSeq(fun((acc, p2) =>
                Let(p2Local =>
                  Join() o
                    MapLcl(fun(p1 => // ( (float4, float4), float4 )

                      ReduceSeq(fun((acc, p2) =>
                        NBody.calcAcc(Get(Get(p1,0), 0), p2, deltaT, espSqr, acc)),
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
}

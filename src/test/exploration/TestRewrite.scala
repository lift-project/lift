package exploration

import apart.arithmetic.Var
import ir.ast._
import opencl.executor.Executor
import ir._
import opencl.ir._
import org.junit.{Ignore, AfterClass, BeforeClass, Test}
import org.junit.Assert._

object TestRewrite {
  @BeforeClass def before() {
    Executor.loadLibrary()
    Executor.init()
  }

  @AfterClass def after() {
    Executor.shutdown()
  }
}

class TestRewrite {

  @Test
  def mapMapMapLowering(): Unit = {
    val N = Var("N")

    val f = fun(ArrayType(ArrayType(ArrayType(Float, N), N), N),
        input => Map(Map(Map(id))) $ input)

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Test
  def reduceLowering(): Unit = {
    val N = Var("N")
    val f = fun(ArrayType(Float, N), input => {
      Reduce(add, 0.0f) $ input
    })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Test
  def scalLowering() : Unit = {
    val f = fun(ArrayType(Float, Var("N")), Float, (input, alpha) => {
      Map(\(x => mult(alpha, x))) $ input
    })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Test
  def asumLowering() : Unit = {
    val f = fun(ArrayType(Float, Var("N")), input => {
      Reduce(add, 0.0f) o Map(abs) $ input
    })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def dotLowering() : Unit = {
    val N = Var("N")
    val f = fun(ArrayType(Float, N), ArrayType(Float, N), (left, right) => {
      Zip(left, right) :>> Map(mult) :>> Reduce(add, 0.0f)
    })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def gemvLowering() : Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      ArrayType(Float, N),
      Float,
      Float,
      (matrix, vectorX, vectorY, alpha, beta) => {
        // combine a single row of the matrix with an element of Y
        Zip(matrix, vectorY) :>>
        // for every row and element pair ...
        Map(fun(pair => {
          val row = pair._0
          val y_i = pair._1
          // ... combine the vector X with a single row ...
          Zip(vectorX, row) :>>
          // ... pairwise multiply them ...
          Map(mult) :>>
          // ... reduce the resulting temporary vector ...
          Reduce(add, 0.0f) :>>
          // ... and finally for the computed sum ...
          Map(fun(sum => {
            // ... multiply it with alpha ...
            val xa = mult(sum, alpha)
            // ... and add it to the element of Y multiplied by beta
            add(xa, mult(y_i, beta))
            })
          )})
        )
      })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def mvLowering() : Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(Float, M),
      (matrix, vector) => {
        matrix :>>
        Map(fun(row =>
          Zip(row, vector) :>> Map(mult) :>> Reduce(add, 0.0f)
        ))
      })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def mvAsMmLowering() : Unit = {
    val N = Var("N")
    val M = Var("M")

    val f = fun(
      ArrayType(ArrayType(Float, M), N),
      ArrayType(ArrayType(Float, 1), M), // Column vector
      (matrix, vector) => {
        matrix :>>
        Map(fun(row =>
          vector :>> Transpose() :>>
          Map(fun(col =>
            Zip(row, col) :>>
            Map(mult) :>>
            Reduce(add, 0.0f)
          ))
        ))
      })


    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def mmLowering() : Unit = {
    val N = Var("N")
    val M = Var("M")
    val K = Var("K")

    val f = fun(
      ArrayType(ArrayType(Float, K), M),
      ArrayType(ArrayType(Float, N), K),
      (A, B) => {
        A :>>
        Map(fun(aRow =>
          B :>> Transpose() :>>
          Map(fun(bCol =>
            Zip(aRow, bCol) :>> Map(mult) :>> Reduce(add, 0.0f)
          ))
        ))
      })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Test
  def mandelbrotLowering() : Unit = {
    val md = UserFun("md", Array("i", "j", "niters", "size"),
      """|{
        |  const float space = 2.0f / size;
        |  float Zr = 0.0f;
        |  float Zi = 0.0f;
        |  float Cr = (j * space - 1.5f);
        |  float Ci = (i * space - 1.0f);
        |  int y = 0;
        |
        |  for (y = 0; y < niters; y++) {
        |    const float ZiN = Zi * Zi;
        |    const float ZrN = Zr * Zr;
        |    if(ZiN + ZrN > 4.0f) break;
        |    Zi *= Zr;
        |    Zi *= 2.0f;
        |    Zi += Ci;
        |    Zr = ZrN - ZiN + Cr;
        |  }
        |  return ((y * 255) / niters);
        |}
        |""".stripMargin, Seq(Int, Int, Int, Int), Int)

    val f = fun(
      ArrayType(Int, Var("N")), Int, Int, (in, niters, size) => {
        in :>> Map(fun(i =>
          in :>> Map(fun(j => md(i, j, niters, size)))
        ))
      })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def kmeansLowering() : Unit = {
    val distance = UserFun("distance", Array("x", "y", "a", "b", "id"),
      "{ Tuple t = {(x - a) * (x - a) + (y - b) * (y - b), id}; return t; }",
      Seq(Float, Float, Float, Float, Int), TupleType(Float, Int))

    val minimum = UserFun("minimum", Array("x", "y"),
      "{ return x._0 < y._0 ? x : y; }",
      Seq(TupleType(Float, Int), TupleType(Float, Int)), TupleType(Float, Int))

    val getSecond = UserFun("getSecond", "x",
      "{ return x._1; }",
      TupleType(Float, Int), Int)

    val N = Var("N")
    val K = Var("K")

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, K),
      ArrayType(Float, K),
      ArrayType(Int, K),
      (x, y, a, b, i) => {
        Zip(x, y) :>>
        Map(fun(xy => {
          Zip(a, b, i) :>>
          Map(fun(abi => distance(xy._0, xy._1, abi._0, abi._1, abi._2))) :>>
          Reduce(minimum, (scala.Float.MaxValue, -1)) :>>
          Map(getSecond)
        }))
      })

    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

  @Ignore
  @Test
  def nbodyLowering() : Unit = {
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

    val f = fun(
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      ArrayType(Float, N),
      Float,
      Float,
      (x, y, z, velX, velY, velZ, mass, espSqr, deltaT) => {
        Zip(x, y, z, velX, velY, velZ, mass) :>>
        Map(fun(x1y1z1 =>
          Zip(x, y, z, velX, velY, velZ, mass) :>>
          Map(fun(x2y2z2 =>
            calcAcc(x1y1z1._0, x1y1z1._1, x1y1z1._2,
                    x2y2z2._0, x2y2z2._1, x2y2z2._2,
                    x2y2z2._6, espSqr))) :>>
          Reduce(reduce, (0.0f, 0.0f, 0.0f)) :>>
          Map(fun(acceleration =>
            update(x1y1z1._0, x1y1z1._1, x1y1z1._2,
                   x1y1z1._3, x1y1z1._4, x1y1z1._5,
                   x1y1z1._6, deltaT, acceleration)))
          ))
      })


    val fs = Lower.lower(f)
    println(s"found: ${fs.size} lowerings")

    assertTrue(fs.nonEmpty)
    assertTrue(fs.forall(_.isGenerable))
  }

}

package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.Compile
import opencl.ir._
import org.junit.Assert._
import org.junit.Test

class TestLowering {

  // To avoid the kernel with the data race in gemv, kmeans and nbody that
  // doesn't compile using private memory for the reduce
  private val simpleMapping = EnabledMappings(
    global0 = true, global01 = false, global10 = false,
    global012 = false, global210 = false,
    group0 = false, group01 = false, group10 = false
  )

  @Test
  def lowering2DStencilWithAt() {
    def vonNeumann5pt(x: Param) = {
      val top = x.at(0).at(1)
      val bottom = x.at(2).at(1)
      val left = x.at(1).at(0)
      val right = x.at(1).at(2)
      val center = x.at(1).at(1)
      (top, bottom, left, right, center)
    }

    val M = 8192
    val N = 8192

    def grad = UserFun("grad", Array("top", "bottom", "left", "right", "center"),
      """return center + 1.0f/sqrt(0.0001f +
        | (center-top)*(center-top) +
        | (center-bottom)*(center-bottom) +
        | (center-right)*(center-right) +
        | (center-left)*(center-left));""".stripMargin,
      Seq(Float, Float, Float, Float, Float), Float)

    val f = λ(
      ArrayType(ArrayType(Float, M), N),
      input => {
        Map(Scatter(Shift(1))) o Scatter(Shift(1)) o Pad2D(1, 1, Pad.Boundary.Clamp) o
          Map(Map(λ(nbh => {

            val (top, bottom, left, right, center) = vonNeumann5pt(nbh)

            λ(x =>
              grad(x, bottom, left, right, center)) $ top

          }))) o Slide2D(3, 1) $ input
      })
    val simpleMapping = EnabledMappings(
      global0 = false, global01 = true, global10 = true,
      global012 = false, global210 = false,
      group0 = false, group01 = false, group10 = false
    )

    val lowered = Lower.mapCombinations(f, simpleMapping)
    assertFalse(lowered.isEmpty)
  }

  @Test
  def mapMapMapLowering(): Unit = {
    val N = SizeVar("N")

    val f = fun(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N), N),
      input => Map(Map(Map(id))) $ input)

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def reduceLowering(): Unit = {
    val N = SizeVar("N")
    val f = fun(ArrayTypeWSWC(Float, N), input => {
      Reduce(add, 0.0f) $ input
    })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def scalLowering(): Unit = {
    val f = fun(ArrayTypeWSWC(Float, SizeVar("N")), Float, (input, alpha) => {
      Map(\(x => mult(alpha, x))) $ input
    })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def asumLowering(): Unit = {
    val f = fun(ArrayTypeWSWC(Float, SizeVar("N")), input => {
      Reduce(add, 0.0f) o Map(abs) $ input
    })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def dotLowering(): Unit = {
    val N = SizeVar("N")
    val f = fun(ArrayTypeWSWC(Float, N), ArrayTypeWSWC(Float, N), (left, right) => {
      Zip(left, right) :>> Map(\(x => mult(x._0, x._1))) :>> Reduce(add, 0.0f)
    })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def gemvLowering(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      ArrayTypeWSWC(Float, N),
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
              Map(\(x => mult(x._0, x._1))) :>>
              // ... reduce the resulting temporary vector ...
              Reduce(add, 0.0f) :>>
              // ... and finally for the computed sum ...
              Map(fun(sum => {
                // ... multiply it with alpha ...
                val xa = mult(sum, alpha)
                // ... and add it to the element of Y multiplied by beta
                add(xa, mult(y_i, beta))
              })
              )
          })
          )
      })

    val fs = Lower.mapCombinations(f, simpleMapping)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def mvLowering(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(Float, M),
      (matrix, vector) => {
        matrix :>>
          Map(fun(row =>
            Zip(row, vector) :>> Map(\(x => mult(x._0, x._1))) :>> Reduce(add, 0.0f)
          ))
      })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def mvAsMmLowering(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, 1), M), // Column vector
      (matrix, vector) => {
        matrix :>>
          Map(fun(row =>
            vector :>> Transpose() :>>
              Map(fun(col =>
                Zip(row, col) :>>
                  Map(\(x => mult(Get(x, 0), Get(x, 1)))) :>>
                  Reduce(add, 0.0f)
              ))
          ))
      })


    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def mmLowering(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        A :>>
          Map(fun(aRow =>
            B :>> Transpose() :>>
              Map(fun(bCol =>
                Zip(aRow, bCol) :>> Map(\(x => mult(x._0, x._1))) :>> Reduce(add, 0.0f)
              ))
          ))
      })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def mandelbrotLowering(): Unit = {
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
      ArrayTypeWSWC(Int, SizeVar("N")), Int, Int, (in, niters, size) => {
        in :>> Map(fun(i =>
          in :>> Map(fun(j => md(i, j, niters, size)))
        ))
      })

    val fs = Lower.mapCombinations(f)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

  @Test
  def kmeansLowering(): Unit = {
    val distance = UserFun("distance", Array("x", "y", "a", "b", "id"),
      "{ Tuple t = {(x - a) * (x - a) + (y - b) * (y - b), id}; return t; }",
      Seq(Float, Float, Float, Float, Int), TupleType(Float, Int))

    val minimum = UserFun("minimum", Array("x", "y"),
      "{ return x._0 < y._0 ? x : y; }",
      Seq(TupleType(Float, Int), TupleType(Float, Int)), TupleType(Float, Int))

    val getSecond = UserFun("getSecond", "x",
      "{ return x._1; }",
      TupleType(Float, Int), Int)

    val N = SizeVar("N")
    val K = SizeVar("K")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, K),
      ArrayTypeWSWC(Float, K),
      ArrayTypeWSWC(Int, K),
      (x, y, a, b, i) => {
        Zip(x, y) :>>
          Map(fun(xy => {
            Zip(a, b, i) :>>
              Map(fun(abi => distance(xy._0, xy._1, abi._0, abi._1, abi._2))) :>>
              Reduce(minimum, (scala.Float.MaxValue, -1)) :>>
              Map(getSecond)
          }))
      })

    val fs = Lower.mapCombinations(f, simpleMapping)

    assertTrue(fs.nonEmpty)
    // TODO: Should MapWrg(MapLcl(_) o ReduceSeq(_) $ ...) be allowed or not?
    // TODO: Probably not, can fail on Intel. Also disabled for now.
    fs.foreach(Compile(_))
  }

  @Test
  def nbodyLowering(): Unit = {
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

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
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


    val fs = Lower.mapCombinations(f, simpleMapping)

    assertTrue(fs.nonEmpty)
    fs.foreach(Compile(_))
  }

}

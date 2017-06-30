package opencl.generator

import benchmarks.{BlackScholes, DotProduct, MolecularDynamics}
import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert.{assertEquals, _}
import org.junit.{AfterClass, BeforeClass, Test}

object TestBenchmark {
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

class TestBenchmark {

  @Test def testBenchmarkMethod(): Unit = {
    def dotProd(left: Array[Float], right: Array[Float]): Float = {
      (left,right).zipped.map(_*_).sum
    }

    val inputSize = 1024
    val leftInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
    val rightInputData = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)

    val (output: Array[Float], runtimes) = Execute(
      128, 1, 1,
      1024, 1, 1,
      (false, false)
    ).benchmark(10, 0.0, Compile(DotProduct.dotProductSimple), DotProduct.dotProductSimple,
      leftInputData, rightInputData)

    val sorted = runtimes.sorted

    println("min runtime = " + sorted.head)
    println("max runtime = " + sorted.last)
    println("median runtime = " + sorted(runtimes.length/2))

    assertEquals(10, runtimes.length)

    assertEquals(dotProd(leftInputData, rightInputData), output.sum, 0.0)
  }

  // A mix between AMD and nVidia versions, with the CND function inlined in the UserFunDef
  @Test def blackScholes(): Unit = {

    val inputSize = 512
    val input = Array.fill(inputSize)(util.Random.nextFloat())

    val gold = BlackScholes.runScala(input)

    val kernel = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      inRand => MapGlb(BlackScholes.blackScholesComp) $ inRand
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(kernel, input)

    assertArrayEquals(gold, output, 0.01f)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def mandelbrot(): Unit = {
    val inputSize = 512

    val iterations = 100

    val input = Array.range(0, inputSize)

    val gold = input.flatMap(i => {
      input.map(j => {
        val space = 2.0f / inputSize

        var Zr = 0.0f
        var Zi = 0.0f
        val Cr = j * space - 1.5f
        val Ci = i * space - 1.0f

        var ZrN = 0.0f
        var ZiN = 0.0f
        var y = 0
        while (ZiN + ZrN <= 4.0f && y < iterations) {
          Zi = 2.0f * Zr * Zi + Ci
          Zr = ZrN - ZiN + Cr
          ZiN = Zi * Zi
          ZrN = Zr * Zr
          y += 1
        }

        (y * 255) / iterations
      })
    })

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
      ArrayTypeWSWC(Int, SizeVar("N")),
      Int,
      Int,
      (in, niters, size) => MapGlb(fun(i => MapSeq(fun(j => md(i, j, niters, size))) $ in)) $ in
    )

    val (output: Array[Int], runtime) = Execute(inputSize)(f, input, iterations, inputSize)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)


    assertArrayEquals(gold, output)
  }

  @Test def md(): Unit = {

    val inputSize = 1024
    val maxNeighbours = 128
  
    val particles = Array.fill(inputSize, 4)(util.Random.nextFloat() * 20.0f)
    val particlesTuple = particles.map { case Array(a, b, c, d) => (a, b, c, d) }
    val neighbours = MolecularDynamics.buildNeighbourList(particlesTuple, maxNeighbours)
    val cutsq = 16.0f
    val lj1 = 1.5f
    val lj2 = 2.0f

    val gold = MolecularDynamics.mdScala(particlesTuple, neighbours.transpose, cutsq, lj1, lj2).map(_.productIterator).reduce(_ ++ _).asInstanceOf[Iterator[Float]].toArray

    val N = SizeVar("N")
    val M = SizeVar("M")

    val f = fun(
      ArrayTypeWSWC(Float4, N),
      ArrayTypeWSWC(ArrayTypeWSWC(Int, M), N),
      Float,
      Float,
      Float,
      (particles, neighbourIds, cutsq, lj1, lj2) =>
        MapGlb(fun(p =>
          toGlobal(MapSeq(id.vectorize(4))) o
            ReduceSeq(fun((force, n) =>
              MolecularDynamics.mdCompute.apply(force, Get(p, 0), n, cutsq, lj1, lj2)
            ), Value(0.0f, Float4)) $ Filter(particles, Get(p, 1))
        )) $ Zip(particles, neighbourIds)
    )

    val (output: Array[Float], runtime) =
      Execute(inputSize)(f, particles, neighbours, cutsq, lj1, lj2)

    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertEquals(output.length, gold.length)

    (output, gold).zipped.map((x, y) => {

      var diff = (x - y) / x

      if (x == 0.0f)
        diff = 0.0f

      math.sqrt(diff * diff).toFloat
    }).zipWithIndex.foreach(x => assertEquals("Error at pos " + x._2, 0.0f, x._1, 0.1f))

  }

  @Test def mdShoc(): Unit = {

    val inputSize = 1024
    val maxNeighbours = 128

    val particles = Array.fill(inputSize, 4)(util.Random.nextFloat() * 20.0f)
    val particlesTuple = particles.map { case Array(a, b, c, d) => (a, b, c, d) }
    val neighbours = MolecularDynamics.buildNeighbourList(particlesTuple, maxNeighbours).transpose
    val cutsq = 16.0f
    val lj1 = 1.5f
    val lj2 = 2.0f

    val gold = MolecularDynamics.mdScala(particlesTuple, neighbours, cutsq, lj1, lj2)
               .map(_.productIterator).reduce(_ ++ _).asInstanceOf[Iterator[Float]].toArray

    val (output: Array[Float], _) =
      Execute(inputSize)(MolecularDynamics.shoc, particles, neighbours, cutsq, lj1, lj2)

    assertEquals(output.length, gold.length)

    (output, gold).zipped.map((x, y) => {

      var diff = (x - y) / x

      if (x == 0.0f)
        diff = 0.0f

      math.sqrt(diff * diff).toFloat
    }).zipWithIndex.foreach(x => assertEquals("Error at pos " + x._2, 0.0f, x._1, 0.1f))

  }

  // Compute single precision a * x + y
  @Test def saxpy(): Unit = {
    // domain size
    val inputSize = 128
    val N = SizeVar("N")

    // Input variables
    val a: Float = 5.0f
    val xs = Array.fill(inputSize)(util.Random.nextFloat())
    val ys = Array.fill(inputSize)(util.Random.nextFloat())

    // Cross validation
    val gold = (xs.map(_ * a), ys).zipped map (_ + _)

    // user function
    val fct = UserFun("saxpy", Array("a", "x", "y"),
      " return a * x + y; ",
      Seq(Float, Float, Float), Float)

    // Expression
    val f = fun(
      Float,
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (a, xs, ys) => MapGlb(
        fun(xy => fct(
          a, Get(xy, 0), Get(xy, 1)
        ))
      ) $ Zip(xs, ys)
    )

    // execute
    val (output: Array[Float], runtime) = Execute(inputSize)(f, a, xs, ys)

    println("runtime = " + runtime)
    assertArrayEquals(gold, output, 0.001f)
  }

  @Test
  def mriQ(): Unit = {
    val phiMag = UserFun("phiMag",
                         Array("phiR", "phiI"),
                         "{ return phiR * phiR + phiI * phiI }",
                         Seq(Float, Float),
                         Float)

    val qFun = UserFun("computeQ",
                           Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag", "acc"),
                           """{
                             |    #define PIx2 6.2831853071795864769252867665590058f
                             |    float expArg = PIx2 * (Kx * sX +
                             |			   Ky * sY +
                             |			   Kz * sZ);
                             |    acc._0 = acc._0 + PhiMag * cos(expArg);
                             |    acc._1 = acc._1 + PhiMag * sin(expArg);
                             |
                             |    return acc;
                             |}""".stripMargin,
                           Seq(Float, Float, Float, Float, Float, Float, Float, TupleType(Float, Float)),
                           TupleType(Float, Float))


    val pair = UserFun("pair", Array("x", "y"), "{ Tuple t = {x, y}; return t; }",
      Seq(Float, Float), TupleType(Float, Float))

    val k = SizeVar("K")

    val computePhiMag = fun(
      ArrayTypeWSWC(Float, k),
      ArrayTypeWSWC(Float, k),
      (phiR, phiI) => MapGlb(phiMag) $ Zip(phiR, phiI)
    )

    val x = SizeVar("X")

    val computeQ = fun(
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, k),
      ArrayTypeWSWC(Float, k),
      ArrayTypeWSWC(Float, k),
      ArrayTypeWSWC(Float, k),
      (x, y, z, Qr, Qi, kx, ky, kz, phiMag) =>
        MapGlb(fun(t =>
          toGlobal(MapSeq(idFF)) o
            ReduceSeq(fun((acc, p) =>
              qFun(Get(t, 0), Get(t, 1), Get(t, 2), Get(p, 0), Get(p, 1), Get(p, 2), Get(p, 3), acc)
            ), toPrivate(fun(x => pair(Get(x, 0), Get(x, 1)))) $ Tuple(Get(t, 3), Get(t, 4))
            ) $ Zip(kx, ky, kz, phiMag)
        )) $ Zip(x, y, z, Qr, Qi)
    )

    Compile(computePhiMag)
    Compile(computeQ)
  }

  @Test
  def mriQ2(): Unit = {
    val phiMag = UserFun("phiMag",
      Array("phiR", "phiI"),
      "{ return phiR * phiR + phiI * phiI; }",
      Seq(Float, Float),
      Float)

    val qFun = UserFun("computeQ",
      Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag", "acc"),
      """{
        |    #define PIx2 6.2831853071795864769252867665590058f
        |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
        |    acc._0 = acc._0 + PhiMag * cos(expArg);
        |    acc._1 = acc._1 + PhiMag * sin(expArg);
        |
        |    return acc;
        |}""".stripMargin,
      Seq(Float, Float, Float, Float, Float, Float, Float, TupleType(Float, Float)),
      TupleType(Float, Float))


    val pair = UserFun("pair", Array("x", "y"), "{ Tuple t = {x, y}; return t; }",
      Seq(Float, Float), TupleType(Float, Float))

    val k = SizeVar("K")

    val computePhiMag = fun(
      ArrayTypeWSWC(Float, k),
      ArrayTypeWSWC(Float, k),
      (phiR, phiI) => MapGlb(phiMag) $ Zip(phiR, phiI)
    )

    val x = SizeVar("X")

    val computeQ = fun(
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(Float, x),
      ArrayTypeWSWC(TupleType(Float, Float, Float, Float), k),
      (x, y, z, Qr, Qi, kvalues) =>
        Zip(x, y, z, Qr, Qi) :>>
          MapGlb(\(t =>
            t._0 :>> toPrivate(id) :>> Let(sX =>
              t._1 :>> toPrivate(id) :>> Let(sY =>
                t._2 :>> toPrivate(id) :>> Let(sZ =>
                  kvalues :>>
                    ReduceSeq(\((acc, p) =>
                      qFun(sX, sY, sZ, p._0, p._1, p._2, p._3, acc)
                    ), toPrivate(fun(x => pair(x._0, x._1))) $ Tuple(t._3, t._4)) :>>
                    toGlobal(MapSeq(idFF))
                )
              )
            )
          ))
    )

    Compile(computePhiMag)
    Compile(computeQ)
  }

  @Test
  def mersenneTwisterScala(): Unit = {
    val inputSize = 256

    val mulFactor = 2
    val width = 4

    val input = Array.fill(inputSize * width)(util.Random.nextInt())

    val l = 0xffffffffl

    val SHIFT_A = 24
    val SHIFT_B = 13
    val SHIFT_C = 15

    val MASK_X = 0xfdff37ff
    val MASK_Y = 0xef7f3f7d
    val MASK_Z = 0xff777b7d
    val MASK_W = 0x7ff7fb2f

    val intMax = 4294967296.0f
    val PI = 3.14159265358979f

    def lshift128(input: Array[Int], shift: Int): Array[Int] = {
      val invshift = 32 - shift

      val temp = Array.ofDim[Int](4)
      temp(0) = input(0) << shift
      temp(1) = (input(1) << shift) | (input(0) >>> invshift)
      temp(2) = (input(2) << shift) | (input(1) >>> invshift)
      temp(3) = (input(3) << shift) | (input(2) >>> invshift)

      temp
    }

    def rshift128(input: Array[Int], shift: Int): Array[Int] = {
      val invshift = 32 - shift

      val temp = Array.ofDim[Int](4)

      temp(3) = input(3) >>> shift
      temp(2) = (input(2) >>> shift) | (input(3) << invshift)
      temp(1) = (input(1) >>> shift) | (input(2) << invshift)
      temp(0) = (input(0) >>> shift) | (input(1) << invshift)

      temp
    }

    val res = input.grouped(width).map(seed => {
      val temp = Array.ofDim[Int](8, width)

      val stateMask = 1812433253
      val thirty = 30

      var r1 = Array.fill(width)(0)
      var r2 = Array.fill(width)(0)

      var a = Array.fill(width)(0)
      var b = Array.fill(width)(0)

      //Initializing states.
      val state1 = Array(seed(0), seed(1), seed(2), seed(3))

      val state2 = state1.map(x => (x ^ (x >>> thirty)) * stateMask + 1)
      val state3 = state2.map(x => (x ^ (x >>> thirty)) * stateMask + 2)
      val state4 = state3.map(x => (x ^ (x >>> thirty)) * stateMask + 3)
      val state5 = state4.map(x => (x ^ (x >>> thirty)) * stateMask + 4)

      for(i <- 0 until mulFactor) {
        i match {
          case 0 =>
            r1 = state4
            r2 = state5
            a = state1
            b = state3
          case 1 =>
            r1 = r2
            r2 = temp(0)
            a = state2
            b = state4
          case 2 =>
            r1 = r2
            r2 = temp(1)
            a = state3
            b = state5
          case 3 =>
            r1 = r2
            r2 = temp(2)
            a = state4
            b = state1
          case 4 =>
            r1 = r2
            r2 = temp(3)
            a = state5
            b = state2
          case 5 =>
            r1 = r2
            r2 = temp(4)
            a = temp(0)
            b = temp(2)
          case 6 =>
            r1 = r2
            r2 = temp(5)
            a = temp(1)
            b = temp(3)
          case 7 =>
            r1 = r2
            r2 = temp(6)
            a = temp(2)
            b = temp(4)
        }

        val e = lshift128(a, SHIFT_A)
        val f = rshift128(r1, SHIFT_A)

        temp(i)(0) = a(0) ^ e(0) ^ ((b(0) >>> SHIFT_B) & MASK_X) ^ f(0) ^ (r2(0) << SHIFT_C)
        temp(i)(1) = a(1) ^ e(1) ^ ((b(1) >>> SHIFT_B) & MASK_Y) ^ f(1) ^ (r2(1) << SHIFT_C)
        temp(i)(2) = a(2) ^ e(2) ^ ((b(2) >>> SHIFT_B) & MASK_Z) ^ f(2) ^ (r2(2) << SHIFT_C)
        temp(i)(3) = a(3) ^ e(3) ^ ((b(3) >>> SHIFT_B) & MASK_W) ^ f(3) ^ (r2(3) << SHIFT_C)
      }

      val gaussianRand = Array.ofDim[Array[Float]](mulFactor)

      for(i <- 0 until mulFactor / 2) {

        val temp1 = temp(i).map(x => (x.toLong & l).toFloat * 1.0f / intMax)
        val temp2 = temp(i + 1).map(x => (x.toLong & l).toFloat * 1.0f / intMax)

        // Applying Box-Muller Transformations.
        val r = temp1.map(x => math.sqrt(-2.0f * math.log(x)).toFloat)
        val phi  = temp2.map(2.0f * PI * _)

        gaussianRand(i * 2 + 0) = (r, phi).zipped.map((a, b) => a * math.cos(b).toFloat)
        gaussianRand(i * 2 + 1) = (r, phi).zipped.map((a, b) => a * math.sin(b).toFloat)
      }

      gaussianRand.flatten
    })

    println(res.toArray.flatten.mkString(" "))
  }
}

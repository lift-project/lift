package opencl.generator

import ir.ast.{CheckedArrayAccess, Join, Zip, fun, _}
import ir.{ArrayType, ArrayTypeWC, ArrayTypeWSWC, TypeChecker, _}
import lift.arithmetic.{Cst, SizeVar}
import opencl.executor.{Compile, Execute, Executor, Utils}
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert.{assertArrayEquals, assertEquals}
import org.junit._


object TestArray {
  private var alignArray: Boolean = false

  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()

    if (Utils.isNvidiaGPU) {
      println("Align headers and content in arrays on nvidia")
      alignArray = AlignArrays()
      AlignArrays(true)
    }
  }

  @AfterClass def after(): Unit = {
    if (Utils.isNvidiaGPU) AlignArrays(alignArray)

    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestArray {
  /**
    * Size is not statically know but the capacity is.
    * The array is filled with integers so we don't need to store the offsets.
    * Layout: [size, elt_0, elt_1, …, elt_{κ-1}]
    */
  @Test
  def unknownSizeReduce(): Unit = {
    val capacity = 1024
    val size = 700
    val input = Vector.fill(size)(util.Random.nextInt(16))

    val f = fun(
      ArrayTypeWC(Int, capacity),
      in =>
        MapGlb(toGlobal(id(Int))) o ReduceSeq(addI, 0) $ in
    )

    val t = TypeChecker(f)
    assertEquals(t, ArrayTypeWSWC(Int, 1, 1))

    val exec = Execute(128)
    val (output, _) = exec[Vector[Int]](f, input)
    assertEquals(Vector(input.sum), output)
  }

  /**
    * Same situation but this time the output is an array of the same shape
    * and not a constant.
    */
  @Test
  def unknownSizeMap(): Unit = {
    val capacity = 1024
    val size = 879

    val iInput = Vector.fill(size)(util.Random.nextInt())
    val fInput = Vector.fill(size)(util.Random.nextFloat())

    def mkMapId(st: ScalarType): Lambda1 = fun(
      ArrayTypeWC(st, capacity),
      MapGlb(toGlobal(id(st))) $ _
    )

    val exec = Execute(128)
    val (iOutput, _) = exec[Vector[Int]](mkMapId(Int), iInput)
    assertEquals(iInput, iOutput)
    val (fOutput, _) = exec[Vector[Float]](mkMapId(Float), fInput)
    assertEquals(fInput, fOutput)
  }

  @Test
  def unknownSizeMapBool(): Unit = {
    val capacity = 128
    val size = 87
    val input = Vector.fill(size)(util.Random.nextBoolean())

    val mapId = fun(
      ArrayTypeWC(Bool, capacity),
      MapGlb(toGlobal(id(Bool))) $ _
    )

    val (bOutput, _) = Execute(128)[Vector[Boolean]](mapId, input)
    assertEquals(input, bOutput)
  }

  @Test
  def unknownSizeMapDouble(): Unit = {
    Assume.assumeTrue("Needs double support", Executor.supportsDouble())

    val capacity = 1024
    val size = 879
    val input = Vector.fill(size)(util.Random.nextDouble())

    val mapId = fun(
      ArrayTypeWC(Double, capacity),
      MapGlb(toGlobal(id(Double))) $ _
    )

    val exec = Execute(128)
    val (dOutput, _) = exec[Vector[Double]](mapId, input)
    assertEquals(input, dOutput)
  }

  /**
    * This time we don't know the size either but we know the shape of the
    * output.
    * Layout: [κ, size, elt_0, elt_1, elt_2, …]
    */
  @Test
  def inputZeroKnowledge(): Unit = {
    val size = 128
    val input = Vector.fill(size)(util.Random.nextInt(16))

    val f = fun(
      ArrayType(Int),
      MapGlb(toGlobal(idI)) o ReduceSeq(addI, 0) $ _
    )

    assertEquals(TypeChecker(f), ArrayTypeWSWC(Int, 1, 1))

    val exec = Execute(128)
    val (output, _) = exec[Vector[Int]](f, input)
    assertEquals(Vector(input.sum), output)
  }

  /**
    * Here, you know nothing (Jon Snow).
    */
  @Test
  def zeroKnowledge(): Unit = {
    val f = fun(
      ArrayType(Int),
      in =>
        MapGlb(toGlobal(idI)) $ in
    )

    assertEquals(TypeChecker(f), ArrayType(Int))

    Compile(f)
  }

  /**
    * Nested arrays with no size
    */
  @Test
  def nestedArraysNoSize(): Unit = {
    val capacity = 128
    val size1 = 90
    val size2 = 42
    val input = Vector.fill(size1, size2)(util.Random.nextFloat())
    val f = fun(
      ArrayTypeWC(ArrayTypeWC(Float, capacity), capacity),
      MapGlb(MapSeq(id(Float))) $ _
    )

    assertEquals(TypeChecker(f), ArrayTypeWC(ArrayTypeWC(Float, capacity), capacity))

    val exec = Execute(capacity)
    val (output, _) = exec[Vector[Vector[Float]]](f, input)

    assertArrayEquals(input.flatten.toArray, output.flatten.toArray, 0.0001f)
  }

  /**
   * If some capacity is not known at compile time in the type of an input
   * we can still allocate it at the last minute.
   */
  @Test
  def unknownInnerCapacity(): Unit = {
    val size = 128
    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(ArrayType(Int), N),

      Join() o
      MapGlb(MapSeq(toGlobal(idI)) o ReduceSeq(addI, 0)) $ _
    )

    val input = Vector.fill(size)(Vector.fill(4 + util.Random.nextInt(8))(util.Random.nextInt(16)))
    val exec = Execute(size)
    val (output, _) = exec[Vector[Int]](f, input)
    assertEquals(input.map(_.sum), output)
  }

  /**
    * Nested arrays.
    */
  @Test
  def nestedArraysZeroKnowledge(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Int)),
      in =>
        MapGlb(MapSeq(toGlobal(idI))) $ in
    )

    assertEquals(TypeChecker(f), ArrayType(ArrayType(Int)))

    Compile(f)
  }

  @Ignore
  @Test
  def arrZipMapWAllocation(): Unit = {
    val f = fun(
      ArrayType(Float), ArrayType(Float), (p1, p2) =>
        ReduceSeq(add, 0.0f) o MapSeq(add) $ Zip(p1, p2)
    )

    assertEquals(TypeChecker(f), ArrayTypeWSWC(Float, Cst(1)))

    Compile(f)
  }

  @Test
  def arrZipMap(): Unit = {
    val f = fun(
      ArrayType(Float), ArrayType(Float), (p1, p2) =>
        toGlobal(MapSeq(id)) o ReduceSeq(fun((init, elem) => add(init, mult(elem._0, elem._1))), 0.0f) $ Zip(p1, p2)
    )

    assertEquals(TypeChecker(f), ArrayTypeWSWC(Float, Cst(1)))

    val p1 = Vector.fill(37)(util.Random.nextFloat())
    val p2 = Vector.fill(78)(util.Random.nextFloat())

    val exec = Execute(128)
    val (output, _) = exec[Vector[Float]](f, p1, p2)
    val (outputRev, _) = exec[Vector[Float]](f, p2, p1)

    val gold = (p1 zip p2.slice(0, 78)).map(p => p._1 * p._2).sum

    assertArrayEquals(Array(gold), output.toArray, 0.0001f)
    assertArrayEquals(Array(gold), outputRev.toArray, 0.0001f)
  }

  @Test
  def twoDArrZipReduce(): Unit = {
    val N = SizeVar("N")
    val f = fun(
      ArrayTypeWSWC(ArrayType(Float), N), ArrayTypeWSWC(ArrayType(Float), N), (p1, p2) =>
        Join() o MapGlb(
          fun(rowPair =>
            MapSeq(toGlobal(id)) o
              ReduceSeq(fun((init, elem) => add(init, mult(elem._0, elem._1))), 0.0f) $
              Zip(rowPair._0, rowPair._1)
          )
        ) $ Zip(p1, p2)
    )

    assertEquals(TypeChecker(f), ArrayTypeWSWC(Float, N))

    val height = 128

    val als: Vector[Int] = Vector.fill(height)(util.Random.nextInt(127) + 1)
    val p1 = als.map(Vector.fill(_)(util.Random.nextFloat()))
    val p2 = als.map(Vector.fill(_)(util.Random.nextFloat()))

    val gold = (p1 zip p2).map {
      case (arr1, arr2) =>
        (arr1 zip arr2)
          .map { case (e1: Float, e2: Float) => e1 * e2 }
          .sum
    }

    val exec = Execute(128)
    val (output, _) = exec[Vector[Float]](f, p1, p2)

    assertArrayEquals(gold.toArray, output.toArray, 0.001f)
  }

  @Test
  def chainedMaps(): Unit = {
    val capacity = 128
    val size = 87 // random value < capacity
    val f = fun(
      ArrayTypeWC(Int, capacity),
      MapSeq(fun(x => addI.apply(x, x)))
        o MapSeq(fun(addI.apply(1, _)))
        o MapSeq(id(Int)) $ _
    )

    val input = Vector.fill(size)(util.Random.nextInt(1024))
    val exec = Execute(capacity)
    val (output, _) = exec[Vector[Int]](f, input)

    assertEquals(input.map(x => 2 * (x + 1)), output)
  }

  @Test
  def highDimension(): Unit = {
    val dimSizes = Array(4, 2, 3, 2, 3)
    val inner = ArrayType(Int)

    val kernel = fun(
      dimSizes.foldRight(inner)({ case (size, t) => ArrayTypeWSWC(t, size, size) }),
      array =>
        MapGlb(
          MapSeq(
            MapSeq(
              MapSeq(
                Join() o MapSeq(MapSeq(toGlobal(id(Int))) o ReduceSeq(add(Int), 0))
              )
            )
          )
        ) $ array
    )

    val input = Vector.fill(dimSizes(0), dimSizes(1), dimSizes(2), dimSizes(3), dimSizes(4))({
      val len = util.Random.nextInt(8) + 1
      Vector.fill(len)(util.Random.nextInt(512))
    })
    val gold = input.map(_.map(_.map(_.map(_.map(_.sum)))))

    val (output, _) = Execute(4, 4)[Vector[Vector[Vector[Vector[Vector[Int]]]]]](kernel, input)
    assertEquals(gold, output)
  }

  @Test
  def basicSpMV(): Unit = {
    val N = SizeVar("VectorLength")
    val M = SizeVar("MatrixHeight")
    val f = fun(
      ArrayTypeWSWC(ArrayType(Int),   M),
      ArrayTypeWSWC(ArrayType(Float), M),
      ArrayTypeWSWC(Float, N),
      (arrayIndices, arrayValues, vector) =>
        Zip(arrayIndices, arrayValues) :>>
          MapGlb(fun( rowPair =>
            Zip(rowPair._0, rowPair._1) :>>
              ReduceSeq(fun((acc, rowElem) =>
                add(acc, mult(rowElem._1, CheckedArrayAccess(rowElem._0, 0.0f) $ vector))), 0.0f
              ) :>> toGlobal(MapSeq(id))
        )) :>> Join()
    )

    val height = 128
    val width = 64

    val als: Vector[Int] = Vector.fill(height)(util.Random.nextInt(width - 1) + 1)
    val indices = als.map(Vector.fill(_)(util.Random.nextInt(width)))
    val values = als.map(Vector.fill(_)(util.Random.nextFloat()))
    val vector  = Vector.fill(width)(util.Random.nextFloat())

    val gold = (indices zip values).map{
      case (ixRow, valRow) =>
        (ixRow zip valRow).map{
        case (ix, v) => v * vector(ix)
      }.sum
    }

    val exec = Execute(128)
    val (output, _) = exec[Vector[Float]](f, indices, values, vector)

    assertArrayEquals(gold.toArray, output.toArray, 0.001f)
  }
}

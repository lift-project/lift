package opencl.generator

import ir._
import ir.UserFunDef._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.IndexFunction.reverse

import org.junit.Assert._
import org.junit.{Test, AfterClass, BeforeClass}

object TestScatterGather {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestScatterGather {

  @Test def testScatterGlb1D(): Unit = {
    val Nsize = 128
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Scatter(reverse)(MapGlb(id)) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testScatterGlbSeqInnerDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => MapGlb(Scatter(reverse)(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testScatterGlbSeqOuterDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Scatter(reverse)(MapGlb(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testScatterGlbSeqBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Scatter(reverse)(MapGlb(Scatter(reverse)(MapSeq(id)))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherGlb1D(): Unit = {
    val Nsize = 128
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Gather(reverse)(MapGlb(id)) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testGatherGlbSeqInnerDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => MapGlb(Gather(reverse)(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherGlbSeqOuterDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(reverse)(MapGlb(MapSeq(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testGatherGlbSeqBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(reverse)(MapGlb(Gather(reverse)(MapSeq(id)))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherWrg1D(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Gather(reverse)(MapWrg(id)) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testGatherWrgLclInnerDim(): Unit = {
    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => MapWrg(Gather(reverse)(MapLcl(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherWrgLclOuterDim(): Unit = {
    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(reverse)(MapWrg(MapLcl(id))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testGatherWrgLclBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayType(ArrayType(Float, Var("M")), Var("N")),
      in => Gather(reverse)(MapWrg(Gather(reverse)(MapLcl(id)))) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, matrix, Nsize, Msize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherSplit(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Gather(reverse)(MapGlb(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(1,Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.reverse.flatten, output, 0.0f)
  }

  @Test def testSplitGather(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapGlb(Gather(reverse)(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.map(_.reverse).flatten, output, 0.0f)
  }

  @Test def testScatterSplit(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => Scatter(reverse)(MapGlb(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(1,Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.reverse.flatten, output, 0.0f)
  }

  @Test def testSplitScatter(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayType(Float, Var("N")),
      in => MapGlb(Scatter(reverse)(MapSeq(id))) o Split(splitSize) $ in
    )

    val (output, runtime) = Execute(Nsize)(f, vector, Nsize)

    println("output.size = " + output.size)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.map(_.reverse).flatten, output, 0.0f)
  }

}

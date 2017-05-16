package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestScatterGather {
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

class TestScatterGather {

  @Test def testScatterGlb1D(): Unit = {
    val Nsize = 128
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Scatter(reverse) o MapGlb(id) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testScatterGlbSeqInnerDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapGlb(Scatter(reverse) o MapSeq(id)) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatMap(_.reverse), output, 0.0f)
  }

  @Test def testScatterGlbSeqOuterDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => Scatter(reverse) o MapGlb(MapSeq(id)) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testScatterGlbSeqBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => Scatter(reverse) o MapGlb(Scatter(reverse) o MapSeq(id)) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverseMap(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherGlb1D(): Unit = {
    val Nsize = 128
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(id) o Gather(reverse) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testGatherGlbSeqInnerDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapGlb(MapSeq(id) o Gather(reverse)) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatMap(_.reverse), output, 0.0f)
  }

  @Test def testGatherGlbSeqOuterDim(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapGlb(MapSeq(id)) o Gather(reverse) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testGatherGlbSeqBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapGlb(MapSeq(id) o Gather(reverse)) o Gather(reverse) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverseMap(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherWrg1D(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapWrg(id) o Gather(reverse) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.reverse, output, 0.0f)
  }

  @Test def testGatherWrgLclInnerDim(): Unit = {
    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapWrg( MapLcl(id) o Gather(reverse)) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.flatMap(_.reverse), output, 0.0f)
  }

  @Test def testGatherWrgLclOuterDim(): Unit = {
    val Nsize = 256
    val Msize = 128
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapWrg( MapLcl(id)) o Gather(reverse) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverse.flatten, output, 0.0f)
  }

  @Test def testGatherWrgLclBothDims(): Unit = {
    val Nsize = 128
    val Msize = 64
    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c + r * Msize.toFloat)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      in => MapWrg( MapLcl(id) o Gather(reverse)) o Gather(reverse) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, matrix)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(matrix.reverseMap(_.reverse).flatten, output, 0.0f)
  }

  @Test def testGatherSplit(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id)) o Gather(reverse) o Split(splitSize) $ in
    )

    val (output: Array[Float], runtime) = Execute(1,Nsize)(f, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.reverse.flatten, output, 0.0f)
  }

  @Test def testSplitGather(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id) o Gather(reverse)) o Split(splitSize) $ in
    )

    val (output: Array[Float], runtime) = Execute(Nsize)(f, vector)

    println("output.size = " + output.length)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)

    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output, 0.0f)
  }

  @Test def testScatterSplit(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => Scatter(reverse) o MapGlb(MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(1,Nsize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.reverse.flatten, output, 0.0f)
  }

  @Test def testSplitScatter(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(Scatter(reverse) o MapSeq(id)) o Split(splitSize) $ in
    )

    val f_broken = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(fun(x => Scatter(reverse)(MapSeq(id)(x))))(Split(splitSize)(in))
    )

    val (output0: Array[Float], _) = Execute(Nsize)(f, vector)
    val (output1: Array[Float], _) = Execute(Nsize)(f_broken, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output0, 0.0f)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output1, 0.0f)
  }

  @Test def mapScatterMap(): Unit = {
    val Nsize = 256
    val vector = Array.tabulate(Nsize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id) o Scatter(reverse) o MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(Nsize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output, 0.0f)

  }

  @Test
  def scatterBetweenMaps0(): Unit = {
    val nSize = 256
    val vector = Array.tabulate(nSize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id)) o MapGlb(Scatter(reverse) o MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(nSize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output, 0.0f)
  }

  @Test
  def scatterBetweenMaps1(): Unit = {
    val nSize = 256
    val vector = Array.tabulate(nSize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id) o Scatter(reverse)) o MapGlb(MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(nSize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output, 0.0f)
  }

  @Test
  def scatterBetweenMaps2(): Unit = {
    val nSize = 256
    val vector = Array.tabulate(nSize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id) o Scatter(reverse)) o MapGlb(Scatter(reverse) o MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(nSize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse.reverse), output, 0.0f)
  }

  @Test
  def gatherBetweenMaps0(): Unit = {
    val nSize = 256
    val vector = Array.tabulate(nSize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id)) o MapGlb(Gather(reverse) o MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(nSize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output, 0.0f)
  }

  @Test
  def gatherBetweenMaps1(): Unit = {
    val nSize = 256
    val vector = Array.tabulate(nSize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id) o Gather(reverse)) o MapGlb(MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(nSize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse), output, 0.0f)
  }

  @Test
  def gatherBetweenMaps2(): Unit = {
    val nSize = 256
    val vector = Array.tabulate(nSize)(_.toFloat)

    val splitSize = 64

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      in => MapGlb(MapSeq(id) o Gather(reverse)) o MapGlb(Gather(reverse) o MapSeq(id)) o Split(splitSize) $ in
    )

    val (output: Array[Float], _) = Execute(nSize)(f, vector)
    assertArrayEquals(vector.grouped(splitSize).toArray.flatMap(_.reverse.reverse), output, 0.0f)
  }
  
  /**
   * Gather must expect an array
   */
  @Test(expected = classOf[TypeException])
  def illegalGather(): Unit = {
    val f = fun(
      ArrayType(Int, 128),
      MapGlb(toGlobal(idI) o Gather(reverse)) $ _
    )
    
    TypeChecker(f)
  }
  
  /**
   * Scatter must expect an array
   */
  @Test(expected = classOf[TypeException])
  def illegalScatter(): Unit = {
    val f = fun(
      ArrayType(Int, 128),
      MapGlb(Scatter(reverse) o toGlobal(idI)) $ _
    )
    
    TypeChecker(f)
  }
}

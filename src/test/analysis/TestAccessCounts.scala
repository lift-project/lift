package analysis

import apart.arithmetic.{Cst, Var}
import ir.ArrayType
import ir.ast._
import opencl.generator._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

class TestAccessCounts {

  val N = Var("N")
  val globalSize0 = new get_global_size(0)
  val globalSize1 = new get_global_size(1)

  val numGroups0 = new get_num_groups(0)
  val localSize0 = new get_local_size(0)

  @Test
  def simple(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      x => MapGlb(MapSeq(id)) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N * (N /^ globalSize0), accessCounts.getStores(f.body.mem))
  }

  @Test
  def simple2(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.getStores(f.body.mem))
  }

  @Test
  def moreReads(): Unit = {

    val f = fun(
      ArrayType(Float, N),
      x => MapSeq(add) $ Zip(x,x)
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N, accessCounts.getStores(f.body.mem))
    assertEquals(2*N, accessCounts.getLoads(f.params.head.mem))

    assertEquals(Cst(0), accessCounts.getLoads(f.body.mem))
    assertEquals(Cst(0), accessCounts.getStores(f.params.head.mem))
  }

  @Test
  def simpleLocal(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, 16), N),
      x => MapWrg(toGlobal(MapLcl(id)) o toLocal(MapLcl(id))) $ x
    )

    val counts = AccessCounts(f)
    val localReads = counts.getLoads(LocalMemory, exact = false)
    val localWrites = counts.getStores(LocalMemory, exact = false)

    assertEquals((N /^ numGroups0) * (Cst(16) /^ localSize0) , localReads)
    assertEquals((N /^ numGroups0) * (Cst(16) /^ localSize0) , localWrites)
  }

  @Test
  def withPattern(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) o Transpose() $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.getLoads(GlobalMemory, UnknownPattern, exact = false))
    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.getStores(GlobalMemory, CoalescedPattern, exact = false))

    assertEquals(Cst(0),
      accessCounts.getLoads(GlobalMemory, CoalescedPattern, exact = false))
    assertEquals(Cst(0),
      accessCounts.getStores(GlobalMemory, UnknownPattern, exact = false))
  }

  @Test
  def vector(): Unit = {

    val f = fun(
      ArrayType(Float4, N),
      x => MapGlb(0)(idF4) $ x
    )

    val accessCounts = AccessCounts(f)

    // TODO: is the pattern correct?
    assertEquals(N /^ globalSize0,
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern))
    assertEquals(N /^ globalSize0,
      accessCounts.vectorStores(GlobalMemory, UnknownPattern))
  }

  @Test
  def vector2(): Unit = {

    val f = fun(
      ArrayType(Float, 4*N),
      x => asScalar() o MapGlb(0)(idF4) o asVector(4) $ x
    )

    val accessCounts = AccessCounts(f)

    // TODO: is the pattern correct?
    assertEquals(N /^ globalSize0,
      accessCounts.vectorLoads(GlobalMemory, UnknownPattern))
    assertEquals(N /^ globalSize0,
      accessCounts.vectorStores(GlobalMemory, UnknownPattern))
  }
}

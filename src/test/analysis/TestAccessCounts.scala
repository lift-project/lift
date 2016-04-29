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

    assertEquals(N * (N /^ globalSize0), accessCounts.storesToMemory(f.body.mem))
  }

  @Test
  def simple2(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

    val accessCounts = AccessCounts(f)

    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.storesToMemory(f.body.mem))
  }

  @Test
  def moreReads(): Unit = {

    val f = fun(
      ArrayType(Float, N),
      x => MapSeq(add) $ Zip(x,x)
    )

    val accessCounts = AccessCounts(f)

    assertEquals(N, accessCounts.storesToMemory(f.body.mem))
    assertEquals(2*N, accessCounts.loadsToMemory(f.params.head.mem))

    assertEquals(Cst(0), accessCounts.loadsToMemory(f.body.mem))
    assertEquals(Cst(0), accessCounts.storesToMemory(f.params.head.mem))
  }

  @Test
  def simpleLocal(): Unit = {
    val f = fun(
      ArrayType(ArrayType(Float, 16), N),
      x => MapWrg(toGlobal(MapLcl(id)) o toLocal(MapLcl(id))) $ x
    )

    val counts = AccessCounts(f)
    val localReads = counts.loadsToAddressSpace(LocalMemory)
    val localWrites = counts.storesToAddressSpace(LocalMemory)

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
      accessCounts.loadsToAddressSpaceWithPattern(GlobalMemory, UnknownPattern))
    assertEquals((N /^ globalSize1) * (N /^ globalSize0),
      accessCounts.storesToAddressSpaceWithPattern(GlobalMemory, CoalescedPattern))

    assertEquals(Cst(0),
      accessCounts.loadsToAddressSpaceWithPattern(GlobalMemory, CoalescedPattern))
    assertEquals(Cst(0),
      accessCounts.storesToAddressSpaceWithPattern(GlobalMemory, UnknownPattern))
  }

  @Test
  def vector(): Unit = {

    val f = fun(
      ArrayType(Float4, N),
      x => MapGlb(0)(idF4) $ x
    )

    val accessCounts = AccessCounts(f)

    // TODO:
  }

}

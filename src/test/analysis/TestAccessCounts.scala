package analysis

import apart.arithmetic.Var
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

    val (_, stores) = AccessCounts(f).accesses

    assertEquals(N * (N / globalSize0), stores(f.body.mem))
  }

  @Test
  def simple2(): Unit = {

    val f = fun(
      ArrayType(ArrayType(Float, N), N),
      x => MapGlb(1)(MapGlb(0)(id)) $ x
    )

    val (_, stores) = AccessCounts(f).accesses

    assertEquals((N / globalSize1) * (N / globalSize0), stores(f.body.mem))
  }

  @Test
  def moreReads(): Unit = {

    val f = fun(
      ArrayType(Float, N),
      x => MapSeq(add) $ Zip(x,x)
    )

    val (loads, stores) = AccessCounts(f).accesses

    assertEquals(N, stores(f.body.mem))
    assertEquals(2*N, loads(f.params.head.mem))

    assertFalse(loads.isDefinedAt(f.body.mem))
    assertFalse(stores.isDefinedAt(f.params.head.mem))
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

    // TODO: assert...
    assertEquals((N / numGroups0) * (16 / localSize0) , localReads)
    assertEquals((N / numGroups0) * (16 / localSize0) , localWrites)
  }

}

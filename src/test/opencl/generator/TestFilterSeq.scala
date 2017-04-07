package opencl.generator

import ir.ArrayType
import ir.ast.{Join, Split, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{
  MapGlb, MapWrg, MapLcl, MapSeq, FilterSeq, ReduceSeq, toGlobal
}
import org.junit.Assert.assertArrayEquals
import org.junit.{AfterClass, BeforeClass, Test}

object TestFilterSeq {
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

class TestFilterSeq {
  // Some user functions
  def lt(n: Int): UserFun =
    UserFun(s"lt$n", "x", s"return x < $n;", Int, Int)
  val int_id: UserFun = id(Int, name="int_id")
  
  @Test def filterSimple(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayType(Int, N),
      l => MapGlb(toGlobal(int_id)) o FilterSeq(lt(5)) $ l
    )
    
    val (output: Array[Int], _) = Execute(size)(expr, input)
    val gold = input.filter(_ < 5)
    
    assertArrayEquals(gold, output.slice(0, gold.length))
  }
  
  @Test def filterMapGlb(): Unit = {
    val size = 1024
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayType(Int, N),
      l => Join() o MapGlb(toGlobal(FilterSeq(lt(5)))) o Split(32) $ l
    )
    
    
    val (output: Array[Int], _) = Execute(size)(expr, input)
    // Because RuntimeSizeArrays are not supported yet, we have to reshape the
    // output
    val reshapedOutput = (input.grouped(32) zip output.grouped(32))
      .map({ case (in, out) =>  out.slice(0, in.count(_ < 5))})
      .toArray
      .flatten
   
    assertArrayEquals(input.filter(_ < 5), reshapedOutput)
  }
  
  @Test def filterMapWrg(): Unit = {
    val size = 4096
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayType(Int, N),
      l => Join() o Join() o MapWrg(
        toGlobal(MapLcl(FilterSeq(lt(5)))) o Split(4)
      ) o Split(32) $ l
    )
    
    val (output: Array[Int], _) = Execute(size)(expr, input)
    // Because RuntimeSizeArrays are not supported yet, we have to reshape the
    // output
    val reshapedOutput = output.grouped(32).map(_.grouped(4)).flatten
    val reshapedInput = input.grouped(32).map(_.grouped(4)).flatten
    val finalOutput = (reshapedOutput zip reshapedInput)
      .map(p => p._1.slice(0, p._2.count(_ < 5)))
      .flatten
      .toArray
    
    assertArrayEquals(input.filter(_ < 5), finalOutput)
  }
  
  @Test def filterArray(): Unit = {
    val size = 1024
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayType(Int, N),
      l => MapGlb(toGlobal(int_id)) o Join() o FilterSeq(
        fun(xs => fun(x => x.at(0)) o MapSeq(lt(100)) o ReduceSeq(addI, 0) $ xs)
      ) o Split(32) $ l
    )
    
    val (output: Array[Int], _) = Execute(size)(expr, input)
    val gold = input.grouped(32)
      .filter(row => row.sum < 100)
      .flatten
      .toArray

    assertArrayEquals(gold, output.slice(0, gold.length))
  }
}

package opencl.generator

import ir.ArrayType
import ir.ast.{Join, PrintType, Split, UserFun, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.{toGlobal, MapGlb, MapWrg, MapLcl, MapSeq, FilterSeq}
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
  val lt5: UserFun =
    UserFun("lt5", "x", "return x < 5;", Int, Int)
  
  @Test def filterSimple(): Unit = {
    val size = 128
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayType(Int, N),
      l => MapGlb(toGlobal(idI)) o FilterSeq(lt5) $ l
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
      l => Join() o MapGlb(toGlobal(FilterSeq(lt5))) o Split(32) $ l
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
        toGlobal(MapLcl(FilterSeq(lt5))) o Split(4)
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
      l => MapGlb(MapSeq(toGlobal(idI))) o PrintType() o FilterSeq(fun(_ => 1)) o Split(32) $ l
    )
    
    val (output: Array[Int], _) = Execute(size)(expr, input)

    assertArrayEquals(input, output)
  }
}

package opencl.generator

import ir._
import ir.ast.{Join, Split, UserFun, Zip, fun}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert.{assertArrayEquals, assertEquals}
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
    UserFun(s"lt$n", "x", s"return x < $n;", Int, Bool)
  
  @Test def filterSimple(): Unit = {
    val size = 1024
    val input = Array.fill(size)(util.Random.nextFloat())
    val N = SizeVar("N")
    
    val predicate = UserFun(
      "predicate", "x", "return (x < 0.8) & (x > 0.2);", Float, Bool
    )
    
    val expr = fun(
      ArrayTypeWSWC(Float, N),
      l => MapGlb(toGlobal(id(Float))) o FilterSeq(predicate) $ l
    )
    
    assertEquals(ArrayTypeWC(Float, N), TypeChecker(expr))
    
    // TODO: uncomment this once PR #86 and #89 are merged
    // val (output: Array[Float], _) = Execute(size)(expr, input)
    // val gold = input.filter(x => (x < 0.8) && (x > 0.2))
    
    // assertArrayEquals(gold, output.slice(0, gold.length), 0f)
  }
  
  @Test def filterMapGlb(): Unit = {
    val size = 1024
    val left = Array.fill(size)(util.Random.nextInt(10))
    val right = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayTypeWSWC(Int, N), ArrayTypeWSWC(Int, N),
      (left, right) => MapGlb(toGlobal(FilterSeq(
        fun((l, r) => equality(Int).apply(l, r))
      ))) o Split(32) $ Zip(left, right)
    )
  
    assertEquals(
      ArrayType(ArrayTypeWC(TupleType(Int, Int), 32), N /^ 32),
      TypeChecker(expr)
    )
  
    // TODO: uncomment this once PR #86 and #89 are merged
    // val (output: Array[Int], _) = Execute(size)(expr, left, right)
    // // Because RuntimeSizeArrays are not supported yet, we have to reshape the
    // // output
    // val reshapedOutput =
    //   ((left zip right).grouped(32) zip output.grouped(2).grouped(32))
    //   .map({ case (in, out) =>
    //     out.slice(0, in.count(t => t._1 == t._2))
    //   })
    //   .toArray.flatten.flatten
    //
    // // We assertArrayEquals does not support arrays of tuples
    // val gold = (left zip right)
    //   .filter(t => t._1 == t._2)
    //   .flatMap(t => Array(t._1, t._2))
   
    // assertArrayEquals(gold, reshapedOutput)
  }
  
  @Test def filterMapWrg(): Unit = {
    val size = 4096
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayTypeWSWC(Int, N),
      l => Join() o MapWrg(
        toGlobal(MapLcl(FilterSeq(lt(5)))) o Split(4)
      ) o Split(32) $ l
    )
    
    assertEquals(
      ArrayType(ArrayTypeWC(Int, 4), N /^ 4),
      TypeChecker(expr)
    )
  
    // TODO: uncomment this once PR #86 and #89 are merged
    // val (output: Array[Int], _) = Execute(size)(expr, input)
    // // Because RuntimeSizeArrays are not supported yet, we have to reshape the
    // // output
    // val reshapedOutput = output.grouped(32).map(_.grouped(4)).flatten
    // val reshapedInput = input.grouped(32).map(_.grouped(4)).flatten
    // val finalOutput = (reshapedOutput zip reshapedInput)
    //   .map(p => p._1.slice(0, p._2.count(_ < 5)))
    //   .flatten
    //   .toArray
    //
    // assertArrayEquals(input.filter(_ < 5), finalOutput)
  }
  
  @Test def filterArray(): Unit = {
    val size = 1024
    val input = Array.fill(size)(util.Random.nextInt(10))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayTypeWSWC(Int, N),
      l => MapGlb(MapSeq(toGlobal(id(Int)))) o FilterSeq(
        fun(xs => fun(x => x.at(0)) o MapSeq(lt(100)) o ReduceSeq(addI, 0) $ xs)
      ) o Split(32) $ l
    )
  
    assertEquals(
      ArrayTypeWC(ArrayType(Int, 32), N /^ 32),
      TypeChecker(expr)
    )
    
    // TODO: uncomment this once PR #86 and #89 are merged
    // val (output: Array[Int], _) = Execute(size)(expr, input)
    // val gold = input.grouped(32)
    //   .filter(row => row.sum < 100)
    //   .flatten
    //   .toArray
  
    // assertArrayEquals(gold, output.slice(0, gold.length))
  }
}

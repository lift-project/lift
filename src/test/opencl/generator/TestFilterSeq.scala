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
  /**
   * Helper function for decoding arrays as they are returned by the executor.
   * /!\ only works for 1D arrays for which the capacity is statically known.
   *
   * @param raw the encoded array
   * @param elemSize the size of one element in a Scala array
   *                 (e.g. 2 for int * int)
   * @tparam T the base type
   * @return the decoded array
   */
  def decodeWC1D[T](raw: Array[T], elemSize: Int = 1)
                   (implicit num: Numeric[T]): Array[T] = {
    val size = num.toInt(raw(0))
    raw.slice(elemSize, elemSize * (1 + size))
  }
  
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
      l => toGlobal(FilterSeq(predicate)) $ l
    )
    
    assertEquals(ArrayTypeWC(Float, N), TypeChecker(expr))
    
    val (output: Array[Float], _) = Execute(size)(expr, input)
    val gold = input.filter(x => (x < 0.8) && (x > 0.2))
    assertArrayEquals(gold, decodeWC1D(output), 0f)
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
  
    val (outputRaw: Array[Int], _) = Execute(size)(expr, left, right)
    
    // Decoding
    val output = outputRaw
      .grouped((32 + 1) * 2)
      .map(decodeWC1D(_, elemSize = 2))
      .toArray
  
    // Flatten tuples
    val gold = (left zip right)
      .filter(t => t._1 == t._2)
      .flatMap(t => Array(t._1, t._2))
  
    assertArrayEquals(gold, output.flatten)
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
  
    val (outputRaw: Array[Int], _) = Execute(size)(expr, input)
  
    // Decoding
    val output = outputRaw
      .grouped(32 + (32 / 4) * 1).map(_.grouped(4 + 1))
      .map(arr => arr.map(decodeWC1D(_)))
      .flatten.flatten
      .toArray
  
    assertArrayEquals(input.filter(_ < 5), output)
  }
  
  @Test def filterArray(): Unit = {
    val size = 1024
    val input = Array.fill(size)(util.Random.nextInt(8))
    val N = SizeVar("N")
    
    val expr = fun(
      ArrayTypeWSWC(Int, N),
      l => MapSeq(MapSeq(toGlobal(id(Int)))) o FilterSeq(
        fun(xs => fun(x => x.at(0)) o MapSeq(lt(100)) o ReduceSeq(addI, 0) $ xs)
      ) o Split(32) $ l
    )
  
    assertEquals(
      ArrayTypeWC(ArrayType(Int, 32), N /^ 32),
      TypeChecker(expr)
    )
  
    val (output: Array[Int], _) = Execute(size)(expr, input)
    val gold = input.grouped(32)
      .filter(row => row.sum < 100)
      .flatten
      .toArray
  
    assertArrayEquals(gold, output.slice(1, 1 + gold.length))
  }
}

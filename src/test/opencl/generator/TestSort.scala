package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor._
import opencl.generator.OpenCLAST.ArithExpression
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Ignore, Test}

object TestSort {
  @BeforeClass def TestSort(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}



class TestSort {
  @Test def bitonic_reordering(): Unit = {
    val inputSize = Math.pow(2, 8).toInt
    val arr = Array.tabulate(inputSize)((i:Int) => i.toFloat)
    val splitSize = 2

    val N = SizeVar("N")

    val idxArr = ArrayFromGenerator( (i, N) => ArithExpression(i), ArrayTypeWSWC(Int, N))
    val tfiadd = UserFun("tuplefiadd", "x", "{return x._0 + (float)x._1;}", TupleType(Float, Int), Float);

    val hypercube_offset = (dim: ArithExpr) => (i: ArithExpr, t: Type) => {
//      val n = Type.getLength(t)
      i ^ Pow(2, dim)
    }

    val kernel = fun(
      ArrayTypeWSWC(Float, N),
      (array) => {
        MapWrg(
          MapLcl(id)
        ) o Split(N) o Gather(hypercube_offset(2)) $ array
      }
    )

    val (output:Array[Float], runtime) = Execute(1,1)(kernel, arr)
    println("Time: " + runtime)
    println(s"Input:  ${arr.take(20).mkString("[",",","]")}")
    println(s"Output: ${output.take(20).mkString("[",",","]")}")
    assertArrayEquals(arr, output, 0.01f)
  }


  @Test def workgroup_sort(): Unit = {
    val inputSize = Math.pow(2, 8).toInt
    val arr = Array.fill(inputSize)(util.Random.nextFloat())
    val gold = arr.sorted

    val N = SizeVar("N")
    val idxArr = ArrayFromGenerator( (i, N) => ArithExpression(i), ArrayTypeWSWC(Int, N))

    val tfiadd = UserFun("tuplefiadd", "x", "{return x._0 + (float)x._1;}", TupleType(Float, Int), Float);

    val kernel = fun(
      ArrayTypeWSWC(Float, N),
      (array) => {
       MapWrg(
         Iterate(Log(2, N))(fun(arrn =>
           MapLcl(tfiadd) $ Zip(arrn, idxArr)
         ))
       ) o Split(N) o Gather(shiftRight) $ array
      }
    )

    val (output:Array[Float], runtime) = Execute(1,1)(kernel, arr)
    println("Time: " + runtime)
    println(s"Input: ${arr.take(20).mkString("[",",","]")}")
    println(s"Output: ${output.take(20).mkString("[",",","]")}")
    assertArrayEquals(output, gold, 0.0f)

  }

}

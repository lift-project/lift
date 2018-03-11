package utils;

import c.executor.Compile
import ir.{ArrayType, ArrayTypeWSWC, TypeChecker}
import ir.ast.{PrintType, Split, _}
import lift.arithmetic.{SizeVar, Var}
import opencl.executor.{Execute, Executor, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit._

object TestVisualization extends TestWithExecutor

class TestVisualization{

  val input2D = Array.tabulate(4, 4) { (i, j) => i * 4.0f + j}

    @Test
    def psiPaperTestExpression(): Unit = {
        def M = Var("M")
        def N = Var("N")
      val expressionText = " PrintType(visual = true,render = true) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)"
        def expression =  PrintType(visual = true,render = true,expressionText) o Join() o  PrintType(visual = true) o Map(Reduce(add, 0.0f))o PrintType(visual = true) o Split(M) o PrintType(visual = true)

                val lambda = \(ArrayType(Float, N), input => expression $ input)
        TypeChecker(lambda)
    }

  @Test
  def using3DArrays(): Unit = {
    val N = SizeVar("N")
    val M = SizeVar("M")
    val O = SizeVar("O")
    val expressionText = "MapGlb(id) o PrintType(visual = true,render = true) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true)"
    val lambda = fun(
      ArrayType(ArrayType(ArrayType(Float, N), M ), O),
      input => MapGlb(id) o PrintType(visual = true,render = true,expressionText) o Join() o PrintType(visual = true) o Join() o PrintType(visual = true) $ input
    )
    TypeChecker(lambda)
    //println(Compile(lambda))
  }


  @Test
  def tupleType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText =" MapGlb(\\(tuple => id(tuple._0))) o PrintType(visual = true,render = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        MapGlb(\(tuple => id(tuple._0))) o PrintType(visual = true,render = true,expressionText) $ Zip(input, input)
    )

    //TypeChecker(lambda)
    val kernel = Compile(lambda)
    println(kernel)
  }

  @Test
  def vectorType(): Unit = {
    val input = Array.tabulate(32){ i => i}
    val N = Var("N")
    val expressionText ="PrintType(visual = true,render = true) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)"
    def lambda = fun(
      ArrayType(Float, N), input =>
        PrintType(visual = true,render = true,expressionText) o MapGlb(toGlobal(idF4)) o PrintType(visual = true)  o asVector(4) o PrintType(visual = true)  $ input
    )

    TypeChecker(lambda)
   // val kernel = Compile(lambda)
    //println(kernel)
  }


}



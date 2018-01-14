package utils;

import ir.{ArrayType, ArrayTypeWSWC, TypeChecker}
import ir.ast._
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
        def expression = Join() o PrintType(true) o Map(Reduce(add, 0.0f)) o Split(M)

                val lambda = \(ArrayType(Float, N), input => expression $ input)
        TypeChecker(lambda)
    }
}



package prog_gen

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Eval, Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit._
import rewriting.{EnabledMappings, Lower}

import scala.language.reflectiveCalls

object ProgGenIssues extends TestWithExecutor

class ProgGenIssues{

  @Test
  def hlGenResultNotEqual1(): Unit = {

    val f = Eval("val add = UserFun(\"add\", Array(\"x\", \"y\"), \"\"\"|{ return x+y; }\"\"\".stripMargin, Seq(Float, Float), Float).setScalaFun (xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])\nfun(Float, ArrayTypeWSWC(ArrayTypeWSWC(Float, 32), 32), ArrayTypeWSWC(Float, 32),(p_0, p_1, p_2) => FunCall(Map(fun((p_3) => FunCall(Reduce(fun((p_4, p_5) => FunCall(add, p_4, p_5))), FunCall(add, p_0, p_3), FunCall(Map(fun((p_6) => FunCall(add, p_6, p_6))), FunCall(Join(), p_1))))), FunCall(Map(fun((p_7) => FunCall(add, p_7, p_7))), p_2)))")
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))

    val lower = fs.head
    TypeChecker(lower)

    val Args = InputGenerator()(fs.head)
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val (output_exe,_)= Execute(1,32)[Array[Float]](lower,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }

  @Test
  def issue76(): Unit = {

    val f = fun(
      Float,
      ArrayTypeWSWC(Float,32),
      (p236,p116) =>{
        Map(fun((p200) =>
          add(p236,add(p236,p200))
        )) $ p116
      })
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))

    val Args = InputGenerator()(fs.head)

    val output_int = Interpreter(f).->[Vector[Float]].run(Args:_*).toArray[Float]
    val (output_exe,_)= Execute(1,1024)[Array[Float]](fs.head,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }

  @Test
  def issue78(): Unit = {

    val f = fun(
      Float,
      ArrayTypeWSWC(Float,32),
      (p252,p174) =>
        toGlobal(MapSeq(fun((p30) =>
          add(p252,p30)
        )))(ReduceSeq(fun((p89,p156) =>
          add(p89,p156)
        ))(id $ p252,p174))
    )

    val args = InputGenerator()(f)

    val output_int = Interpreter(f).->[Vector[Float]].run(args:_*).toArray[Float]
    val (output_exe,_)= Execute(1,1024)[Array[Float]](f, args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }
}

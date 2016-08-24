package ir.hlGenerator


import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Eval, Execute, Executor}
import org.junit._
import opencl.ir._
import opencl.ir.pattern.toGlobal
import rewriting.{EnabledMappings, Lower}

import scala.language.reflectiveCalls

object hlGenIssues{
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
class hlGenIssues{

  @Ignore @Test def IssueResultNe():Unit={

    val f = Eval("val add = UserFun(\"add\", Array(\"x\", \"y\"), \"\"\"|{ return x+y; }\"\"\".stripMargin, Seq(Float, Float), Float).setScalaFun (xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])\nfun(ArrayType(Float, 32), ArrayType(ArrayType(Float, 32), 32),(p_0, p_1) => FunCall(Reduce(fun((p_2, p_3) => FunCall(Map(fun((p_4) => FunCall(add, p_3, p_4))), p_2))), FunCall(Map(fun((p_5) => FunCall(add, p_5, p_5))), p_0), FunCall(Map(fun((p_6) => FunCall(add, p_6, p_6))), FunCall(Join(), p_1))))")
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    //val lower = hlGenerator.testSolve(fs.head)
    val lower = fs.head
    TypeChecker(lower)
    val code = Compile(lower)
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- f.params.indices) {
      f.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _=>
      }
    }
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1)(code,lower,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))

  }
  @Ignore @Test def hlGenResultNotEqual1():Unit={
    val f = Eval("val add = UserFun(\"add\", Array(\"x\", \"y\"), \"\"\"|{ return x+y; }\"\"\".stripMargin, Seq(Float, Float), Float).setScalaFun (xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])\nfun(Float, ArrayType(ArrayType(Float, 32), 32), ArrayType(Float, 32),(p_0, p_1, p_2) => FunCall(Map(fun((p_3) => FunCall(Reduce(fun((p_4, p_5) => FunCall(add, p_4, p_5))), FunCall(add, p_0, p_3), FunCall(Map(fun((p_6) => FunCall(add, p_6, p_6))), FunCall(Join(), p_1))))), FunCall(Map(fun((p_7) => FunCall(add, p_7, p_7))), p_2)))")
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    //val lower = hlGenerator.testSolve(fs.head)
    val lower = fs.head
    TypeChecker(lower)
    val code = Compile(lower)
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- f.params.indices) {
      f.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _=>
      }
    }
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,32)(code,lower,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))

  }
  @Ignore @Test def issue76(): Unit ={
    val f = fun(
      Float,
      ArrayType(Float,32),
      (p236,p116) =>{
        Map(fun((p200) =>
          add(p236,add(p236,p200))
        )) $ p116
      })
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    val code = Compile(fs.head)
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- f.params.indices) {
      f.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _=>
      }
    }
    val output_int = Interpreter(f).->[Vector[Float]].run(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1024)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))
  }
  @Ignore @Test def issue78(): Unit={
    val f = fun(
      Float,
      ArrayType(Float,32),
      (p252,p174) =>{
        Map(fun((p30) =>
          add(p252,p30)
        ))(Reduce(fun((p89,p156) =>
          add(p89,p156)
        ))(p252,p174))
      }
    )
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    val code = Compile(fs.head)
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- f.params.indices) {
      f.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _=>
      }
    }
    val output_int = Interpreter(f).->[Vector[Float]].run(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1024)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))

  }
}
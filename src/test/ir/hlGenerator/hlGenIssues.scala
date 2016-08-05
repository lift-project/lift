package ir.hlGenerator


import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Execute, Executor}
import org.junit._
import opencl.ir._
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
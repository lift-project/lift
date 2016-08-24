package ir.hlGenerator


import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Eval, Execute, Executor}
import org.junit._
import opencl.ir._
import rewriting.{EnabledMappings, Lower}

import scala.language.reflectiveCalls

object hlGenFinishedIssue{
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
class hlGenFinishedIssue{
  @Ignore @Test def hlGenTest1(): Unit ={
    val f = fun(
      ArrayType(Float,32),
      Float,
      ArrayType(ArrayType(Float,32),32),
      (p241,p152,p10) =>{
        Map(fun((p2)=>
          Reduce(fun((p165,p139) =>
            add(p165,p139)
          ))(p2,p241)
        ))(Reduce(fun((p51,p177) =>
          add(p51,p177)
        ))(add(p152,p152), Join() $ p10))
      }
    )
    TypeChecker(f)
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
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
    val code = Compile(fs.head)
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],runtime)= Execute(1,1)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))
  }
  //The result should be Nan
  @Ignore @Test def hlGenLower1(): Unit= {
    val f = fun(
      ArrayType(Float,32),
      ArrayType(ArrayType(Float,32),32),
      (p10,p40) =>{
        Reduce(fun((p230,p54) =>
          Join() o Map(fun((p67) =>
            Reduce(fun((p122,p187)=>
              add(p122,p187)
            ))(p67,p230)
          )) $ p54
        ))(p10,p40)
      })
    TypeChecker(f)
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
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1024)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))
  }

  //The high-level expression could not find a correct lower one
  @Ignore @Test def hlGenCompiler1():Unit={
    val f = fun(
      Float,
      ArrayType(Float,32),
      ArrayType(Float,32),
      (p104,p243,p10) =>{
        Map(fun((p199)=>
          Reduce(fun((p51,p177)=>
            add(p51,p177)
          ))(add(p104,p199),p243)
        ))(Reduce(fun((p223,p14) =>
          add(p223,p14)
        ))(add(p104,p104),p10))
      }
    )
    TypeChecker(f)
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
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1024)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))
  }

  @Ignore @Test def hlGenCompiler2():Unit={
    val f = fun(
      Float,
      ArrayType(ArrayType(Float,32),32),
      Float,
      ArrayType(Float,32),
      (p99,p102,p226,p239) =>{
        Map(fun((p24) =>
          Join() o Map(fun((p157) =>
            Reduce(fun((p20,p195)=>
              add(p20,p195)
            ))(add(p24,p99),p157)
          )) $ p102
        )) $ Reduce(fun((p215,p49) =>
          add(p215,p49)
        ))(add(p226,p226),p239)
      }
    )
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    TypeChecker(fs.head)
    //val lowLevel = testSolve(fs.head)
    val lowLevel = fs.head
    val code = Compile(lowLevel)
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
    val(output_exe:Array[Float],_)= Execute(1,32)(code,lowLevel,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))
  }
  @Ignore @Test def hlGenWriteIntoReadonlyMemory1():Unit={
    val f = fun(
      Float,
      ArrayType(ArrayType(Float,32),32),
      Float,
      (p226,p243,p99) =>{
        Map(fun((p16) =>
          Reduce(fun((p230,p12) =>
            add(p230,p12)
          ))(add(p226,p16),Join() $ p243)
        ))(Reduce(fun((p230,p12) =>
          add(p230,p12)
        ))(add(p226,p99),Join() $ p243))
      }
    )
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    TypeChecker(fs.head)
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
    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))

  }
  @Test def hlGenWriteIntoReadonlyMemory2():Unit={
    val f = fun(
      ArrayType(Float,32),
      ArrayType(Float,32),
      (p101,p241) =>{
        Map(fun((p66) =>
          Reduce(fun((p171,p223)=>
            add(p171,p223)
          ))(p66,p101)
        ))(p241)
      }
    )
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
}
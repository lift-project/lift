package ir.hlGenerator

import java.io.PrintWriter

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Eval, Execute, Executor}
import org.junit._
import opencl.ir._
import opencl.ir.pattern.toGlobal
import rewriting.{EnabledMappings, Lower, Rewrite}

import scala.language.reflectiveCalls

/**
  * Created by potato on 25/07/16.
  */
object hlGeneratorTest{
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
class hlGeneratorTest {

  @Ignore
  @Test
  def testNewGen():Unit={
    val hlGen = new hlGenerator
    //hlGen.generateProgram()
    hlGen.tryPrograms(new PrintWriter("/home/potato/exprs/8-19/test_new_generator.txt"))
    //val result = hlGen.RefinedResult
    //assert(true)
  }



  @Ignore
  @Test
  def seekTestBugs():Unit={
    val t1 = ArrayType(ArrayType(Float,32),32)
    val t2 = ArrayType(ArrayType(Float,32),32)
    if(t1 == t2){
      println("true")
    }
    else{
      println("false")
    }
    assert(true)

  }

  @Ignore
  @Test
  def seekParserBugs():Unit= {
  }

  @Ignore
  @Test
  def seekCompilerBugs():Unit={
    val f = fun(
      Float,
      ArrayType(ArrayType(Float,32),32),
      Float,
      ArrayType(Float,32),
      (p99,p102,p226,p239) =>{
        Map(fun((p24) =>
          Split(4) o Join() o Map(fun((p157) =>
            Reduce(fun((p20,p195)=>
              add(p20,p195)
            ))(toGlobal(add)(p24,p99),p157)
          )) $ p102
        ))(Reduce(fun((p215,p49) =>
        add(p215,p49)
        ))(add(p226,p226),p239))
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
    val(output_exe1:Array[Float],_)= Execute(1,32)(fs.head,Args:_*)
    val(output_exe:Array[Float],_)= Execute(1,32)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))
  }


  @Ignore
  @Test
  def seekExeBugs():Unit={
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
    //val test = rewriting.Rewrite.rewrite(f,rewriting.allRules,1)
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    val test = rewriting.Rewrite.rewriteJustGenerable(fs.head,rewriting.allRules,5)
    for(i<- test.indices) {
      val rewrited = test(i)

      TypeChecker(rewrited)
      val code = Compile(rewrited)
      val Args = scala.collection.mutable.ArrayBuffer[Any]()
      for (j <- f.params.indices) {
        f.params(j).t match {
          case ArrayType(ArrayType(Float, l1), l2) =>
            Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
          case ArrayType(Float, l1) =>
            Args += Array.fill(l1.eval)(2.0f)
          case Float =>
            Args += 3.0f
          case _ =>
        }
      }
      val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args: _*).toArray[Float]
      //val (output_exe1: Array[Float], _) = Execute(1, 32)(fs.head, Args: _*)
      val (output_exe: Array[Float], _) = Execute(1, 32)(code, rewrited, Args: _*)
      assert(output_exe.corresponds(output_int)(_ == _))
    }

  }

  @Ignore
  @Test
  def seekExeBugs1():Unit={
    for(i<-0 until 10000){

    val f = fun(
      Float,
      ArrayType(Float,32),
      ArrayType(Float,32),
      (p236,p116,p93) =>{
        Reduce(fun((p183,p247) =>
          Map(fun((p18) =>
            add(p247,p18)
          )) $ p183
        ))(Map(fun((p18) =>
          add(p236,p18)
        ))(p116),p93)

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
    //val(output_exe1:Array[Float],_)= Execute(1,32)(fs.head,Args:_*)
    val(output_exe:Array[Float],_)= Execute(1,32)(code,fs.head,Args:_*)
    assert(output_exe.corresponds(output_int)(_==_))

  }}

  @Ignore
  @Test
  def ResultNotEqualBugs():Unit={
    //val f = Eval("val add = UserFun(\"add\", Array(\"x\", \"y\"), \"\"\"|{ return x+y; }\"\"\".stripMargin, Seq(Float, Float), Float).setScalaFun (xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float])\nfun(ArrayType(Float, 4), ArrayType(Float, 32), Float, ArrayType(ArrayType(Float, 32), 32),(p_0, p_1, p_2, p_3) => FunCall(Map(fun((p_4) => FunCall(Join(), FunCall(Reduce(fun((p_5, p_6) => FunCall(Map(fun((p_7) => FunCall(add, p_6, p_7))), p_5))), FunCall(Map(fun((p_8) => FunCall(add, p_8, p_4))), p_0), p_1)))), FunCall(Map(fun((p_9) => FunCall(add, p_9, p_2))), FunCall(Join(), p_3))))")
    val f = fun(
      Float,
      ArrayType(Float,32),
      ArrayType(Float,32),
      (p236,p116,p93) =>{
        Reduce(fun((p183,p247) =>
          Map(fun((p18) =>
            add(p247,p18)
          )) $ p183
        ))(Map(fun((p18) =>
          add(p236,p18)
        ))(p116),p93)

      }
    )
    TypeChecker(f)
    val fAfterRewrite = Rewrite.rewriteWithoutLowering(f,rewriting.allRulesWithoutLowering,1)
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, true, true, true, true, true),true)
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


}

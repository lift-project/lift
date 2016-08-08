package ir.hlGenerator

import java.io.PrintWriter

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Execute, Executor}
import org.junit._
import opencl.ir._
import opencl.ir.pattern.{toGlobal, toPrivate}
import rewriting.{EnabledMappings, Lower}

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
  def testLoops():Unit={
    hlGenerator.tryPrograms(new PrintWriter("/home/potato/testLambda.txt"))
  }


  @Ignore
  @Test
  def seekTestBugs():Unit={

  }

  @Ignore
  @Test
  def seekParserBugs():Unit= {
  }

  @Ignore
  @Test
  def seekCompilerBugs():Unit={
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
  def seekExeBugs1():Unit={
    val f = fun(
      ArrayType(Float,32),
      Float,
      (p241,p29) =>{
      Map(fun((p55) =>
      Reduce(fun((p3,p38) =>
        add(p3,p38)
      ))(p55,p241)
      ))(Reduce(fun((p3,p38) =>
        add(p3,p38)
      ))(p29,p241))
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
  def ResultNotEqualBugs():Unit={

  }


}

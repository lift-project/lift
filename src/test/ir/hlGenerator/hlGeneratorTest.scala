package ir.hlGenerator

import java.io.PrintWriter

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Execute, Executor}
import org.junit._
import opencl.ir._
import rewriting.{EnabledMappings, Lower}

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
  def seekTestBugs():Unit={
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
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    val code = Compile(fs.head)
    val(output_exe,runtime)= Execute(1,1)(code,fs.head,Args:_*)
    assert(true)

  }

  @Ignore
  @Test
  def seekParserBugs():Unit={
    /*val f = fun(
      Float,
      ArrayType(ArrayType(Float,32),32),
      ArrayType(Float,32),
      Float,
      (p182,p242,p63,p122) => {
       Join() o Reduce(fun((p196,p203) => Map(fun((p251) => add(p203,p251))) $ p196),Join() o Map(fun((p199) =>Reduce(fun((p58,p89) => add(p58,p89)),add(p199,p182)) o Join() $ p242)) $ p63) o Map(fun((p92) => add(p122,p92))) o Join() $ p242
      }
    )*/
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
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    val code = Compile(fs.head)
    assert(true)
  }

  @Ignore
  @Test
  def seekCompilerBugs():Unit={
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
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
    val code = Compile(fs.head)
    assert(true)
  }

  @Ignore
  @Test
  def testLoops():Unit={
    hlGenerator.tryPrograms(new PrintWriter("/home/potato/testLambda.txt"))
  }

  @Ignore
  @Test
  def ResultNotEqualBugs():Unit={


  }

}

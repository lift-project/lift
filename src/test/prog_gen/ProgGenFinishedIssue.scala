package prog_gen

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Execute, Executor}
import opencl.ir._
import org.junit.Assert._
import org.junit._
import rewriting.{EnabledMappings, Lower}

import scala.language.reflectiveCalls

object ProgGenFinishedIssue {
  @BeforeClass
  def before(): Unit =
    Executor.loadAndInit()

  @AfterClass
  def after(): Unit =
    Executor.shutdown()
}

class ProgGenFinishedIssue {

  @Test
  def hlGenTest1(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float,32),
      Float,
      ArrayTypeWSWC(ArrayTypeWSWC(Float,32),32),
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
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))

    val Args = InputGenerator()(fs.head)

    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_) = Execute(1,1)(fs.head,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }

  //The result should be Nan
  @Ignore
  @Test
  def hlGenLower1(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float,32),
      ArrayTypeWSWC(ArrayTypeWSWC(Float,32),32),
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
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))

    val Args = InputGenerator()(fs.head)

    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1024)(fs.head,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }

  //The high-level expression could not find a correct lower one
  @Test
  def hlGenCompiler1(): Unit = {

    val f = fun(
      Float,
      ArrayTypeWSWC(Float,32),
      ArrayTypeWSWC(Float,32),
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
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))

    val Args = InputGenerator()(fs.head)

    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,1024)(fs.head,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }

  @Ignore
  @Test
  def hlGenCompiler2():Unit={
    val f = fun(
      Float,
      ArrayTypeWSWC(ArrayTypeWSWC(Float,32),32),
      Float,
      ArrayTypeWSWC(Float,32),
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
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))
    TypeChecker(fs.head)
    val lowLevel = fs.head

    val Args = InputGenerator()(fs.head)

    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,32)(lowLevel,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }
}

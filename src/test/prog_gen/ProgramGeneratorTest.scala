package prog_gen

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Execute, Executor}
import opencl.ir._
import opencl.ir.pattern.toGlobal
import org.junit.Assert._
import org.junit._
import rewriting.{EnabledMappings, Lower}

import scala.language.reflectiveCalls

object ProgramGeneratorTest {
  val generator = new ProgramGenerator
  val generatedPrograms = generator.generatePrograms()

  @BeforeClass
  def before(): Unit =
    Executor.loadAndInit()

  @AfterClass
  def after(): Unit =
    Executor.shutdown()

}

class ProgramGeneratorTest {

  import ProgramGeneratorTest._

  @Test
  def usefulZipGeneration(): Unit = {

    // Count the number of different components projected out of tuples
    val numComponentsUsed =
      generatedPrograms.flatMap( f => Expr.visitWithState(Seq[(Expr, Int)]())(f.body, {
        case (FunCall(Get(n), p), getArgs) => getArgs :+ (p, n)
        case (_, getArgs) => getArgs
      }).groupBy(e => e._1).map(a => a._2.map(_._2).distinct.length))


    assertTrue(numComponentsUsed.exists(_ >= 2))
  }

  @Test
  def noUselessMaps(): Unit = {
    assertTrue(generatedPrograms.forall(!_.body.contains({
      case FunCall(Map(Lambda(Array(p), b)), _)
        if !b.contains({ case p1: Param if p1 == p => }) =>
    })))
  }

  @Test
  def mapGeneration(): Unit =
    assertTrue(generator.LambdaList.exists({
      case Lambda(_, FunCall(Map(_), _)) => true
      case _ => false
    }))

  @Test
  def splitGeneration(): Unit =
    assertTrue(generatedPrograms.exists(
      _.body.contains({ case FunCall(Split(_), _*) => })))

  @Test
  def joinGeneration(): Unit =
    assertTrue(generatedPrograms.exists(
      _.body.contains({ case FunCall(Join(), _*) => })))

  @Test
  def reduceGeneration(): Unit =
    assertTrue(generatedPrograms.exists(
      _.body.contains({ case FunCall(Reduce(_), _*) => })))

  @Test
  def zipGeneration(): Unit =
    assertTrue(generatedPrograms.exists(
      _.body.contains({ case FunCall(Zip(_), _*) => })))

  @Test
  def generatesSomething(): Unit = {
    assertTrue(generator.UnpackedToExpr.nonEmpty)
    assertTrue(generator.LambdaList.nonEmpty)
    assertTrue(generator.ParamToFunCall.nonEmpty)
    assertTrue(generatedPrograms.nonEmpty)
  }

  @Test
  def mapReduceComposition(): Unit =
    assertTrue(generatedPrograms.exists(
      _.body.contains({ case FunCall(Reduce(_), _, FunCall(Map(_), _)) => })))

  @Test
  def reduceMapComposition(): Unit =
    assertTrue(generatedPrograms.exists(
      _.body.contains({ case FunCall(Map(_), FunCall(Reduce(_), _, _)) => })))

  @Ignore
  @Test
  def seekCompilerBugs(): Unit = {
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
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false,
        group0 = false, group01 = false, group10 = false))
    TypeChecker(fs.head)
    val code = Compile(fs.head)
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- f.params.indices) {
      f.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.fill(l1.eval, l2.eval)(1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _=>
      }
    }

    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,32)(code,fs.head,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }


  @Ignore
  @Test
  def seekExeBugs(): Unit = {
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
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false,
        group0 = false, group01 = false, group10 = false))
    val test = rewriting.Rewrite.rewriteJustGenerable(fs.head,rewriting.allRules,5)
    for(i<- test.indices) {
      val rewrited = test(i)

      TypeChecker(rewrited)
      val code = Compile(rewrited)
      val Args = scala.collection.mutable.ArrayBuffer[Any]()
      for (j <- f.params.indices) {
        f.params(j).t match {
          case ArrayType(ArrayType(Float, l1), l2) =>
            Args += Array.fill(l1.eval, l2.eval)(1.0f)
          case ArrayType(Float, l1) =>
            Args += Array.fill(l1.eval)(2.0f)
          case Float =>
            Args += 3.0f
          case _ =>
        }
      }
      val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args: _*).toArray[Float]
      val (output_exe: Array[Float], _) = Execute(1, 32)(code, rewrited, Args: _*)
      assertArrayEquals(output_int, output_exe, 0.0f)
    }

  }

}

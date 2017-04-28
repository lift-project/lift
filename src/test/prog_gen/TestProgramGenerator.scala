package prog_gen

import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Execute, Executor, LongTestsEnabled}
import opencl.ir._
import opencl.ir.pattern.{ReduceSeq, toGlobal}
import org.junit.Assert._
import org.junit._
import rewriting.{EnabledMappings, Lower}

import scala.language.reflectiveCalls

object TestProgramGenerator {

  var generator: ProgramGenerator = _
  var generatedPrograms = Array[Lambda]()

  @BeforeClass
  def before(): Unit = {
    Executor.loadAndInit()
    generator = new ProgramGenerator

    // TODO: No randomness and less iterations for testing?
    if (LongTestsEnabled.areEnabled)
      generatedPrograms = generator.generatePrograms()

  }

  @AfterClass
  def after(): Unit =
    Executor.shutdown()

}

class TestProgramGenerator {

  import TestProgramGenerator._

  LongTestsEnabled()

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
  def transposeGeneration(): Unit = {
    assertTrue(generatedPrograms.exists(_.body.contains({
      case FunCall(Transpose(), _*) =>
    })))
  }

  @Test
  def addGeneration(): Unit = {
    assertTrue(generatedPrograms.exists(_.body.contains({
      case FunCall(uf: UserFun, _*) if uf == add =>
    })))
  }

  @Test
  def multGeneration(): Unit = {
    assertTrue(generatedPrograms.exists(_.body.contains({
      case FunCall(uf: UserFun, _*) if uf == mult=>
    })))
  }

  @Test
  def compositionGeneration(): Unit = {
    assertTrue(generatedPrograms.exists(_.body.contains({
      case FunCall(_: UserFun, args@_*)
        if args.collectFirst({case FunCall(_: UserFun, _*) => }).isDefined =>
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
      _.body.contains({ case FunCall(Reduce(_), _: Value, _) => })))

  @Test
  def reduceGeneration2(): Unit =
    assertFalse(generatedPrograms.exists(
      _.body.contains({ case FunCall(Reduce(_), _: FunCall, _) => })))

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
      ArrayTypeWSWC(ArrayTypeWSWC(Float,32),32),
      Float,
      ArrayTypeWSWC(Float,32),
      (p99,p102,p226,p239) =>{
        Map(fun((p24) =>
          Split(4) o Join() o Map(fun((p157) =>
            ReduceSeq(fun((p20,p195)=>
              add(p20,p195)
            ))(toGlobal(add)(p24,p99),p157)
          )) $ p102
        ))(ReduceSeq(fun((p215,p49) =>
        add(p215,p49)
        ))(add(p226,p226),p239))
      }
    )
    val fs = Lower.mapCombinations(f,
      EnabledMappings(global0 = true, global01 = false, global10 = false, false, false, group0 = false, group01 = false, group10 = false))
    TypeChecker(fs.head)
    val code = Compile(fs.head)
    val Args = InputGenerator()(fs.head)

    val output_int = Interpreter(f).->[Vector[Vector[Float]]].runAndFlatten(Args:_*).toArray[Float]
    val(output_exe:Array[Float],_)= Execute(1,32)(code,fs.head,Args:_*)
    assertArrayEquals(output_int, output_exe, 0.0f)
  }
}

package ir.hlGenerator

import java.io.PrintWriter

import apart.arithmetic.{ArithExpr, Log, SizeVar}
import ir._
import ir.ast._
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Executor}
import org.junit._
import org.junit.Assert._
import opencl.ir._
import rewriting.{EnabledMappings, Lower}
import rewriting.utils.ScalaPrinter

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
  /*
  @Ignore
  @Test
  def testGen():Unit={
    val l = codeGenerator.generateLambda( ArrayType(Float,1024),0,20)
    if(l.params.length == 1){
      val test = l.checkType(l.params.head.t,true)
      assert(test == ArrayType(Float,1024))
    }
    else {
      val argType = ArrayBuffer[Type]()
      for (i <- 0 until l.params.length) {
        argType += l.params(i).t
      }
      val test = l.checkType(TupleType(argType.toArray[Type]: _*), true)
      assert(l.checkType(TupleType(argType.toArray[Type]:_*),true) == ArrayType(Float,1024))
    }
    assert(true)

  }*/
  /*
  @Ignore
  @Test
  def testHlGen():Unit={
    hlGenerator.generateProgram()
    val res = hlGenerator.LambdaList
    var ExceptionInInterpreter = 0
    for(i <- 0 until res.length){
      val l = res(i)
      println("Lambda Num:" + i)
      println(l.toString)
      if (l.toString.contains("add")) {
        //1. Generate Input Data
        val Args = scala.collection.mutable.ArrayBuffer[Any]()
        for (j <- l.params.indices) {
          l.params(j).t match {
            case ArrayType(ArrayType(Float, l1), l2) =>
              Args += Array.tabulate(l1.eval, l2.eval)((r, c) => 1.0f)
            case ArrayType(Float, l1) =>
              Args += Array.fill(l1.eval)(2.0f)
            case Float =>
              Args += 3.0f

          }
        }
        //2. Pass the TypeChecker
        val outType = TypeChecker(l)
        outType match {
          case ArrayType(Float, _) =>
            //1. pass the Interpreter
            val output = Interpreter(l).->[Vector[Float]].run(Args: _*)



            l.params.foreach(p => println(p.t.toString))
            val fs = Lower.mapCombinations(l,new EnabledMappings(true, false, false, false, false, false))
            assertTrue(fs.nonEmpty)
            assertTrue(fs.forall(_.isGenerable))
            println(ScalaPrinter(fs.head))

            println(fs.head.toString)

            val code = Compile(fs.head)
          case _ =>
            assert(true)
        }

      }
      else{
        println("Doesn't contains UserFun, Ignored")
      }



      assert(true)

    }
    assert(true)
  }*/

  @Ignore
  @Test
  def seekParserBugs():Unit={
    val f = fun(
      Float,
      ArrayType(ArrayType(Float,32),32),
      ArrayType(Float,32),
      Float,
      (p182,p242,p63,p122) => {
       Join() o Reduce(fun((p196,p203) => Map(fun((p251) => add(p203,p251))) $ p196),Join() o Map(fun((p199) =>Reduce(fun((p58,p89) => add(p58,p89)),add(p199,p182)) o Join() $ p242)) $ p63) o Map(fun((p92) => add(p122,p92))) o Join() $ p242
      }
    )
    val fs = Lower.mapCombinations(f,new EnabledMappings(true, false, false, false, false, false))
  }

  @Ignore
  @Test
  def testLoops():Unit={
    hlGenerator.tryPrograms(new PrintWriter("/home/potato/testLambda.txt"))
  }
  /*
  @Test
  def replaceArg(): Unit = {
    val lambda: Lambda = fun(x => MapGlb(id) $ x)

    /*
    val arg1 = lambda match {
      case Lambda(_, FunCall(_, a)) => a
    }
    val arg = arg1 match {
      case p: Param => p
    }*/
    val arg = lambda.params(0)

    val replacementArg = Param()

    val result = hlGenerator.replaceParam(lambda, arg, replacementArg)

    assert(true)
  }*/
}

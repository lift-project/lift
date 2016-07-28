package ir.codeGenerator

import ir._
import ir.Type
import ir.ast._
import ir.interpreter.Interpreter
import org.junit._
import opencl.ir._
import opencl.ir.pattern.MapGlb
import org.junit.Assert._

import scala.collection.mutable.ArrayBuffer
/**
  * Created by potato on 25/07/16.
  */
class codeGeneratorTest {

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

  }
  @Test
  def testHlGen():Unit={
    hlGenerator.generateProgram()
    val res = hlGenerator.LambdaList
    for(i <- res.indices){
      val l = res(i)
      if(l.params.length == 1){
        val test = l.checkType(l.params.head.t,true)
      }
      else {
        val argType = ArrayBuffer[Type]()
        for (i <- 0 until l.params.length) {
          argType += l.params(i).t
        }
        val test = l.checkType(TupleType(argType.toArray[Type]: _*), true)
      }
    }
    assert(true)
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

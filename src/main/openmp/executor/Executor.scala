package openmp.executor
import java.io.PrintWriter

import apart.arithmetic.SizeVar
import ir.{ArrayType, TupleType, Type}

import sys.process._
import scala.language.postfixOps
import ir.ast.{Lambda, UserFun, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.generator.OMPGenerator

/**
  * Created by Federico on 15-Jul-16.
  */
object Executor {
  def apply(kernel:Lambda, data:Any) = compileAndRun(kernel, data)

  def compileAndGenerateScript(kernel: Lambda, data:Any, path:String) = {
    val commands = compileAndGenerateCommands(kernel,data, path)
    val text = commands.reduce((x,y) => { x ++ "\n" ++ y})
    new PrintWriter(path + "/run.sh") { write(text); close}
  }

  def compileAndRun(kernel:Lambda, data:Any) = {
    val commands = compileAndGenerateCommands(kernel,data, "")
    runCommands(commands)
  }

  private def compileAndGenerateCommands(kernel:Lambda, data:Any, path:String):List[String] = {
    val programSource = Harness.generate(OMPGenerator, kernel)
    new PrintWriter(path + "/lift.c") { write(programSource); close }
    val commands = generateCommands(data)
    commands
  }

  private def runCommands(ls:List[String]) = {
    ls.foreach{x => x.! }
  }

  private def generateCommands(data:Any):List[String] = {
    val compileCommand = "gcc lift.c -o a.out -std=c99 -fopenmp"
    val programName = "./a.out"
    val args = generateCommandLineArgs(data)
    val runCommmand = if (isWindows) (s"$programName $args") else (s"./$programName $args")
    List(compileCommand,runCommmand)
  }


  private def generateCommandLineArgs(data:Any) = "\"#@" ++ encode(data) ++ "\""

  private def encode(data:Any):String = data match {
    case f:Float => f.toString
    case i:Int => i.toString
    case s:String => s
    case xs:List[Any] => encodeList(xs)
    case x => throw new Exception("Cannot encode scala value " + x.toString)
  }

  private def encodeList(ls:List[Any]):String = ls.reduce((x,y) => encode(x) ++ "_" ++ encode(y)).asInstanceOf[String]

  private def isWindows:Boolean = System.getProperty("os.name").startsWith("Windows")

  def main(args: Array[String]) {
    def genID(t:Type) = UserFun("id","x", "return x;",t,t)
    val f = fun(
      ArrayType(TupleType(Float,Float),2),
      A => {
        MapSeq(genID(TupleType(Float,Float))) $ A
      })
    val f2 = fun (
      ArrayType(Float, SizeVar("N")),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, init) $ in
      })
    val trivial = fun(Float, x => toGlobal(id) $ x)
    this.compileAndGenerateScript(f,List(List(3,2),List(4,5)),"D:/Test")
  }
}

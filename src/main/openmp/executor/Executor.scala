package openmp.executor
import java.io.{BufferedWriter, FileWriter, PrintWriter}

import apart.arithmetic.SizeVar
import ir.{ArrayType, TupleType, Type}

import sys.process._
import scala.language.postfixOps
import ir.ast.{Get, Lambda, UserFun, Value, Zip, fun}
import opencl.ir.{pattern, _}
import opencl.ir.pattern.{MapGlb, MapSeq, ReduceSeq, toGlobal}
import openmp.generator.OMPGenerator

/**
  * Created by Federico on 15-Jul-16.
  */
object Executor {
  def apply(kernel:Lambda, data:Any) = compileAndRun(kernel, data)

  def compileAndGenerateScript(kernel: Lambda, data:Any, path:String) = {
    val commands = compileAndGenerateCommands(kernel,data, path)
    val text = commands.reduce((x,y) => { x ++ "\n" ++ y})
    new PrintWriter(new BufferedWriter(new FileWriter(path + "/run.sh"))) { write(text); close}
    val params = generateDataParameter(data)
    new PrintWriter(new BufferedWriter(new FileWriter(path + "/data.txt"))) {write(params); close}
  }

  def compileAndRun(kernel:Lambda, data:Any) = {
    val commands = compileAndGenerateCommands(kernel,data, "")
    runCommands(commands)
  }

  private def compileAndGenerateCommands(kernel:Lambda, data:Any, path:String):List[String] = {
    val programSource = Harness.generate(OMPGenerator, kernel, Harness.GenerationOption(true,false,false))
    new PrintWriter(path + "/lift.c") { write(programSource); close }
   // new PrintWriter(path + "/data.txt") { write(generateDataParameter(data)); close}

    val commands = generateCommands(data)
    commands
  }

  private def runCommands(ls:List[String]) = {
    ls.foreach{x => x.! }
  }

  private def generateCommands(data:Any):List[String] = {
    val compileCommand = "gcc lift.c -o a.out -std=c99 -fopenmp -O3 -ftree-vectorize -lm"
    val programName = "./a.out"
    val runCommmand = if (isWindows) (s"$programName") else (s"./$programName")
    List(compileCommand,runCommmand)
  }


  private def generateDataParameter(data:Any) = {
    val sb = new StringBuilder
    sb.append("\"#@")
    encode(data,sb)
    sb.append("\"")
    sb.toString()
  }

  private def encode(data:Any,sb:StringBuilder):Unit = data match {
    case f:Float => sb.append(f.toString)
    case i:Int => sb.append(i.toString)
    case s:String => sb.append(s)
    case xs:List[Any] => encodeList(xs,sb)
    case x => throw new Exception("Cannot encode scala value " + x.toString)
  }

  private def encodeList(ls:List[Any],sb:StringBuilder):Unit = ls match {
    case x::xs => {
      encode(x,sb)
      sb.append("_")
      encodeList(xs,sb)
      //ls.reduce((x,y) => encode(x) ++ "_" ++ encode(y)).asInstanceOf[String]
    }
    case Nil =>
  }

  private def isWindows:Boolean = System.getProperty("os.name").startsWith("Windows")
}

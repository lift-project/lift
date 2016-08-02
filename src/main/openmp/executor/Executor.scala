package openmp.executor
import java.io.{BufferedWriter, FileWriter, PrintWriter}

import apart.arithmetic.SizeVar
import ir.{ArrayType, TupleType, Type}

import sys.process._
import scala.language.postfixOps
import ir.ast.{Lambda, UserFun, Zip, fun}
import opencl.ir.{pattern, _}
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.generator.OMPGenerator
import openmp.ir.pattern.{:+, MapPar, ReducePar}

/**
  * Created by Federico on 15-Jul-16.
  */
object Executor {
  def apply(kernel:Lambda, data:Any) = compileAndRun(kernel, data)

  def compileAndGenerateScript(kernel: Lambda, data:Any, path:String) = {
    val commands = compileAndGenerateCommands(kernel,data, path)
    val text = commands.reduce((x,y) => { x ++ "\n" ++ y})
    new PrintWriter(new BufferedWriter(new FileWriter(path + "/run.sh"))) { write(text); close}
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
    val N = 1000
    def genID(t:Type) = UserFun("id","x", "return x;",t,t)
    def increment = UserFun("inc","x", "return x + 1;", Float, Float)
    val f = fun(
      ArrayType(Float,N),
      A => {
        MapSeq(increment) $ A
      })
    val f2Seq = fun (
      ArrayType(Float, N),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, init) $ in//o MapSeq(increment) o MapSeq(increment)  $ in
      })
    val f2Par = fun (
      ArrayType(Float, N),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReducePar(:+(Float), init) $ in //o MapPar(increment) o MapPar(increment)  $ in
      })
    val trivial = fun(Float, x => toGlobal(id) $ x)

    val dotProdSeq = fun(
      ArrayType(Float,N),
      ArrayType(Float,N),
      Float,
      (inA,inB, init) => {
          toGlobal(MapSeq(id)) o ReduceSeq(add, init) o MapSeq(mult) $ Zip(inA,inB)
      }
    )

    val ls = (List.iterate(0,N)(x => x + 1)).map(_ => 1)
    this.compileAndGenerateScript(dotProdSeq,ls ++ ls ++ List(0.0f),"D:/Test")
  }
}

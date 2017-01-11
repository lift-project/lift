package prog_gen

import java.io.PrintWriter

import ir._
import ir.ast.Lambda
import ir.interpreter.Interpreter
import opencl.executor.{Compile, Eval, Execute}
import opencl.ir._
import rewriting.{EnabledMappings, Lower, Rewrite}

import scala.language.reflectiveCalls

class ProgramRunner {

  //controllers for run programs
  val PrintDebugInfo = true
  var PassCounter = 0
  val GlobalSize = 512
  val LocalSize = 32
  val RunInterpreter = false
  val useRandomRewrite = false
  val rewriteDepth = 3

  private def writeln(w:PrintWriter,s:String):Unit={
    w.write(s+"\n")
  }

  def tryPrograms(w:PrintWriter, generator: ProgramGenerator):Unit = {
    generator.generateProgram()
    val res = generator.RefinedResult
    for(i <- res.indices){
      val lStr = rewriting.utils.Utils.dumpLambdaToString(res(i))
      println(lStr)
      writeln(w,lStr)
      //val UserFunIndex = lStr.indexOf('\n')

      //The compiler have a bug here, so we need to convert it to string and convert back
      //val l:Lambda = Eval(lStr)
      val l:Lambda = try {
        Eval(lStr)
      } catch{
        case _: Throwable=>
          println("parse back failed")
          return
      }

      //prints the basic informations about l
      println("Lambda Num:" + i)
      println(l.toString)
      l.params.foreach(p => println(p.t.toString))


      writeln(w,"Lambda Num:" + i)
      writeln(w,l.toString)
      l.params.foreach(p => writeln(w,p.t.toString))


      trySingleLambda(l,res(i),w)
      w.flush()
      System.out.flush()
    }
  }

    //Testing methods
  def trySingleLambda(l: Lambda, oril: Lambda, w: PrintWriter): Unit = {
    if(l.params.length > 7){
      if(PrintDebugInfo) {
        println("too many argument, Ignored-by-user")
        writeln(w, "too many argument, Ignored-by-user")
      }
      return
    }
    //1. Generate Input Data
    val Args = scala.collection.mutable.ArrayBuffer[Any]()
    for (j <- l.params.indices) {
      l.params(j).t match {
        case ArrayType(ArrayType(Float, l1), l2) =>
          Args += Array.fill(l1.eval, l2.eval)(1.0f)
        case ArrayType(Float, l1) =>
          Args += Array.fill(l1.eval)(2.0f)
        case Float =>
          Args += 3.0f
        case _ =>
          if(PrintDebugInfo) {
            println("input type not inplemented, Ignored-by-user")
            writeln(w, "input type not inplemented, Ignored-by-user")
          }
          return
      }
    }

    //2. Pass the TypeChecker
    val outType = TypeChecker(l)

    //2.5 run the interpreter if we want
    val output_int:Array[Float] = if (RunInterpreter) {
      outType match {
        case Float =>
          Array[Float](Interpreter(oril).->[Float].run(Args: _*))
        case ArrayType(Float, _) =>
          Interpreter(oril).->[Vector[Float]].run(Args: _*).toArray[Float]
        case ArrayType(ArrayType(Float, _), _) =>
          Interpreter(oril).->[Vector[Vector[Float]]].runAndFlatten(Args: _*).toArray[Float]
        case ArrayType(ArrayType(ArrayType(Float, _), _), _) =>
          Interpreter(oril).->[Vector[Vector[Vector[Float]]]].runAndFlatten(Args: _*).toArray[Float]
        case _ =>
          if (PrintDebugInfo) {
            println("OutPut Type unimplemented,Ignored-by-user")
            writeln(w, "OutPut Type unimplemented,Ignored-by-user")
          }
          return
      }
    } else {
      Array[Float](0)
    }

    //3. random rewriting
    val afterRewriting:scala.Seq[Lambda] = tryRewriting(l,w)

    if(afterRewriting.length < 1){
      return
    }


    for(rewriteId <- afterRewriting.indices) {

      //3. lower the lambda
      //var fs = List[Lambda]()
      val afterLowering = tryLowering(afterRewriting(rewriteId), w)

      if (afterLowering.length < 1) {
        if (PrintDebugInfo) {
          println("No suitable lower-expressions, Ignored-by-user")
          writeln(w, "No suitable lower-expressions, Ignored-by-user")
        }
      }
      else {
        for (lowId <- afterLowering.indices) {

          val lowLevel = afterLowering(lowId)
          if (lowLevel.isGenerable) {

            //4. compile the lambda
            val (code,compileSucc) = tryCompile(lowLevel,w)
            if(compileSucc) {

              //5. execute the OpenCL kernel
              try {
                //val (output_exe: Array[Float], runtime) = Execute(LocalSize, GlobalSize)(code, lowLevel, Args: _*)
                //val (output_exe: Array[Float], runtime1) = Execute(GlobalSize, GlobalSize)(code, lowLevel, Args: _*)

                var currGlobalSize = GlobalSize
                while (currGlobalSize >= 1) {
                  var currLclSize = currGlobalSize
                  while (currLclSize >= 1) {
                    val (output_exe: Array[Float], runtime) = Execute(currLclSize, currGlobalSize)(code, lowLevel, Args: _*)
                    if (RunInterpreter) {
                      if (output_exe.corresponds(output_int)(_ == _)) {
                        writeln(w, "results eq-by-user")
                        println("results eq-by-user")
                      }
                      else {
                        writeln(w, "results ne-by-user")
                        println("results ne-by-user")
                      }
                    }
                    else {
                      println("pass-by-user")
                      println("globalsize = " + currGlobalSize + " localsize = " + currLclSize)
                      println("time = " + runtime)
                      writeln(w, "pass-by-user")
                      writeln(w, "globalsize = " + currGlobalSize + " localsize = " + currLclSize)
                      writeln(w, "time = " + runtime)
                      PassCounter += 1
                      println(PassCounter + "passed for now")
                      writeln(w, PassCounter + "passed for now")
                    }
                    currLclSize /= 4
                  }
                  currGlobalSize /= 4
                }
              }
              catch {
                case e: Throwable =>
                  println("catch a exception in execator-by-user")
                  e.printStackTrace()
                  writeln(w, "catch a exception in execator-by-user")
                  e.printStackTrace(w)
              }
            }
          }
        }
      }
    }

  }


  private def tryRewriting(l:Lambda,w:PrintWriter):Seq[Lambda] ={
    if (useRandomRewrite) {
      try {
        //Rewrite.rewriteWithoutLowering(l,rewriting.allRulesWithoutLowering,rewriteDepth)
        Rewrite.rewriteWithoutLowering(l, rewriting.allRulesWithoutMapsLowering(1, 1, 1), rewriteDepth)
      }
      catch {
        case e: Throwable =>
          println("catch a exception in random-rewrite-by-user")
          writeln(w, "catch a exception in random-rewrite-by-user")

          if (PrintDebugInfo) {
            e.printStackTrace(w)
            e.printStackTrace()
          }
          Seq()
      }
    } else {
      Seq(l)
    }
  }

  private def tryLowering(l:Lambda,w:PrintWriter):List[Lambda] = {
    try {
      Lower.mapCombinations(l, EnabledMappings(global0 = true, global01 = true, global10 = true, group0 = true, group01 = true, group10 = true))
    }
    catch {
      case e: Throwable =>
        if(PrintDebugInfo) {
          println("catch a exception in lower-parser-by-user")
          writeln(w, "catch a exception in lower-parser-by-user")
          e.printStackTrace(w)
          e.printStackTrace ()
        }
        List[Lambda]()
    }
  }

  private def tryCompile(l:Lambda,w:PrintWriter):(String,Boolean)={
    try {
      if(PrintDebugInfo) {
        println(l.toString)
        writeln(w, l.toString)
      }
      (Compile(l),true)
    }
    catch {
      case e: Throwable =>
        if(PrintDebugInfo) {
          println("catch a exception in compiler-by-user")
          writeln(w, "catch a exception in compiler-by-user")
        }
        e.printStackTrace(w)
        ("",false)
    }
  }
}

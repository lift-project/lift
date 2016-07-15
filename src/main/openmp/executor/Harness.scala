package openmp.executor

import java.util.Scanner

import apart.arithmetic.{?, SizeVar}
import c.generator.CGenerator
import ir.{ArrayType, TupleType, Type, TypeChecker}
import ir.ast.{Lambda, Param, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.generator.OMPGenerator
import openmp.ir.pattern.MapPar


/**
  * Created by Federico on 05-Jul-16.
  */
object Harness {

  def apply(gen:CGenerator, kernel:Lambda):String = {
    val cstring = this.generate(gen,kernel)
    //Must now strip comments because the can screw up with openmp
    cstring
  }

  def generate(gen:CGenerator, kernel: Lambda):String = {
    TypeChecker.check(kernel.body)
    val kernelSource = gen.generate(kernel,Array(?,?,?),Array(?,?,?), Map())
    val mainMethod = generateMainMethod(kernel)
    kernelSource ++ "\n" ++ mainMethod
  }

  private def generateMainMethod(kernel:Lambda):String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append("//Auto-generated runtime harness\n")
    stringBuilder.append(harnessIncludes)
    stringBuilder.append("int main(int argc, char** argv) {\nint arrCount;\n")
    stringBuilder.append("char* inputData = strtok(argv[1],\"_\");\n")
    kernel.params.foreach {x => stringBuilder.append(generateParameterCode(x))}
    stringBuilder.append(generateInvocationCode(kernel))
    stringBuilder.append("}")
    stringBuilder.toString()
  }

  private def harnessIncludes = "#include <stdio.h>\n#include <string.h>\n"

  private def generateParameterCode(param:Param):String =s"//code for parameter $param\n" ++ declareVariable(param.t, param.toString) ++  scanInput(param.t, param.toString)

  private def writeVariable(t:Type, varName:String):String = {
    val str = t match {
      case Float => writeSimple(t,varName)
      case Int => writeSimple(t,varName)
      case t:TupleType => writeTuple(t,varName)
      case t:ArrayType => writeArray(t,varName)
      case _ => throw new Exception("Cannot write variable of unsupported type " ++ t.toString)
    }
    str
  }

  private def declareVariable(t:Type, varName:String):String = {
    val sb = new StringBuilder
    val tName = typeName(t)
    t match {
      case t:ArrayType => sb.append(s"${typeName(t.elemT)} $varName [${t.len}]")
      case x => sb.append(s"$tName $varName")
    }
    sb.append(";\n")
    sb.toString
  }

  private def scanInput(t:Type, varName:String):String = {
    t match {
      case Float => scanSimple("%f",varName)
      case Int => scanSimple("%d", varName)
      case t:TupleType => scanTuple(t,varName)
      case arr:ArrayType => scanArray(arr,varName)
      case _ => throw new Exception("Cannot read variable of unsupported type " ++ t.toString)
    }
  }

  private def scanSimple(format:String, varName:String) = {
    val sb = new StringBuilder
    sb.append("inputData = strtok(NULL, \"_\"); ")
    sb.append("sscanf(inputData,\"" ++ format ++ "\"" ++ s",&($varName));\n")
    sb.toString
  }
  private def scanTuple(tupleType: TupleType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//reading tuple of t ype ${tupleType.toString}\n")
    //sb.append(declareVariable(tupleType,varName))
    //Instantiate elements
    sb.append("{\n")
    for(i <- 0 to tupleType.elemsT.size -1) {
      val t = tupleType.elemsT(i)
      sb.append(scanInput(t,s"$varName._$i"))
    }
    sb.append("\n}\n")
    sb.toString
  }

  private def scanArray(t: ArrayType, varName:String):String = {
    val sb = new StringBuilder
    val arrayInnerType = typeName(t.elemT)
    sb.append(s"//generating array of type $arrayInnerType\n")
    //sb.append(scanInput(Int, "arrCount"))
    //sb.append(declareVariable(t,varName))
    sb.append(s"for(int i = 0; i < ${t.len}; i++){\n")
    //sb.append(readInputVariable(t.elemT,"temp"))
    //sb.append(s"$varName[i] = temp;\n")
    sb.append(scanInput(t.elemT, s"$varName[i]"))
    sb.append("}\n")
    sb.toString
  }

  private def writeSimple(t:Type, varName:String):String = {
    val pattern = t match {
      case Int => "%d"
      case Float => "%f"
      case _ => throw new Exception(s"Cannot output ${typeName(t)}, it is not a simple type")
    }
    val sb = new StringBuilder
    sb.append("printf(\"")
    sb.append(pattern)
    sb.append("\"")
    sb.append(s",$varName);\n")
    sb.toString
  }

  private def writeTuple(t:TupleType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//Outputting tuple of type ${typeName(t)}\n")
    for(i <- 0 to t.elemsT.size-1) {
      sb.append(writeVariable(t.elemsT(i),s"${varName}._$i"))
      if(i < t.elemsT.size-1) {
        sb.append(separator)
      }
    }
    sb.toString
  }

  private def writeArray(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//Outputting array of type ${typeName(t)}\n")
    sb.append("printf(\"%d \"," ++  s"${t.len});\n")
    sb.append(s"for(int i = 0; i < ${t.len};i++){\n")
    sb.append(writeVariable(t.elemT,s"$varName[i]"))
    sb.append("}\n")
    sb.toString
  }

  private  def getSizeFromData = s"arrCount = ${getData("int")};${advanceData("int")};\n"
  private def getData(typeName:String) = s"(($typeName*)data)[0]"
  private def advanceData(typeName: String) = s"data += sizeof($typeName)"
  private def getDataAndAdvance(typeName: String) = getData(typeName) ++ ";" ++ advanceData(typeName) ++ ";"
  private def separator = "printf(\" \");\n"
  private def block(str:String) = s"{\n $str }\n"

  private def generateInvocationCode(kernel:Lambda):String = {
    val sb = new StringBuilder()
    sb.append("//main invocation\n")
    sb.append(declareVariable(kernel.body.t,"output"))
    val paramList = (kernel.params.foldLeft("")((x,y) => x ++ ", " ++ y.toString) ++ ", output").substring(1)
    sb.append(s"liftKernel($paramList);\n")
    sb.append(writeVariable(kernel.body.t, "output"))
    sb.toString
  }

  private def typeName(t: Type):String = t match {
    case Float => "float"
    case Int => "int"
    case t:TupleType => t.elemsT.foldLeft("Tuple")((x,y) => s"${x}_${typeName(y)}")
    case t:ArrayType => s"${typeName(t.elemT)}*"
    case t => throw new Exception("Unsupported type " ++ t.toString)
  }

  private def stripComments(input:String) = {
    val sb = new StringBuilder
    val scanner = new Scanner(input)
    while(scanner.hasNext()) {
      val line = scanner.nextLine()
      if(!(line.contains("//") || line.contains("/*") || line.contains("*/")))
        sb.append(line + "\n")
    }
    sb.toString
  }

  def main(args:Array[String]) = {
    val f = fun(
      ArrayType(TupleType(Float,Float),1),
      A => {
        MapPar(add) $ A
      })
    val f2 = fun (
      ArrayType(Float, SizeVar("N")),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, init) $ in
      })
    val trivial = fun(Float, x => toGlobal(id) $ x)
    println(Harness(OMPGenerator,f))

  }
}
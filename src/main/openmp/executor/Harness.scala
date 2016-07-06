package openmp.executor

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
  def apply(gen:CGenerator, kernel: Lambda):String = {
    TypeChecker.check(kernel.body)
    val kernelSource = gen.generate(kernel,Array(?,?,?),Array(?,?,?), Map())
    val mainMethod = generateMainMethod(kernel)
    kernelSource ++ "\n" ++ mainMethod
  }

  def generateMainMethod(kernel:Lambda):String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append("//Auto-generated runtime harness\n")
    stringBuilder.append("int main(char** argv) {\nint arrCount;\n")
    stringBuilder.append("char* data = argv[0];\n")
    kernel.params.foreach {x => stringBuilder.append(generateParameterCode(x))}
    stringBuilder.append(generateInvocationCode(kernel))
    stringBuilder.append("}")
    stringBuilder.toString()
  }

  def generateParameterCode(param:Param):String =s"//code for parameter $param\n" ++ readInputVariable(param.t, param.toString)

  def readInputVariable(t:Type, param :String) = {
    val sb = new StringBuilder
    val str = t match {
      case Float => readSimple(t, param)
      case Int => readSimple(t, param)
      case t:TupleType => readTuple(t,param)
      case t:ArrayType => readArray(t,param)
      case t => throw new Exception("Unsupported type " ++ t.toString)
    }
    sb.append(str)
    sb.toString
  }

  def writeVariable(t:Type, varName:String):String = {
    val str = t match {
      case Float => writeSimple(t,varName)
      case Int => writeSimple(t,varName)
      case t:TupleType => writeTuple(t,varName)
      case t:ArrayType => writeArray(t,varName)
      case _ => throw new Exception("Cannot write variable of unsupported type " ++ t.toString)
    }
    str
  }

  def declareVariable(t:Type, varName:String):String = {
    val sb = new StringBuilder
    val tName = typeName(t)
    sb.append(s"$tName $varName")
    t match {
      case t:ArrayType => sb.append(s"[${t.len}]")
      case _ =>
    }
    sb.append(";\n")
    sb.toString
  }

  def readSimple(t:Type, varName:String) = {
    val init = s"$varName = ${getData(typeName(t))};\n${advanceData(typeName(t))};\n"
    declareVariable(t,varName) ++ init
  }

  def writeSimple(t:Type, varName:String):String = {
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

  def readTuple(tupleType: TupleType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//generating tuple type ${tupleType.toString}\n")
    sb.append(declareVariable(tupleType,varName))
    sb.append("{\n")
    for(i <- 0 to tupleType.elemsT.size-1) {
      val t = tupleType.elemsT(i)
      sb.append(readInputVariable(t,s"tuple_temp_$i"))
      sb.append(s"$varName._$i = tuple_temp_$i;\n")
    }
    sb.append("}\n")
    sb.toString
  }

  def writeTuple(t:TupleType, varName:String):String = {
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

  def readArray(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    val arrayTypeName = typeName(t)
    val elemTypeName = typeName(t.elemT)
    sb.append(s"//generating array of type $arrayTypeName\n")
    sb.append(getSizeFromData)
    sb.append(declareVariable(t,varName))
    sb.append(s"for(int i = 0; i < arrCount; i++){\n")
    sb.append(readInputVariable(t.elemT,"temp"))
    sb.append(s"$varName[i] = temp;\n")
    sb.append("}\n")
    sb.toString
  }

  def writeArray(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//Outputting array of type ${typeName(t)}\n")
    sb.append("printf(\"%d \"," ++  s"${t.len});\n")
    sb.append(s"for(int i = 0; i < ${t.len};i++{\n")
    sb.append(writeVariable(t.elemT,s"$varName[i]"))
    sb.append("}\n")
    sb.toString
  }

  def getSizeFromData = s"arrCount = ${getData("int")};${advanceData("int")};\n"
  def getData(typeName:String) = s"(($typeName*)data)[0]"
  def advanceData(typeName: String) = s"data += sizeof($typeName)"
  def getDataAndAdvance(typeName: String) = getData(typeName) ++ ";" ++ advanceData(typeName) ++ ";"
  def separator = "printf(\" \");\n"


  def generateInvocationCode(kernel:Lambda):String = {
    val sb = new StringBuilder()
    sb.append("//main invocation\n")
    sb.append(declareVariable(kernel.body.t,"output"))
    val paramList = (kernel.params.foldLeft("")((x,y) => x ++ ", " ++ y.toString) ++ ", output").substring(1)
    sb.append(s"liftKernel($paramList);\n")
    sb.append(writeVariable(kernel.body.t, "output"))
    sb.toString
  }

  def typeName(t: Type):String = t match {
    case Float => "float"
    case Int => "int"
    case t:TupleType => t.elemsT.foldLeft("Tuple")((x,y) => s"${x}_${typeName(y)}")
    case t:ArrayType => s"${typeName(t.elemT)}*"
    case t => throw new Exception("Unsupported type " ++ t.toString)
  }

  def main(args:Array[String]) = {
    val f = fun(
      ArrayType(TupleType(Float,Float),100),
      A => {
        MapSeq(add) $ A
      })
    val f2 = fun (
      ArrayType(Float, SizeVar("N")),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, init) $ in
      })
    println(Harness(OMPGenerator,f))
  }
}
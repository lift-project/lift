package openmp.executor

import apart.arithmetic.?
import c.generator.CGenerator
import ir.{ArrayType, TupleType, Type, TypeChecker}
import ir.ast.{Lambda, Param, fun}
import opencl.ir._
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
    stringBuilder.append("TODO: Properly load data here. From file? From Command line?")
    kernel.params.foreach {x => stringBuilder.append(generateParameterCode(x))}
    stringBuilder.append(generateInvocationCode(kernel))
    stringBuilder.append("TODO: Reverse the original protocol to return data to the caller!!")
    stringBuilder.append("\n}")
    stringBuilder.toString()
  }

  def generateParameterCode(param:Param):String =s"//code for parameter $param\n" ++ readInputVariable(param.t, param.toString)

  def readInputVariable(t:Type, param :String) = {
    val sb = new StringBuilder
    val str = t match {
      case Float => generateSimpleParam(typeName(t), param)
      case Int => generateSimpleParam(typeName(t), param)
      case t:TupleType => generateTupleType(t,param)
      case t:ArrayType => generateArrayParam(t,param)
      case t => throw new Exception("Unsupported type " ++ t.toString)
    }
    sb.append(str)
    sb.toString
  }

  def generateSimpleParam(typeName:String, varName:String) = {
    s"$typeName $varName = ${getData(typeName)};\n${advanceData(typeName)};\n"
  }

  def generateTupleType(tupleType: TupleType, varName:String):String = {
    val sb = new StringBuilder
    val tupleTypeName = typeName(tupleType)
    sb.append(s"//generating tuple type ${tupleType.toString}\n")
    sb.append(s"$tupleTypeName $varName;\n")
    sb.append("{\n")
    for(i <- 0 to tupleType.elemsT.size-1) {
      val t = tupleType.elemsT(i)
      sb.append(readInputVariable(t,s"tuple_temp_$i"))
      sb.append(s"$varName._$i = tuple_temp_$i;\n")
    }
    sb.append("}\n")
    sb.toString
  }

  def generateArrayParam(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    val arrayTypeName = typeName(t)
    val elemTypeName = typeName(t.elemT)
    sb.append(s"//generating array of type $arrayTypeName\n")
    sb.append(getSizeFromData)
    sb.append(s"$arrayTypeName $varName[${t.len}];\n") // = malloc(arrCount * sizeof($elemTypeName));\n")
    sb.append(s"for(int i = 0; i < arrCount; i++){\n")
    sb.append(readInputVariable(t.elemT,"temp"))
    sb.append(s"$varName[i] = temp;\n")
    sb.append("}\n")
    sb.toString
  }

  def getSizeFromData = s"arrCount = ${getData("int")};${advanceData("int")};\n"
  def getData(typeName:String) = s"(($typeName*)data)[0]"
  def advanceData(typeName: String) = s"data += sizeof($typeName)"
  def getDataAndAdvance(typeName: String) = getData(typeName) ++ ";" ++ advanceData(typeName) ++ ";"

  def generateInvocationCode(kernel:Lambda):String = {
    val sb = new StringBuilder()
    sb.append("//main invocation\n")
    val paramList = kernel.params.foldLeft("")((x,y) => x ++ "," ++ y.toString).substring(1)
    sb.append(s"liftKernel($paramList);\n")
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
        MapPar(add) $ A
      })
    println(Harness(OMPGenerator,f))
  }
}
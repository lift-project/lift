package openmp.executor

import lift.arithmetic.?
import c.generator.CAst.ParamDecl
import c.generator.{CAst, CGenerator}
import ir.{ArrayType, TupleType, Type, TypeChecker}
import ir.ast.{Lambda, Param}
import opencl.generator.NDRange
import opencl.ir._


/**
  * Created by Federico on 05-Jul-16.
  */
object Harness {

  private var counter = 0

  case class GenerationOption(val timing:Boolean, val ouput:Boolean, val debug:Boolean)
  private val defaultOpt = GenerationOption(true,true,false)

  def apply(gen:CGenerator, kernel:Lambda, opt:GenerationOption = defaultOpt):String = {
    val cstring = this.generate(gen,kernel, opt)
    //Must now strip comments because the can screw up with openmp
    cstring
  }

  def generate(gen:CGenerator, kernel: Lambda, opt:GenerationOption = defaultOpt):String = {
    this.counter = 0
    TypeChecker.check(kernel.body)
    val kernelSource = gen.generate(kernel,NDRange(?,?,?),NDRange(?,?,?), Map())
    println(kernelSource)
    //This function is the actual C function, rather then lift function. We need it because the
    //parameters of this function who are not in the kernel proper need to be allocated nevertheless.
    //To obtain this information, for now, simply re-run the internal generator function which produced the
    //kernel in the first place. Call this function the cKernel.
    val cKernel = gen.generateKernel(kernel).asInstanceOf[CAst.Function]
    val mainMethod = generateMainMethod(kernel, cKernel, opt)
    harnessIncludes ++ kernelSource ++ "\n" ++ mainMethod
  }

  private def generateMainMethod(kernel:Lambda, cKernel:CAst.Function, opt:GenerationOption):String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append("//Auto-generated runtime harness\n")
    stringBuilder.append(readFileCode)
    stringBuilder.append("int main(int argc, char** argv) {\nint arrCount;\n")
    //The code that loads the actual lift program parameters (those of the lambda)
    stringBuilder.append("char* inputData = strtok(readFile(),\"@\");\n")
    kernel.params.foreach {x => stringBuilder.append(generateParameterCode(x))}
    //The code that allocates the intermediate arrays.
    //These are going to be those parameters which come after the proper parameters, minus the last one
    //which is the final output parameter
    val intermediateParameters = cKernel.params.slice(kernel.params.size,cKernel.params.size-1)
    if(intermediateParameters.size > 0) {
      stringBuilder.append("//Code for intermediate parameters\n")
      intermediateParameters.foreach { x => stringBuilder.append(generateIntermediateParameterAllocation(x)) }
    }
    //The invocation line
    stringBuilder.append(generateInvocationCode(kernel, intermediateParameters.map(_.name),opt))
    stringBuilder.append("}")
    stringBuilder.toString()
  }

  private def harnessIncludes = List("stdio.h","string.h", "sys/types.h", "sys/time.h", "malloc.h", "math.h")
    .map(x => s"#include <$x>").reduce(_ ++ "\n" ++ _) ++ "\n"


  private val readFileCode =
    """
      |char *readFile() {
      |  FILE* f = fopen("data.txt", "rb");
      |  fseek(f, 0, SEEK_END);
      |  long length = ftell(f);
      |  fseek(f, 0, SEEK_SET);
      |  char* buffer = malloc(length);
      |  if(buffer) {
      |    fread(buffer, 1, length, f);
      |  }
      |  fclose(f);
      |  return buffer;
      |}
    """.stripMargin

  private def generateParameterCode(param:Param):String =s"//code for parameter $param\n" ++ declareVariable(param.t, param.toString) ++  scanInput(param.t, param.toString)

  private def generateIntermediateParameterAllocation(param:ParamDecl) = {
    if(!param.t.isInstanceOf[ArrayType])
      throw new Exception(s"Intermediate collection parameter ${param} is not of an array type: what is a poor compiler to do?")
    declareVariable(param.t,param.name)
  }

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
      case t:ArrayType => sb.append(declareArray(t, varName))//sb.append(s"${typeName(t.elemT)} *$varName = malloc(sizeof(${typeName(t.elemT)}) * ${t.len})")
      case x => sb.append(s"$tName $varName")
    }
    sb.append(";\n")
    sb.toString
  }

  private def declareArray(t:ArrayType, varName:String) = {
    val context = ArrayContext(varName,t)
    val elemT = typeName(context.groundType)
    val size = context.totalSize
    s"$elemT *$varName = malloc(sizeof($elemT) * $size)"
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

  private def scanArray(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//geerating array of type ${typeName(t.elemT)}\n")
    val context = ArrayContext(varName,t)
    val body = scanInput(context.groundType, context.accessFormula)
    sb.append(context.embed(body))
    sb.toString
  }

  private def writeSimple(t:Type, varName:String):String = {
    val pattern = t match {
      case Int => "%d"
      case Float => "%f"
      case _ => throw new Exception(s"Cannot output ${typeName(t)}, it is not a simple type")
    }
    cprintf(pattern, List(varName))
  }

  private def writeTuple(t:TupleType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//Outputting tuple of type ${typeName(t)}\n")
    sb.append(cprintf("["))
    for(i <- 0 to t.elemsT.size-1) {
      sb.append(writeVariable(t.elemsT(i),s"${varName}._$i"))
      if(i < t.elemsT.size-1) {
        sb.append(cprintf(","))
      }
    }
    sb.append(cprintf("]"))
    sb.toString
  }


  private def writeArray(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//Outputting array of type ${typeName(t)}\n")
    val context = ArrayContext(varName,t)
    val bb = new StringBuilder
    bb.append(writeVariable(context.groundType, context.accessFormula))
    bb.append(cprintf(","))

    sb.append(context.wrap(bb.toString, cprintf("["), cprintf("]")))
    sb.toString
  }

  //Represents a ForLoop's iteration variable and limit
  private case class CFor(limit:String) {
    val i = "i" ++ freshCount.toString
    override def toString: String = s"for(unsigned long $i = 0; $i < $limit; $i++){\n"
  }

  //An abstraction operations that have to execute within an array, independently of the
  //dimension
  private case class ArrayContext(varName:String,t:ArrayType) {
    val dimensions = arrayDimensions(t, List())
    val groundType = arrayGroundType(t)
    val totalSize = dimensions.map(_.limit).reduce(_ ++ " * " ++ _)

    private def arrayDimensions(t:ArrayType, accum:List[CFor]):List[CFor] = t.elemT match {
      case inner:ArrayType => arrayDimensions(inner, accum ++ List(CFor(t.asInstanceOf[Size].size.toString)))
      case _ => accum ++List(CFor(t.asInstanceOf[Size].size.toString))
    }

    private def arrayGroundType(t:ArrayType):Type = t.elemT match {
      case inner:ArrayType => arrayGroundType(inner)
      case x => x
    }

    //Embeds a piece of code within the loops level of this array
    def embed(str:String):String = wrap(str,"","")

    //Same as embed, only with prologue and epilogue after each iteration but the innermost
    def wrap(str:String, prologue:String, epilogue:String) = {
      val sb = new StringBuilder
      dimensions.foreach{x => sb.append(prologue);sb.append(x)}
      val t = sb.toString
      sb.append(str)
      dimensions.foreach{_ => sb.append("}\n");sb.append(epilogue)}
      sb.toString()
    }

    def accessFormula:String = {
      //For each dimension, generate triple name, size and limit
      val index = dimensions
        .zip(computeDimensionStride(dimensions,totalSize))
        .map{case(dim,stride) => s"(${dim.i} * $stride)"}
        .reduce(_ ++ " + " ++ _)
      s"$varName[$index]"
    }

    private def computeDimensionStride(dimensions:List[CFor], memorySize:String):List[String] = dimensions match {
      case Nil => Nil
      case x::xs => {
        val stride = s"$memorySize/${x.limit}"
        stride::computeDimensionStride(xs, stride)
      }
    }
  }

  private def cprintf(pattern:String):String = "printf(\"" ++ pattern ++ "\");\n"
  private def cprintf(pattern:String, vals:List[String]):String = {
    val valString = vals.reduce((x,y) => x ++ ", " ++ y)
    "printf(\"" ++ pattern ++ "\"," ++ valString ++ ");\n"
  }

  private def getData(typeName:String) = s"(($typeName*)data)[0]"
  private def advanceData(typeName: String) = s"data += sizeof($typeName)"

  private def freshCount = {
    counter = counter + 1
    counter - 1
  }


  private def generateInvocationCode(kernel:Lambda, intermediateParameterNames:List[String], opt:GenerationOption):String = {
    val sb = new StringBuilder()
    sb.append("//main invocation\n")
    sb.append(declareVariable(kernel.body.t,"output"))
    val paramNames = kernel.params.map(x => x.toString) ++ intermediateParameterNames
    val paramList = (paramNames.foldLeft("")((x,y) => x ++ ", " ++ y.toString) ++ ", output").substring(1)
    if(opt.debug) {
      sb.append(cprintf("Calling liftKernel...\\n"))
    }
    if(opt.timing) {
      sb.append("struct timeval startTime;\n")
      sb.append("gettimeofday(&startTime, NULL);\n")
    }
    sb.append(s"liftKernel($paramList);\n")
    if(opt.timing) {
      sb.append("struct timeval endTime;")
      sb.append("gettimeofday(&endTime, NULL);\n")
      sb.append("float totalTime = ((endTime.tv_sec * 1e6 + endTime.tv_usec) - (startTime.tv_sec * 1e6 + startTime.tv_usec))/1000;\n")
    }
    if(opt.debug) {
      sb.append(cprintf("liftKernel returned...\\n"))
    }
    if(opt.ouput) {
      sb.append(writeVariable(kernel.body.t, "output"))
    }
    sb.append(cprintf("\\n"))
    if(opt.timing) {
      sb.append(cprintf("Elapsed time = %f\\n", List("totalTime")))
    }
    sb.toString
  }

  private def typeName(t: Type):String = t match {
    case Float => "float"
    case Int => "int"
    case t:TupleType => t.elemsT.foldLeft("Tuple")((x,y) => s"${x}_${typeName(y)}")
    case t:ArrayType =>  {
      if(!t.isInstanceOf[ArrayType])
        s"${typeName(t.elemT)}*"
      else
        //If nested array, just forward deeper
        typeName(t.elemT)
    }
    case t => throw new Exception("Unsupported type " ++ t.toString)
  }
}
package openmp.executor

import apart.arithmetic.{?, SizeVar}
import c.generator.CAst.{ForLoop, ParamDecl}
import c.generator.{CAst, CGenerator}
import ir.{ArrayType, TupleType, Type, TypeChecker}
import ir.ast.{Lambda, Param, UserFun, fun}
import opencl.ir._
import opencl.ir.pattern.{MapSeq, ReduceSeq, toGlobal}
import openmp.generator.OMPGenerator
import openmp.ir.pattern.{:+, ReducePar}


/**
  * Created by Federico on 05-Jul-16.
  */
object Harness {

  private var counter = 0

  def apply(gen:CGenerator, kernel:Lambda):String = {
    val cstring = this.generate(gen,kernel)
    //Must now strip comments because the can screw up with openmp
    cstring
  }

  def generate(gen:CGenerator, kernel: Lambda):String = {
    this.counter = 0
    TypeChecker.check(kernel.body)
    val kernelSource = gen.generate(kernel,Array(?,?,?),Array(?,?,?), Map())
    //This function is the actual C function, rather then lift function. We need it because the
    //parameters of this function who are not in the kernel proper need to be allocated nevertheless.
    //To obtain this information, for now, simply re-run the internal generator function which produced the
    //kernel in the first place. Call this function the cKernel.
    val cKernel = gen.generateKernel(kernel).asInstanceOf[CAst.Function]
    val mainMethod = generateMainMethod(kernel, cKernel)
    kernelSource ++ "\n" ++ mainMethod
  }

  private def generateMainMethod(kernel:Lambda, cKernel:CAst.Function):String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append("//Auto-generated runtime harness\n")
    stringBuilder.append(harnessIncludes)
    stringBuilder.append("int main(int argc, char** argv) {\nint arrCount;\n")
    //The code that loads the actual lift program parameters (those of the lambda)
    stringBuilder.append("char* inputData = strtok(argv[1],\"@\");\n")
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
    stringBuilder.append(generateInvocationCode(kernel, intermediateParameters.map(_.name)))
    stringBuilder.append("}")
    stringBuilder.toString()
  }

  private def harnessIncludes = List("stdio.h","string.h", "sys/types.h", "sys/time.h", "malloc.h")
    .map(x => s"#include <$x>").reduce(_ ++ "\n" ++ _) ++ "\n"

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

  private def writeArray2(t:ArrayType, varName:String):String = {
    val sb = new StringBuilder
    sb.append(s"//Outputting array of type ${typeName(t)}\n")
    //sb.append("printf(\"%d \"," ++  s"${t.len});\n")
    sb.append(cprintf("["))
    val forLoop = CFor(t.len.toString)
    sb.append(forLoop.toString)
    sb.append(writeVariable(t.elemT,s"$varName[${forLoop.i}]"))
    sb.append(s"if(${forLoop.i} < ${t.len}-1)")
    sb.append(cprintf(","))
    sb.append("}\n")
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
    override def toString: String = s"for(int $i = 0; $i < $limit; $i++){\n"
  }

  //An abstraction operations that have to execute within an array, independently of the
  //dimension
  private case class ArrayContext(varName:String,t:ArrayType) {
    val dimensions = arrayDimensions(t, List())
    val groundType = arrayGroundType(t)
    val totalSize = dimensions.map(_.limit).reduce(_ ++ " * " ++ _)

    private def arrayDimensions(t:ArrayType, accum:List[CFor]):List[CFor] = t.elemT match {
      case inner:ArrayType => arrayDimensions(inner, accum ++ List(CFor(t.len.toString)))
      case _ => accum ++List(CFor(t.len.toString))
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
      dimensions.foreach{sb.append(prologue);x => sb.append(x)}
      sb.append(str)
      dimensions.foreach{_ => sb.append("}\n");sb.append(epilogue)}
      sb.toString()
    }

    def accessFormula:String = {
      //Pair up the dimension variable names with the size of the following dimension,
      //using a null for the last dimension as a size is missing
      val names = dimensions.map(_.i)
      val sizes = dimensions.map(_.limit).take(dimensions.length-1) ++ List(null)
      val bundled = names.zip(sizes)
      //Now, for each pair, generate something of the form(size * variable) and then reduce with +
      val index = bundled.map{
        case (x,null) => x
        case (x,y) => s"($y * $x)"
      }.reduce(_ ++ " + " ++ _)
      s"$varName[$index]"
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


  private def generateInvocationCode(kernel:Lambda, intermediateParameterNames:List[String]):String = {
    val sb = new StringBuilder()
    sb.append("//main invocation\n")
    sb.append(declareVariable(kernel.body.t,"output"))
    val paramNames = kernel.params.map(x => x.toString) ++ intermediateParameterNames
    val paramList = (paramNames.foldLeft("")((x,y) => x ++ ", " ++ y.toString) ++ ", output").substring(1)
    sb.append("struct timeval tv;\n")
    sb.append("gettimeofday(&tv, NULL);\n")
    sb.append("suseconds_t startTime = tv.tv_usec;\n")
    sb.append(s"liftKernel($paramList);\n")
    sb.append("gettimeofday(&tv, NULL);\n")
    sb.append("suseconds_t endTime = tv.tv_usec;\n")
    sb.append("suseconds_t totalTime = endTime - startTime;\n")
    sb.append(writeVariable(kernel.body.t, "output"))
    sb.append(cprintf("\\n"))
    sb.append(cprintf("Elapsed time = %d\\n",List("totalTime")))
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

  def main(args:Array[String]) = {
    def genID(t:Type) = UserFun("id","x", "return x;",t,t)
    def increment = UserFun("increment", "x", "return x + 1", Float,Float)
    val f = fun(
      ArrayType(TupleType(Float,Float),2),
      A => {
        MapSeq(genID(TupleType(Float,Float))) $ A
      })
    val f2 = fun (
      ArrayType(Float, 100),
      Float,
      (in,init) => {
        toGlobal(MapSeq(id)) o ReduceSeq(add, init) o MapSeq(increment) o MapSeq(increment)  $ in
      })
    val reducePar = fun(
      ArrayType(Float, SizeVar("N")),
      Float,
      (in, init) => {
        toGlobal(MapSeq(id)) o ReducePar(:+(Float), init) $ in
      }
    )
    val trivial = fun(Float, x => toGlobal(id) $ x)
    println(Harness(OMPGenerator,reducePar))
  }
}
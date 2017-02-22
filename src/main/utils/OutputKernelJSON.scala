package utils

import java.io.PrintWriter

import ir.ast.Lambda

import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.util.parsing.json._
import java.io._
import scala.io._

import opencl.executor.Compile
import opencl.ir.TypedOpenCLMemory

/**
  *  apply: output lambda parameters to JSON file and outputs resulting OpenCL kernel to CL file.
  *  Use in conjunction with $LIFT_ROOT/scripts/createKernelParametersFromJSON.py to create the necessary boilerplate C/OpenCL code from the JSON file.
  *
  *  getJSON: just return the json string for a lambda
  *
  *  getKernelParamStr: string of prefix for parameter values (this really shouldn't be hardcoded but where this gets built is obscured away to oblivion)
 */

object OutputKernelJSON {

  def apply(lambda: Lambda, outputDir: String, jsonfilename: String = "kernel.json", kernelfilename: String = "liftkernel.cl", printJson: Boolean = false) = (new OutputKernelJSON(outputDir,jsonfilename,kernelfilename,printJson))(lambda)

  def getJsonString(lambda:Lambda) = {

    val source = Compile(lambda)
    convertKernelParameterstoJSON(lambda,source)

  }

  val kernelParamNameString = "v__"


  private def getPrivateParamsList(params: Seq[TypedOpenCLMemory]): Seq[String] =
  {
    val memParams = params.map(p => p.mem)
    val privateParams = memParams.filter( p => p.addressSpace.toString.contains("private"))
    privateParams.map( b => b.variable.toString)
  }

  private def getParamSizeMap(params: Seq[TypedOpenCLMemory]): ListMap[String,String] =
  {
    var lmPSizes = ListMap[String,String]()
    val memParams = params.map(p => p.mem)
    memParams.foreach(x => lmPSizes += (x.variable.toString -> x.size.toString))
    lmPSizes
  }

  private def getGeneralValuesFromParameterList(parameters: Array[String], lmPSizes: ListMap[String,String]): Array[String] =
  {
    parameters.filter(x => (!x.contains("*") && !lmPSizes.contains(x.trim().split(" ")(1).trim())))
  }

  private def getParameterValuesFromParameterList(parameters: Array[String], privateNameParams: Seq[String]): Array[String] =
  {
    parameters.filter(x => x.contains("restrict") || privateNameParams.exists(y => x.contains(y)))
  }

  private def getOtherValuesFromParameterList(parameters: Array[String], lmPSizes: ListMap[String,String], privateNameParams: Seq[String]): Array[String] =
  {
    val paramVals = getParameterValuesFromParameterList(parameters,privateNameParams)
    val generalVals = getGeneralValuesFromParameterList(parameters,lmPSizes)
    parameters.filter(x => !paramVals.contains(x) && !generalVals.contains(x))
  }

  private def getSizeKernelValuesMap(parameters: Array[String], lmPSizes: ListMap[String,String], toStrip: String): ListMap[String,String] =
  {
    var lmS = ListMap[String,String]()
    val generalVals = getGeneralValuesFromParameterList(parameters,lmPSizes)
    val genVals = generalVals.map(x => x.trim.stripSuffix(toStrip))
    genVals.foreach(x => lmS += (x -> "4"))
    lmS
  }

  private def getParameterKernelValuesMap(parameters: Array[String], lmPSizes: ListMap[String,String], privateNameParams: Seq[String]): ListMap[String,String] =
  {
    var lmP = ListMap[String,String]()
    val notArr =Array[String]("const","global","restrict") // parameter decorations we don't want
    val paramVals = getParameterValuesFromParameterList(parameters,privateNameParams)
    val paramNames = paramVals.map(x => x.split(" ").filter(y => y contains kernelParamNameString)).flatten
    val paramTypes = paramVals.map(x => x.split(" ").filter(y => !notArr.contains(y) && !paramNames.contains(y))).map(z => z.mkString(""))
    for((pType,pName) <- paramTypes zip paramNames ) yield lmP +=((pType.toString()+" "+pName.toString()) -> lmPSizes(pName.toString()))
    lmP
  }

  private def getOutputKernelValuesMap(parameters: Array[String], lmPSizes: ListMap[String,String], privateNameParams: Seq[String]): ListMap[String,String] =
  {
    var lmO = ListMap[String,String]()
    val others = getOtherValuesFromParameterList(parameters,lmPSizes,privateNameParams)
    val outputs = others.slice(0,1)
    val otherTypes = outputs.map(x => x.split(" ").filter(y => y contains "*")).flatten
    val otherNames = outputs.map(x => x.split(" ").filter(y => y contains kernelParamNameString)).flatten
    for((oType,oName) <- otherTypes zip otherNames ) yield lmO +=((oType.toString()+" "+oName.toString()) -> lmPSizes(oName.toString()))
    lmO
  }

  private def getTempBufferKernelValuesMap(parameters: Array[String], lmPSizes: ListMap[String,String], privateNameParams: Seq[String], toStrip: String): ListMap[String,String] =
  {
    var lmTB = ListMap[String,String]()
    val others = getOtherValuesFromParameterList(parameters,lmPSizes,privateNameParams)
    val tmpBuffers = others.slice(1,others.length)
    val tmpBTypes = tmpBuffers.map(x => x.split(" ").filter(y => y contains "*")).flatten
    val tmpBNames = tmpBuffers.map(x => x.split(" ").filter(y => y contains kernelParamNameString)).flatten
    for((tbType,tbName) <- tmpBTypes zip tmpBNames ) yield lmTB +=((tbType.toString()+" "+tbName.stripSuffix(toStrip).toString()) -> lmPSizes(tbName.stripSuffix(toStrip).toString()))
    lmTB
  }

  /**
    * be wary of changing the types of Map and JSON parsing in this function as the current setup maintains order which is crucial
    * for creating the correct kernel (JSONs do not promise to retain order!)
    *
    * for use with "writeKernelJSONToFile", which is why the source string is also passed in (to print out the kernel, too)
    */
  private def convertKernelParameterstoJSON( lambda: Lambda, source: String): String =
  {
    val params = TypedOpenCLMemory.get(lambda.body, lambda.params, includePrivate = false)  // pull out parameters with sizes
    val ignoreable = ", \t({}"
    val toStrip = "){"

    // get all constant parameters to ignore
    val privateNameParams = getPrivateParamsList(params)
    // map variable names to sizes
    val lmPSizes = getParamSizeMap(params)
    var lm = ListMap[String,JSONObject]()

    // pull out values from kernel string to get c types
    val kernelStr = source.split("\n").filter(x => x.toString().contains("kernel"))
    val parameters = kernelStr(0).split(",").map(x => x.stripPrefix("kernel void KERNEL("))

    // get size values (ints)
    val lmS = getSizeKernelValuesMap(parameters,lmPSizes,toStrip)//ListMap[String,String]()

    // get parameter values
    val lmP = getParameterKernelValuesMap(parameters,lmPSizes,privateNameParams)

    // get output value
    val lmO = getOutputKernelValuesMap(parameters,lmPSizes,privateNameParams)

    // get temp buffer values
    val lmTB = getTempBufferKernelValuesMap(parameters,lmPSizes,privateNameParams,toStrip)

    // converge to megamap
    lm+=("parameters" -> JSONObject(lmP))
    lm+=("outputs" -> JSONObject(lmO))
    lm+=("temporary buffers" -> JSONObject(lmTB))
    lm+=("sizes" -> JSONObject(lmS))

    // convert megamap json object
    JSONObject(lm).toString()
  }

}

class OutputKernelJSON(outputDir: String, jsonfilename: String, kernelfilename: String, printJson: Boolean) {

  def apply(lambda: Lambda) = {
    writeKernelJSONToFile(lambda,outputDir)
  }

  def writeKernelJSONToFile(lambda: Lambda, outputDir: String) =
  {

    val source = Compile(lambda)

    val jsonString = OutputKernelJSON.convertKernelParameterstoJSON(lambda, source)

    if(printJson) println(jsonString)

    writeStringToFile(jsonString,outputDir+jsonfilename)

    writeStringToFile(source,outputDir+kernelfilename)
  }

  private def writeStringToFile(str: String, outputFile: String): Unit =
  {
    val out = new File(outputFile)
    out.createNewFile()
    val pw = new PrintWriter(out)
    pw.write(str)
    pw.close
  }

}

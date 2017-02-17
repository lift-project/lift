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
  *  getJSON: just return the json string for a lambda
 */

object OutputKernelJSON {

  def apply(lambda: Lambda, outputDir: String, jsonfilename: String = "kernel.json", kernelfilename: String = "liftstencil.cl", printJson: Boolean = false) = (new OutputKernelJSON(outputDir,jsonfilename,kernelfilename,printJson))(lambda)

  def getJSONString(lambda: Lambda) = (new OutputKernelJSON("")).getJson(lambda)

  def getKernelParamStr() = (new OutputKernelJSON("")).getKernelParamStr()

}

class OutputKernelJSON(outputDir: String, jsonfilename: String = "kernel.json", kernelfilename: String = "liftstencil.cl", printJson: Boolean = false) {

  def apply(lambda: Lambda) = {
    writeKernelJSONToFile(lambda,outputDir,jsonfilename,kernelfilename,printJson)
  }

  def getJson(lambda:Lambda) = {

    val source = Compile(lambda)
    convertKernelParameterstoJSON(lambda,source)

  }

  def getKernelParamStr() =
  {
    "v__"
  }

  def writeKernelJSONToFile(lambda: Lambda, outputDir: String, jsonfilename: String = "kernel.json", kernelfilename: String = "liftstencil.cl", printJson: Boolean = false) =
  {

    val source = Compile(lambda)

    val jsonString = convertKernelParameterstoJSON(lambda, source)

    if(printJson) println(jsonString)

    // write to file
    writeStringToFile(jsonString,outputDir+jsonfilename)

    // print kernel to same place (pass in param!)
    writeStringToFile(source,outputDir+kernelfilename)
  }

  // be wary of changing the types of Map and JSON parsing in this function as the current setup maintains order which is crucial
  // for creating the correct kernel (JSONs do not promise to retain order!)
  //
  // for use with "writeKernelJSONToFile", which is why the source string is also passed in (to print out the kernel, too)
  // ...this could be changed!
  def convertKernelParameterstoJSON( lambda: Lambda, source: String): String =
  {

    val kernelValStr = getKernelParamStr() // for finding parameter names -- ideally should not be hardcoded !
    val params = TypedOpenCLMemory.get(lambda.body, lambda.params, includePrivate = false)  // pull out parameters with sizes

    // setup maps
    var lm = ListMap[String,JSONObject]()
    var lmPSizes = ListMap[String,String]()  // map for sizes of parameters
    var lmP = ListMap[String,String]()
    var lmO = ListMap[String,String]()
    var lmTB = ListMap[String,String]()
    var lmS = ListMap[String,String]()

    var newParams = params.toString().stripPrefix("ArrayBuffer(").split(",").filter(x => x.contains("global") || (x.contains("private") && !x.contains("*")))
    val ignoreable = ", \t({}" // trim some stuff
    val toStrip = "){" // trim some more stuff
    val notArr =Array[String]("const","global","restrict") // parameter decorations we don't want
    val stripParamsArr = newParams.map(x => x.split(":")(0).dropWhile(c => ignoreable.indexOf(c) >= 0).stripSuffix("}").split(";"))
    val stripParams = newParams.map(x => x.split(":")(0).dropWhile(c => ignoreable.indexOf(c) >= 0).stripSuffix("}").split(";").filter(x => !x.contains(" global") && !x.contains(" private")))

    // store sizes for parameters we have
    stripParams.foreach(x => lmPSizes += (x(0) -> x(1)))

    // pull out values from kernel string because it's easier to work with
    val kernelStr = source.split("\n").filter(x => x.toString().contains("kernel"))
    val parameters = kernelStr(0).split(",").map(x => x.stripPrefix("kernel void KERNEL(")) // pull out ALL parameters, including sizes

    // get size values (ints)
    val generalVals = parameters.filter(x => (!x.contains("*") && !lmPSizes.contains(x.trim().split(" ")(1).trim())))
    val genVals = generalVals.map(x => x.trim.stripSuffix(toStrip))
    genVals.foreach(x => lmS += (x -> ""))

    // get parameter values
    val privateValueNames = stripParamsArr.filter(x => x.mkString(" ").contains("private")).map(x => x(0))
    val paramVals = parameters.filter(x => x.contains("restrict") || privateValueNames.exists(y => x.contains(y))) // pull out the arrays

    val paramNames = paramVals.map(x => x.split(" ").filter(y => y contains kernelValStr)).flatten
    val paramTypes = paramVals.map(x => x.split(" ").filter(y => !notArr.contains(y) && !paramNames.contains(y))).map(z => z.mkString(""))
    // then add in size from lmPSizes!!
    for((pType,pName) <- paramTypes zip paramNames ) yield lmP +=((pType.toString()+" "+pName.toString()) -> lmPSizes(pName.toString()))

    // get output value
    val others = parameters.filter(x => !paramVals.contains(x) && !generalVals.contains(x))
    val outputs = others.slice(0,1)
    val otherTypes = outputs.map(x => x.split(" ").filter(y => y contains "*")).flatten
    val otherNames = outputs.map(x => x.split(" ").filter(y => y contains kernelValStr)).flatten
    // then add in size from lmPSizes!!
    for((oType,oName) <- otherTypes zip otherNames ) yield lmO +=((oType.toString()+" "+oName.toString()) -> lmPSizes(oName.toString()))

    // get temp buffer values
    val tmpBuffers = others.slice(1,others.length)
    val tmpBTypes = tmpBuffers.map(x => x.split(" ").filter(y => y contains "*")).flatten
    val tmpBNames = tmpBuffers.map(x => x.split(" ").filter(y => y contains kernelValStr)).flatten
    // then add in size from lmPSizes!!
    for((tbType,tbName) <- tmpBTypes zip tmpBNames ) yield lmTB +=((tbType.toString()+" "+tbName.stripSuffix(toStrip).toString()) -> lmPSizes(tbName.stripSuffix(toStrip).toString()))

    // converge to megamap
    lm+=("parameters" -> JSONObject(lmP))
    lm+=("outputs" -> JSONObject(lmO))
    lm+=("temporary buffers" -> JSONObject(lmTB))
    lm+=("sizes" -> JSONObject(lmS))

    // convert megamap json object
    JSONObject(lm).toString()

  }

  def writeStringToFile(str: String, outputFile: String): Unit =
  {
    val out = new File(outputFile)
    out.createNewFile()
    val pw = new PrintWriter(out)
    pw.write(str)
    pw.close
  }

  def readStringFromFile(inputFile: String): String =
  {
      Source.fromFile(inputFile).mkString
  }
}

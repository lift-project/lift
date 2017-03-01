package prog_gen

import java.io.File

import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import ir.{ArrayType, Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.Eval
import org.clapper.argot.ArgotConverters._
import org.clapper.argot.{ArgotParser, ArgotUsageException}
import play.api.libs.json._
import rewriting.utils.Utils

import scala.sys.process._

object GeneratePrograms {

  private val logger = Logger(this.getClass)

  private val splitFactors = Seq[ArithExpr](64, 128)
  private val inputSizes = Seq[Cst](Cst(512), Cst(1024), Cst(2048))

  private val parser = new ArgotParser("GeneratePrograms")

  private val outputDirectoryFlag = parser.option[String](List("o", "output"), "name.",
    "Store the created lambdas into this folder."
  ) {
    (s, _) =>
      val file = new File(s)
      if (file.exists)
        parser.usage("Output location \"" + s + "\" already exists")
      s
  }

  private val loopNumFlag = parser.option[Int](List("l", "loop-num"), "number",
    "Number of generation loops to run.")

  private val limitNumFlag = parser.option[Int](List("limit-num"), "number",
    "Number of expressions to generate in a loop iteration.")

  private var outputDirectory = "generated_programs"
  private var loopNum = 30
  private var limitNum = 40

  def main(args: Array[String]): Unit = {
    try {
      parser.parse(args)

      logger.info(s"Arguments: ${args.mkString(" ")}")

      outputDirectory = outputDirectoryFlag.value.getOrElse(outputDirectory)
      loopNum = loopNumFlag.value.getOrElse(loopNum)
      limitNum = limitNumFlag.value.getOrElse(limitNum)

      s"mkdir -p $outputDirectory".!

      val programs = generatePrograms

      val concretePrograms = substituteSplitFactors(programs)

      generateAndSaveInputs(concretePrograms)

      savePrograms(concretePrograms)

    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  private def generatePrograms = {
    val generator = new ProgramGenerator(loopNum)
    val programs = generator.generatePrograms()

    logger.info(s"${programs.length} programs generated.")
    programs
  }

  private def savePrograms(concretePrograms: Seq[Lambda]) = {
    val lambdaDirectory = outputDirectory + "/programs"
    val configurationDirectory = outputDirectory + "/configuration"

    concretePrograms.foreach(lambda => try {
      TypeChecker(lambda)

      val vars = lambda.getVarsInParams()

      val sizes = inputSizes.combinations(vars.length)

      val lambdaString = Utils.dumpLambdaToString(lambda)

      val hash = Utils.Sha256Hash(lambdaString)
      val hashPrefix = hash(0) + "/" + hash(1)
      val thisLambdaConf = s"$configurationDirectory/$hashPrefix/$hash"

      Utils.dumpToFile(lambdaString, hash, s"$lambdaDirectory/$hashPrefix")

      sizes.foreach(size => {

        val substitutions = (vars, size).zipped.toSeq.toMap[ArithExpr, ArithExpr]
        val types = lambda.params.map(p => Type.substitute(p.t, substitutions))
        val outputType = Type.substitute(lambda.body.t, substitutions)

        val settings = JsObject(Seq(
          "kernel" -> JsString(hash),
          "inputs" -> JsArray(
            types.map(t => { JsObject(Seq(
              "filename" -> JsString(getTypeFilename(t)),
              "size" -> JsNumber(Type.getSize(t).eval)
            ))})
          ),
          "output" -> JsNumber(Type.getSize(outputType).eval),
          "sizes" -> JsArray(size.map(s => JsNumber(s.eval)))
        ))

        val settingsString = Json.prettyPrint(settings)
        val settingsFilename = Utils.Sha256Hash(settingsString) + ".json"

        Utils.dumpToFile(settingsString, settingsFilename, thisLambdaConf)

        // TODO: Run sequential for output
      })

    } catch {
      case t: Throwable =>
        logger.warn(t.getMessage)
    })
  }

  private def generateAndSaveInputs(concretePrograms: Seq[Lambda]) = {

    val allSizeCombinations = concretePrograms.flatMap(lambda => {
      val vars = lambda.getVarsInParams()

      val sizes = inputSizes.combinations(vars.length)

      val types = lambda.params.map(_.t)

      sizes.flatMap(s => {

        val substitutions = (vars, s).zipped.toSeq.toMap[ArithExpr, ArithExpr]
        types.map(Type.substitute(_, substitutions))

      })

    }).toSet

    val inputGenerator = InputGenerator()

    val allInputs = allSizeCombinations.map(a => (a, inputGenerator(a))).toMap

    logger.info(s"${allInputs.size} unique inputs generated.")

    val generatedInputsDirectory = outputDirectory + "/inputs"

    s"mkdir -p $generatedInputsDirectory".!

    allInputs.foreach(pair => {
      val t = pair._1
      val input = pair._2

      val filename = getTypeFilename(t)

      val inputString = getInputString(input)

      Utils.dumpToFile(inputString, filename, generatedInputsDirectory)
    })

    logger.info(s"Inputs saved.")
  }

  private def substituteSplitFactors(programs: Seq[Lambda]): Seq[Lambda] = {
    val concretePrograms = programs.par.flatMap(substituteSplitFactors).seq

    logger.info(s"${concretePrograms.length} programs with split factors assigned.")
    concretePrograms
  }

  private def substituteSplitFactors(lambda: Lambda): Seq[Lambda] = {

    val nodes = Utils.findTunableNodes(lambda)

    if (nodes.isEmpty)
      return Seq(lambda)

    val toReplace = nodes.map(Utils.extractArithExpr)
    val vars = lambda.getVarsInParams()

    val factory = Eval.getMethod(Utils.dumpLambdaToMethod(lambda))

    val replacementCombinations = splitFactors.combinations(toReplace.length)

    replacementCombinations
      .map(combinations => factory(vars ++ combinations))
      .filter(l => try { TypeChecker(l); true } catch { case _: Throwable => false} )
      .toSeq

  }

  private def getTypeFilename(t: Type): String = {
    t match {
      case ArrayType(elem, len) => len.toString + "_" + getTypeFilename(elem)
      case opencl.ir.Float => "float"
      case _ => throw new NotImplementedError()
    }
  }

  private def getInputString(a: Any): String = {
    a match {
      case f: Float =>
        f.toString
      case af: Array[Float] =>
        af.mkString(" ")
      case aaf: Array[Array[Float]] =>
        aaf.flatten.mkString(" ")
      case aaaf: Array[Array[Array[Float]]] =>
        aaaf.flatten.flatten.mkString(" ")
      case aaaaf: Array[Array[Array[Array[Float]]]] =>
        aaaaf.flatten.flatten.flatten.mkString(" ")

      case _ => throw new NotImplementedError()
    }
  }

}

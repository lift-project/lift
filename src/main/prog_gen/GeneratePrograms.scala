package prog_gen

import java.io.File

import com.typesafe.scalalogging.Logger
import ir.ast.Lambda
import ir.{ArrayType, ArrayTypeWS, Type, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst}
import opencl.executor.Eval
import play.api.libs.json._
import rewriting.utils.{DumpToFile, Utils}
import _root_.utils.CommandLineParser
import scopt.OParser

import scala.sys.process._

object GeneratePrograms {

  private var outputDirectory = "generated_programs"
  private var loopNum = 30
  private var limitNum = 40

  private val logger = Logger(this.getClass)

  private val splitFactors = Seq[ArithExpr](64, 128)
  private[prog_gen] val inputSizes = Seq[Cst](Cst(512), Cst(1024), Cst(2048))

  case class Config(output: File = null,
                    loopNumFlag: Int = loopNum,
                    limitNumFlag: Int = limitNum)

  val builder = OParser.builder[Config]
  var cmdArgs: Option[Config] = None
  val parser = {
    import builder._
    OParser.sequence(
      programName("GeneratePrograms"),

      opt[File]('o', "output").text("Store the created lambdas into this folder.").required()
        .validate(f => if (f.exists) success else failure("File \"" + f.getName + "\" does not exist"))
        .action((arg, c) => c.copy(output = arg)),

      opt[Int]('l', "loop-num").text("Number of generation loops to run.")
        .action((arg, c) => c.copy(loopNumFlag = arg)),

      opt[Int]("limit-num").text("Number of expressions to generate in a loop iteration.")
        .action((arg, c) => c.copy(limitNumFlag = arg)),

      help("help").text("Show this message.")
    )
  }

  def main(args: Array[String]): Unit = {
    cmdArgs = Some(CommandLineParser(parser, args, Config()))

    logger.info(s"Arguments: ${args.mkString(" ")}")

    outputDirectory = cmdArgs.get.output.getName
    loopNum = cmdArgs.get.loopNumFlag
    limitNum = cmdArgs.get.limitNumFlag

    s"mkdir -p $outputDirectory".!

    val programs = generatePrograms

    val concretePrograms = substituteSplitFactors(programs)

    generateAndSaveInputs(concretePrograms)

    savePrograms(concretePrograms)
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

      val sizes = getInputSizeCombinations(vars.length)

      val lambdaString = DumpToFile.dumpLambdaToString(lambda)

      val hash = DumpToFile.Sha256Hash(lambdaString)
      val hashPrefix = hash(0) + "/" + hash(1)
      val thisLambdaConf = s"$configurationDirectory/$hashPrefix/$hash"

      DumpToFile.dumpToFile(lambdaString, hash, s"$lambdaDirectory/$hashPrefix")

      generateConfigurations(sizes, hash, thisLambdaConf, lambda)

    } catch {
      case t: Throwable =>
        logger.warn(t.toString)
    })
  }

  private[prog_gen] def getInputSizeCombinations(numVars: Int) = {

    val length = inputSizes.length
    numVars match {
      case 1 =>
        Seq.tabulate(length)((a) => Seq(inputSizes(a)))
      case 2 =>
        Seq.tabulate(length, length)((a, b) =>
          Seq(inputSizes(a), inputSizes(b))).flatten
      case 3 =>
        Seq.tabulate(length, length, length)((a, b, c) =>
          Seq(inputSizes(a), inputSizes(b), inputSizes(c))).flatten.flatten
      case _ => throw new NotImplementedError()
    }
  }

  private[prog_gen] def generateConfigurations(
                                                sizes: Seq[Seq[Cst]],
                                                hash: String,
                                                thisLambdaConf: String,
                                                lambda: Lambda) = {

    sizes.foreach(size => {

      val substitutions =
        (lambda.getVarsInParams(), size).zipped.toSeq.toMap[ArithExpr, ArithExpr]

      val types = lambda.params.map(p => Type.substitute(p.t, substitutions))
      val outputType = Type.substitute(lambda.body.t, substitutions)

      val settings = JsObject(Seq(
        "kernel" -> JsString(hash),
        "inputs" -> JsArray(
          types.map(t => {
            JsObject(Seq(
              "filename" -> JsString(getTypeFilename(t)),
              "size" -> JsNumber(Type.getAllocatedSize(t).eval)
            ))
          })
        ),
        "output" -> JsNumber(Type.getAllocatedSize(outputType).eval),
        "sizes" -> JsArray(size.map(s => JsNumber(s.eval)))
      ))

      val settingsString = Json.prettyPrint(settings)
      val settingsFilename = DumpToFile.Sha256Hash(settingsString) + ".json"

      DumpToFile.dumpToFile(settingsString, settingsFilename, thisLambdaConf)

      // TODO: Run sequential for output
    })
  }

  private def generateAndSaveInputs(concretePrograms: Seq[Lambda]) = {

    val allSizeCombinations = concretePrograms.flatMap(lambda => {
      val vars = lambda.getVarsInParams()

      val sizes = getInputSizeCombinations(vars.length)

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

      DumpToFile.dumpInputOutputToFile(input, filename, generatedInputsDirectory)
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

    val factory = Eval.getMethod(DumpToFile.dumpLambdaToMethod(lambda))

    val replacementCombinations = splitFactors.combinations(toReplace.length)

    replacementCombinations
      .map(combinations => factory(vars ++ combinations))
      .filter(l => try { TypeChecker(l); true } catch { case _: Throwable => false} )
      .toSeq
  }

  private def getTypeFilename(t: Type): String = {
    t match {
      case ArrayTypeWS(elem, len) => len.toString + "_" + getTypeFilename(elem)
      case opencl.ir.Float => "float"
      case _ => throw new NotImplementedError()
    }
  }

}

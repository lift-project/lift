package rewriting.utils

import java.nio.file.{Files, Paths}
import java.security.MessageDigest

import ir.{ByDeclarationOrder, TypeChecker}
import ir.ast._
import opencl.executor.Execute
import opencl.ir.ast.OpenCLBuiltInFun

import scala.sys.process._

object DumpToFile {

  /**
   * Generate the SHA-256 hash for a string.
   * @param value The string to hash.
   * @return The SHA-256 hash as a string.
   */
  def Sha256Hash(value: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    md.update(value.getBytes("UTF-8"))
    val digest = md.digest()
    f"${new java.math.BigInteger(1, digest)}%064x"
  }

  /**
   * Dump content to a file.
   * If the filename exists, appends the current time.
   *
   * @param content The content to dump.
   * @param filename The filename to use.
   * @param path Path for the file.
   * @return If a new file was created returns true, if the content existed at path/filename returns false.
   */
  def dumpToFile(content: String, filename: String, path: String): Boolean = {
    var uniqueFilename = filename

    ("mkdir -p " + path).!

    if (Files.exists(Paths.get(path + "/" + uniqueFilename))) {
      val warningString = "Warning! Clash at " + uniqueFilename + ".\n"

      val clashingContent = new String(Files.readAllBytes(Paths.get(path + "/" + uniqueFilename)))

      if (clashingContent != content) {
        println(warningString + "Content is different, adding System.currentTimeMillis().")
        uniqueFilename = uniqueFilename + "_" + System.currentTimeMillis()
      } else {
        return false
      }

    }

    scala.tools.nsc.io.File(path + "/" + uniqueFilename).writeAll(content)
    true
  }

  private def dumpLambdaToStringWithoutDecls(lambda: Lambda, printNonFixedVarIds: Boolean = true): String = {
    val userFuns = Expr.visitWithState(Set[UserFun]())(lambda.body, (expr, state) => {
      expr match {
        case FunCall(uf: UserFun, _*) if !uf.isInstanceOf[OpenCLBuiltInFun] => state + uf
        case FunCall(VectorizeUserFun(_, uf:UserFun), _*) => state + uf
        case _ => state
      }
    })

    val userFunString = userFuns.map(ScalaPrinter(printNonFixedVarIds)(_)).mkString("\n") + "\n"

    val types = lambda.params.map(p => ScalaPrinter(printNonFixedVarIds)(p.t)).mkString(", ")
    val expr = ScalaPrinter(printNonFixedVarIds)(lambda)
    val fullString = expr.substring(0, 4) + types + "," + expr.substring(4)

    val param = """p_\d+""".r

    val params = param.findAllMatchIn(fullString).map(_.toString()).toList.distinct

    val lambdaString = params.zipWithIndex.foldRight(fullString)((toReplace, currentString) =>
      currentString.replaceAll(toReplace._1, "p_" + toReplace._2))

    userFunString + lambdaString
  }

  /**
   * Dumps a lambda to a string representing it's declaration in Scala.
   * Variables and parameters are renamed, so the string would always be the same
   * and it's hash deterministic.
   *
   * @param lambda The lambda to dump to a string
   * @return
   */
  def dumpLambdaToString(lambda: Lambda, printNonFixedVarIds: Boolean = true): String = {
    TypeChecker(lambda)

    val inputVars = lambda.getVarsInParams(ordering = ByDeclarationOrder)

    val tunableVars =
      Utils.findTunableNodes(lambda)
        .map(Utils.extractArithExpr)
        .collect({ case Some(c) => c.varList })
        .flatten.filterNot(x => inputVars contains x).distinct

    val fullString = dumpLambdaToStringWithoutDecls(lambda, printNonFixedVarIds)
    val allVars = inputVars.distinct ++ tunableVars
    val orderedVars = allVars.toList
    val withIndex = orderedVars.map(x => x.toString).zipWithIndex

    val declStrings = orderedVars.zipWithIndex.map(tuple => {
      val param = tuple._1
      val index = tuple._2
      val newName = getNewName(param.toString, index)
      val replacedRange = replaceVariableNames(param.range.toString, withIndex)

      "val " + newName + " = Var(\"" + param.name + "\", " + replacedRange + ")"
    })

    /*declStrings.mkString("\n") + "\n\n" + */replaceVariableNames(fullString, withIndex)
  }

  /**
   * Dumps a lambda to a string representing a method declaration in Scala that will return
   * the lambda.
   * Variables and parameters are renamed, so the string would always be the same
   * and it's hash deterministic.
   *
   * @param lambda The lambda to dump to a method declaration
   * @return
   */
  def dumpLambdaToMethod(lambda: Lambda, printNonFixedVarIds: Boolean = true): String = {
    val fullString =  dumpLambdaToStringWithoutDecls(lambda, printNonFixedVarIds)

    val variables = findVariables(fullString)

    val replacedVariableNames = replaceVariableNames(fullString, variables)

    val seqName = "variables"

    val declarations = variables.map(pair => {
      "val " + getNewName(pair) + " = " + seqName +"(" + pair._2 + ")"
    }).mkString("\n")

    val method =
      s"""($seqName: Seq[ArithExpr]) => {
         |$declarations
         |
         |$replacedVariableNames
         |}
      """.stripMargin

    method
  }

  def findVariables(fullString: String): List[(String, Int)] = {
    val variable = """v_\p{Alnum}*(_id)?_\d+""".r

    val vars = variable
      .findAllIn(fullString)
      .map(_.toString)
      .toList
      .distinct

    val withIndex = vars.zipWithIndex
    withIndex
  }

  def getStringForInputOutput(a: Any): String = {
    a match {
      case b: Boolean =>
        b.toString
      case f: Float =>
        f.toString
      case i: Int =>
        i.toString
      case d: Double =>
        d.toString
      case arr: Array[_] =>
        Execute.flatten(arr).mkString(" ")
      case _ => throw new NotImplementedError()
    }
  }

  def dumpInputOutputToFile(inout: Any, filename: String, path: String): Boolean = {
    val inputString = DumpToFile.getStringForInputOutput(inout)
    DumpToFile.dumpToFile(inputString, filename, path)
  }

  private def replaceVariableNames(fullString: String, withIndex: List[(String, Int)]): String = {

    val numVariables = withIndex.length

    var tempIds = Set[Int]()

    while (tempIds.size != numVariables) {
      tempIds += util.Random.nextInt()
    }

    val withTemps = (withIndex, tempIds).zipped

    val tempString =
      withTemps.foldRight(fullString)((toReplace, currentString) =>
        currentString.replaceAll(
          toReplace._1._1 + "(\\D)",
          getNewName((toReplace._1._1, toReplace._2)) + "$1"
        ))

    withTemps.foldRight(tempString)((toReplace, currentString) =>
      currentString.replaceAll(
        getNewName((toReplace._1._1, toReplace._2)) + "(\\D)",
        getNewName(toReplace._1) + "$1"
      ))
  }

  def findAndReplaceVariableNames(code: String): String = {
    val variables = findVariables(code)
    replaceVariableNames(code, variables)
  }

  private def getIdentifier(toReplace: (String, Int)): String = {
    toReplace._1.substring(toReplace._1.indexOf("_") + 1, toReplace._1.lastIndexOf("_"))
  }

  private def getNewName(toReplace: (String, Int)): String = {
    "v_" + getIdentifier(toReplace) + "_" + toReplace._2
  }
}

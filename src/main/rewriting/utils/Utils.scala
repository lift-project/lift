package rewriting.utils

import java.nio.file.{Files, Paths}
import java.security.MessageDigest

import lift.arithmetic._
import ir.ast._
import ir.{ArrayType, ArrayTypeWS, Type, TypeException}
import opencl.ir.ast.OpenCLBuiltInFun

import scala.sys.process._

object Utils {

  /**
   * Quick and dirty substitution of arithmetic expressions.
   * This relies on the reference on the nodes gathered in the original expression.
   * As long as we substitute from right to left, we do only shallow copies of the expression tree,
   * so it seems to work.
   * @param st Substitutions to perform.
   * @param tunableNodes Nodes where to perform the substitutions.
   * @param expr The original expression.
   * @return An expression where all the substitutions have been performed
   */
  def quickAndDirtySubstitution(st: collection.immutable.Map[ArithExpr, ArithExpr],
                                tunableNodes: List[Expr],
                                expr: Expr): Expr = {
    var tuned_expr = expr

    tunableNodes.foreach(node => {
      tuned_expr = Expr.replace(tuned_expr, node, node match {
        case f@FunCall(s: Split, x) =>
          FunCall(Split(ArithExpr.substitute(s.chunkSize, st)), x)
        case f@FunCall(s@Scatter(idx: ReorderWithStride), x) =>
          FunCall(Scatter(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case f@FunCall(s@Gather(idx: ReorderWithStride), x) =>
          FunCall(Gather(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case f@FunCall(sl: Slide, x) =>
          FunCall(Slide(ArithExpr.substitute(sl.size, st), ArithExpr.substitute(sl.step, st)), x)
        case v: Value =>
          Value(v.value, Type.substitute(v.t, st))
        case _ =>
          // If you end up here, it is most likely because of one of the following:
          // - a Scatter/Gather with an index function other than a ReorderWithStride
          throw new scala.RuntimeException("Cannot substitute node " + node)
      })
    })

    tuned_expr
  }

  /**
   * Quick and dirty substitution of arithmetic expressions.
   * This relies on the reference on the nodes gathered in the original lambda.
   * As long as we substitute from right to left, we do only shallow copies of the expression tree,
   * so it seems to work.
   * @param st Substitutions to perform.
   * @param tunableNodes Nodes where to perform the substitutions.
   * @param lambda The original expression.
   * @return A lambda where all the substitutions have been performed
   */
  def quickAndDirtySubstitution(st: collection.immutable.Map[ArithExpr, ArithExpr],
                                tunableNodes: List[Expr],
                                lambda: Lambda): Lambda = {
    val tuned_expr = quickAndDirtySubstitution(st, tunableNodes, lambda.body)
    Lambda(lambda.params, tuned_expr)
  }

  /** List all the tunable parameters in the expression */
  def findTunableNodes(expr: Expr): List[Expr] = {
    val tunableNodes = Expr.visitLeftToRight(List[Expr]())(expr, (x, set) => {
      if (Utils.isParametric(x)) x :: set
      else set
    })

    tunableNodes//.reverse
    //          ^--- this is necessary to traverse from right to left
  }

  /** List all the tunable parameters in the lambda */
  def findTunableNodes(lambda: Lambda): List[Expr] =
    findTunableNodes(lambda.body)

  /** Extract an arithmetic expression from an expression. */
  def extractArithExpr(expr: Expr): Option[ArithExpr] = expr match {
    case f@FunCall(s: Split, _) => Some(s.chunkSize)
    case f@FunCall(sl: Slide, _) => Some(sl.step)
    case f@FunCall(Scatter(ReorderWithStride(arithExpr)), _) => Some(arithExpr)
    case f@FunCall(Gather(ReorderWithStride(arithExpr)), _) => Some(arithExpr)

    // Sanity checks: [[ReorderWithStride]] is currently the only index function defined.
    // The two tests below are just sanity checks to introduce a failure in case we add other functions.
    case FunCall(_:Scatter,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case FunCall(_:Gather,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case _ => None
  }

  /** Filter function to find the nodes affected by parameters */
  def isParametric(expr: Expr): Boolean = expr match {
    case _ if extractArithExpr(expr).isDefined => true
    case v: Value => true // this is necessary to propagate the parameters in the types
    case _ => false
  }

  def innerLengthOfTypeMatches(t: Type, n: ArithExpr): Boolean =
    expressionsMatch(n, getLengthOfSecondDim(t))

  def expressionsMatch(a: ArithExpr, b: ArithExpr): Boolean =
    (a, b) match {
      case (Var(_, r1), Var(_, r2)) => r1 == r2
      case (IntDiv(n1, d1), IntDiv(n2, d2)) => expressionsMatch(n1, n2) && expressionsMatch(d2, d2)
      case (i, j) => i == j
    }

  def findGets(expr: Expr, tupleParam: Expr): List[FunCall] = {
    Expr.visitWithState(List[FunCall]())(expr, (e, s) => {
      e match {
        case get@FunCall(Get(_), getParam) if getParam eq tupleParam => get :: s
        case _ => s
      }
    })
  }

  def getLengthOfSecondDim(t: Type) = t match {
    case ArrayType(ArrayTypeWS(_, m)) => m
    case _ => throw new TypeException(t, "ArrayType(ArrayType(), _)", null)
  }

  def validSplitVariable(t: Type): ArithExpr = {
    t match {
      case ArrayTypeWS(_, len) => Var(RangeMul(Cst(1), len, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType", null)
    }
  }

  def validSlideStep(t: Type, overlap: ArithExpr): ArithExpr = {
    t match {
      case ArrayTypeWS(_, s) =>
        Var(RangeMul(Cst(1), s - overlap, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType", null)
    }
  }

  @scala.annotation.tailrec
  def getExprForPatternInCallChain(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    if (pattern.isDefinedAt(expr))
      Some(expr)
    else
      expr match {
        case FunCall(_, arg) => getExprForPatternInCallChain(arg, pattern)
        case FunCall(_ : AbstractPartRed, _, arg) => getExprForPatternInCallChain(arg, pattern)
        case _ => None
      }
  }

  @scala.annotation.tailrec
  def getIndexForPatternInCallChain(expr: Expr, pattern: PartialFunction[Expr, Unit], currentId: Int = 0): Int = {
    if (pattern.isDefinedAt(expr))
      currentId
    else
      expr match {
        case FunCall(_, arg) => getIndexForPatternInCallChain(arg, pattern, currentId + 1)
        case FunCall(_ : AbstractPartRed, _, arg) => getIndexForPatternInCallChain(arg, pattern, currentId + 1)
        case _ => -1
      }
  }

  @scala.annotation.tailrec
  def visitFunCallChain(expr: Expr, visitFun: Expr => Unit): Unit = {
    visitFun(expr)
    expr match {
      case FunCall(_, arg) => visitFunCallChain(arg, visitFun)
      case FunCall(_ : AbstractPartRed, _, arg) => visitFunCallChain(arg, visitFun)
      case _ =>
    }
  }

  def visitFunCallChainWithState[T](init: T)(expr: Expr, visitFun: (Expr, T) => T): T = {
    val result = visitFun(expr, init)
    expr match {
      case FunCall(_, arg) => visitFunCallChainWithState(result)(arg, visitFun)
      case FunCall(_ : AbstractPartRed, _, arg) => visitFunCallChainWithState(result)(arg, visitFun)
      case FunCall(Zip(_), args@_*) => args.foldRight(result)((arg, x) => {
        visitFunCallChainWithState(x)(arg, visitFun)
      })
      case _ => result
    }
  }

  @scala.annotation.tailrec
  def getFinalArg(expr: Expr): Expr = {
    expr match {
      case FunCall(_, arg) => getFinalArg(arg)
      case FunCall(_ : AbstractPartRed, _, arg) => getFinalArg(arg)
      case _ => expr
    }
  }

  // Utilities for dumping to files

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
   * Dumps a lambda to a string representing it's declaration in Scala.
   * Variables and parameters are renamed, so the string would always be the same
   * and it's hash deterministic.
   *
   * @param lambda The lambda to dump to a string
   * @return
   */
  def dumpLambdaToString(lambda: Lambda): String = {

    val fullString =  dumpLambdaToStringWithoutDecls(lambda)

    val withIndex: List[(String, Int)] = findVariables(fullString)

    val decls = withIndex.map(pair =>
      "val " + getNewName(pair) + " = SizeVar(\"" + getIdentifier(pair) + "\")\n"
    ).mkString("")

    decls + "\n" + replaceVariableNames(fullString, withIndex)
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

  def findAndReplaceVariableNames(code: String) = {
    val variables = findVariables(code)
    replaceVariableNames(code, variables)
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
  def dumpLambdaToMethod(lambda: Lambda): String = {
    val fullString =  dumpLambdaToStringWithoutDecls(lambda)

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

  private def getIdentifier(toReplace: (String, Int)): String = {
    toReplace._1.substring(toReplace._1.indexOf("_") + 1, toReplace._1.lastIndexOf("_"))
  }

  private def getNewName(toReplace: (String, Int)): String = {
    "v_" + getIdentifier(toReplace) + "_" + toReplace._2
  }

  private def dumpLambdaToStringWithoutDecls(lambda: Lambda): String = {
    val userFuns = Expr.visitWithState(Set[UserFun]())(lambda.body, (expr, state) => {
      expr match {
        case FunCall(uf: UserFun, _*) if !uf.isInstanceOf[OpenCLBuiltInFun] => state + uf
        case FunCall(VectorizeUserFun(_, uf:UserFun), _*) => state + uf
        case _ => state
      }
    })

    val userFunString = userFuns.map(ScalaPrinter(_)).mkString("\n") + "\n"

    val types = lambda.params.map(p => ScalaPrinter(p.t)).mkString(", ")
    val expr = ScalaPrinter(lambda)
    val fullString = expr.substring(0, 4) + types + "," + expr.substring(4)

    val param = """p_\d+""".r

    val params = param.findAllMatchIn(fullString).map(_.toString()).toList.distinct

    val lambdaString = params.zipWithIndex.foldRight(fullString)((toReplace, currentString) =>
      currentString.replaceAll(toReplace._1, "p_" + toReplace._2))

    userFunString + lambdaString
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

  def countMapsAtCurrentLevel(expr: Expr): Int =
    Utils.visitFunCallChainWithState(0)(expr, {
      case (FunCall(m: AbstractMap, _), count) if m.f.body.isConcrete => count + 1
      case (_, count) => count
    })
}

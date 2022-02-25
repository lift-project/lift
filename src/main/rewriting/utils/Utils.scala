package rewriting.utils

import ir.ast._
import ir.{ArrayType, ArrayTypeWS, Type, TypeException}
import lift.arithmetic._

object Utils {

  private[rewriting] val mapPattern: PartialFunction[Expr, Unit] =
  { case FunCall(map: AbstractMap, _) if map.f.body.isConcrete => }

  private[rewriting] val reducePattern: PartialFunction[Expr, Unit] =
  { case FunCall(_: AbstractPartRed, _, _) => }

  private[rewriting] val splitPattern: PartialFunction[Expr, Unit] =
  { case FunCall(Split(_), _) => }

  private[rewriting] val concretePattern: PartialFunction[Expr, Unit] =
  { case call: FunCall if call.isConcrete(false) => }

  def getHash(lambda: Lambda): String =
    DumpToFile.Sha256Hash(DumpToFile.dumpLambdaToMethod(lambda))

  @scala.annotation.tailrec
  def getMapAtDepth(expr:Expr, depth: Int): Expr = {
    val outermostMap = Utils.getExprForPatternInCallChain(expr, mapPattern).get

    if (depth == 0)
      outermostMap
    else
      getMapAtDepth(getMapBody(outermostMap), depth - 1)
  }

  def getMapBody(expr: Expr): Expr = {
    expr match {
      case FunCall(m: AbstractMap, _) => m.f.body
    }
  }

  case class MapNDPattern(mapNDbuilder: Lambda => AbstractMap,
                          outerArg: Expr,
                          independentInnermostF: Option[Lambda],
                          nd: Int)

  def getMapND(expr: Expr): Option[MapNDPattern] =
    expr match {
      case FunCall(map @ AbstractMap(mapF: Lambda), outerArg) =>
        val flatMapNDbuilder = (newInnermostF: Lambda) => map.copy(newInnermostF).asInstanceOf[AbstractMap]

        mapF.body match {
          case FunCall(_, mapBodyFunArg) if mapBodyFunArg == mapF.params.head =>
            // Potentially a map with dependent inner map

            getMapND(mapF.body) match {
              // nested dependent map
              case Some(MapNDPattern(mapNDbuilder, _, innermostF, nd)) => Some(MapNDPattern(
                (newInnermostF: Lambda) => map.copy(mapNDbuilder(newInnermostF)).asInstanceOf[AbstractMap],
                outerArg, innermostF, nd + 1))

              case None =>
                // flat dependent map
                Some(MapNDPattern(flatMapNDbuilder, outerArg, Some(mapF), 1))
            }

          case FunCall(_, _*) =>
            // a flat map with an independent lambda inside
            Some(MapNDPattern(flatMapNDbuilder, outerArg, Some(mapF), 1))

          case _: Param =>
            // a flat map with no independent lambdas inside
            Some(MapNDPattern(flatMapNDbuilder, outerArg, None, 1))
          case _ => throw new NotImplementedError()
        }
      case _ => None
    }


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
        case FunCall(s: Split, x) =>
          FunCall(Split(ArithExpr.substitute(s.chunkSize, st)), x)
        case FunCall(Scatter(idx: ReorderWithStride), x) =>
          FunCall(Scatter(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case FunCall(Gather(idx: ReorderWithStride), x) =>
          FunCall(Gather(ReorderWithStride(ArithExpr.substitute(idx.s, st))), x)
        case FunCall(sl: Slide, x) =>
          FunCall(Slide(ArithExpr.substitute(sl.size, st), ArithExpr.substitute(sl.step, st)), x)
        case FunCall(sl: Unslide, x) =>
          FunCall(Unslide(ArithExpr.substitute(sl.size, st), ArithExpr.substitute(sl.step, st)), x)
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
    case FunCall(s: Split, _) => Some(s.chunkSize)
    case FunCall(sl: Slide, _) => Some(sl.step)
    case FunCall(Scatter(ReorderWithStride(arithExpr)), _) => Some(arithExpr)
    case FunCall(Gather(ReorderWithStride(arithExpr)), _) => Some(arithExpr)
    case FunCall(Scatter(Shift(arithExpr)), _) => Some(arithExpr)

    // Sanity checks: [[ReorderWithStride]] is currently the only index function defined.
    // The two tests below are just sanity checks to introduce a failure in case we add other functions.
    case FunCall(_:Scatter,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case FunCall(_:Gather,_) => throw new RuntimeException("rewrite can only handle reorder function")
    case _ => None
  }

  /** Filter function to find the nodes affected by parameters */
  def isParametric(expr: Expr): Boolean = expr match {
    case _ if extractArithExpr(expr).isDefined => true
    case _: Value => true // this is necessary to propagate the parameters in the types
    case _ => false
  }

  def innerLengthOfTypeMatches(t: Type, n: ArithExpr): Boolean =
    expressionsMatch(n, getLengthOfSecondDim(t))

  def expressionsMatch(a: ArithExpr, b: ArithExpr): Boolean =
    (a, b) match {
      case (Var(_, r1), Var(_, r2)) => r1 == r2
      case (IntDiv(n1, d1), IntDiv(n2, d2)) => expressionsMatch(n1, n2) && expressionsMatch(d1, d2)
      case (Var(_, r), n: Cst) => r.max == r.min && r.max == n
      case (n: Cst, Var(_, r)) => r.max == r.min && r.max == n
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

  def findExpressionForPattern(lambda: Lambda, pattern: PartialFunction[Expr, Unit]): Option[Expr] =
    findExpressionForPattern(lambda.body, pattern)

  def findExpressionForPattern(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    Expr.visitWithStateDepthFirst(None: Option[Expr])(expr, (e, a) =>
      a match {
        case None if pattern.isDefinedAt(e) => Some(e)
        case _ => a
      })
  }

  def oneDependsOnAnother(node: IRNode, anotherExpr: Expr, allArgsMustDepend: Boolean = true): Boolean = node match {
    case f: FunDecl => oneDependsOnAnother(f, anotherExpr, allArgsMustDepend)
    case expr: Expr => oneDependsOnAnother(expr, anotherExpr, allArgsMustDepend)
    case _ => throw new IllegalArgumentException()
  }


  def oneDependsOnAnother(f: FunDecl, anotherExpr: Expr, allArgsMustDepend: Boolean): Boolean = f match {
    case _: UserFun     => false
    case _: Pattern     => false
    case fp: FPattern   => oneDependsOnAnother(fp.f.body, anotherExpr, allArgsMustDepend)
    case l: Lambda      => oneDependsOnAnother(l.body, anotherExpr, allArgsMustDepend)
    case _              => throw new NotImplementedError()
  }

  /**
   * (allArgsMustDepend == true) aka strict dependence means that expr is considered dependent on anotherExpr iff
   * expr itself is anotherExpr or all arguments of expr are dependent on anotherExpr.
   * If strict dependence is turned off, then dependence of even one of the expr arguments on anotherExpr will be
   * considered a dependence of expr on anotherExpr.
   */
  def oneDependsOnAnother(expr: Expr, anotherExpr: Expr, allArgsMustDepend: Boolean): Boolean = {
    val isDependant = Expr.visitWithState[Option[Boolean]](None)(expr,
      {
        // (state == Some(false) && strictDependence == true) means that we have already evaluated
        // all arguments of expr through an extra call to oneDependsOnAnother below and at least one
        // was found to be independent of anotherExpr, and now we are visiting those arguments again since
        // visitWithState has to complete its traversal.
        case (_, state @ Some(_)) => state
        case (subExpr, _) if subExpr == anotherExpr => Some(true)
        // if even one argument of multi-arg expression is independent of anotherExpr,
        // then the whole expr is also independent of anotherExpr
        case (FunCall(_, args @ _*), _) if allArgsMustDepend =>
          Some(args.forall(arg => oneDependsOnAnother(arg, anotherExpr)))
        case (_, None) => None
      }
    )
    isDependant.isDefined && isDependant.get
  }

  def oneIsIndependentOfAnother(expr: Expr, anotherExpr: Expr, allArgsMustDepend: Boolean = true): Boolean =
    !oneDependsOnAnother(expr, anotherExpr, allArgsMustDepend)

  def countOccurrencesOfPattern(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Int] = {
    Expr.visitWithStateDepthFirst(None: Option[Int])(expr, (e, occurrences) =>
      occurrences match {
        case None if pattern.isDefinedAt(e) => Some(1)
        case Some(oc) if pattern.isDefinedAt(e) =>
          Some(oc + 1)
        case _ => occurrences
      })
  }

  def collect(expr: Expr, pattern: PartialFunction[Expr, Unit]): List[Expr] = {
    Expr.visitWithState(List[Expr]())(expr, (e, s) => {
      e match {
        case currentExpr if pattern.isDefinedAt(currentExpr) => currentExpr :: s
        case _ => s
      }
    })
  }

  def getLengthOfSecondDim(t: Type): ArithExpr = t match {
    case ArrayType(ArrayTypeWS(_, m)) => m
    case _ => throw new TypeException(t, "ArrayType(ArrayType(), _)", null)
  }

  def validSplitVariable(t: Type): ArithExpr = {
    t match {
      case ArrayTypeWS(_, len) => Var(RangeMul(Cst(1), len + 1, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType", null)
    }
  }

  def splitVariable(given: ArithExpr, t: Type): ArithExpr =
    if (given == ?) Utils.validSplitVariable(t) else given

  def validSlideStep(t: Type, overlap: ArithExpr): ArithExpr = {
    t match {
      case ArrayTypeWS(_, s) =>
        Var(RangeMul(Cst(1), s - overlap, Cst(2)))
      case _ => throw new TypeException(t, "ArrayType", null)
    }
  }

//  def getCompositionForPattern(fun: FunDecl, funPattern: PartialFunction[FunDecl, Unit]): Option[FunDecl] = {
//    fun match {
//      case f if funPattern.isDefinedAt(f) => Some(f)
//      case fp: FPattern =>
//        val matchOnInnerFun = getCompositionForPattern(fp.f, funPattern)
//        if (matchOnInnerFun.isDefined)
//          Some(fp.copy(matchOnInnerFun.get))
//        else None
////      case Lambda(params, body, _) =>
////        val matchOnBody = getCompositionForPattern(body, funPattern)
//      case _: Pattern => None
//      case Lambda(params, FunCall(innerFun, args @ _*), _) =>
//        val matchOnInnerFun = getCompositionForPattern(innerFun, funPattern)
//        if (matchOnInnerFun.isDefined) {
//          assert(params.length == args.length) // Simplifying / restricting the pattern
//
//          Some(Lambda(params, matchOnInnerFun.get(params: _*)))
//
//        } else {
//          assert(args.length == 1)  // Simplifying / restricting the pattern
//
//          val matchOnArg = getCompositionForPattern(args.head, funPattern, replacementArgs = params)
//          if (matchOnArg.isDefined)
//            Some(Lambda(params, innerFun(matchOnArg.get)))
//          else None
//        }
//    }
//  }
//
//  def getCompositionForPattern(expr: Expr, funPattern: PartialFunction[FunDecl, Unit], replacementArgs: Seq[Param]): Option[Expr] = {
//    expr match {
//      case _: Param => None
//      case fc: FunCall =>
//        val matchOnInnerFun = getCompositionForPattern(fc.f, funPattern)
//        if (matchOnInnerFun.isDefined)
//          Some(matchOnInnerFun.get(replacementArgs: _*))
//        else None
//      case _ => throw new NotImplementedError("")
//    }
//  }

  // A param is free in respect to lambda A if it's a parameter of lambda B, in whose body A is defined.
  // E.g., in fun(p0 => .. o fun(p1 => f(.., p0, .., p1, ..)) o ..), p0 is a free param in respect to the inner lambda
  def getFreeParamReferences(lambda: Lambda): List[Expr] =
    Expr.visitWithState[(List[Expr], List[Expr], Option[Expr])]((lambda.params.toList, List(), None))(lambda.body, {

      case (expr: Expr, (boundedParamRefs: List[Expr], freeParamRefs: List[Expr], getArgToSkip: Option[Expr])) => {
        getArgToSkip match {
          case Some(someGetArgToSkip) =>
            assert(expr == someGetArgToSkip)
            (boundedParamRefs, freeParamRefs, None)

          case None =>
            expr match {

              case _: Value                     => (boundedParamRefs, freeParamRefs, None)

              case call @ FunCall(_: Get, p: Param) if !boundedParamRefs.contains(p) && !freeParamRefs.contains(call)
              => (boundedParamRefs, freeParamRefs :+ call, Some(p))

              case p: Param if !boundedParamRefs.contains(p) && !freeParamRefs.contains(p)
              => (boundedParamRefs, freeParamRefs :+ p, None)

              case FunCall(fp: FPattern, _@_*)  => (boundedParamRefs ++ fp.f.params, freeParamRefs, None)
              case FunCall(l: Lambda, _@_*)     => (boundedParamRefs ++ l.params, freeParamRefs, None)
              case _                            => (boundedParamRefs, freeParamRefs, None)
            }
        }
      }
    })._2

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

  /**
   * Similar to getExprForPatternInCallChain, but return value is not the whole matching expression, but
   * one defined by extractor. An example usage is where extractor returns a subexpression.
   */
  @scala.annotation.tailrec
  def extractExprForPatternInCallChain(expr: Expr, extractor: Expr => Option[Expr]): Option[Expr] = {
    if (extractor(expr).isDefined) extractor(expr)
    else
      expr match {
        case FunCall(_, arg) => extractExprForPatternInCallChain(arg, extractor)
        case FunCall(_ : AbstractPartRed, _, arg) => extractExprForPatternInCallChain(arg, extractor)
        case _ => None
      }
  }

  /**
   * Finds an expression matching a pattern in a nest of expressions, ignoring arguments on the first level
   */
  def getExprForPatternInANest(expr: Expr, pattern: PartialFunction[Expr, Unit]): Option[Expr] = {
    if (pattern.isDefinedAt(expr))
      Some(expr)
    else {
      val nest = expr match {
        case FunCall(fp: FPattern, _*) => Some(fp.f.body)
        case FunCall(l: Lambda, _*) => Some(l.body)
        case _ => None
      }
      nest match {
        case None => None
        case Some(subExpr) => Expr.visitWithState[Option[Expr]](None)(subExpr, {
          case (_, Some(matchedExpr)) => Some(matchedExpr)
          case (e, None) if pattern.isDefinedAt(e) => Some(e)
          case (_, None) => None
        }, visitArgs = true)
      }
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

  def countMapsAtCurrentLevel(expr: Expr): Int =
    Utils.visitFunCallChainWithState(0)(expr, {
      case (FunCall(m: AbstractMap, _), count) if m.f.body.isConcrete => count + 1
      case (_, count) => count
    })

  def isTranspose(funDecl: FunDecl): Boolean =
    funDecl.isInstanceOf[Transpose] || funDecl.isInstanceOf[TransposeW]

  def countConsecutiveOccurrencesOfPatternAndGetChainArg(pattern: PartialFunction[FunDecl, Unit],
                                                         expr: Expr): (Int, Expr) =
    Utils.visitFunCallChainWithState[(Int, Option[Expr])]((0, None))(expr, {
      case (_,             (nOccurences, Some(arg)))                      => (nOccurences, Some(arg))
      case (FunCall(f, _), (nOccurences, None)) if pattern.isDefinedAt(f) => (nOccurences + 1, None)
      case (nonJoinExpr,   (nOccurences, None))                           => (nOccurences, Some(nonJoinExpr))
      case x => throw new IllegalStateException(x.toString())
    }) match {
      case (joinsVisited, Some(arg)) => (joinsVisited, arg)
      case x => throw new IllegalStateException(x.toString())
    }

  /* Returns ZipND args */
  def getZipNDArgs(expr: Expr): Option[Seq[Expr]] = expr match {

    case FunCall(_: Zip, zipArgs @ _*) =>
      Some(zipArgs)

    case FunCall(AbstractMap(Lambda(_, body, _)), FunCall(_: Zip, zipArgs @ _*))
      if getZipNDArgs(body).isDefined =>
      Some(zipArgs)

    case _ =>
      getArgsOfZipNDasSplitNDZipJoinND(expr)
  }

  def getArgsOfZipNDasSplitNDZipJoinND(expr: Expr): Option[Seq[Expr]] = {
    val (nSplits, splitsArg) = countConsecutiveOccurrencesOfPatternAndGetChainArg({ case _: Split => }, expr)

    if (nSplits == 0)
      None
    else splitsArg match {
      case FunCall(_: Zip, zipArgs @ _*) =>
        val (nJoinsInZipArgs, _) = zipArgs.map(zipArg =>
          countConsecutiveOccurrencesOfPatternAndGetChainArg({ case _: Join => }, zipArg)).unzip

        if (nJoinsInZipArgs.forall(_ >= nSplits)) {
          Some(
            zipArgs.map(zipArg =>
              visitFunCallChainWithState[(Int, Option[Expr])](init = (0, None))(zipArg, {
                case (_, (nodesVisited, joinNDarg))               if nodesVisited > nSplits   => (nodesVisited, joinNDarg)
                case (FunCall(_: Join, _), (nodesVisited, None))  if nodesVisited < nSplits   => (nodesVisited + 1, None)
                case (e, (nodesVisited, None))                    if nodesVisited == nSplits  => (nodesVisited + 1, Some(e))
                case x => throw new IllegalStateException(x.toString)
              }) match {
                case (_, None) => throw new IllegalStateException(zipArg.toString)
                case (_, Some(joinNDarg)) => joinNDarg
              }
            ))
        } else None

      case _ => None
    }
  }
}

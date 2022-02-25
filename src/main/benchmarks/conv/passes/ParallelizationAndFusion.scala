package benchmarks.conv.passes

import benchmarks.conv.ConvExplorationJSONSettings
import com.typesafe.scalalogging.Logger
import exploration.PredicateWrappers._
import exploration.ParamConstraints.greaterThanOrEqual
import exploration.constraint_solvers.ChocoConstraintSolver.SyntacticSugar._
import exploration.constraint_solvers.ConstraintSolver
import exploration.ParamConstraint
import ir.TypeChecker
import ir.ast.{AbstractMap, Expr, FPattern, FunCall, Lambda, Map}
import lift.arithmetic.{ArithExpr, Cst, SimplifiedExpr, Var}
import opencl.ir.{InferOpenCLAddressSpace, LocalMemory, OpenCLAddressSpace, PrivateMemory, UndefAddressSpace}
import org.chocosolver.solver.Model
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.expression.discrete.relational.ReExpression
import rewriting.Rewrite
import benchmarks.conv.ConvExplorationJSONSettings
import benchmarks.conv.ConvExploration.{debugRun, useRefactoredParCstrs}
import benchmarks.conv.passes.ParallelizationAndFusion.Code.{allParMapTypes, mapGlb, mapLcl, mapSeq, mapWrg, parDims, parallelMaps, replaceInnerMapWithOuter}
import benchmarks.conv.ConvExplorationJSONSettings
import rewriting.passes.RewritingPass
import rewriting.passes.RewritingPass.RuleApplicationHelper.{exprEquals, matchedExprItself}
import rewriting.passes.RewritingPass.{RewritePassParams, RuleApplicationHelper, globalNumbering}
import rewriting.rules.{OpenCLRules, Rule, Rules}
import rewriting.utils.NumberExpression

import scala.collection.immutable.ListMap
import scala.language.{implicitConversions, postfixOps}

object ParallelizationAndFusion extends ConvAutoRewritePassSpaceFactory {

  override val name: String = "Parallelization and fusion"

  private val logger = Logger(this.getClass)

//  private var lambdaWithGeneralizedMapsLogged: Boolean = false

  /**
   * Numeric representation of map transformations for the constraint solver
   */
  //noinspection TypeAnnotation
  object Code extends Enumeration {
    case class Val(value: Int) extends super[Enumeration].Val {
      def apply(): Int = value
    }

    val mapSeq = Val(0)
    val replaceInnerMapWithOuter = Val(1)

    val mapLcl = Val(1) // 10, 11, 12
    val mapWrg = Val(2) // 20, 21, 22
    val mapGlb = Val(3) // 30, 31, 32

    val parallelMaps = Val(mapLcl() * 10) // or larger

    val parDims = Seq(Val(0), Val(1), Val(2))

    object SyntacticSugarArithExpr {
      implicit def mapType(map: Expr)(implicit args: Array[ArithExpr], context: ASTContext): ArithExpr = args(context.indexOf(map))

      def parDomain(p: ArithExpr): ArithExpr with SimplifiedExpr = p / 10
      def parDim(p: ArithExpr): ArithExpr with SimplifiedExpr = p % 10

      def isParallel(p: ArithExpr): Boolean = greaterThanOrEqual(p, parallelMaps())
      def isFused(p: ArithExpr): Boolean = p == Cst(replaceInnerMapWithOuter.value)
    }

    object SyntacticSugarChoco {
      implicit def mapType(map: Expr)(implicit args: Array[ArithExpr],
                                      context: ASTContext,
                                      model: Model): ArExpression = toChoco(args(context.indexOf(map)))

      implicit def mapTypeToAE(map: Expr)(implicit args: Array[ArithExpr], context: ASTContext): ArithExpr =
        SyntacticSugarArithExpr.mapType(map)

      def parDomain(p: ArExpression)(implicit m: Model): ArExpression = p / 10
      def parDim(p: ArExpression)(implicit m: Model): ArExpression = p % 10

      def isSeq(p: ArExpression)(implicit m: Model): ReExpression = p === mapSeq.value
      def isFused(p: ArExpression)(implicit m: Model): ReExpression = p === replaceInnerMapWithOuter.value
      def isParallel(p: ArExpression)(implicit m: Model): ReExpression = p >= parallelMaps()
      def isMapGlb(p: ArExpression)(implicit m: Model): ReExpression = parDomain(p) === mapGlb()
      def isMapWrg(p: ArExpression)(implicit m: Model): ReExpression = parDomain(p) === mapWrg()
      def isMapLcl(p: ArExpression)(implicit m: Model): ReExpression = parDomain(p) === mapLcl()
      def isDim(p: ArExpression, dim: Int)(implicit m: Model): ReExpression = parDim(p) === parDims(dim)()
    }

//    object ParFuseEncoding {
//      implicit def mapType(map: Expr)(implicit args: Array[ArithExpr], context: ASTContext): ArExprVar =
//        args(context.indexOf(map)) match {
//          case v: Var => ArExprVar(v)
//          case ae => throw new IllegalArgumentException(s"The parameter representing $map is not a Var: $ae")
//        }
//
//      def parDomain(p: ArExpr): ArExpr = p / 10
//      def parDim(p: ArExpr): ArExpr = p % 10
//
//      def isFused(p: ArExpr): ReExpr = p === 1
//      def isParallel(p: ArExpr): ReExpr = p >= parallelMaps()
//      def isMapGlb(p: ArExpr): ReExpr = parDomain(p) === mapGlb()
//      def isMapWrg(p: ArExpr): ReExpr = parDomain(p) === mapWrg()
//      def isMapLcl(p: ArExpr): ReExpr = parDomain(p) === mapLcl()
//      def isDim(p: ArExpr, dim: Int): ReExpr = parDim(p) === parDims(dim)()
//    }

    val allParMapTypes: Seq[Int] = Seq(
      10*mapLcl() + parDims(0)(), 10*mapLcl() + parDims(1)(), 10*mapLcl() + parDims(2)(),
      10*mapWrg() + parDims(0)(), 10*mapWrg() + parDims(1)(), 10*mapWrg() + parDims(2)(),
      10*mapGlb() + parDims(0)(), 10*mapGlb() + parDims(1)(), 10*mapGlb() + parDims(2)())
  }

  val ruleApplicationHelpers: Predef.Map[Rule, RuleApplicationHelper] = Predef.Map(
    OpenCLRules.mapSeq -> RuleApplicationHelper(mapSeq(), matchedExprItself, exprEquals),

    Rules.joinSplitPreserveOuterMap ->
      RuleApplicationHelper(replaceInnerMapWithOuter(),
        anchorExprExtractor = {
          case FunCall(AbstractMap(Lambda(_, innerMapExpr @ FunCall(AbstractMap(_), _), _)), _) =>
            innerMapExpr
        },
        narrowedPatternGenerator = (innerMapExpr: Expr) => {
          case FunCall(AbstractMap(Lambda(_, matchedInnerMapExpr @ FunCall(AbstractMap(_), _),_)), _)
            if matchedInnerMapExpr == innerMapExpr => }),

    OpenCLRules.mapLclRelaxed(0) -> RuleApplicationHelper(10*mapLcl() + parDims(0)(), matchedExprItself, exprEquals),
    OpenCLRules.mapLclRelaxed(1) -> RuleApplicationHelper(10*mapLcl() + parDims(1)(), matchedExprItself, exprEquals),
    OpenCLRules.mapLclRelaxed(2) -> RuleApplicationHelper(10*mapLcl() + parDims(2)(), matchedExprItself, exprEquals),

    OpenCLRules.mapWrgRelaxed(0) -> RuleApplicationHelper(10*mapWrg() + parDims(0)(), matchedExprItself, exprEquals),
    OpenCLRules.mapWrgRelaxed(1) -> RuleApplicationHelper(10*mapWrg() + parDims(1)(), matchedExprItself, exprEquals),
    OpenCLRules.mapWrgRelaxed(2) -> RuleApplicationHelper(10*mapWrg() + parDims(2)(), matchedExprItself, exprEquals),

    OpenCLRules.mapGlbRelaxed(0) -> RuleApplicationHelper(10*mapGlb() + parDims(0)(), matchedExprItself, exprEquals),
    OpenCLRules.mapGlbRelaxed(1) -> RuleApplicationHelper(10*mapGlb() + parDims(1)(), matchedExprItself, exprEquals),
    OpenCLRules.mapGlbRelaxed(2) -> RuleApplicationHelper(10*mapGlb() + parDims(2)(), matchedExprItself, exprEquals)
  ).map(pair => (concretizeRule(pair._1) -> pair._2))


  case class ParASTContext(override val lambda: Lambda,
                           override val rewritingPass: RewritingPass)
    extends ASTContext(lambda, rewritingPass) {

    val memUsedAndCorrespondingProhibitedParDomains: Predef.Map[Expr, (Set[OpenCLAddressSpace], Set[Int])] =
      Predef.Map(
        rewritingPass.rewriteParams.exprParams.keys.map(mapExpr => {
          var privateMemUsed = false
          var localMemUsed = false

          val prohibitedDomains = mapExpr match {
            case FunCall(_, arg) =>
              // Return memory + input memory
              Set(mapExpr, arg).flatMap(expr => expr.addressSpace match {
                case UndefAddressSpace =>
                  throw new IllegalStateException(s"Address space is unknown for the expression below. " +
                    s"Perhaps, address space inference was not performed.\n$expr")

                case addressSpace =>
                  (if (addressSpace.containsAddressSpace(PrivateMemory)) {
                    privateMemUsed = true
                    Set(mapLcl(), mapWrg(), mapGlb())
                  } else Set[Int]()) ++
                    (if (addressSpace.containsAddressSpace(LocalMemory)) {
                      localMemUsed = true
                      Set(mapWrg(), mapGlb())
                    } else Set[Int]())
              })
            case _ => throw new IllegalStateException()
          }

          mapExpr -> (
            (if (privateMemUsed) Set(PrivateMemory) else Set()) ++ (if (localMemUsed) Set(LocalMemory) else Set()),
            prohibitedDomains)
        }
        ).toSeq: _*)

    // For each nested map chain, count how many maps can be mapLcl.
    // Take the minimum of these counts across chains.
    val maxNumberOfNestedMapLcls: Int = nestingChains.toSeq.map(nestingChain =>
      nestingChain.count(mapExpr => !memUsedAndCorrespondingProhibitedParDomains(mapExpr)._2.contains(Code.mapLcl()))
    ) match {
      case Nil => 0
      case counts => counts.min / 2
    }

    val maxNumberOfNestedMapGlbs: Int = nestingChains.toSeq.map(nestingChain =>
      nestingChain.count(mapExpr => !memUsedAndCorrespondingProhibitedParDomains(mapExpr)._2.contains(Code.mapGlb()))
    ) match {
      case Nil => 0
      case counts => counts.min
    }
  }


  def parallelizationRules(context: ParASTContext,
                           passParams: RewritePassParams): Vector[ParamConstraint] = {
    implicit val c: ASTContext = context

    val cannotMixMapWrgLclAndMapGlb = ParamConstraint("cannotMixMapWrgLclAndMapGlb",
      "MapWrg/MapLcl cannot be used with MapGlb",
      params = passParams.paramList.map(_.getAEVar).toList,
      arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
        import Code.SyntacticSugarArithExpr._

        // NOT (MapGlb used AND (MapWrg used OR MapLcl used))
        !args.exists(parDomain(_) == Cst(Code.mapGlb()) &&
          args.exists(v => (parDomain(v) == Cst(Code.mapWrg())) || (parDomain(v) == Cst(Code.mapLcl()))))
      },
      predicate = (model: Model, params: List[ArithExpr]) => {
        import Code.SyntacticSugarChoco._
        implicit val (m, p) = (model, params.toArray)

        not(
          or(params.map(isMapGlb(_)): _*) and
            or(params.map(p => isMapWrg(p) or isMapLcl(p)): _*)
        ).post()
      })

    /**
     * Based on the lambda, we know which maps nest other maps. For each map that has inner maps,
     * create a constraint that if it is parallel, all its nested maps must not be parallelized in the same way
     */
    val nestedParMapsMustNotUseSameDims: Vector[ParamConstraint] =
      context.allNestedMaps.map {
        case (parentMap: Expr, childMaps: Seq[Expr]) =>
          ParamConstraint(s"nested.parmaps.of.${passParams.exprParams(parentMap).name}.must.not.use.same.dims",
            "Nested parallel map dimensions must be different, e.g. MapWrg(1) cannot have another MapWrg(1) inside",
            params = passParams.paramList.map(_.getAEVar).toList,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
              val parentMapParamValue = args(context.indexOf(parentMap))
              val parentIsParallel = greaterThanOrEqual(parentMapParamValue, Cst(parallelMaps()))

              if (parentIsParallel)
                childMaps.map(childMap => args(context.indexOf(childMap)) !== parentMapParamValue).reduce(_ && _)
              else true
            },
            predicate = (model: Model, params: List[ArithExpr]) => {
              import Code.SyntacticSugarChoco._
              implicit val (m, p) = (model, params.toArray)

              if (!useRefactoredParCstrs)
                ifThen(
                  ifCstr = isParallel(parentMap),
                  thenCstr = and(
                    childMaps.map(childMap =>
                      toChoco(childMap) !== toChoco(parentMap)): _*))
              else
                (isSeq(parentMap) or isFused(parentMap) or and(
                  childMaps.map(childMap =>
                    toChoco(childMap) !== toChoco(parentMap)): _*)).post()
            }
          )
      }.toVector

    val overallMapWrgAndLclCardinality: Vector[ParamConstraint] =
      if (!useRefactoredParCstrs)
        Vector(ParamConstraint("mapWrg.and.Lcl.1to1.cardinality.overall",
          "The number of dimensions used for MapLcls must equal that of MapWrg across the whole expression",
          params = passParams.paramList.map(_.getAEVar).toList,
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
            val nLclDimsUsed = args.filter(_ / 10 == Cst(mapLcl())).map(_ % 10).distinct.length
            val nWrgDimsUsed = args.filter(_ / 10 == Cst(mapWrg())).map(_ % 10).distinct.length

            nLclDimsUsed == nWrgDimsUsed },

          predicate = (model: Model, params: List[ArithExpr]) => {
            import Code.SyntacticSugarChoco._
            implicit val (m, p) = (model, params.toArray)

            // ((mapWrg(0) exists) nxor (mapLcl(0) exists)) and ((mapWrg(1) exists) nxor (mapLcl(1) exists)) and ..
            // nxor = both or neither
            and((0 until 3).map(dim =>
              not(xor(
                or(params.map(p => isMapWrg(p) and isDim(p, dim)): _*),
                or(params.map(p => isMapLcl(p) and isDim(p, dim)): _*)))
            ): _*).post()
          }))
      else
        context.nestingChains.flatMap(mapsInNestingChain => Seq(
          ParamConstraint("mapWrg.and.Lcl.1to1.cardinality.overall",
            "The number of dimensions used for MapLcls must equal that of MapWrg across the whole expression",
            params = passParams.paramList.map(_.getAEVar).toList,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
              val nLclDimsUsed = args.filter(_ / 10 == Cst(mapLcl())).map(_ % 10).distinct.length
              val nWrgDimsUsed = args.filter(_ / 10 == Cst(mapWrg())).map(_ % 10).distinct.length

              nLclDimsUsed == nWrgDimsUsed },

            predicate = (model: Model, params: List[ArithExpr]) => {
              import Code.SyntacticSugarChoco._
              implicit val (m, args) = (model, params.toArray)

              and((0 until 3).map(dim =>

                // mapWrg(dim) doesn't exist in this chain
                not(or(mapsInNestingChain.map(p => isMapWrg(mapType(p)) and isDim(p, dim)): _*))
                  or
                  // mapLcl(dim) exists in this chain
                  or(mapsInNestingChain.map(p => isMapLcl(p) and isDim(p, dim)): _*)

              ): _*).post()
            }),
          ParamConstraint("mapLcl.and.Wrg.1to1.cardinality.overall",
            "The number of dimensions used for MapWrgs must equal that of MapLcls across the whole expression",
            params = passParams.paramList.map(_.getAEVar).toList,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
              val nLclDimsUsed = args.filter(_ / 10 == Cst(mapLcl())).map(_ % 10).distinct.length
              val nWrgDimsUsed = args.filter(_ / 10 == Cst(mapWrg())).map(_ % 10).distinct.length

              nLclDimsUsed == nWrgDimsUsed },

            predicate = (model: Model, params: List[ArithExpr]) => {
              import Code.SyntacticSugarChoco._
              implicit val (m, args) = (model, params.toArray)

              and((0 until 3).map(dim =>

                // mapLcl(dim) doesn't exist in this chain
                not(or(mapsInNestingChain.map(p => isMapLcl(mapType(p)) and isDim(p, dim)): _*))
                  or
                  // mapWrg(dim) exists in this chain
                  or(mapsInNestingChain.map(p => isMapWrg(p) and isDim(p, dim)): _*)

              ): _*).post()
            }))
        ).toVector

    // TODO: remove the overall one maybe? this one subsumes it
    val allParMapCardinalityAcrossChains: Vector[ParamConstraint] =
      context.nestingChains.map(mapsInNestingChain =>
        ParamConstraint("mapGlb.and.mapWrg.and.Lcl.1to1.cardinality.within.a.chain",
          "MapGlb and MapWrg and MapLcl cardinality within a chain = the numbers of par dimensions used by MapGlbs " +
            "and MapWrgs and MapLcls in a chain must be the same as those of the whole expression",
          params = passParams.paramList.map(_.getAEVar).toList,
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
            import Code.SyntacticSugarArithExpr._
            implicit val a: Array[ArithExpr] = args.toArray

            val nGlbDimsUsedAcrossWholeExpr = args.filter(_ / 10 == mapGlb()).map(_ % 10).distinct.length
            val nLclDimsUsedAcrossWholeExpr = args.filter(_ / 10 == mapLcl()).map(_ % 10).distinct.length
            val nWrgDimsUsedAcrossWholeExpr = args.filter(_ / 10 == mapWrg()).map(_ % 10).distinct.length

            val nGlbDimsUsedAcrossThisChain = mapsInNestingChain.map(mapType).filter(_ / 10 == mapGlb()).map(_ % 10).distinct.length
            val nLclDimsUsedAcrossThisChain = mapsInNestingChain.map(mapType).filter(_ / 10 == mapLcl()).map(_ % 10).distinct.length
            val nWrgDimsUsedAcrossThisChain = mapsInNestingChain.map(mapType).filter(_ / 10 == mapWrg()).map(_ % 10).distinct.length

            // Either MapGlbs or (MapWrgs+MapLcls) can be used and not both, but that's checked by the cannotMixMapWrgLclAndMapGlb constraint
            nGlbDimsUsedAcrossThisChain == nGlbDimsUsedAcrossWholeExpr &&
              // All _four_ numbers must be the same; overall cardinality is checked within the overallMapWrgAndLclCardinality constraint
              nLclDimsUsedAcrossThisChain == nLclDimsUsedAcrossWholeExpr &&
              nWrgDimsUsedAcrossThisChain == nWrgDimsUsedAcrossWholeExpr
          },

          predicate = (model: Model, params: List[ArithExpr]) => {
            import Code.SyntacticSugarChoco._
            implicit val (m, p) = (model, params.toArray)

            // ((mapWrg(0) exists in whole expr) nxor (mapWrg(0) exists in chain)) and
            //   ((mapLcl(0) exists in whole expr) nxor (mapLcl(0) exists in chain)) and
            //     ((mapWrg(1) exists in whole expr) nxor (mapWrg(1) exists in chain)) and
            //       etc
            // nxor = both or neither
            if (!useRefactoredParCstrs)
              and(
                allParMapTypes.map(parMapType => {
                  not(xor(
                    // parMapType exists in the whole expression
                    or(params.map(exprMap => toChoco(exprMap) === parMapType): _*),
                    // parMapType exists in this chain
                    or(mapsInNestingChain.map(chainMap => mapType(chainMap) === parMapType): _*)
                  ))
                }): _*).post()
            else
              and(
                allParMapTypes.map(parMapType => {(
                  // parMapType doesnt't exist in the whole expression
                  not(or(params.map(exprMap => toChoco(exprMap) === parMapType): _*))
                    or
                    // parMapType exists in this chain
                    or(mapsInNestingChain.map(chainMap => mapType(chainMap) === parMapType): _*))
                }): _*).post()
          }
        )
      ).toVector

    // Turned off since this is too strict. The nested parMaps can compose each other if there are no transformations
    // of views in-between, i.e. the producer threads remain the same as consumer threads, so there is no need for a barrier
    // Now, we rely on the compiler to detect barriers not taken by all threads.
    val onlyOutermostParMapInEachParDomainCanCompose: Vector[ParamConstraint] =
    context.allNestedMaps.map {
      case (parentMap: Expr, childMaps: Seq[Expr]) =>
        ParamConstraint(s"if.${passParams.exprParams(parentMap).name}.is.outermost.parmap.in.par.domain.then.only.it.can.compose",
          "If a map is both parallel and outermost in its parDomain, only it can compose with other parmaps in the " +
            "same parDomain. This protects against cases like MapLcl(1)(MapLcl(0)(f) o MapLcl(0)(f)), " +
            "where not all threads take the same barrier",
          params = passParams.paramList.map(_.getAEVar).toList,
          arithExprPredicate = (args: List[ArithExpr]) => {
            import Code.SyntacticSugarArithExpr._
            implicit val a: Array[ArithExpr] = args.toArray

            def isOutermostParMapInItsDomain(aMap: Expr): Boolean =
              isParallel(aMap) && (
                // either is outermost map across all maps, or outermost across maps of the same par domain
                !(context.allOuterMaps.contains(aMap) &&
                  context.allOuterMaps(aMap).exists(outerMap => parDomain(outerMap) == parDomain(aMap))))

            !isOutermostParMapInItsDomain(parentMap) ||
              parDims.forall(parDim =>
                childMaps.count(childMap => mapType(childMap) == 10 * parDomain(parentMap) + parDim()) <= 1)
          },

          predicate = (model: Model, params: List[ArithExpr]) => {
            import Code.SyntacticSugarChoco._
            implicit val (m, p) = (model, params.toArray)

            def isOutermostParMapInItsDomain(aMap: Expr) =
              if (!context.allOuterMaps.contains(aMap))
                isParallel(aMap) // the outermost map of all maps
              else
                isParallel(aMap) and not(or(
                  context.allOuterMaps(aMap).map(outerMap => parDomain(outerMap) === parDomain(aMap)) :_*))

            // If this map is parallel and outermost parMap in its domain (Wrg, Lcl, Glb), then for each
            // dimension of this domain, there can only be one par map of this dimension and this domain among
            // the nested maps of this map
            ifThen(
              ifCstr = isOutermostParMapInItsDomain(parentMap),
              thenCstr =
                and(parDims.map(dim =>
                  // A nested map cannot have the same domain and dim as parent, but that's checked against by another cstr
                  toChoco(dim()) === parDim(parentMap) or
                    count(
                      toChoco(10) * parDomain(parentMap) + dim(), // value that at most 1 childMap can take
                      childMaps.map(mapType(_).intVar),
                      model.intVar(0, 1)
                    )): _*))
          }
          /*predicate = (params: List[ArithExpr]) => {
            import Code.ParFuseEncoding._
            implicit val p: Array[ArithExpr] = params.toArray

            def isOutermostParMapInItsDomain(aMap: Expr) =
              if (!context.allOuterMaps.contains(aMap))
                isParallel(aMap) // the outermost map of all maps
              else
                isParallel(aMap) and Not(Or(
                  context.allOuterMaps(aMap).map(outerMap => parDomain(outerMap) === parDomain(aMap)) :_*))


            // If this map is parallel and outermost parMap in its domain (Wrg, Lcl, Glb), then for each
            // dimension of this domain, there can only be one par map of this dimension and this domain among
            // the nested maps of this map
            IfThen(
              ifExpr = isOutermostParMapInItsDomain(parentMap),
              thenExpr =
                And(parDims.map(dim =>
                  // A nested map cannot have the same domain and dim as parent, but that's checked against by another cstr
                  arExpr(dim()) === parDim(parentMap) or
                    Count(
                      arExpr(10) * parDomain(parentMap) + dim(), // value that at most 1 childMap can take
                      childMaps.map(mapType(_)),
                      lb = 0, ub = 1)
                    ): _*))
          }*/)
    }.toVector

    // Turning off since this is allowed when a mid-level mapLcl that's nested in a mapSec produces a view that's
    // not transformed before the next iteration of MapSeq and hence the producer thread remains the same as consumer
    // thread, so there is no need for a barrier.
    // Now, we rely on the compiler to detect barriers not taken by all threads.
    val noMapSeqsInbetweenMapParsOfSameDomain: Vector[ParamConstraint] = passParams.paramExprs.values.flatMap(mapExpr =>
      context.immediateNestedMap(mapExpr) match {
        case Some(immediateNestedMap) => Some(ParamConstraint("", "",

          params = passParams.paramList.map(_.getAEVar).toList,
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
            import Code.SyntacticSugarArithExpr._
            implicit val a: Array[ArithExpr] = args.toArray

            def isInnermostParMapInItsDomain(aMap: Expr): Boolean =
              !context.allNestedMaps(aMap).exists(nestedMap => parDomain(nestedMap) == parDomain(aMap))

            // If this map is parallel and not innermost parMap in its domain (Wrg, Lcl, Glb), then its immediate
            // nested map cannot be a mapSeq
            if (isParallel(mapExpr) && !isInnermostParMapInItsDomain(mapExpr))
              immediateNestedMap !== Code.mapSeq()
            else true
          },
          predicate = (model: Model, params: List[ArithExpr]) => {
            import Code.SyntacticSugarChoco._
            implicit val (m, p) = (model, params.toArray)

            def isInnermostParMapInItsDomain(aMap: Expr) =
              not(or(context.allNestedMaps(aMap).map(nestedMap => parDomain(nestedMap) === parDomain(aMap)) :_*))

            // If this map is parallel and not innermost parMap in its domain (Wrg, Lcl, Glb), then its immediate
            // nested map cannot be a mapSeq
            ifThen(
              ifCstr = isParallel(mapExpr) and not(isInnermostParMapInItsDomain(mapExpr)),
              thenCstr = not(mapType(immediateNestedMap) === Code.mapSeq()))
          }))
        case None => None
      }
    ).toVector

    val nestedMapParGranularityMustDecreaseInSameDim: Vector[ParamConstraint] =
      context.allNestedMaps.map {
        case (parentMap: Expr, childMaps: Seq[Expr]) =>
          ParamConstraint(s"nested.parmaps.of.${passParams.exprParams(parentMap).name}.must.not.be.par.in.coarser.domain.and.same.dim",
            "Nested parallel map domains must not be coarser in the same dim, e.g. MapLcl(1) cannot have a MapWrg(1) nested inside",
            params = passParams.paramList.map(_.getAEVar).toList,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {

              if (args(context.indexOf(parentMap)) / 10 == Cst(mapLcl()))
                childMaps.forall(childMap =>
                  !(args(context.indexOf(childMap)) / 10 === mapWrg() &&
                    args(context.indexOf(childMap)) % 10 === args(context.indexOf(parentMap))))
              else true
            },

            predicate = (model: Model, params: List[ArithExpr]) => {
              import Code.SyntacticSugarChoco._
              implicit val (m, p) = (model, params.toArray)

              if (!useRefactoredParCstrs)
                ifThen(
                  ifCstr = isMapLcl(parentMap),
                  thenCstr = and(
                    childMaps.map(childMap => not(
                      isMapWrg(childMap) and (parDim(parentMap) === parDim(childMap)))): _*))
              else
                (not(isMapLcl(parentMap)) or
                  and(
                    childMaps.map(childMap => not(
                      isMapWrg(childMap) and (parDim(parentMap) === parDim(childMap)))): _*)).post()
            })
      }.toVector

    // These constraints require that memory address space inference has been run. This is handled by Context()
    val parMapsMustReturnAppropriatelySharedMemory: Vector[ParamConstraint] =
      passParams.exprParams.keys.flatMap(mapExpr => {

        val prohibitedParDomains = context.memUsedAndCorrespondingProhibitedParDomains(mapExpr)
        val usedMemory: String =
          ((if (prohibitedParDomains._1.contains(PrivateMemory)) List("private") else List()) ++
            (if (prohibitedParDomains._1.contains(LocalMemory)) List("local") else List())).mkString(".")

        if (prohibitedParDomains._2.isEmpty) None
        else Some(ParamConstraint(
          s"map.${passParams.exprParams(mapExpr).name}.cannot.be.parallelized.across.some.domains.due.to.its.$usedMemory.memory",
          "Map that returns memory of restricted scope cannot be parallelized across domains where that memory is not accessible",
          params = passParams.paramList.map(_.getAEVar).toList,
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
            import Code.SyntacticSugarArithExpr._
            implicit val a: Array[ArithExpr] = args.toArray

            prohibitedParDomains._2.forall( parDomain(mapExpr) !== _ )
          },
          predicate = (model: Model, params: List[ArithExpr]) => {
            import Code.SyntacticSugarChoco._
            implicit val (m, p) = (model, params.toArray)

            and(
              prohibitedParDomains._2.toSeq.map(domain => parDomain(mapExpr) !== domain): _*
            ).post()
          }))
      }).toVector

    val mapWrgMustBeUsedIfLocalMemIsUsed: Vector[ParamConstraint] = {
      if (!useRefactoredParCstrs) {
        if (context.localMemUsed)
        Vector(ParamConstraint("mapWrgMustBeUsedWithLocalMemory",
          "The expression uses local memory, so there must be at least one MapWrg",
          params = passParams.paramList.map(_.getAEVar).toList,
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
            import Code.SyntacticSugarArithExpr._
            implicit val a: Array[ArithExpr] = args.toArray

            args.exists(arg => parDomain(arg) === Code.mapWrg())
          },
          predicate = (model: Model, params: List[ArithExpr]) => {
            import Code.SyntacticSugarChoco._
            implicit val (m, p) = (model, params.toArray)

            or(params.map(param =>
              parDomain(param) === Code.mapWrg()): _*).post()
          })
        ) else Vector()
      } else {
        passParams.exprParams.keys
          .filter(
            context.memUsedAndCorrespondingProhibitedParDomains(_)._1.contains(LocalMemory))
          .map(thisMap =>
            ParamConstraint("mapUsingLocMemMustBeWrappedInMapWrgs",
              "A map uses local memory, so it must be wrapped in MapWrg across all dimensions used in the expression (1 or more)",
              params = passParams.paramList.map(_.getAEVar).toList,
              arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
                import Code.SyntacticSugarArithExpr._
                implicit val a: Array[ArithExpr] = args.toArray

                val wrgDimsUsed = args.filter(_ / Cst(10) == Cst(mapWrg())).map(_ % 10).distinct

                wrgDimsUsed.forall(dim =>
                  // there is an outerMap that is MapWrg in this dim
                  context.allOuterMaps(thisMap).exists(outerMap =>
                    (outerMap / Cst(10) == Cst(mapWrg())) && (outerMap % 10 == dim)))
              },
              predicate = (model: Model, params: List[ArithExpr]) => {
                import Code.SyntacticSugarChoco._
                implicit val (m, p) = (model, params.toArray)

                // there must be an outerMap that is a MapWrg
                (or((0 until 3).map(dim =>
                  // there is an outerMap that is MapWrg in this dim
                  or(context.allOuterMaps(thisMap).map(outerMap =>
                    isMapWrg(outerMap) and parDim(outerMap) === dim):_*)): _*
                ) and
                  and((0 until 3).map(dim =>
                    // parMapType doesn't exist in the whole expression
                    not(or(params.map(aMap => isMapWrg(aMap) and parDim(aMap) === dim): _*))
                      or
                      // there is an outerMap that is MapWrg in this dim
                      or(context.allOuterMaps(thisMap).map(outerMap =>
                        isMapWrg(outerMap) and parDim(outerMap) === dim):_*)
                  ): _*)).post()
              })).toVector
      }
    }

    Vector(cannotMixMapWrgLclAndMapGlb) ++
      nestedParMapsMustNotUseSameDims ++ // Rule (3)
      overallMapWrgAndLclCardinality ++ // Rules (4) & (5)
      allParMapCardinalityAcrossChains ++ //  Rule (7)
      /*onlyOutermostParMapInEachParDomainCanCompose ++*/ // has to be checked by the compiler when all barriers have been inserted
      /*noMapSeqsInbetweenMapParsOfSameDomain ++*/
      nestedMapParGranularityMustDecreaseInSameDim ++ // Rule (6)
      parMapsMustReturnAppropriatelySharedMemory ++ // Rules (1)
      mapWrgMustBeUsedIfLocalMemIsUsed // Rule (2)
  }

  def heuristics(context: ASTContext,
                 passParams: RewritePassParams): Vector[ParamConstraint] = {
    implicit val c: ASTContext = context

    val fastestMovingMemDimMustCorrespondToFineGrainedParallelism: Vector[ParamConstraint] =
      context.allNestedMaps.map {
        case (parentMap: Expr, childMaps: Seq[Expr]) =>
          ParamConstraint(s"heu.nested.parmaps.of.${passParams.exprParams(parentMap).name}.must.use.smaller.par.domains",
            "Heuristic: nested par maps must use non-strictly decreasing parallel dimensions",
            params = passParams.paramList.map(_.getAEVar).toList,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
              import Code.SyntacticSugarArithExpr._
              implicit val a: Array[ArithExpr] = args.toArray

              val parentIsParallel = greaterThanOrEqual(parentMap, Cst(parallelMaps()))

              if (parentIsParallel)
                childMaps.map(childMap => {

                  !(parDomain(childMap) == parDomain(parentMap) &&
                    greaterThanOrEqual(parDim(childMap), parDim(parentMap)))
                }).reduce(_ && _)
              else true
            },
            predicate = (model: Model, params: List[ArithExpr]) => {
              import Code.SyntacticSugarChoco._
              implicit val (m, p) = (model, params.toArray)

              ifThen(
                ifCstr = isParallel(parentMap),
                thenCstr = and(
                  childMaps.map(childMap => {

                    not(parDomain(childMap) === parDomain(parentMap) and
                      parDim(childMap) >= parDim(parentMap))
                  }): _*
                ))
            }
          )
      }.toVector

    // [a,b,c,d] => [(a,[b,c,d]), (b,[c,d]), (c,[d])]
    def pairElementsWithTails[T](input: Seq[T]): Seq[(T, Seq[T])] = {

      // [a,b,c,d] => [[b,c,d], [c,d], [d]]
      def getTails(s: Seq[T]): Seq[Seq[T]] =
        s match {
          case Nil => Seq()
          case el :: Nil => Seq(Seq(el))
          case _ :: remaining => remaining +: getTails(remaining)
        }

      input.reverse.tail.reverse.zip(getTails(input))
    }

    val sequentialMapsMustBeFused: Vector[ParamConstraint] =
      context.nestingChains.flatMap(mapsInNestingChain => {

        val mapsAndTheirPerfectlyNestedChildrenMapsInChainPotentiallyChildless: Seq[(Expr, Seq[Expr])] =
          pairElementsWithTails(mapsInNestingChain).map {
            case (outerMap, nestedMaps) => (outerMap, context.getPerfectlyNestedChildChain(outerMap +: nestedMaps))
          }
        val mapsAndTheirPerfectlyNestedChildrenMapsInChain =
          mapsAndTheirPerfectlyNestedChildrenMapsInChainPotentiallyChildless.filter {
            case (_, nestedMaps) if nestedMaps.nonEmpty => true
            case _ => false
          }

        if (mapsAndTheirPerfectlyNestedChildrenMapsInChain.isEmpty)
          None
        else Some(
          ParamConstraint(s"heu.nested.seqmaps.must.be.fused",
            "Heuristic: perfectly nested sequential maps must always be fused. This also applies to " +
              "indirectly nested maps, where intermediate maps are fused.",
            params = passParams.paramList.map(_.getAEVar).toList,
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
              import Code.SyntacticSugarArithExpr._
              implicit val a: Array[ArithExpr] = args.toArray

              def isFusedOrParallel(map: Expr): Boolean = isFused(map) || isParallel(map)

              mapsAndTheirPerfectlyNestedChildrenMapsInChain.forall {
                case (outerMap: Expr, Nil) =>
                  throw new IllegalStateException()

                case (outerMap: Expr, perfectlyNestedMap :: Nil) =>
                  isFusedOrParallel(outerMap) || /* Sequential outerMap, so the nested mapSeqs must be fused or parallel */
                    isFusedOrParallel(perfectlyNestedMap)

                case (outerMap: Expr, perfectlyNestedMaps: Seq[Expr]) =>
                  isFusedOrParallel(outerMap) || /* Sequential parentMap, so the nested mapSeqs must be fused or parallel */
                    perfectlyNestedMaps.forall(isFusedOrParallel)
              }
            },
            predicate = (model: Model, params: List[ArithExpr]) => {
              import Code.SyntacticSugarChoco._
              implicit val (m, p) = (model, params.toArray)

              def isFusedOrParallel(map: Expr) = isFused(map) or isParallel(map)

              and(
                mapsAndTheirPerfectlyNestedChildrenMapsInChain.map {
                  case (outerMap: Expr, Nil) =>
                    throw new IllegalStateException()

                  case (outerMap: Expr, perfectlyNestedMap :: Nil) =>
                    isFusedOrParallel(outerMap) or /* Sequential outerMap, so the nested mapSeqs must be fused or parallel */
                      isFusedOrParallel(perfectlyNestedMap)

                  case (outerMap: Expr, perfectlyNestedMaps: Seq[Expr]) =>
                    isFusedOrParallel(outerMap) or /* Sequential parentMap, so the nested mapSeqs must be fused or parallel */
                      and(perfectlyNestedMaps.map(isFusedOrParallel): _*)
                }: _*).post()
            }
          ))
      }).toVector

    //    fastestMovingMemDimMustCorrespondToFineGrainedParallelism // THIS IS BAD because when Transposes are used, the outer maps might actually iterate over inner dimensions in memory
    Vector(sequentialMapsMustBeFused).flatten
//    Vector()
  }

  def debuggingConstraints(context: ParASTContext,
                           passParams: RewritePassParams): Vector[ParamConstraint] = {
    implicit val c: ASTContext = context

    val disableMapGlbs = ParamConstraint("disableMapGlbs",
      "Disabled MapGlbs temporarily",
      params = passParams.paramList.map(_.getAEVar).toList,
      arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
        import Code.SyntacticSugarArithExpr._

        // NOT (MapGlb used)
        !args.exists(parDomain(_) == Cst(Code.mapGlb()))
      },

      predicate = (model: Model, params: List[ArithExpr]) => {
        import Code.SyntacticSugarChoco._
        implicit val (m, p) = (model, params.toArray)

        not(or(params.map(isMapGlb(_)): _*)
        ).post()
      })

    val moreMapWrgsMapLcl = ParamConstraint("minNumberofMapWrgMapLclPairs",
      s"Min number of MapWrg-MapLcl pairs for this lambda must be the maximum possible number " +
        s"(${context.maxNumberOfNestedMapLcls / 2})",
      params = passParams.paramList.map(_.getAEVar).toList,
      arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
        import Code.SyntacticSugarArithExpr._

        // MapWrg(0) (AND MapWrg(1) (AND MapWrg(2)))
        (0 until context.maxNumberOfNestedMapLcls).map(dim =>
          args.exists(a => parDomain(a) == Cst(Code.mapWrg()) && parDim(a) == Cst(Code.parDims(dim)()))
        ).reduce(_ && _)
      },

      predicate = (model: Model, params: List[ArithExpr]) => {
        import Code.SyntacticSugarChoco._
        implicit val (m, p) = (model, params.toArray)

        and(
          (0 until context.maxNumberOfNestedMapLcls).map(dim =>
            or(params.map(p => isMapWrg(p) and parDim(p) === parDims(dim)()): _*)
          ): _*).post()
      })

    val disableMapFuse = ParamConstraint("disableMapFuse",
      "Disabled MapFuse temporarily",
      params = passParams.paramList.map(_.getAEVar).toList,
      arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => {
        import Code.SyntacticSugarArithExpr._

        // NOT (MapFuse used)
        !args.contains(Cst(Code.replaceInnerMapWithOuter()))
      },

      predicate = (model: Model, params: List[ArithExpr]) => {
        import Code.SyntacticSugarChoco._
        implicit val (m, p) = (model, params.toArray)

        not(or(params.map(isFused(_)): _*)
        ).post()
      })

    //    Vector(disableMapGlbs, moreMapWrgsMapLcl)
    Vector(/*disableMapFuse*/)
  }

  def manualTransformationScheme(passParams: RewritePassParams): ListMap[Var, Cst] = {
    val mapTransforms = ListMap[String, Int](
//      "mapTransform.24." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.26." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.32." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.33." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.84." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.114." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.115." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.126." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.127." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.160." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.161." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.162." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.194." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.199." -> (Code.replaceInnerMapWithOuter())
      // Lu's L2 kernel
//      "mapTransform.24." -> (10*Code.mapGlb() + Code.parDims(2)()),
//      "mapTransform.25." -> (10*Code.mapGlb() + Code.parDims(0)()),
//      "mapTransform.26." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.63." -> (10*Code.mapGlb() + Code.parDims(1)()),
//      "mapTransform.238." -> (Code.replaceInnerMapWithOuter())

      // Our old best with old data layout (direct->GEMM too)
//      "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(1)()),
//      "mapTransform.30." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.51." -> (10*Code.mapWrg() + Code.parDims(2)()),
//      "mapTransform.52." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.68." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(0)())

    // Our old best with old data layout (direct->GEMM too) with 69 always fused with 70 for more par iters
//    "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(0)()),
//    "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(1)()),
//    "mapTransform.30." -> (Code.replaceInnerMapWithOuter()),
//    "mapTransform.51." -> (10*Code.mapWrg() + Code.parDims(2)()),
//    "mapTransform.52." -> (10*Code.mapLcl() + Code.parDims(2)()),
//    "mapTransform.68." -> (10*Code.mapLcl() + Code.parDims(1)()),
//    "mapTransform.69." -> (Code.replaceInnerMapWithOuter()),
//    "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(0)()),
//    "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(1)()),
//    "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(0)())



      // GEMM: old best from test/opencl/generator/matrixMultiplication/Best::maliGEMM()
//      "mapTransform.65." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.64." -> (0),
//      "mapTransform.30." -> (0),
//      "mapTransform.23." -> (0),
//      "mapTransform.21." -> (0),
//      "mapTransform.13." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.12." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.11." -> (0),
//      "mapTransform.4." -> (10*Code.mapGlb() + Code.parDims(1)()),
//      "mapTransform.1." -> (10*Code.mapGlb() + Code.parDims(0)())

      // GEMM: hand optimized (wrong!)
//      "mapTransform.125." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.124." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.88." -> (0),
//      "mapTransform.87." -> (0),
//      "mapTransform.79." -> (0),
//      "mapTransform.78." -> (0),
//      "mapTransform.70." -> (0),
//      "mapTransform.69." -> (0),
//      "mapTransform.49." -> (0),
//      "mapTransform.44." -> (0),
//      "mapTransform.43." -> (0),
//      "mapTransform.36." -> (0),
//      "mapTransform.35." -> (0),
//      "mapTransform.34." -> (0),
//      "mapTransform.30." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.29." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.15." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.14." -> (10*Code.mapWrg() + Code.parDims(1)())

      // Naive: outermost parallelism (not tested)
//      "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(2)()),
//      "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(1)()),
//      "mapTransform.30." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.51." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.52." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.68." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(0)())

      // debug_direct_autotune_4_cache_heur_0 (vectorizing illegally)
//       "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(0)()),
//       "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(1)()),
//       "mapTransform.30." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.51." -> (10*Code.mapWrg() + Code.parDims(2)()),
//       "mapTransform.52." -> (10*Code.mapLcl() + Code.parDims(2)()),
//       "mapTransform.53." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.68." -> (10*Code.mapLcl() + Code.parDims(1)()),
//       "mapTransform.69." -> (0),
//       "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(0)()),
//       "mapTransform.71." -> (0),
//       "mapTransform.72." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.79." -> (0),
//       "mapTransform.89." -> (0),
//       "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(1)()),
//       "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(0)()),
//       "mapTransform.113." -> (0),
//       "mapTransform.114." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.115." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.116." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.117." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.127." -> (0),
//       "mapTransform.128." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.129." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.130." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.164." -> (0),
//       "mapTransform.165." -> (0),
//       "mapTransform.166." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.169." -> (0),
//       "mapTransform.170." -> (0),
//       "mapTransform.171." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.208." -> (0),
//       "mapTransform.209." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.210." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.211." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.212." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.242." -> (0),
//       "mapTransform.243." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.244." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.245." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.283." -> (0),
//       "mapTransform.284." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.285." -> (Code.replaceInnerMapWithOuter()),
//       "mapTransform.286." -> (Code.replaceInnerMapWithOuter())

      // L8 (conv4_2), arm-compute-tuner/logs/benchmark_graph_vgg16_exhaustive_no_winograd_no_bias_2021-12-06-21-21-29.log
//      "mapTransform.74." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.73." -> (0),
//      "mapTransform.60." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.59." -> (0),
//      "mapTransform.51." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.50." -> (0),
//      "mapTransform.27." -> (0),
//      "mapTransform.25." -> (0),
//      "mapTransform.15." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.14." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.13." -> (0),
//      "mapTransform.12." -> (10*Code.mapGlb() + Code.parDims(0)()),
//      "mapTransform.10." -> (10*Code.mapGlb() + Code.parDims(1)())

      // debugging explore_parmappings_with_solver_for_cc22_shepherd_05.02.2022_17.44.27
//      "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.89." -> (0),
//      "mapTransform.79." -> (0),
//      "mapTransform.72." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.71." -> (0),
//      "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.69." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.68." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.53." -> (0),
//      "mapTransform.52." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.51." -> (10*Code.mapWrg() + Code.parDims(2)()),
//      "mapTransform.30." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(1)()),
//      "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(0)())


      // debugging explore_parmappings_with_solver_for_cc22_shepherd_3_496_10.02.2022_00.04.39/8/0
      // insertion bug
//      "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(1)()),
//      "mapTransform.30." -> (10*Code.mapWrg() + Code.parDims(2)()),
//      "mapTransform.51." -> (0),
//      "mapTransform.52." -> (0),
//      "mapTransform.53." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.68." -> (0),
//      "mapTransform.69." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.71." -> (0),
//      "mapTransform.72." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.79." -> (0),
//      "mapTransform.89." -> (0),
//      "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(1)())

      // debugging generated_files_refactored_cstrs_for_cc22_artifact_2_15.02.2022_21.15.13/8/31
      // insertion bug
//      "mapTransform.28." -> (10*Code.mapWrg() + Code.parDims(1)()),
//      "mapTransform.29." -> (10*Code.mapWrg() + Code.parDims(0)()),
//      "mapTransform.30." -> (10*Code.mapWrg() + Code.parDims(2)()),
//      "mapTransform.51." -> (0),
//      "mapTransform.52." -> (0),
//      "mapTransform.53." -> (10*Code.mapLcl() + Code.parDims(0)()),
//      "mapTransform.68." -> (0),
//      "mapTransform.69." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.70." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.71." -> (0),
//      "mapTransform.72." -> (Code.replaceInnerMapWithOuter()),
//      "mapTransform.79." -> (0),
//      "mapTransform.89." -> (0),
//      "mapTransform.109." -> (10*Code.mapLcl() + Code.parDims(1)()),
//      "mapTransform.110." -> (10*Code.mapLcl() + Code.parDims(2)()),
//      "mapTransform.113." -> (0)

    )
    // The rest are sequential UPD: or fused! or parallel!

    val (parMapParams, seqMapParams) = passParams.exprParams.values.partition(param =>
      mapTransforms.keys.exists(aKey => param.getFullName.startsWith(aKey)))

    ListMap(
      (if (debugRun)//(false)
        (parMapParams.map(param =>
          mapTransforms.find(mapTransform => param.getFullName.startsWith(mapTransform._1)) match {
            case None => throw new IllegalStateException()
            case Some(mapTransform) => param.getAEVar -> Cst(mapTransform._2)
          })
//          ++ seqMapParams.map(param => param.getAEVar -> Cst(Code.mapSeq()))
          ).toSeq
      else Seq()): _*
    )
  }


  def preprocessLambda(lambda: Lambda): Lambda = {

    // Replace all AbstractMaps with Maps to open them up to further rewriting
    val lambdaWithGenericMaps = Rewrite.applyRuleUntilCannot(lambda, Rule("Map* => Map", {
      case FunCall(m @ AbstractMap(f), args @ _*) if !m.isInstanceOf[Map] => Map(f).apply(args: _*)
    }))

    // Infer information about lambda required for constraint generation
    TypeChecker(lambdaWithGenericMaps)
    InferOpenCLAddressSpace(lambdaWithGenericMaps)

    lambdaWithGenericMaps
  }

  def apply(layerConfigIdx: Int,
            lambda: Lambda,
            initialSeed: Int
           )(implicit jsonSettings: ConvExplorationJSONSettings): Option[ConvAutoRewritePassSearchSpace] = {

    val preprocessedLambda = preprocessLambda(lambda)

//    if (!lambdaWithGeneralizedMapsLogged) {
//      globalNumbering = NumberExpression.depthFirst(preprocessedLambda.body) // NumberExpression.depthFirst(lambda.body)
//      logger.info("Lambda with generalized maps before parallelization/fusion:")
//      logger.info(preprocessedLambda.toString)
//      lambdaWithGeneralizedMapsLogged = true
//    }

    val rewritingPass: RewritingPass = new RewritingPass(
      ruleApplicationHelpers, paramsLabel = "mapTransform", preprocessedLambda, rewriteWhereverPossible = true)

    val context = ParASTContext(preprocessedLambda, rewritingPass)

    if (context.maxNumberOfNestedMapGlbs == 0 && context.maxNumberOfNestedMapLcls == 0) {
      logger.info("Could not initialize rewrite pass search space since the lambda cannot be parallelized")
      None
    } else {
//      globalNumbering = NumberExpression.depthFirst(preprocessedLambda.body)

//      globalNumbering = NumberExpression.breadthFirst(preprocessedLambda.body)
      globalNumbering = NumberExpression.depthFirst(preprocessedLambda.body)
      println("preprocessedLambda:")
      println(preprocessedLambda)
      val space = ConvAutoRewritePassSearchSpace(rewritingPass,
        independentParameters = manualTransformationScheme(rewritingPass.rewriteParams),

        inferredAndManualConstraints = parallelizationRules(context, rewritingPass.rewriteParams) ++
          heuristics(context, rewritingPass.rewriteParams) ++ debuggingConstraints(context, rewritingPass.rewriteParams))

      space.setSolver(ConstraintSolver.Choco, initialSeed)
//      space.setSolver(ConstraintSolver.Z3, initialSeed)

//      logger.info(s"Parallelization rules(${parRules.size}):\n${parRules.mkString("\n")}")
//      logger.info(s"Heuristics(${heur.size}):\n${heur.mkString("\n")}")
//      logger.info(s"Debugging constraints(${debugCstrs.size}):\n${debugCstrs.mkString("\n")}")
//      logger.info(s"Parallelization rules(${parRules.size})")
//      logger.info(s"Heuristics(${heur.size})")
//      logger.info(s"Debugging constraints(${debugCstrs.size})")

      logger.info(s"$name rewrite pass search space initialized")
      Some(space)
    }
  }

  def concretizeRule(regularRule: Rule): Rule =
    Rule(regularRule.desc, {
      val originalRewrite = regularRule.rewrite

      // Adds the isConcrete clause to the pattern
      { case e if originalRewrite.isDefinedAt(e) && (
        e match {
          case FunCall(fp: FPattern, _*) => fp.f.body.isConcrete
          case FunCall(l: Lambda, _*) => l.body.isConcrete
          case call: FunCall => call.isConcrete(visitArgs = false) // UserFun or Concat
          case _ => false
        }) =>
        originalRewrite(e)
      }
    })
}

package exploration

import exploration.ParamConstraints.greaterThanOrEqual
import exploration.PredicateWrappers._
import ir.ast._
import ir.{ArrayType, Size, TypeChecker}
import lift.arithmetic.{ArithExpr, Cst, SimplifiedExpr, Var}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object ParameterConstraintInference {

  def apply(lambda: Lambda): (Vector[Var], Vector[ParamConstraint]) = {
    val parameters = mutable.Set[Var]()
    val constraints = ListBuffer[ParamConstraint]()

    // First, infer all the types
    TypeChecker.check(lambda.body)

    // Traverse nodes in search for constraints
    Expr.visit(lambda.body, {

      case FunCall(split@Split(chunkSize), args@_*) if (args.head.t match {
        case _: ArrayType with Size => true
        case _ => false
      }) =>
        val argType = args.head.t match {
          case at: ArrayType with Size => at
          case _ => throw new IllegalArgumentException()
        }
        // Collect all variables
        val chunkSizeVars = ArithExpr.collectVars(chunkSize).sortBy(_.id)
        val argSizeVars = ArithExpr.collectVars(argType.size).sortBy(_.id)
        val allVars = (chunkSizeVars ++ argSizeVars).distinct

        if (allVars.nonEmpty) {
          // Update the list of parameters
          allVars.foreach(parameters.add)

          argType.size % chunkSize match {
            case Cst(0) =>
            // No constraint to add since argType.size is guaranteed to be divisible by chunkSize (e.g. nInputs % nInputs)
            case condition =>
              // Update the list of constraints
              constraints.append(ParamConstraint(
                name = s"splittableArgument_${split.gid}>=1",
                comment = s"In Split (gid=${split.gid}), the chunk size must be greater or equal than 1. " +
                  s"Argument size:\n${argType.size}\nChunk size:\n$chunkSize",
                params = List(chunkSize, Cst(1)),
                arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => greaterThanOrEqual(args.head, args(1)),
                predicate = ">="
              ))
              constraints.append(ParamConstraint(
                name = s"splittableArgument_${split.gid}",
                comment = s"In Split (gid=${split.gid}), the argument size has to be divisible by the chunk size. " +
                  s"Argument size:\n${argType.size}\nChunk size:\n$chunkSize",
                params = List(condition, Cst(0)),
                arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => args.head == args(1),
                predicate = "=="
              ))
          }
        }

      case FunCall(slide@Slide(windowSize, step), args@_*) if (args.head.t match {
        case _: ArrayType with Size => true
        case _ => false
      }) =>
        val argType: ArrayType with Size = args.head.t match {
          case at: ArrayType with Size => at
          case _ => throw new IllegalArgumentException()
        }
        // Collect all variables
        val windowSizeVars = ArithExpr.collectVars(windowSize).sortBy(_.id)
        val stepVars = ArithExpr.collectVars(step).sortBy(_.id)
        val argSizeVars = ArithExpr.collectVars(argType.size).sortBy(_.id)
//        val allVars = (windowSizeVars ++ stepVars ++ argSizeVars).distinct
        val allVars = (windowSizeVars ++ argSizeVars).distinct

        if (allVars.nonEmpty) {
          // Update the list of parameters
          allVars.foreach(parameters.add)
        }

        // Commenting out this rule since Slide uses integer division and hence can deal with sliding that results
        // partial sliding windows at the borders of an input
        if (allVars.nonEmpty) {
          // Update the list of constraints
//          constraints.append(new ParamConstraint(
//            name = s"slidableArgument_${slide.gid}",
//            comment = s"In Slide (gid=${slide.gid}), the argument size should be such that sliding can be performed " +
//              s"using a window of chosen size and stride. (argType.size - (windowSize - step)) % step = " +
//              s"(${argType.size} - ($windowSize - $step)) % $step == 0",
//            params = allVars.toVector,
//            lhs = (argType.size - (windowSize - step)) % step,
//            rhs = Cst(0),
//            predicate = (lhs: ArithExpr, rhs: ArithExpr) => {
//              lhs === rhs
//            }
//          ))
        }

        if (argSizeVars.nonEmpty || windowSizeVars.nonEmpty) {
          constraints.append(ParamConstraint(
            name = s"sliderWindowSize_${slide.gid}",
            comment = s"In Slide (gid=${slide.gid}), the argument size should be equal or bigger to that of the " +
              s"sliding window. ((argType.size = ${argType.size}) >= (windowSize = $windowSize))",
            params = List(argType.size, windowSize),
            arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => greaterThanOrEqual(args.head, args(1)),
            predicate = ">="
          ))
        }
        // TODO: Unslide for general case

//      case FunCall(slide@Slide(windowSize, step), args@_*) if (args.head.t match {
//        case _: ArrayType with Size => true
//        case _ => false
//      }) =>
      case FunCall(asVector(vectorLen), args @ _*) if (args.head.t match {
        case _: ArrayType with Size => true
        case _ => false
      }) =>

        val argType: ArrayType with Size = args.head.t match {
          case at: ArrayType with Size => at
          case _ => throw new IllegalArgumentException()
        }

        constraints.append(ParamConstraint(
          name = s"vectorisableBy_${vectorLen}",
          comment = s"The argument to asVector(${vectorLen}) must be a multiple of ${vectorLen}",
          params = List(argType.size % vectorLen, Cst(0)),
          arithExprPredicate = (args: List[ArithExpr with SimplifiedExpr]) => args.head == args(1),
          predicate = "=="
        ))

        // TODO: other FunCalls
      // TODO: add Reorder handling
      case _ =>
    }, _ => Unit)

    (parameters.toVector, constraints.toVector)
  }
}

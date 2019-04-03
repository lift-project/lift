package ir.ast

import lift.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.interpreter.Interpreter._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection._

/**
 * Iterate pattern.
 * Code for this pattern can be generated.
 *
 * The iterate pattern has the following high-level semantics:
 *   `Iterate(0, f)( xs ) = xs`
 *   `Iterate(n, f)( xs ) = Iterate(n-1, f)( f(xs) )`
 *
 * The iterate pattern has the following type:
 *   `Iterate(n, f) : [a],,m,, -> [a],,F^n^(m),,`
 * where `n: Int` and `f: [a],,k,, -> [a],,F(k),,`.
 *
 * We know the following algorithmic rewrite rules for the iterate pattern
 * (so far):
 *  - Iterate(n+m, f) => Iterate(n, f) o Iterate(m, f)
 *
 * @param n Number of times to iterate
 * @param f Lambda to be iterated
 * @param vPtrIn the variable representing the pointer for reading the input
 * @param vPtrOut the variable representing the pointer for writing the output
 */
case class Iterate(n: ArithExpr, f: Lambda, var loopVar : Var = PosVar("i"), vPtrIn: Var = Var("inPtr"), vPtrOut: Var = Var("outPtr")) extends Pattern(arity = 1)
                                                     with FPattern {
  var iterationCount: ArithExpr = ?

  var swapBuffer: Memory = UnallocatedMemory

  var indexVar = PosVar("i")

  override def copy(f: Lambda): Pattern = Iterate(n, f)

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    args.head match {
      case v: Vector[_] =>
        (1 to n.eval).foldLeft[Any](v)((a, _) => {
          f.eval(valueMap, a)
        })
    }
  }

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at: ArrayType with Size with Capacity =>
        // perform simple cases where we don't need to do the closed form iterate to work out
        // the output type
        f.params(0).t = argType
        if (
             // check if we iterate only once, than the type of the body is the type of iterate
             n == Cst(1) |
             // perform a simple (and hopefully quick!) check to see if the
             // input/output types of the nested function match
                 TypeChecker.check(f.body, setType=false) == argType) {
          // return the type of the body.
          return TypeChecker.check(f.body)
        } else {
          // reset the input parameter to "UndefType" ready for closed form
          // iterate checking
          f.params(0).t = UndefType
        }
        // substitute all the expression in the input type with type variables
        val initialTvValMap = scala.collection.mutable.HashMap[TypeVar, ArithExpr]()
        var inputTypeWithTypeVar = Type.visitAndRebuild(at, t => t, {
          case at: ArrayType with Size with Capacity =>
            val inLenTV = TypeVar(StartFromRange(1))
            initialTvValMap += inLenTV -> at.size
            ArrayTypeWSWC(at.elemT, inLenTV)
          case _:ArrayType => throw new NotImplementedException
          case t: Type => t
        })

        if (f.params.length != 1) throw new NumberOfArgumentsException
        f.params(0).t = inputTypeWithTypeVar
        val outputTypeWithTypeVar = TypeChecker.check(f.body, setType = false)

        // find all the type variable in the output type
        val outputTvSet = TypeVar.getTypeVars(outputTypeWithTypeVar)

        // put back the expression when the type variable is not present
        val fixedTvMap = initialTvValMap -- outputTvSet
        inputTypeWithTypeVar =
          Type.substitute(inputTypeWithTypeVar, fixedTvMap.toMap)

        // assign the type for f
        TypeChecker.check(f.body)

        val closedFormInOutType = closedFormIterate(inputTypeWithTypeVar,
                                                    outputTypeWithTypeVar,
                                                    n, initialTvValMap)
        // patch up the input type of the f (new type variables with range information may have been produced)
        f.params(0).t = closedFormInOutType._1
        TypeChecker.check(f.body)

        Type.substitute(closedFormInOutType._2, initialTvValMap.toMap)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  /**
    * Returns the new input and output types after calculating the closed form
    *
    * @param inT The input type of the function to be iterated
    * @param ouT The output type of the function to be iterated
    * @param n The number of iteration steps
    * @param initialTvValMap The initial mapping of type variables to arithmetic expressions.
    *                        These type variables might appear in the input and output types.
    * @return A pair of input and output type after calculating the closed form iteration of `n` iteration steps
    */
  private def closedFormIterate(inT: Type,
                                ouT: Type,
                                n: ArithExpr,
                                initialTvValMap: mutable.HashMap[TypeVar, ArithExpr]): (Type,Type) = {
    (inT,ouT) match {
      case (ArrayTypeWSWC(_,inS,inC), ArrayTypeWSWC(_,outS,outC)) if inS == inC && outS == outC =>
          val inLen = inS
          val outLen = outS

          inLen match {
            case inLenTV: TypeVar =>
              if (inLen == outLen) {
                // the input type of the function inside the iterate is independent from the number of iterations
                return (Type.substitute(inT,inLenTV, initialTvValMap(inLenTV)),
                        Type.substitute(ouT,inLenTV, initialTvValMap(inLenTV)))
              }
              if (!ArithExpr.contains(outLen, inLenTV))
                // output independent of any type variables
                return (Type.substitute(inT,inLenTV, initialTvValMap(inLenTV)),ouT)

              val computingNewTypes = (computeLength: ArithExpr => ArithExpr) => {
                // fix the range for inLenTV
                val (min, max) = ArithExpr.minmax(initialTvValMap(inLenTV),
                                                  computeLength(initialTvValMap(inLenTV)))
                //// TODO: deal with growing output size
                //inLenTV.range = ContinuousRange(min,max)
                val inLenTVWithRange = TypeVar(ContinuousRange(min, max + 1))
                initialTvValMap += ((inLenTVWithRange, initialTvValMap(inLenTV)))
                val substOuT = Type.substitute(ouT, inLenTV, inLenTVWithRange).asInstanceOf[ArrayType]
                val substInT = Type.substitute(inT, inLenTV, inLenTVWithRange).asInstanceOf[ArrayType]

                // we have outLen*inLenTV where inLenTV is not present inside outLen

                val len = computeLength(inLenTVWithRange)

                val (inElemsT, outElemsT) = closedFormIterate(substInT.elemT, substOuT.elemT, n, initialTvValMap)
                (ArrayTypeWSWC(inElemsT, inLenTVWithRange), ArrayTypeWSWC(outElemsT, len))
              }

              val a = outLen /^ inLenTV
              if (!ArithExpr.contains(a, inLenTV)) {
                return computingNewTypes( inLength => (a pow n) * inLength )
              }

              val b = outLen - inLenTV
              if (!ArithExpr.contains(b, inLenTV)) {
                return computingNewTypes( inLength =>  (n * b) + inLength )
              }

              // if nothing has matched yet throw
              throw TypeException(
                "Cannot infer closed form for iterate return type " +
                s"(only support x*a). inT = $inT, ouT = $ouT"
              )
            case _ =>
              throw TypeException(
                "Cannot infer closed form for iterate return type " +
                s"(only support x*a). inT = $inT, ouT = $ouT"
              )
          }

      case (_ : ArrayType, _ : ArrayType) => throw new NotImplementedError()

      case (inTT:TupleType, outTT:TupleType) =>

        val inOutElemsT = inTT.elemsT.zip(outTT.elemsT).map({case (tIn,tOut) =>
            closedFormIterate(tIn,tOut,n,initialTvValMap)}).unzip
        (TupleType(inOutElemsT._1:_*), TupleType(inOutElemsT._2:_*))

      case _ =>
        if (inT == ouT)
        // the input type of the function inside the iterate is independent from the number of iterations
          (Type.substitute(inT, initialTvValMap.toMap), Type.substitute(ouT,initialTvValMap.toMap))
        else throw TypeException(
          "Cannot infer closed form for iterate return type " +
          s"(only support x*a). inT = $inT, ouT = $ouT"
        )
    }
  }


  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode =
    Iterate(n, f.visitAndRebuild(pre,post).asInstanceOf[Lambda], loopVar, vPtrIn, vPtrOut )
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda1) => Iterate) =
    (f: Lambda1) => Iterate(n ,f)

  def varName(): String = "iterSize"
}

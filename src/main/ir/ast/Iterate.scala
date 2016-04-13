package ir.ast

import apart.arithmetic._
import arithmetic.TypeVar
import ir._
import ir.interpreter.Interpreter._

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
 */
case class Iterate(n: ArithExpr, f: Lambda) extends Pattern(arity = 1)
                                                     with FPattern
                                                     with isGenerable {
  var iterationCount: ArithExpr = ?

  var swapBuffer: Memory = UnallocatedMemory

  var indexVar = Var("i", RangeUnknown)

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
      case at: ArrayType =>
        // perform a simple (and hopefully quick!) check to see if the
        // input/output types of the nested function match.
        // If they do, we don't need to do the closed form iterate to work out
        // the output type
        f.params(0).t = argType
        if(TypeChecker.check(f.body, setType=false) == argType) {
          //if they match, check the body as normal, and return that type.
          return TypeChecker.check(f.body, setType=true)
        } else {
          //reset the input parameter to "UndefType" ready for closed form
          // iterate checking
          f.params(0).t = UndefType
        }
        // substitute all the expression in the input type with type variables
        val tvMap = scala.collection.mutable.HashMap[TypeVar, ArithExpr]()
        var inputTypeWithTypeVar = Type.visitAndRebuild(at, t => t, {
          case at: ArrayType =>
            val tv = TypeVar()
            tvMap += tv -> at.len
            new ArrayType(at.elemT, tv)
          case t: Type => t
        })

        if (f.params.length != 1) throw new NumberOfArgumentsException
        f.params(0).t = inputTypeWithTypeVar
        val outputTypeWithTypeVar = TypeChecker.check(f.body, setType = false)

        // find all the type variable in the output type
        val outputTvSet = scala.collection.mutable.HashSet[TypeVar]()
        Type.visit(outputTypeWithTypeVar, t => {}, {
          case at: ArrayType => outputTvSet ++= TypeVar.getTypeVars(at.len)
          case vt: VectorType => outputTvSet ++= TypeVar.getTypeVars(vt.len)
          case _ =>
        })

        // put back the expression when the type variable is not present
        val fixedTvMap = tvMap -- outputTvSet
        inputTypeWithTypeVar =
          Type.substitute(inputTypeWithTypeVar, fixedTvMap.toMap)

        // assign the type for f
        TypeChecker.check(f.body, setType = true)

        val closedFormOutputType = closedFormIterate(inputTypeWithTypeVar,
                                                     outputTypeWithTypeVar,
                                                     n, tvMap)
        Type.substitute(closedFormOutputType, tvMap.toMap)

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }

  private def closedFormIterate(inT: Type,
                                ouT: Type,
                                n: ArithExpr,
                                tvMap: mutable.HashMap[TypeVar,
                                                       ArithExpr]): Type = {
    (inT,ouT) match {
      case (inAT : ArrayType, outAT : ArrayType) =>
        val closedFormLen = {
          val inLen = inAT.len
          val outLen = outAT.len

          inLen match {
            case tv: TypeVar =>
              if (inLen == outLen) {
                tv.range = ContinuousRange(tvMap.get(tv).get, tvMap.get(tv).get)
                return ouT
              }
              // recognises output independent of tv
              if (!ArithExpr.contains(outLen, tv))
                return ouT

              // recognises outLen*tv
              val a = outLen /^ tv
              if (!ArithExpr.contains(a, tv)) {

                // fix the range for tv
                // TODO: Pow(a, n) or Pow(a, n-1)???
                val (min, max) = ArithExpr.minmax(tvMap.get(tv).get,
                                                  (a pow n)*tvMap.get(tv).get)
                // TODO: deal with growing output size
                tv.range = ContinuousRange(min,max)

                // we have outLen*tv where tv is not present inside outLen
                (a pow n)*tv
              }
              else throw new TypeException("Cannot infer closed form for" +
                "iterate return type (only support x*a). inT = " + inT +
                " ouT = " + ouT)
            case _ => throw new TypeException("Cannot infer closed form for " +
              "iterate return type. inT = " + inT + " ouT = " + ouT)
          }
        }

        new ArrayType(closedFormIterate(inAT.elemT, outAT.elemT, n, tvMap),
                      closedFormLen)


      case (inTT:TupleType, outTT:TupleType) =>
        new TupleType( inTT.elemsT.zip(outTT.elemsT)
                                  .map({case (tIn,tOut) =>
                                      closedFormIterate(tIn,tOut,n,tvMap)} ):_*)

      case _ =>
        if (inT == ouT) ouT
        else throw new TypeException("Cannot infer closed form for iterate " +
          "return type. inT = "+inT+" ouT = "+ouT)
    }
  }
}

object Iterate {
  def apply(n: ArithExpr): ((Lambda1) => Iterate) =
    (f: Lambda1) => Iterate(n ,f)

  def varName(): String = "iterSize"
}

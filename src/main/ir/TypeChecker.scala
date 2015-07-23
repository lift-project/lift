package ir

import apart.arithmetic.{ContinuousRange, Cst, ArithExpr}
import arithmetic._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

object TypeChecker {

  def check(expr: Expr, setType: Boolean = true): Type = {

    var inferredOuT = expr match {
      case param: Param => checkParam(param)
      case call: FunCall => checkFunCall(call, setType)
    }

    inferredOuT = inferredOuT match {
      case ArrayType(et, len) => ArrayType(et, len)
      case _ => inferredOuT
    }

    if (setType)
      expr.t = inferredOuT

    inferredOuT
  }

  private def checkParam(param: Param): Type = {
    param match {
      case pr: ParamReference => Type.getTypeAtIndex(pr.p.t, pr.i)
      case p: Param => p.t
    }
  }

  private def checkFunCall(call: FunCall, setType: Boolean): Type = {
    assert(call.f != null)

    val inT = getInTFromArgs(call, setType)

    call.f match {
      case l: Lambda =>           checkLambda(l, call, inT, setType)
      case am: AbstractMap =>     checkMap(am, inT, setType)
      case ar: AbstractPartRed => checkReduce(ar, inT, setType)
      case cf: CompFun =>         checkCompFunDef(cf, inT, setType)
      case z: Zip =>              checkZip(z, inT, setType)
      case t: Tuple =>            checkTuple(t, inT, setType)
      case uz: Unzip =>           checkUnzip(uz, inT, setType)
      case Split(n) =>            checkSplit(n, inT)
      case _: Join =>             checkJoin(inT)
      case _: asScalar  =>        checkAsScalar(inT)
      case asVector(n) =>         checkAsVector(n, inT)
      case uf: UserFun =>         checkUserFunDef(uf, inT)
      case tP: toPrivate =>       checkToPrivate(tP, inT, setType)
      case tL: toLocal =>         checkToLocal(tL, inT, setType)
      case tG: toGlobal =>        checkToGlobal(tG, inT, setType)
      case i: Iterate =>          checkIterate(i, inT)
      case _: Transpose =>        checkTranspose(inT)
      case _: TransposeW =>       checkTranspose(inT)
      case f: Filter =>           checkFilter(f, inT, setType)
      case g: Group =>            checkGroup(g, inT)
      case h: Head =>             checkHead(inT)
      case t: Tail =>             checkTail(inT)
      case Barrier() | Gather(_) | Scatter(_) | Epsilon() => inT
    }
  }

  private def getInTFromArgs(call: FunCall, setType: Boolean): Type = {
    if (call.args.isEmpty) {
      NoType
    } else if (call.args.length == 1) {
      check(call.args.head, setType)
    } else {
      TupleType( call.args.map(check(_, setType)):_* )
    }
  }

  private def checkLambda(l: Lambda, call: FunCall, inT: Type, setType: Boolean): Type = {
    assert(call.args.nonEmpty)
    if (call.args.length == 1) {
      if (l.params.length != 1) throw new NumberOfArgumentsException
      l.params(0).t = inT
    } else {
      val tt = inT match { case tt: TupleType => tt }
      if (l.params.length != tt.elemsT.length) throw new NumberOfArgumentsException

      (l.params zip tt.elemsT).foreach({case (p,t) => p.t = t })
    }
    check(l.body, setType)
  }

  private def checkMap(am: AbstractMap, inT: Type, setType: Boolean): Type = {
    inT match {
      case at: ArrayType =>
        if (am.f.params.length != 1) throw new NumberOfArgumentsException
        am.f.params(0).t = Type.getElemT(inT)
        ArrayType(check(am.f.body, setType), Type.getLength(inT))
//      case mt: MatrixType =>
//        if (am.f.params.length != 1) throw new NumberOfArgumentsException
//        am.f.params(0).t = Type.getElemT(inT)
//        //recursively check the type - this won't work on nested maps
//        //(e.g. mapWrg over rows, then mapLcl over elements)
//        //as it's not expecting to be passed to nested maps
//        MatrixType(check(am.f.body, setType), mt.dx, mt.dy)
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkReduce(ar: AbstractPartRed, inT: Type, setType: Boolean): Type = {
    inT match {
      case tt: TupleType =>
        if (tt.elemsT.length != 2) throw new NumberOfArgumentsException // get the input type to the reduction, got to be tuple of array + initial val
      val initT = tt.elemsT.head //get the type of the initial value
      val elemT = Type.getElemT(tt.elemsT(1)) // get the type of the array values to the reduce
        if (ar.f.params.length != 2) throw new NumberOfArgumentsException // make sure our function has two args
        ar.f.params(0).t = initT // initial elem type
        ar.f.params(1).t = elemT // array element type
        check(ar.f.body, setType) // check the body - setting the type
        ArrayType(initT, new Cst(1)) // return a constant length array
      case _ => throw new TypeException(inT, "TupleType")
    }
  }

  private def checkCompFunDef(cf: CompFun, inT: Type, setType: Boolean): Type = {
    // combine the parameter of the first function to call with the type inferred from the argument

    cf.funs.foldRight(inT)((f, inputT) => {
      if (f.params.length != 1) throw new NumberOfArgumentsException
      f.params(0).t = inputT
      check(f.body, setType)
    })
  }

  private def checkZip(z: Zip, inT: Type, setType: Boolean): Type = {
    inT match {
      case tt: TupleType =>
        if (tt.elemsT.length < 2) throw new NumberOfArgumentsException

        tt.elemsT.map({
                        case at: ArrayType => at
                        case t => throw new TypeException(t, "ArrayType")
                      })

        val arrayTypes = tt.elemsT.map(_.asInstanceOf[ArrayType])

        if (arrayTypes.map(_.len).distinct.length != 1) {
          Console.err.println("Warning: can not statically prove that sizes (" + tt.elemsT.mkString(", ") + ") match!")
          // throw TypeException("sizes do not match")
        }
        ArrayType(TupleType(arrayTypes.map(_.elemT):_*), arrayTypes.head.len)
      case _ => throw new TypeException(inT, "TupleType")
    }
  }

  private def checkTuple(t: Tuple, inT: Type, setType: Boolean): Type = {
    inT match {
      case tt: TupleType =>
        if (tt.elemsT.length < 2) throw new NumberOfArgumentsException

        tt
      case _ => throw new TypeException(inT, "TupleType")
    }
  }

  private def checkUnzip(uz: Unzip, inT: Type, setType: Boolean): Type = {
    inT match {
      case at: ArrayType =>
        val tt = at.elemT match {
          case tt: TupleType => tt
          case _ => throw new TypeException(at.elemT, "TupleType")
        }

        TupleType( tt.elemsT.map(t => ArrayType(t, at.len)):_* )
      case _ => throw new TypeException(inT, "TupleType")
    }
  }

  private def checkJoin(inT: Type): Type = {
    inT match {
      case at0: ArrayType => at0.elemT match {
        case at1: ArrayType => ArrayType(at1.elemT, at0.len * at1.len)
        case _ => throw new TypeException(at0.elemT, "ArrayType")
      }
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkGroup(group: Group, inT: Type): Type = {
    inT match {
      case at: ArrayType =>
        assert(group.arity == 1)
        group.paramType = ArrayType(ArrayType(at.elemT, group.relIndices.length), at.len)
        group.paramType

      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkSplit(n: ArithExpr, inT: Type): Type = {
    inT match {
      case at: ArrayType => ArrayType(ArrayType(at.elemT, n), at.len /^ n)
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkHead(inT: Type): Type = {
    inT match {
      case at: ArrayType =>
        ArrayType(at.elemT, Cst(1))
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkTail(inT: Type): Type = {
    inT match {
      case at: ArrayType =>
        if(at.len == Cst(1)) {
          ArrayType(at.elemT, Cst(1))
        }else {
          ArrayType(at.elemT, at.len - 1)
        }
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkAsScalar(inT: Type): Type = {
    inT match {
      case at: ArrayType => Type.asScalarType(at)
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkAsVector(n: ArithExpr, inT: Type): Type = {
    inT match {
      case at: ArrayType => at.vectorize(n)
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkUserFunDef(uf: UserFun, inT: Type): Type = {
    val substitutions = Type.reify(uf.inT, inT)
    Type.substitute(uf.outT, substitutions.toMap)
  }

  private def checkFilter(f: Filter, inT: Type, setType: Boolean): Type = {
    inT match {
      case TupleType(ArrayType(t, n), ArrayType(Int, m)) =>
        ArrayType(t, m)
      case _ => throw new TypeException(inT, "TupleType(ArrayType(_, _), ArrayType(Int, _))")
    }
  }

  private def checkToPrivate(tP: toPrivate, inT: Type, setType: Boolean): Type = {
    if (tP.f.params.length != 1) throw new NumberOfArgumentsException
    tP.f.params(0).t = inT
    check(tP.f.body, setType)
  }

  private def checkToLocal(tL: toLocal, inT: Type, setType: Boolean): Type = {
    if (tL.f.params.length != 1) throw new NumberOfArgumentsException
    tL.f.params(0).t = inT
    check(tL.f.body, setType)
  }

  private def checkToGlobal(tG: toGlobal, inT: Type, setType: Boolean): Type = {
    if (tG.f.params.length != 1) throw new NumberOfArgumentsException
    tG.f.params(0).t = inT
    check(tG.f.body, setType)
  }

  private def checkIterate(i: Iterate, inT: Type): Type = {
    inT match {
      case at: ArrayType =>
        //perform a simple (and hopefully quick!) check to see if the input/output types
        //of the nested function match. If they do, we don't need to do the closed
        //form iterate to work out the output type
        i.f.params(0).t = inT
        if(check(i.f.body, setType=false) == inT)
        {
          //if they match, check the body as normal, and return that type.
          return check(i.f.body, setType=true)
        }else{
          //reset the input parameter to "UndefType" ready for closed form iterate checking
          i.f.params(0).t = UndefType
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

        if (i.f.params.length != 1) throw new NumberOfArgumentsException
        i.f.params(0).t = inputTypeWithTypeVar
        val outputTypeWithTypeVar = check(i.f.body, setType = false)

        // find all the type variable in the output type
        val outputTvSet = scala.collection.mutable.HashSet[TypeVar]()
        Type.visit(outputTypeWithTypeVar, t => {}, {
          case at: ArrayType => outputTvSet ++= TypeVar.getTypeVars(at.len)
          case vt: VectorType => outputTvSet ++= TypeVar.getTypeVars(vt.len)
          case _ =>
        })

        // put back the expression when the type variable is not present
        val fixedTvMap = tvMap -- outputTvSet
        inputTypeWithTypeVar = Type.substitute(inputTypeWithTypeVar, fixedTvMap.toMap)

        // assign the type for f
        check(i.f.body, setType = true)

        val closedFormOutputType = closedFormIterate(inputTypeWithTypeVar, outputTypeWithTypeVar, i.n, tvMap)
        Type.substitute(closedFormOutputType, tvMap.toMap)

      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  def checkTranspose(t: Type): Type = {
    t match {
      case outer: ArrayType =>
        outer.elemT match {
          case inner: ArrayType =>
            val T = inner.elemT
            val innerLen = inner.len
            val outerLen = outer.len
            new ArrayType(new ArrayType(T, outerLen), innerLen)

          case _ => throw new TypeException(outer.elemT, "ArrayType")
        }
      case _ => throw new TypeException(t, "ArrayType")
    }
  }

  private def closedFormIterate(inT: Type, ouT: Type, n: ArithExpr, tvMap : scala.collection.mutable.HashMap[TypeVar, ArithExpr]) : Type = {
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
                val (min, max) = ArithExpr.minmax(tvMap.get(tv).get, (a pow n)*tvMap.get(tv).get)
                // TODO: deal with growing output size
                tv.range = ContinuousRange(min,max)

                // we have outLen*tv where tv is not present inside outLen
                (a pow n)*tv
              }
              else throw new TypeException("Cannot infer closed form for iterate return type (only support x*a). inT = " + inT + " ouT = " + ouT)
            case _ => throw new TypeException("Cannot infer closed form for iterate return type. inT = " + inT + " ouT = " + ouT)
          }
        }

        new ArrayType(closedFormIterate(inAT.elemT, outAT.elemT, n, tvMap), closedFormLen)


      case (inTT:TupleType, outTT:TupleType) =>
        new TupleType(inTT.elemsT.zip(outTT.elemsT).map({case (tIn,tOut) => closedFormIterate(tIn,tOut,n, tvMap)} ) :_*)

      case _ => if (inT == ouT) ouT else throw new TypeException("Cannot infer closed form for iterate return type. inT = "+inT+" ouT = "+ouT)
    }
  }

}

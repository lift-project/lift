package ir

import arithmetic._
import ir.ast._
import opencl.ir._
import opencl.ir.ast._

import scala.collection.{immutable, mutable}


case class TypeException(msg: String) extends Exception(msg) {
  def this() = this("")
  def this(found: Type, expected: String) = this(found + " found but " + expected + " expected")
  def this(found: Type, expected: Type) = this(found + " found but " + expected + " expected")

}

case class NumberOfArgumentsException(msg: String) extends Exception(msg) {
  def this()  = this("number of arguments is wrong!")
}


sealed abstract class Type {
  private def asVector(at0: ArrayType, len: ArithExpr): Type = {
    at0.elemT match {
      case pt:ScalarType => new ArrayType(new VectorType(pt,len), at0.len/^len)
      case at1:ArrayType => new ArrayType(asVector(at1,len), at0.len)
      case _ => throw new TypeException(at0.elemT, "ArrayType or PrimitiveType")
    }
  }

  /**
   * Vectorize the current type
   * @param n The vectori width
   * @return A vectorized type
   */
  def vectorize(n: ArithExpr): Type = this match {
    case sT: ScalarType => new VectorType(sT, n)
    case tT: TupleType => new TupleType( tT.elemsT.map( _.vectorize(n) ):_* )
    case aT: ArrayType => asVector(aT, n)
    case v: VectorType => v
    case _ => throw new TypeException(this, "anything else")
  }
}

case class ScalarType(name: String, size: ArithExpr) extends Type {
  override def toString = name
}

// TODO: Is the VectorType OpenCL specific? If yes -> move to opencl.ir package
case class VectorType(scalarT: ScalarType, len: ArithExpr) extends Type

case class TupleType(elemsT: Type*) extends Type {
  override def toString = "Tuple(" + elemsT.map(_.toString).reduce(_ + ", " + _) + ")"
}

case class ArrayType(elemT: Type, len: ArithExpr) extends Type {
  override def toString = "Arr(" +elemT+","+len+ ")"
}

case class MatrixType(elemT: Type, dx: ArithExpr, dy: ArithExpr) extends Type {
  override def toString = "Matrix("+elemT+","+dx+","+dy+")"
  def len = dx*dy //is this an acceptable length calculation? Why do we want the length?
}

//case class UnboundArrayType(et: Type, te: TypeExpr) extends ArrayType(et)
//case class BoundArrayType(et: Type, n: Int) extends ArrayType(et)

object UndefType extends Type {override def toString = "UndefType"}

object NoType extends Type {override def toString = "NoType"}

object Type {

  def getBaseType(t: Type): Type = {
    t match {
      case at: ArrayType  => getBaseType(at.elemT)
      case vt: VectorType => vt.scalarT
      case _ => t
    }
  }

  def getValueType(t: Type): Type = {
    t match {
      case at: ArrayType  => getValueType(at.elemT)
      case _ => t
    }
  }

  def name(t: Type): String = {
    t match {
      case st: ScalarType => st.name
      case vt: VectorType => vt.scalarT.name + vt.len.toString
      case tt: TupleType  => "Tuple_" + tt.elemsT.map(Type.name).reduce(_+"_"+_)
      case at: ArrayType  => "Array_" + Type.name(at.elemT)
      case mt: MatrixType => "Matrix_"+ Type.name(mt.elemT)
    }
  }

  def visitRebuild(t: Type, pre: (Type) => (Type), post: (Type) => (Type)) : Type = {
    var newT = pre(t)
    newT = newT match {
      case at: ArrayType => new ArrayType(visitRebuild(at.elemT, pre, post), at.len)
      case mt: MatrixType => new MatrixType(visitRebuild(mt.elemT, pre, post),mt.dx, mt.dy)
      case vt: VectorType => new VectorType(visitRebuild(vt.scalarT, pre, post).asInstanceOf[ScalarType],vt.len)
      case tt: TupleType => new TupleType(tt.elemsT.map(et => visitRebuild(et,pre,post)):_*)
      case _ => newT // nothing to do
    }
    post(newT)
  }

  def visit(t: Type, pre: (Type) => (Unit), post: (Type) => (Unit)) : Unit = {
    pre(t)
    t match {
      case at: ArrayType => visit(at.elemT, pre, post)
      case mt: MatrixType => visit(mt.elemT, pre, post)
      case vt: VectorType => visit(vt.scalarT, pre, post)
      case tt: TupleType => tt.elemsT.foreach(et => visit(et,pre,post))
      case _ => // nothing to do
    }
    post(t)
  }

  def getElemT(t: Type): Type = {
    t match {
      case at: ArrayType => at.elemT
      case mt: MatrixType => mt.elemT
      case vt: VectorType => vt.scalarT
      case _ => throw new TypeException(t, "ArrayType")
    }
  }

  def getSize(t: Type) : ArithExpr = {
    t match {
      case st: ScalarType => st.size
      case vt: VectorType => vt.scalarT.size * vt.len
      case tt: TupleType => tt.elemsT.map(getSize).reduce(_+_)
      case at: ArrayType => at.len * getSize(at.elemT)
      case _ => throw new TypeException(t, "ArrayType")
    }
  }

  def getLength(t: Type) : ArithExpr = {
    t match {
      case at: ArrayType => at.len
      case mt: MatrixType => mt.len
      case st: ScalarType => Cst(1)
      case vt: VectorType => getVectorSize(vt)
      case tt: TupleType => Cst(1)
      case _ => throw new TypeException(t, "ArrayType")
    }
  }
  def getWidth(t: Type):ArithExpr = {
    t match {
      case at: ArrayType => at.len
      case mt: MatrixType => mt.dx
      case st: ScalarType => Cst(1)
      case vt: VectorType => Cst(1)
      case tt: TupleType => Cst(1)
      case _ => throw new TypeException(t, "MatrixType")
    }
  }
  def getHeight(t: Type):ArithExpr = {
    t match {
      case at: ArrayType => Cst(1)
      case mt: MatrixType => mt.dy
      case st: ScalarType => Cst(1)
      case vt: VectorType => Cst(1)
      case tt: TupleType => Cst(1)
      case _ => throw new TypeException(t, "MatrixType")
    }
  }

  def getLengths(t: Type): Seq[ArithExpr] = {
    t match {
      case at: ArrayType => Seq(at.len) ++ getLengths(at.elemT)
      case mt: MatrixType => Seq(mt.len) ++ getLengths(mt.elemT)
      case _ => Seq(getLength(t))
    }
  }

  def getVectorSize(t: Type): ArithExpr = {
    t match {
      case vt: VectorType => vt.len
      case _ => Cst(1)
    }
  }

  def changeLength(t: Type, length: ArithExpr): Type = {
    t match {
      case at: ArrayType => ArrayType(at.elemT, length)
      case st: ScalarType => ScalarType(st.name, length)
      case t: Type => t
    }
  }

  private def asScalar(at0: ArrayType): Type = {
    at0.elemT match {
      case vt:VectorType => new ArrayType(vt.scalarT,at0.len*vt.len)
      case at:ArrayType =>  new ArrayType(asScalar(at),at0.len)
      case _ => throw new TypeException(at0.elemT , "ArrayType or VectorType")
    }
  }

  def length(t: Type, array: Array[ArithExpr] = Array.empty[ArithExpr]) : Array[ArithExpr] = {
    t match {
      case ArrayType(elemT, len) => Type.length(elemT, array :+ len)
      case MatrixType(elemT, dx, dy) => Type.length(elemT, array :+ (dx*dy))
      case TupleType(_) => throw new TypeException(t, "ArrayType")
      case VectorType(_, len) => array :+ len
      //throw new TypeException(t, "ArrayType") // TODO: Think about what to do with vector types
      case _ => array
    }
  }

  def reifyExpr(e1: ArithExpr, e2: ArithExpr) : immutable.Map[TypeVar, ArithExpr] = {
    val result = mutable.Map[TypeVar, ArithExpr]()
    (e1,e2) match {
      case (tv: TypeVar, _) => result += (tv->e2)
      case (_, tv: TypeVar) => result += (tv->e1)
      case _ => // todo check that the two expressions are equivalent
    }
    result.toMap
  }

  // Throw an exception if the two types do not match
  def reify(t1: Type, t2: Type): immutable.Map[TypeVar, ArithExpr] = {
    val result = mutable.Map[TypeVar, ArithExpr]()
    (t1, t2) match {
      case (at1: ArrayType, at2: ArrayType) => result ++= reifyExpr(at1.len, at2.len) ++= reify(at1.elemT, at2.elemT)
      case (tt1: TupleType, tt2: TupleType) => result ++= tt1.elemsT.zip(tt2.elemsT).foldLeft(mutable.Map[TypeVar, ArithExpr]())((map, types) => map ++= reify(types._1, types._2))
      case (vt1: VectorType, vt2: VectorType) => result ++= reifyExpr(vt1.len, vt2.len)
      case _ => if (t1 != t2) throw new TypeException(t1, t2)
    }
    result.toMap
  }

  def substitute(t: Type, substitutions: immutable.Map[ArithExpr,ArithExpr]) : Type = {
    Type.visitRebuild(t, t1 => t1, {
      case ArrayType(et,len) => new ArrayType(et, ArithExpr.substitute(len, substitutions.toMap))
      case VectorType(st,len) => new VectorType(st, ArithExpr.substitute(len, substitutions.toMap))
      case t: Type => t
    })
  }

  private def closedFormIterate(inT: Type, ouT: Type, n: ArithExpr, tvMap : scala.collection.mutable.HashMap[TypeVar, ArithExpr]) : Type = {
    (inT,ouT) match {
      case (inAT : ArrayType, outAT : ArrayType) =>
        val closedFormLen = {
          val inLen = ExprSimplifier(inAT.len)
          val outLen = ExprSimplifier(outAT.len)

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
              val a = ExprSimplifier(outLen /^ tv)
              if (!ArithExpr.contains(a, tv)) {

                // fix the range for tv
                // TODO: Pow(a, n) or Pow(a, n-1)???
                val (min, max) = ArithExpr.minmax(tvMap.get(tv).get, ExprSimplifier(Pow(a, n)*tvMap.get(tv).get))
                // TODO: deal with growing output size
                tv.range = ContinuousRange(min,max)

                // we have outLen*tv where tv is not present inside outLen
                Pow(a, n)*tv
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


  def isEqual(l: Type, r: Type): Boolean = {
    (l, r) match {
      case (lst: ScalarType, rst: ScalarType) => isEqual(lst, rst)
      case (ltt: TupleType, rtt: TupleType) => isEqual(ltt, rtt)
      case (lat: ArrayType, rat: ArrayType) => isEqual(lat, rat)
      case (lvt: VectorType, rvt: VectorType) => isEqual(lvt, rvt)
      case _ => false
    }
  }

  private def isEqual(l: ScalarType, r: ScalarType): Boolean = {
    l.size == r.size && l.name == r.name
  }

  private def isEqual(lt: TupleType, rt: TupleType): Boolean = {
    if (lt.elemsT.length != rt.elemsT.length) return false

    (lt.elemsT zip rt.elemsT).forall({ case (l,r) => isEqual(l, r) })
  }

  private def isEqual(l: ArrayType, r: ArrayType): Boolean = {
    l.len == r.len && isEqual(l.elemT, r.elemT)
  }

  private def isEqual(l: VectorType, r: VectorType): Boolean = {
    l.len == r.len && isEqual(l.scalarT, r.scalarT)
  }


  def getTypeAtIndex(t: Type, index: Int): Type = {
    t match {
      case tt: TupleType =>
        assert(index < tt.elemsT.length)
        tt.elemsT(index)
      case at: ArrayType =>
        ArrayType(getTypeAtIndex(at.elemT, index), at.len)
      case _ => t
    }
  }

  def check(expr: Expr, setType: Boolean = true): Type = {

    var inferredOuT = expr match {
      case param: Param => checkParam(param)
      case call: FunCall => checkFunCall(call, setType)
    }

    inferredOuT = inferredOuT match {
      case ArrayType(et, len) => ArrayType(et, ExprSimplifier(len))
      case _ => inferredOuT
    }

    if (setType)
      expr.t = inferredOuT

    inferredOuT
  }

  private def checkParam(param: Param): Type = {
    param match {
      case pr: ParamReference => getTypeAtIndex(pr.p.t, pr.i)
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
      case cf: CompFun =>      checkCompFunDef(cf, inT, setType)
      case z: Zip =>              checkZip(z, inT, setType)
      case t: Tuple =>            checkTuple(t, inT, setType)
      case uz: Unzip =>           checkUnzip(uz, inT, setType)
      case Split(n) =>            checkSplit(n, inT)
      case _: Join =>             checkJoin(inT)
      case _: asScalar  =>        checkAsScalar(inT)
      case asVector(n) =>         checkAsVector(n, inT)
      case uf: UserFun =>      checkUserFunDef(uf, inT)
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
      case Barrier() | Gather(_) | Scatter(_) => inT
    }
  }

  private def getInTFromArgs(call: FunCall, setType: Boolean): Type = {
    if (call.args.isEmpty) {
      NoType
    } else if (call.args.length == 1) {
      check(call.args(0), setType)
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
        am.f.params(0).t = getElemT(inT)
        ArrayType(check(am.f.body, setType), getLength(inT))
      case mt: MatrixType =>
        if (am.f.params.length != 1) throw new NumberOfArgumentsException
        am.f.params(0).t = getElemT(inT)
        //recursively check the type - this won't work on nested maps
        //(e.g. mapWrg over rows, then mapLcl over elements)
        //as it's not expecting to be passed to nested maps
        MatrixType(check(am.f.body, setType), mt.dx, mt.dy)
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkReduce(ar: AbstractPartRed, inT: Type, setType: Boolean): Type = {
    inT match {
      case tt: TupleType =>
        if (tt.elemsT.length != 2) throw new NumberOfArgumentsException // get the input type to the reduction, got to be tuple of array + initial val
        val initT = tt.elemsT.head //get the type of the initial value
        val elemT = getElemT(tt.elemsT(1)) // get the type of the array values to the reduce
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
        case at1: ArrayType => ArrayType(at1.elemT, ExprSimplifier(at0.len * at1.len))
        case _ => throw new TypeException(at0.elemT, "ArrayType")
      }
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkGroup(group: Group, inT: Type): Type = {
    inT match {
      case at: ArrayType =>
        assert(group.params.length == 1)
        group.params(0).t = ArrayType(ArrayType(at.elemT, group.relIndices.length), at.len)
        group.params(0).t

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
      case at: ArrayType => asScalar(at)
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
    val substitutions = reify(uf.inT, inT)
    substitute(uf.outT, substitutions.toMap)
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
        var inputTypeWithTypeVar = visitRebuild(at, t => t, {
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
        visit(outputTypeWithTypeVar, t => {}, {
          case at: ArrayType => outputTvSet ++= TypeVar.getTypeVars(at.len)
          case vt: VectorType => outputTvSet ++= TypeVar.getTypeVars(vt.len)
          case _ =>
        })

        // put back the expression when the type variable is not present
        val fixedTvMap = tvMap -- outputTvSet
        inputTypeWithTypeVar = substitute(inputTypeWithTypeVar, fixedTvMap.toMap)

        // assign the type for f
        check(i.f.body, setType = true)

        val closedFormOutputType = closedFormIterate(inputTypeWithTypeVar, outputTypeWithTypeVar, i.n, tvMap)
        substitute(closedFormOutputType, tvMap.toMap)

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

  @deprecated("replaced by Type.vectorize(n)")
  def vectorize(t: Type, n: ArithExpr): Type = t.vectorize(n)

  def devectorize(t: Type): Type = {
    t match {
      case vt: VectorType => vt.scalarT
      case tt: TupleType => TupleType( tt.elemsT.map( devectorize ):_* )
      case at: ArrayType => ArrayType(devectorize(at.elemT), at.len)
      case mt: MatrixType => MatrixType(devectorize(mt.elemT), mt.dx, mt.dy)
      case _ => t
    }
  }

}

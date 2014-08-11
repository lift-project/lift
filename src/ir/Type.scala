package ir

import opencl.ir._
import scala.collection.mutable
import scala.collection.immutable


case class TypeException(msg: String) extends Exception(msg) {
  def this() = this("")
  def this(found: Type, expected: String) = this(found + " found but " + expected + " expected")
  def this(found: Type, expected: Type) = this(found + " found but " + expected + " expected")

}

case class NumberOfArgumentsException(msg: String) extends Exception(msg) {
  def this()  = this("number of arguments is wrong!")
}


sealed abstract class Type

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

//case class UnboundArrayType(et: Type, te: TypeExpr) extends ArrayType(et)
//case class BoundArrayType(et: Type, n: Int) extends ArrayType(et)

object UndefType extends Type {override def toString = "UndefType"}

object NoType extends Type {override def toString = "NoType"}

object Type {
  
  /*def visitExpr(t: Type, pre: (Expr) => (Unit), post: (Expr) => (Unit)) : Unit = {    
    t match {
      case at: ArrayType => {
        pre(at.len) 
        visitExpr(at.elemT, pre, post)
        post(at.len)
      }
      case tt: TupleType => tt.elemsT.map(et => visitExpr(et,pre,post))              
      case _ => //throw new NotImplementedError()
    }
  } */

  def visitRebuild(t: Type, pre: (Type) => (Type), post: (Type) => (Type)) : Type = {
    var newT = pre(t)
    newT = newT match {
      case at: ArrayType => new ArrayType(visitRebuild(at.elemT, pre, post), at.len)
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
      case vt: VectorType => visit(vt.scalarT, pre, post)      
      case tt: TupleType => tt.elemsT.map(et => visit(et,pre,post))
      case _ => // nothing to do
    }
    post(t)
  }  
  
  def getElemT(t: Type): Type = {
    t match {
      case at: ArrayType => at.elemT
      case vt: VectorType => vt.scalarT
      case _ => throw new TypeException(t, "ArrayType")
    }
  }
  
  def getLength(t: Type) : ArithExpr = {
    t match {
      case at: ArrayType => at.len
      case st: ScalarType => Cst(1)
      case vt: VectorType => Cst(1)
      case tt: TupleType => Cst(1)
      case _ => throw new TypeException(t, "ArrayType")
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
  
  private def asVector(at0: ArrayType, len: ArithExpr): Type = {
    at0.elemT match {      
      case pt:ScalarType => new ArrayType(new VectorType(pt,len), at0.len/len)
      case at1:ArrayType => new ArrayType(asVector(at1,len), at0.len)
      case _ => throw new TypeException(at0.elemT, "ArrayType or PrimitiveType")
    }
  }
  
  def length(t: Type, array: Array[ArithExpr] = Array.empty[ArithExpr]) : Array[ArithExpr] = {
    t match {
      case ArrayType(elemT, len) => Type.length(elemT, array :+ len)
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
          val inLen = ExprSimplifier.simplify(inAT.len)
          val outLen = ExprSimplifier.simplify(outAT.len)
          if (inLen == outLen)
            return ouT

          inLen match {
            case tv: TypeVar =>
              // recognises output independent of tv
              if (!ArithExpr.contains(outLen, tv))
               return ouT

              // recognises outLen*tv
              val a = ExprSimplifier.simplify(outLen / tv)
              if (!ArithExpr.contains(a, tv)) {

                // fix the range for tv
                val max = ArithExpr.max(tvMap.get(tv).get, ExprSimplifier.simplify(Pow(a, n)*tvMap.get(tv).get))
                val min = ArithExpr.min(tvMap.get(tv).get, ExprSimplifier.simplify(Pow(a, n)*tvMap.get(tv).get))
                tv.range = ContinousRange(min,max)

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

  def check(expr: Expr, inputT: Type, setType: Boolean = true): Type = {

    var inferredOuT = expr match {
      case param: Param => checkParam(param, inputT)
      case call: FunCall => checkFunCall(call, inputT, setType)
    }

    inferredOuT = inferredOuT match {
      case ArrayType(et, len) => ArrayType(et, ExprSimplifier.simplify(len))
      case _ => inferredOuT
    }

    if (setType)
      expr.outT = inferredOuT

    inferredOuT
  }

  private def checkParam(param: Param, inputT: Type): Type = {
    // The order is important, as a set outT should always be picked over any inferred inputT!
    if (param.outT != UndefType) param.outT
    else if (inputT != NoType) inputT
    else throw new TypeException(inputT, "some type")
  }

  private def checkFunCall(call: FunCall, inputT: Type, setType: Boolean): Type = {
    assert(call.f != null)

    val inT = inferInputType(call, inputT, setType)
    if (setType)
      call.inT = inT

    call.f match {
      case l: Lambda =>           check(l.body, inT, setType)
      case am: AbstractMap =>     checkMap(am, inT, setType)
      case ar: AbstractPartRed => checkReduce(ar, inT, setType)
      case cf: CompFunDef =>      checkCompFunDef(cf, inT, setType)
      case z: Zip =>              checkZip(z, inT, setType)
      case Split(n) =>            checkSplit(n, inT)
      case _: Join =>             checkJoin(inT)
      case _: asScalar  =>        checkAsScalar(inT)
      case asVector(n) =>         checkAsVector(n, inT)
      case uf: UserFunDef =>      checkUserFunDef(uf, inT)
      case tL: toLocal =>         check(tL.f.body, inT, setType)
      case tG: toGlobal =>        check(tG.f.body, inT, setType)
      case i: Iterate =>          checkIterate(i, inT)
      case _: ReorderStride =>    inT
    }
  }

  private def inferInputType(call: FunCall, inputT: Type, setType: Boolean): Type = {
    val inTs = if (call.args.nonEmpty) {
      // check arguments and get the output types from there
      inputT match {
        // unpack tuple
        case tt: TupleType =>
          (call.args zip tt.elemsT).map( {
            case (a,t) => check(a, t, setType)
          } )
        case t: Type => call.args.map(check(_, t, setType))
      }
    } else {
      Seq(inputT)
    }

    if (inTs.length == 1) inTs(0) else TupleType(inTs:_*)
  }

  private def checkMap(am: AbstractMap, inT: Type, setType: Boolean): Type = {
    inT match {
      case at: ArrayType =>
        val elemT = getElemT(inT)
        ArrayType(check(am.f.body, elemT, setType), getLength(inT))
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkReduce(ar: AbstractPartRed, inT: Type, setType: Boolean): Type = {
    inT match {
      case tt: TupleType =>
        if (tt.elemsT.length != 2) throw new NumberOfArgumentsException
        val elemT = getElemT(tt.elemsT(1))
        val initT = tt.elemsT(0)
        check(ar.f.body, TupleType(initT, elemT), setType)
        ArrayType(initT, new Cst(1))
      case _ => throw new TypeException(inT, "TupleType")
    }
  }

  private def checkCompFunDef(cf: CompFunDef, inT: Type, setType: Boolean): Type = {
    inT match {
      case at: ArrayType =>
        // combine the parameter of the first function to call with the type inferred from the argument
        cf.funs.last.params(0).outT = inT
        cf.funs.foldRight(inT)((f, inputT) => check(f.body, inputT, setType))
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkZip(z: Zip, inT: Type, setType: Boolean): Type = {
    inT match {
      case tt: TupleType =>
        if (tt.elemsT.length != 2) throw new NumberOfArgumentsException

        val at1 = tt.elemsT(0) match {
          case at: ArrayType => at
          case _ => throw new TypeException(tt.elemsT(0), "ArrayType")
        }
        val at2 = tt.elemsT(1) match {
          case at: ArrayType => at
          case _ => throw new TypeException(tt.elemsT(1), "ArrayType")
        }
        if (at1.len != at2.len) {
          println("Warning: can not statically proof that sizes (" + at1 + " and " + at2 + ") match!")
          // throw TypeException("sizes do not match")
        }
        ArrayType(TupleType(at1.elemT, at2.elemT), at1.len)
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

  private def checkSplit(n: ArithExpr, inT: Type): Type = {
    inT match {
      case at: ArrayType => ArrayType(ArrayType(at.elemT, n), at.len / n)
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
      case at: ArrayType => asVector(at, n)
      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  private def checkUserFunDef(uf: UserFunDef, inT: Type): Type = {
    val substitutions = reify(uf.inT, inT)
    substitute(uf.outT, substitutions.toMap)
  }

  private def checkIterate(i: Iterate, inT: Type): Type = {
    inT match {
      case at: ArrayType =>
        // substitute all the expression in the input type with type variables
        val tvMap = scala.collection.mutable.HashMap[TypeVar, ArithExpr]()
        var inputTypeWithTypeVar = visitRebuild(at, t => t, {
            case at: ArrayType =>
              val tv = TypeVar()
              tvMap += tv -> at.len
              new ArrayType(at.elemT, tv)
            /*
            case vt: VectorType =>
              val tv = TypeVar()
              tvMap += tv -> vt.len
              new VectorType(vt.scalarT, tv)
            */
            case t: Type => t
          })

        val outputTypeWithTypeVar = check(i.f.body, inputTypeWithTypeVar, setType = false)

        // find all the type variable in the output type
        val outputTvSet = scala.collection.mutable.HashSet[TypeVar]()
        visit(outputTypeWithTypeVar, t => {}, {
          case at: ArrayType => outputTvSet ++= ArithExpr.getTypeVars(at.len)
          case vt: VectorType => outputTvSet ++= ArithExpr.getTypeVars(vt.len)
          case _ =>
        })

        // put back the expression when the type variable is not present
        val fixedTvMap = tvMap -- outputTvSet
        inputTypeWithTypeVar = substitute(inputTypeWithTypeVar, fixedTvMap.toMap)

        // assign the type for f
        check(i.f.body, inputTypeWithTypeVar)

        val closedFormOutputType = closedFormIterate(inputTypeWithTypeVar, outputTypeWithTypeVar, i.n, tvMap)
        substitute(closedFormOutputType, tvMap.toMap)

      case _ => throw new TypeException(inT, "ArrayType")
    }
  }

  def vectorize(t: Type, n: ArithExpr): Type = {
    t match {
      case sT: ScalarType => new VectorType(sT, n)
      case tT: TupleType => new TupleType( tT.elemsT.map( vectorize(_, n) ):_* )
      case aT: ArrayType => asVector(aT, n)
      case _ => t // throw new TypeException(t, "anything else")
    }
  }

  def devectorize(t: Type): Type = {
    t match {
      case vt: VectorType => vt.scalarT
      case tt: TupleType => TupleType( tt.elemsT.map( devectorize ):_* )
      case at: ArrayType => ArrayType(devectorize(at.elemT), at.len)
      case _ => t
    }
  }

}
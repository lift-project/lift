package ir

// hm ...
import opencl.ir._
import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

case class TypeException(msg: String) extends Exception(msg) {
  def this() = this("")
  def this(found: Type, expected: String) = this(found + " found but " + expected + " expected")
  def this(found: Type, expected: Type) = this(found + " found but " + expected + " expected")

}


sealed abstract class Type

case class ScalarType(val name: String, val size: Expr) extends Type {
  override def toString = name
}

// TODO: Is the VectorType OpenCL specific? If yes -> move to opencl.ir package
case class VectorType(val scalarT: ScalarType, val len: Expr) extends Type

case class TupleType(val elemsT: Type*) extends Type {
  override def toString = "Tuple(" + elemsT.map(_.toString).reduce(_ + ", " + _) + ")"
}

case class ArrayType(val elemT: Type, val len: Expr) extends Type {
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
  
  def getLength(t: Type) : Expr = {
    t match {
      case at: ArrayType => at.len
      case st: ScalarType => Cst(1)
      case vt: VectorType => Cst(1)
      case tt: TupleType => Cst(1)
      case _ => throw new TypeException(t, "ArrayType")
    }
  }

  def getVectorSize(t: Type): Expr = {
    t match {
      case vt: VectorType => vt.len
      case _ => Cst(1)
    }
  }

  def changeLength(t: Type, length: Expr): Type = {
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
  
  private def asVector(at0: ArrayType, len: Expr): Type = {
    at0.elemT match {      
      case pt:ScalarType => new ArrayType(new VectorType(pt,len), at0.len/len)
      case at1:ArrayType => new ArrayType(asVector(at1,len), at0.len)
      case _ => throw new TypeException(at0.elemT, "ArrayType or PrimitiveType")
    }
  }
  
  def length(t: Type, array: Array[Expr] = Array.empty[Expr]) : Array[Expr] = {
    t match {
      case ArrayType(elemT, len) => Type.length(elemT, array :+ len)
      case TupleType(_) => throw new TypeException(t, "ArrayType")
      case VectorType(_, len) => array :+ len
        //throw new TypeException(t, "ArrayType") // TODO: Think about what to do with vector types
      case _ => array
    }
  }

  def reifyExpr(e1: Expr, e2: Expr) : immutable.Map[TypeVar, Expr] = {
    val result = mutable.Map[TypeVar, Expr]()
    (e1,e2) match {
      case (tv: TypeVar, _) => result += (tv->e2)
      case (_, tv: TypeVar) => result += (tv->e1)
      case _ => // todo check that the two expressions are equivalent
    }
    result.toMap
  }

  // Throw an exception if the two types do not match
  def reify(t1: Type, t2: Type): immutable.Map[TypeVar, Expr] = {
    val result = mutable.Map[TypeVar, Expr]()
    (t1, t2) match {
      case (at1: ArrayType, at2: ArrayType) => result ++= reifyExpr(at1.len, at2.len) ++= reify(at1.elemT, at2.elemT)
      case (tt1: TupleType, tt2: TupleType) => result ++= tt1.elemsT.zip(tt2.elemsT).foldLeft(mutable.Map[TypeVar, Expr]())((map, types) => map ++= reify(types._1, types._2))
      case (vt1: VectorType, vt2: VectorType) => result ++= reifyExpr(vt1.len, vt2.len)
      case _ => if (t1 != t2) throw new TypeException(t1, t2)
    }
    result.toMap
  }

  def substitute(t: Type, substitutions: immutable.Map[Expr,Expr]) : Type = {
    Type.visitRebuild(t, t1 => t1, t1 => {
      t1 match {
        case ArrayType(et,len) => new ArrayType(et, Expr.substitute(len, substitutions.toMap))
        case VectorType(st,len) => new VectorType(st, Expr.substitute(len, substitutions.toMap))
        case _ => t1
      }

    })
  }

 /* def isSubtype(top: Type, sub: Type) : Boolean = {
    (top,sub) match {
      case (tat : ArrayType, sat : ArrayType) => {
        val tatLen = ExprSimplifier.simplify(tat.len)
        val satlen = ExprSimplifier.simplify(sat.len)
        val lenIsSubtype = tatLen match {
          case TypeVar(_) => true
          case _ => (tatLen == satlen)
        }
        if (!lenIsSubtype)
          false
        else
          isSubtype(tat.elemT,sat.elemT)
      }
      case (ttt:TupleType, stt:TupleType) =>
        val pairs = ttt.elemsT.zip(stt.elemsT)
        pairs.foldLeft(true)((result,pair) => if (!isSubtype(pair._1,pair._2)) return false else true)

      case _ => top == sub
    }
  }*/

  private def closedFormIterate(inT: Type, ouT: Type, n: Expr, tvMap : scala.collection.mutable.HashMap[TypeVar, Expr]) : Type = {
    (inT,ouT) match {
      case (inAT : ArrayType, outAT : ArrayType) => {

        val closedFormLen = {
          val inLen = ExprSimplifier.simplify(inAT.len)
          val outLen = ExprSimplifier.simplify(outAT.len)
          if (inLen == outLen)
            return ouT

          inLen match {
            case tv: TypeVar => {

              // recognises output independent of tv
              if (!Expr.contains(outLen, tv))
               return ouT

              // recognises outLen*tv
              val a = ExprSimplifier.simplify(outLen / tv)
              if (!Expr.contains(a, tv)) {

                // fix the range for tv
                val max = Expr.max(tvMap.get(tv).get, ExprSimplifier.simplify(Pow(a, n)*tvMap.get(tv).get))
                val min = Expr.min(tvMap.get(tv).get, ExprSimplifier.simplify(Pow(a, n)*tvMap.get(tv).get))
                tv.range = ContinousRange(min,max)

                // we have outLen*tv where tv is not present inside outLen
                Pow(a, n)*tv
              }
              else throw new TypeException("Cannot infer closed form for iterate return type (only support x*a). inT = " + inT + " ouT = " + ouT)
            }
            case _ => throw new TypeException("Cannot infer closed form for iterate return type. inT = " + inT + " ouT = " + ouT)
          }
        }

        new ArrayType(closedFormIterate(inAT.elemT, outAT.elemT, n, tvMap), closedFormLen)

      }
      case (inTT:TupleType, outTT:TupleType) =>
        new TupleType(inTT.elemsT.zip(outTT.elemsT).map({case (tIn,tOut) => closedFormIterate(tIn,tOut,n, tvMap)} ) :_*)

      case _ => if (inT == ouT) ouT else throw new TypeException("Cannot infer closed form for iterate return type. inT = "+inT+" ouT = "+ouT)
    }
  }
  
  //def check(f: Fun) : Type = { check(f, UndefType) }
  
  def check(f: Fun, inT: Type, setType: Boolean = true): Type = {

    if (setType)
      f.inT = inT // set the input type

    // type inference
    var inferredOuT = f match {
                  
      case AbstractMap(inF) =>
        val elemT = getElemT(inT)
        ArrayType(check(inF, elemT, setType), getLength(inT))      
      
      case AbstractReduce(inF, initValue) =>
        val elemT = getElemT(inT)
        val initT = initValue.expectedOutT
        check(inF, TupleType(initT, elemT), setType)
        ArrayType(initT, new Cst(1))

      
      case PartRed(inF, initValue) =>
        val elemT = getElemT(inT)
        val initT = initValue.expectedOutT
        check(inF, TupleType(initT, elemT), setType)
        ArrayType(initT,?)
      
      case cf: CompFun =>
        cf.funs.last.inT = inT
        cf.funs.foldRight(inT)((f, inputT) => check(f, inputT, setType))

      case l: Lambda =>
        inT match {
          case NoType =>
          case tt: TupleType =>
            assert(tt.elemsT.length == l.params.length)
            (l.params,tt.elemsT).zipped.map( (p,t) => p.expectedOutT = t )
          case _ => l.params(0).expectedOutT = inT
        }
        check(l.f, inT, setType)

      case z : Zip => // zip ignores the inT (as input does ...)
        val t1 = check(z.f1, NoType, setType)
        val at1 = t1 match {
          case at: ArrayType => at
          case _ => throw new TypeException(t1, "ArrayType")
        }
        val t2 = check(z.f2, NoType, setType)
        val at2 = t2 match {
          case at: ArrayType => at
          case _ => throw new TypeException(t2, "ArrayType")
        }
        if (at1.len != at2.len) {
          println("Warning: can not statically proof that sizes (" + at1 + " and " + at2 + ") match!")
          // throw TypeException("sizes do not match")
        }
        ArrayType(TupleType(at1.elemT, at2.elemT), at1.len)


      case _:Join => inT match {
        case at0: ArrayType => at0.elemT match {
          case at1: ArrayType => ArrayType(at1.elemT, at0.len * at1.len)
          case _=>  throw new TypeException(at0.elemT, "ArrayType")
        }
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      
      case Split(cs) => inT match {
        case at: ArrayType => ArrayType(ArrayType(at.elemT,cs), at.len / cs)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      
      case _:asScalar  => inT match {
        case at: ArrayType => asScalar(at)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }

      case asVector(len) => inT match {
        case at: ArrayType => asVector(at, len)
        case _ =>  throw new TypeException(inT, "ArrayType")
      }
      
      case uf : UserFun => {
        val substitutions = reify(uf.expectedInT, inT)
        substitute(uf.expectedOutT, substitutions.toMap)
      }
      
      case input : Input => input.expectedOutT
      case param : Param => param.expectedOutT

      case tL:toLocal => check(tL.f, inT, setType)

      case tG:toGlobal => check(tG.f, inT, setType)

      case i : Iterate => inT match {
        case at: ArrayType => {


          // substitute all the expression in the input type with type variables
          val tvMap = scala.collection.mutable.HashMap[TypeVar, Expr]()
          var inputTypeWithTypeVar = visitRebuild(at, t => t, t =>
            t match {
              case at: ArrayType => {
                val tv = TypeVar()
                tvMap += tv -> at.len
                new ArrayType(at.elemT, tv)
              }
              /*
              case vt: VectorType => {
                val tv = TypeVar()
                tvMap += tv -> vt.len
                new VectorType(vt.scalarT, tv)
              }
              */
              case _ => t
            }
          )

          // type checking
          var outputTypeWithTypeVar = check(i.f, inputTypeWithTypeVar, false)

          // find all the type variable in the output type
          val outputTvSet = scala.collection.mutable.HashSet[TypeVar]()
          visit(outputTypeWithTypeVar, t => {}, {
              case at: ArrayType  => outputTvSet ++= Expr.getTypeVars(at.len)
              case vt: VectorType => outputTvSet ++= Expr.getTypeVars(vt.len)
              case _ =>
            }
          )

          // put back the expression when the type variable is not present
          val fixedTvMap = tvMap -- outputTvSet
          inputTypeWithTypeVar = substitute(inputTypeWithTypeVar, fixedTvMap.toMap)

          // assign the type for f
          check(i.f, inputTypeWithTypeVar)

          val closedFormOutputType = closedFormIterate(inputTypeWithTypeVar, outputTypeWithTypeVar, i.n, tvMap)
          substitute(closedFormOutputType, tvMap.toMap)
        }
        case _ => throw new TypeException(inT, "ArrayType")
      }

      case _: ReorderStride => inT

      //case vec: Vectorize => check(vec.f, inT, setType)
        // check(vectorize(vec.f, vec.n), inT, setType)

      case NullFun => inT // TODO: change this
      
      // TODO: continue
      //case _ => UndefType
    }


    inferredOuT = inferredOuT match {
      case ArrayType(et, len) => ArrayType(et, ExprSimplifier.simplify(len))
      case _ => inferredOuT
    }

    if (setType)
      f.ouT = inferredOuT

    inferredOuT

  }

  def vectorize(t: Type, n: Expr): Type = {
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
      case tt: TupleType => TupleType( tt.elemsT.map( devectorize(_) ):_* )
      case at: ArrayType => ArrayType(devectorize(at.elemT), at.len)
      case _ => t
    }
  }
/*
  def vectorize(f: Fun, n: Expr): Fun = {
    f match {
      case uf: UserFun => UserFun.vectorize(uf, n)
    }
  }
*/

}
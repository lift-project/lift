package ir

// hm ...
import opencl.ir._

import scala.collection.mutable.ArrayBuffer

case class TypeException(msg: String) extends Exception(msg) {
  def this() = this("")
  def this(found: Type, expected: String) = this(found + " found but " + expected + " expected")
}


sealed abstract class Type

case class ScalarType(val name: String, val size: Expr) extends Type {
  override def toString() = name
}

// TODO: Is the VectorType OpenCL specific? If yes -> move to opencl.ir package
case class VectorType(val scalarT: ScalarType, val len: Expr) extends Type

case class TupleType(val elemsT: Type*) extends Type {
  override def toString() = "(" + elemsT.map(_.toString).reduce(_ + ", " + _) + ")"
}

case class ArrayType(val elemT: Type, val len: Expr) extends Type {
  override def toString() = elemT + "[" + len + "]"
}

//case class UnboundArrayType(et: Type, te: TypeExpr) extends ArrayType(et)
//case class BoundArrayType(et: Type, n: Int) extends ArrayType(et)

object UndefType extends Type {override def toString() = "UndefType"}

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
  
  def visit(t: Type, pre: (Type) => (Unit), post: (Type) => (Unit)) : Unit = {
    pre(t)
    t match {
      case at: ArrayType => visit(at.elemT, pre, post)
      case vt: VectorType => visit(vt.scalarT, pre, post)      
      case tt: TupleType => tt.elemsT.map(et => visit(et,pre,post))
      case _ => throw new NotImplementedError()
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
      case _ => throw new TypeException(t, "ArrayType")
    }
  }
  
  def getSizeInBytes(t: Type) : Expr = {
    Expr.simplify(
      t match {
        case st: ScalarType => st.size
        case vt: VectorType => vt.len * getSizeInBytes(vt.scalarT)
        case at: ArrayType => at.len * getSizeInBytes(at.elemT)
        case tt: TupleType => tt.elemsT.map(getSizeInBytes).reduce(_ + _)
        case _ => throw new TypeException(t, "??")
      }
    )
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
      case VectorType(_, _) => throw new TypeException(t, "ArrayType") // TODO: Think about what to do with vector types
      case _ => array
    }
  }
  
  def check(f: Fun) : Type = { check(f, UndefType) }
  
  def check(f: Fun, inT: Type, setType: Boolean = true): Type = {

    if (setType)
      f.inT = inT // set the input type

    // type inference
    var inferredOuT = f match {
                  
      case AbstractMap(inF) => {
        val elemT = getElemT(inT)
        ArrayType(check(inF, elemT), getLength(inT))
      }
      
      case AbstractReduce(inF) => {
        val elemT = getElemT(inT)
        check(inF, TupleType(elemT, elemT)) // TODO change this probably
        ArrayType(elemT, new Cst(1))
      }
      
      case PartRed(inF) => {
        // TODO: check id !? 
        new ArrayType(getElemT(inT),?)
      }
      
      case cf: CompFun => {
        cf.funs.last.inT = inT
        cf.funs.foldRight(inT)((f, inputT) => check(f, inputT))        
      }

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
      
      case uf : UserFun => if (inT != uf.expectedInT) {
        throw new TypeException(inT, "UserFun does not macht " + uf.expectedInT )
      } else {
        uf.expectedOutT
      }
      
      case input : Input => input.expectedOutT

      case tL:toLocal => check(tL.f, inT)

      case tG:toGlobal => check(tG.f, inT)

      case i : Iterate => inT match {
        case at: ArrayType => {

          check(i.f, inT)

          // check that the output type can be used as an input
          check(i.f, i.f.ouT, false)

          // TODO: CD to implement logic to automatically infer the return type in the general case (not only for reduction)

          val inN = getLength(i.f.inT)
          val outN = getLength(i.f.ouT)

          // compute i.n times a over b (b is the reduction factor of one iteration, a is the input length)
          var a = inN
          var b = inN / outN
          for (index <- 1 to Expr.toInt(i.n)) {
            a = Expr.simplify(a / b)
          }

          new ArrayType(getElemT(inT), a)
        }
        case _ => throw new TypeException(inT, "ArrayType")
      }

      case _: ReorderStride => inT

      case vec: Vectorize => check(vec.f, inT) // Type.vectorize(vec.n, inT)

      // Type.vectorize(vec.n, inT)

      case NullFun => inT // TODO: change this
      
      // TODO: continue
      //case _ => UndefType
    }

    inferredOuT = inferredOuT match {
      case ArrayType(et, len) => ArrayType(et, Expr.simplify(len))
      case _ => inferredOuT
    }

    if (setType)
      f.ouT = inferredOuT

    inferredOuT
  }

  /*
  def vectorize(n: Expr, t: Type): Type = {
    t match {
      case sT: ScalarType => new VectorType(sT, n)
      case tT: TupleType => new TupleType( tT.elemsT.map( vectorize(n, _) ):_* )
      case aT: ArrayType => asVector(aT, n)
      case _ => throw new TypeException(t, "anything else")
    }
  }
  */

}
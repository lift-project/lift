package test

sealed abstract class Type

case class PrimitiveType() extends Type

case class TupleType(val elemsT: Type*) extends Type

case class ArrayType(val elemT: Type, val len: Expr) extends Type

//case class UnboundArrayType(et: Type, te: TypeExpr) extends ArrayType(et)
//case class BoundArrayType(et: Type, n: Int) extends ArrayType(et)

object UndefType extends Type {override def toString() = "UndefType"}

object Type {
  
  def getElemT(t: Type): Type = {
    t match {
      case at: ArrayType => at.elemT
      case _ => UndefType // Error!!!
    }
  }

  private def iJoin(at0: ArrayType): Type = {
    at0.elemT match {
      case at1:ArrayType => at1.elemT match {
        case _:ArrayType => new ArrayType(iJoin(at1),at0.len)
        case _ => new ArrayType(at1.elemT, at0.len*at1.len)
      }
      case _ => UndefType
    }
  }
  
  private def iSplit(at0: ArrayType, chunkSize: Expr): Type = {
    at0.elemT match {
      case at1:ArrayType => new ArrayType(iSplit(at1,chunkSize), at0.len)
      case _ => new ArrayType(new ArrayType(at0.elemT,chunkSize), at0.len/chunkSize)
    }
  }
  
  def check(f: Fun, inT: Type): Type = {

    f.inT = inT // set the input type

    // set the output type
    f.ouT = f match {
            
      case AbstractMap(inF) => {
        val elemT = getElemT(inT)
        check(inF, elemT)
        inT
      }
      
      case AbstractReduce(inF) => {
        val elemT = getElemT(inT)
        check(inF, TupleType(elemT, elemT)) // TODO change this probably
        ArrayType(elemT, new Cst(1))
      }
      
      case PartRed(inF) => {
        new ArrayType(getElemT(inT),?)
      }
      
      case cf: CompFun => {
        cf.funs.last.inT = inT
        cf.funs.foldRight(inT)((f, inputT) => check(f, inputT))        
      }

      case _:oJoin => inT match {
        case at0: ArrayType => at0.elemT match {
          case at1: ArrayType => new ArrayType(at1.elemT, at0.len * at1.len)
          case _=> UndefType
        }
        case _ => UndefType
      }
      
      case _:iJoin  => inT match {               
        case at: ArrayType => iJoin(at)
        case _ => UndefType  
      }
            
      case oSplit(cs) => inT match {
        case at: ArrayType => new ArrayType(new ArrayType(at.elemT,cs), at.len / cs)
        case _ => UndefType
      }
                          
      case iSplit(cs) => inT match {
        case at: ArrayType => iSplit(at, cs)
        case _ => UndefType  
      }

      case NullFun => inT // TODO: change this
      
      // TODO: continue
      //case _ => UndefType
    }
    
    f.ouT = f.ouT match {
      case ArrayType(et, len) => ArrayType(et, Expr.simplify(len))
      case _ => f.ouT
    }

    f.ouT
  }
}
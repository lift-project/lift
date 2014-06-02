package test

sealed abstract class Type

case class TupleType(val elemsT: Type*) extends Type

abstract class ArrayType(val elemT: Type) extends Type {
  //def unapply(at: ArrayType): Option[(Type)] = Some(at.elemT)
}

case class UnboundArrayType(et: Type) extends ArrayType(et)
case class BoundArrayType(et: Type, n: Int) extends ArrayType(et)

object UndefType extends Type

object Type {

  def getElemT(t: Type): Type = {
    t match {
      case at: ArrayType => at.elemT
      case _ => UndefType // Error!!!
    }
  }

  def check(f: Fun, inT: Type): Type = {

    f.inT = inT // set the input type

    f match {
      case AbstractMap(inF) => {
        val elemT = getElemT(inT)
        check(inF, elemT)
        f.ouT = inT
      }
      case AbstractReduce(inF) => {
        val elemT = getElemT(inT)
        check(inF, TupleType(elemT, elemT)) // TODO change this probably
        f.ouT = BoundArrayType(elemT,1)
      }

      // TODO: continue
      //case _ => UndefType
    }

    f.ouT
  }
}
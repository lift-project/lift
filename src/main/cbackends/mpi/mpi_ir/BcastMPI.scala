package cbackends.mpi.mpi_ir

import ir._
import ir.ast.Pattern
import ir.interpreter.Interpreter.ValueMap
import lift.arithmetic.{ArithExpr, Cst}

case class BcastMPI(root: ArithExpr = Cst(0), comm: MPI_Comm = MPI_Comm_world) extends Pattern(arity = 1) {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case at: ArrayType with Size with Capacity => ArrayTypeWSWC(at.elemT, at.size, at.capacity)

      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {//Vector[Vector[_]] = {
    assert(args.length == arity)
    printf(args.head.toString)
    args.head match {
      case v: Vector[_] => v
    }
  }
}

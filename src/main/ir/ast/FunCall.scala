package ir.ast

import ir.{Memory, TupleType, Type}
import opencl.ir.{OpenCLMemory, UndefAddressSpace, OpenCLMemoryCollection}


/** Function calls, ie.: map(f, x), zip(x, y), ...
  *
  * Refers back to the function decl (e.g. map(f)) and the arguments (e.g. x)
  */
case class FunCall(f: FunDecl, args: Expr*) extends Expr with Cloneable {
  assert(f != null)

  override def toString = {
    val fS = f.toString

    this match {
      case FunCall(Reduce(_), init, argCall@FunCall(_, _*)) => fS + s"($init) o " + argCall
      case FunCall(_, argCall@FunCall(_, _*)) => fS + " o " + argCall
      case _ => fS + "(" + args.mkString(", ") + ")"
    }
  }

  override def copy: FunCall = {
    this.clone().asInstanceOf[FunCall]
  }

  /**
   * One type for all arguments (i.e. a tuple if there are more than one args
   * */
  def argsType: Type = {
    if (args.length == 1) args(0).t
    else TupleType(args.map(_.t): _*)
  }

  def argsMemory: Memory = {
    if (args.length == 1) args(0).mem
    else OpenCLMemoryCollection(UndefAddressSpace,
                                args.map(_.mem.asInstanceOf[OpenCLMemory]): _*)
  }
}

object FunCallInst {
  def unapply(l: Lambda): Option[(Pattern,Expr)] = l match {
    case Lambda(_, FunCall(x, a)) if x.isInstanceOf[Pattern] => Some(x.asInstanceOf[Pattern],a)
    case _ => None
  }
}

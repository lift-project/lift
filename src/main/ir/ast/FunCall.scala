package ir.ast

import ir.interpreter.Interpreter.ValueMap
import ir.{Memory, TupleType, Type}
import opencl.ir.{OpenCLMemory, OpenCLMemoryCollection}


/** Function calls, ie.: map(f, x), zip(x, y), ...
  *
  * Refers back to the function decl (e.g. map(f)) and the arguments (e.g. x)
  */
case class FunCall(f: FunDecl, args: Expr*) extends Expr with Cloneable {
  assert(f != null)

  override def toString = {
    val fS = if (f == null) {
      "null"
    } else {
      f.toString
    }
    val argS =
      if (args.nonEmpty) args.map(_.toString).reduce(_ + ", " + _) else ""

    fS + "(" + argS + ")"
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
    else OpenCLMemoryCollection(args.map(_.mem.asInstanceOf[OpenCLMemory]))
  }

  override def eval(valueMap: ValueMap): Any = {
    val argValues = args.map(_.eval(valueMap))
    f match {
      case l: Lambda  => l.eval(valueMap, argValues:_*)

      case p: Pattern => p.eval(valueMap, argValues:_*)

      case uf: UserFun => uf.eval(valueMap, argValues:_*)
    }
  }
}

object FunCallInst {
  def unapply(l: Lambda): Option[(Pattern,Expr)] = l match {
    case Lambda(_, FunCall(x, a)) if x.isInstanceOf[Pattern] => Some(x.asInstanceOf[Pattern],a)
    case _ => None
  }
}

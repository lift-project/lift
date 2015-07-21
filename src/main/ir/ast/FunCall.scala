package ir.ast

import ir.{Memory, TupleType, Type}
import opencl.ir.{OpenCLMemory, UndefAddressSpace, OpenCLMemoryCollection}


/** Function calls, ie.: map(f, x), zip(x, y), ...
  *
  * Refers back to the function decl (e.g. map(f)) and the arguments (e.g. x)
  */
case class FunCall(f: FunDecl, args: Expr*) extends Expr with Cloneable {
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

  def apply(args: Expr*): FunCall = {
    val oldArgs = this.args
    val newArgs = oldArgs ++ args
    assert(newArgs.length <= f.params.length)

    new FunCall(f, newArgs: _*)
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

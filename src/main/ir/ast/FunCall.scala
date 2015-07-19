package ir.ast

import arithmetic.{RangeUnknown, ?, ArithExpr, Var}
import ir.{UnallocatedMemory, Memory, TupleType, Type}
import opencl.ir.{OpenCLMemory, UndefAddressSpace, OpenCLMemoryCollection}


/** Function calls, ie.: map(f, x), zip(x, y), ...
  *
  * Refers back to the function decl (e.g. map(f)) and the arguments (e.g. x)
  */
sealed class FunCall(val f: FunDecl, val args: Expr*) extends Expr with Cloneable {

  assert(if (f.isInstanceOf[Iterate]) {
    this.isInstanceOf[IterateCall]
  } else {
    true
  })

  override def toString = {
    val fS = if (f == null) {
      "null"
    } else {
      f.toString
    }
    val argS = if (args.nonEmpty) args.map(_.toString).reduce(_ + ", " + _) else ""

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

  /** One type for all arguments (i.e. a tuple if there are more than one args)*/
  def argsType: Type = {
    if (args.length == 1) args(0).t
    else TupleType(args.map(_.t): _*)
  }

  def argsMemory: Memory = {
    if (args.length == 1) args(0).mem
    else OpenCLMemoryCollection(UndefAddressSpace, args.map(_.mem.asInstanceOf[OpenCLMemory]): _*)
  }

}
// specific types of function calls ...

case class MapCall(name: String, loopVar: Var,
                   override val f: AbstractMap, override val args: Expr*) extends FunCall(f, args.head) {
  assert(args.length == 1)

  var iterationCount: ArithExpr = ?

  def arg: Expr = args(0)
}

case class ReduceCall(loopVar: Var,
                      override val f: AbstractPartRed,
                      override val args: Expr*) extends FunCall(f, args.head, args(1)) {
  assert(args.length == 2)

  var iterationCount: ArithExpr = ?

  def arg0: Expr = args(0)

  def arg1: Expr = args(1)
}

case class IterateCall(override val f: Iterate, override val args: Expr*) extends FunCall(f, args.head) {
  assert(args.length == 1)

  def arg: Expr = args(0)

  var iterationCount: ArithExpr = ?

  var swapBuffer: Memory = UnallocatedMemory
  var indexVar = Var("i", RangeUnknown)
}

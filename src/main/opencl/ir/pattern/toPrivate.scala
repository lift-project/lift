package opencl.ir.pattern

import ir._
import ir.ast._
import ir.interpreter.Interpreter._

case class toPrivate(f: Lambda) extends Pattern(arity = f.arity)
                                 with FPattern {

  override def copy(f: Lambda): Pattern = toPrivate(f)

  override def checkType(argType: Type, setType: Boolean): Type =
    f.checkType(argType, setType)

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = toPrivate(f.visitAndRebuild(pre,post).asInstanceOf[Lambda])

}

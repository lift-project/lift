package opencl.ir.pattern

import ir.Type
import ir.ast._
import ir.interpreter.Interpreter.ValueMap

case class toGlobal(f: Lambda) extends Pattern(arity = f.arity)
                                with FPattern {
  override def copy(f: Lambda): Pattern = toGlobal(f)

  override def checkType(argType: Type, setType: Boolean): Type =
    f.checkType(argType, setType)

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    f.eval(valueMap, args:_*)
  }

  override def _visit(prePost: IRNode => IRNode => Unit): Unit = f.visit_pp(prePost)
  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = toGlobal(f.visitAndRebuild(pre,post).asInstanceOf[Lambda])
}

package backends.spatial.spatial_host.ir

import ir.{Type, TypeChecker}
import ir.ast.{FPattern, Lambda, Pattern}
import ir.interpreter.Interpreter.ValueMap

case class AccelFun(override val f: Lambda)
  extends Pattern(arity = f.params.length) with FPattern {

  def copy(f: Lambda): Pattern = AccelFun(f)

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    TypeChecker.checkAndSetTypeForParams(f.params, argType, this)

    TypeChecker.check(f.body, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    throw new NotImplementedError()
  }
}

package ir

import opencl.ir._

abstract class Expr {
  var context : Context = null

  // type information
  var inT: Type = UndefType
  var outT: Type = UndefType

  // memory information
  var inM: Memory = UnallocatedMemory
  var outM: Memory = UnallocatedMemory

  // memory access information
  var inAccess: AccessFunctions = EmptyAccessFuntions
  var outAccess: AccessFunctions = EmptyAccessFuntions


  def setContext(ctx: Context): Expr = {
    if (ctx != null)
      this.context = ctx
    this
  }

  def copy : Expr
}

sealed class FunCall(val f : FunDecl, val args : Expr*) extends Expr with Cloneable {

  assert( if (f.isInstanceOf[Iterate]) {
    this.isInstanceOf[IterateCall]
  } else {
    true
  } )

  override def toString = {
    val fS = if (f == null) { "null" } else { f.toString }
    val argS = if (args.length > 0) args.map(_.toString).reduce(_ + ", " + _) else ""

    fS + "(" + argS + ")"
  }

  override def copy: FunCall = {
    //val c = new FunExpr(this.f, this.args:_*)
    //c.context = this.context
    //c.inT = this.inT
    //c.ouT = this.ouT
    this.clone().asInstanceOf[FunCall]
  }

  def apply(args : Expr*) : FunCall = {
    val oldArgs = this.args
    val newArgs = oldArgs ++ args
    assert (newArgs.length <= f.params.length)

    new FunCall(f, newArgs:_*)
  }

}

class ParamReference(val p: Param, val i: Int) extends Param {
  override def toString = "PARAM REF"
}

object Get {
  def apply(p: Param, i: Int): ParamReference = new ParamReference(p, i)
}

class Param() extends Expr {
  outT = UndefType

  override def toString = "PARAM"

  override def copy: Param =  this.clone().asInstanceOf[Param]

  /*
  def get(index: Int): Param = {
    assert(outT.isInstanceOf[TupleType])
    assert(outM.isInstanceOf[OpenCLMemoryCollection])
    assert(outAccess.isInstanceOf[AccessFunctionsCollection])

    val t = outT match { case tt: TupleType => tt }
    val m = outM match { case coll: OpenCLMemoryCollection => coll }
    val a = outAccess match { case coll: AccessFunctionsCollection => coll }

    assert(index < t.elemsT.length && index < m.subMemories.length && index < a.elems.length)

    val p = new Param

    p.outT = t.elemsT(index)
    p.outM = m.subMemories(index)
    p.outAccess = a.elems(index)

    p
  }
  */
}

object Param {
  def apply(): Param = new Param

  def apply(outT: Type): Param = {
    val p = Param()
    p.outT =outT
    p
  }
}

case class Value(var value: String) extends Param {
  override def copy: Value =  this.clone().asInstanceOf[Value]
  
  override def toString = "VALUE"
}

object Value {
  def apply(outT: Type): Value = {
    val v = Value("")
    v.outT = outT
    v
  }

  def apply(value: String, outT: Type): Value = {
    val v = Value(value)
    v.outT = outT
    v
  }

  def vectorize(v: Value, n: ArithExpr): Value = {
    Value(v.value, Type.vectorize(v.outT, n))
  }
}

case class IterateCall(override val f: Iterate, arg: Expr) extends FunCall(f, arg) {
  var swapBuffer: Memory = UnallocatedMemory
}

case class MapCall(name: String, loopVar: Var, override val f: AbstractMap, arg: Expr) extends FunCall(f, arg)

case class ReduceCall(loopVar: Var, override val f: AbstractPartRed, arg0: Expr, arg1: Expr) extends FunCall(f, arg0, arg1)

object Expr {

  implicit def IntToValue(i: Int) = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float) = Value(f.toString + "f", opencl.ir.Float)

  def replace(e: Expr, oldE: Expr, newE: Expr) : Expr =
    visit (e, (e:Expr) => if (e.eq(oldE)) newE else oldE, (e:Expr) => e)

  
  def visit[T](z:T)(expr: Expr, visitFun: (Expr,T) => T): T = {
    val result = visitFun(expr,z)
    expr match {
      case call: FunCall =>
        // visit args first
        val newResult = call.args.foldRight(result)( (arg,x) => visit(x)(arg, visitFun) )

        // do the rest ...
        call.f match {
          case fp: FPattern => visit(newResult)(fp.f.body, visitFun)
          case cf: CompFunDef => cf.funs.foldRight(newResult)((inF,x)=>visit(x)(inF.body, visitFun))
          case l: Lambda => visit(newResult)(l.body, visitFun)
          case _ => newResult
        }
      case _ => result
    }
  }
  
  /*
   * Visit the expression of function f (recursively) and rebuild along the way
   */
  def visitArithExpr(expr: Expr, exprF: (ArithExpr) => (ArithExpr)) : Expr = {
    visit(expr,
          (inExpr: Expr) => inExpr match {
            case call: FunCall =>
              call.f match {
                case Split(e) => Split(exprF(e))(call.args:_*)
                case asVector(e) => asVector(exprF(e))(call.args:_*)
                case _ => inExpr
              }
            case _ => inExpr
            },
          (inExpr: Expr) => inExpr)
  }
  
  /*
   * Visit the function f recursively and rebuild along the way
   */
   def visit(expr: Expr, pre: (Expr) => (Expr), post: (Expr) => (Expr)) : Expr = {
    var newExpr = pre(expr)
    newExpr = newExpr match {
      case call: FunCall =>
        val newArgs = call.args.map( (arg) => visit(arg, pre, post) )
        call.f match {
          case cf: CompFunDef => CompFunDef(cf.funs.map(inF => new Lambda(inF.params, visit(inF.body, pre, post)) ):_*)(newArgs:_*)
          case ar: AbstractPartRed => ar.getClass.getConstructor(classOf[Lambda],classOf[Value]).newInstance(visit(ar.f.body, pre, post),ar.init)(newArgs:_*)
          case fp: FPattern => fp.getClass.getConstructor(classOf[Expr]).newInstance(visit(fp.f.body,pre,post))(newArgs:_*)
          case _ => newExpr.copy
        }
      case _ => newExpr.copy
    }
    post(newExpr)
  }

  /*
   * Visit the function f recursively
   */
  def visit(expr: Expr, pre: (Expr) => (Unit), post: (Expr) => (Unit)): Unit = {
    pre(expr)
    expr match {
      case call: FunCall =>
        call.args.map( (arg) => visit(arg, pre, post) )

        call.f match {
          case fp: FPattern => visit(fp.f.body, pre, post)
          case l: Lambda => visit(l.body, pre, post)
          case cf: CompFunDef => cf.funs.reverseMap(inF => visit(inF.body, pre, post))
          case _ =>
        }
      case _ =>
    }
    post(expr)
  }
   
}

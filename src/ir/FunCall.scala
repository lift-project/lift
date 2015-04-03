package ir

import opencl.ir._

import language.implicitConversions

abstract class Expr {
  var context : Context = null

  // type information
  var t: Type = UndefType

  // memory information
  var mem: Memory = UnallocatedMemory

  // view explaining how to access the memory
  var view: View = NoView

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
    //c.ouT = this.ouT
    this.clone().asInstanceOf[FunCall]
  }

  def apply(args : Expr*) : FunCall = {
    val oldArgs = this.args
    val newArgs = oldArgs ++ args
    assert (newArgs.length <= f.params.length)

    new FunCall(f, newArgs:_*)
  }

  // One type for all arguments (i.e. a tuple if there are more than one args)
  def argsType: Type = {
    if (args.length == 1) args(0).t
    else TupleType( args.map(_.t):_* )
  }

  def argsMemory: Memory = {
    if (args.length == 1) args(0).mem
    else OpenCLMemoryCollection( UndefAddressSpace, args.map(_.mem.asInstanceOf[OpenCLMemory]):_* )
  }

}

class ParamReference(val p: Param, val i: Int) extends Param {
  override def toString = "PARAM REF"
}

object Get {
  def apply(p: Param, i: Int): ParamReference = new ParamReference(p, i)
}

class Param() extends Expr {
  t = UndefType

  override def toString = "PARAM"

  override def copy: Param =  this.clone().asInstanceOf[Param]
}

class VectorParam(val p: Param, n: ArithExpr) extends Param {
  t = Type.vectorize(p.t, n)
}

object Param {
  def apply(): Param = new Param

  def apply(outT: Type): Param = {
    val p = Param()
    p.t =outT
    p
  }

  def vectorize(p: Param, n: ArithExpr): Param = {
    new VectorParam(p, n)
  }
}

case class Value(var value: String) extends Param {
  override def copy: Value =  this.clone().asInstanceOf[Value]
  
  override def toString = "VALUE"
}

object Value {
  def apply(outT: Type): Value = {
    val v = Value("")
    v.t = outT
    v
  }

  def apply(value: String, outT: Type): Value = {
    val v = Value(value)
    v.t = outT
    v
  }

  def vectorize(v: Value, n: ArithExpr): Value = {
    Value(v.value, Type.vectorize(v.t, n))
  }
}

case class IterateCall(override val f: Iterate, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 1)
  def arg: Expr = args(0)

  var swapBuffer: Memory = UnallocatedMemory
}

case class IterateFixedSizeCall(override val f: IterateFixedSize, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 1)
  def arg: Expr = args(0)

  var swapBuffer: Memory = UnallocatedMemory
}

case class HeadCall(override val f: Head, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 1)
  def arg: Expr = args(0)
}

case class TailCall(loopVar: Var, override val f: Tail, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 1)
  def arg: Expr = args(0)
}

case class MapCall(name: String, loopVar: Var, override val f: AbstractMap, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 1)

  def arg: Expr = args(0)
}

case class ReduceCall(loopVar: Var, override val f: AbstractPartRed, override val args: Expr*) extends FunCall(f, args(0), args(1)) {
  assert(args.length == 2)

  def arg0: Expr = args(0)
  def arg1: Expr = args(1)
}


case class DropLeftCall(loopVar: Var, override val f: AbstractDropLeft, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 1)
  def arg: Expr = args(0)
}

case class SearchCall(loopVar: Var, override val f: AbstractSearch, override val args: Expr*) extends FunCall(f, args(0)) {
  assert(args.length == 2)
  def arg0: Expr = args(0)
  def arg1: Expr = args(1)
}

object Expr {

  implicit def IntToValue(i: Int): Value = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float): Value = Value(f.toString + "f", opencl.ir.Float)

  implicit def Tuple2ToValue[T1 , T2](t : (T1, T2)): Value = {
    Value(t.toString().replace('(', '{').replace(')', '}'), TupleType(getType(t._1), getType(t._2)))
  }

  implicit def Tuple3ToValue[T1 , T2, T3](t : (T1, T2, T3)): Value = {
    Value(t.toString().replace('(', '{').replace(')', '}'), TupleType(getType(t._1), getType(t._2), getType(t._3)))
  }

  private def getType(a : Any) : Type = a match {
    case _ : Float => Float
    case _ : Int => Int
    case _ => throw new IllegalArgumentException
  }

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
    {
      case call: FunCall =>
        call.f match {
          case Split(e) => Split(exprF(e))(call.args:_*)
          case asVector(e) => asVector(exprF(e))(call.args:_*)
          case _ => call
        }
      case inExpr => inExpr
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
